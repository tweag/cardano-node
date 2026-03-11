{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implements a server that waits for an incoming connection to ChainSync or
-- BlockFetch, and forwards the resulting channels to a TMVar so they can be
-- picked up by the peer simulator.
module MiniProtocols (peerSimServer, forallVersionsN2N) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import           Ouroboros.Consensus.Node (stdVersionDataNTN)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.KeepAlive (keepAliveServer)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
                   OuroborosApplication (..), OuroborosApplicationWithMinimalCtx,
                   RunMiniProtocol (..))
import           Ouroboros.Network.NodeToNode (NodeToNodeVersionData (..), Versions (..))
import qualified Ouroboros.Network.NodeToNode as N2N
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.Handshake.Version (Version (..))
import           Ouroboros.Network.Protocol.KeepAlive.Server (keepAliveServerPeer)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Monad (forever)
import           Control.Monad.Class.MonadSay
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import           Data.Void (Void)
import qualified Network.Mux as Mux

import           Test.Consensus.PeerSimulator.Resources (BlockFetchResources (..),
                   ChainSyncResources (..), PeerResources (..))

peerSimServer ::
  forall m blk addr.
  ( IOLike m
  , SerialiseNodeToNodeConstraints blk
  , SupportedNetworkProtocolVersion blk
  , ShowProxy blk
  , ShowProxy (Header blk)
  , MonadSay m
  ) =>
  PeerResources m blk ->
  StrictTVar m Bool ->
  StrictTVar m Bool ->
  CodecConfig blk ->
  (NodeToNodeVersion -> addr -> CBOR.Encoding) ->
  (NodeToNodeVersion -> forall s. CBOR.Decoder s addr) ->
  NetworkMagic ->
  Versions
    NodeToNodeVersion
    NodeToNodeVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode addr BL.ByteString m Void ())
peerSimServer res csChanTMV bfChanTMV codecCfg encAddr decAddr networkMagic = do
  forallVersionsN2N (Proxy @blk) networkMagic $ \version blockVersion -> do
    let Consensus.N2N.Codecs
          { cKeepAliveCodec
          , cChainSyncCodec
          , cBlockFetchCodec
          } =
            Consensus.N2N.defaultCodecs codecCfg blockVersion encAddr decAddr version
    OuroborosApplication
      [ -- Responds to KeepAlive pings from the NUT. Started on demand by
        -- either side; without it the NUT would time out the connection.
        mkMiniProtocol
          Mux.StartOnDemandAny
          N2N.keepAliveMiniProtocolNum
          N2N.keepAliveProtocolLimits
          $ MiniProtocolCb
          $ \_ctx channel ->
            runPeer nullTracer cKeepAliveCodec channel $
              keepAliveServerPeer keepAliveServer
        -- Serves the simulated chain to the NUT via the typed ChainSync
        -- server from the peer simulator. Signals readiness via csChanTMV
        -- so the test harness knows the NUT has connected to this peer.
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.chainSyncMiniProtocolNum
          N2N.chainSyncProtocolLimits
          $ MiniProtocolCb
          $ \_ctx channel -> do
            atomically $ writeTVar csChanTMV True
            runPeer nullTracer cChainSyncCodec channel
              $ chainSyncServerPeer $ csrServer $ prChainSync res
        -- Serves block bodies requested by the NUT after it has seen their
        -- headers via ChainSync. Signals readiness via bfChanTMV.
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.blockFetchMiniProtocolNum
          N2N.blockFetchProtocolLimits
          $ MiniProtocolCb
          $ \_ctx channel -> do
            atomically $ writeTVar bfChanTMV True
            runPeer nullTracer cBlockFetchCodec channel
              $ blockFetchServerPeer $ bfrServer $ prBlockFetch res
        -- Required by the N2N protocol set but unused in this test context:
        -- simulated peers do not receive transactions from the NUT.
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.txSubmissionMiniProtocolNum
          N2N.txSubmissionProtocolLimits
          $ MiniProtocolCb
          $ \_ctx _channel -> forever $ threadDelay 10
      ]

mkMiniProtocol
  :: Mux.StartOnDemandOrEagerly
  -> Mux.MiniProtocolNum
  -> (N2N.MiniProtocolParameters -> Mux.MiniProtocolLimits)
  -> MiniProtocolCb responderCtx bytes m b
  -> MiniProtocol Mux.ResponderMode initiatorCtx responderCtx bytes m Void b
mkMiniProtocol miniProtocolStart miniProtocolNum limits proto =
  MiniProtocol
    { miniProtocolNum
    , miniProtocolLimits = limits N2N.defaultMiniProtocolParameters
    , miniProtocolRun = ResponderProtocolOnly proto
    , miniProtocolStart
    }

-- | Builds a 'Versions' map suitable for use with 'N2N.connectTo' or
-- 'N2N.withServer'. It advertises every 'NodeToNodeVersion' that the given
-- block type declares as supported, each paired with 'NodeToNodeVersionData'
-- carrying the supplied 'NetworkMagic'. During the N2N handshake both peers
-- present their version maps and agree on the highest mutually supported
-- version; the supplied factory @mkR@ is then called with that version and
-- its corresponding 'BlockNodeToNodeVersion' to produce the application.
--
-- The version data is configured with 'InitiatorOnlyDiffusionMode' and
-- 'PeerSharingDisabled' — appropriate for a test client that connects
-- outward but does not participate in peer discovery.
forallVersionsN2N
  :: SupportedNetworkProtocolVersion blk
  => Proxy blk
  -> NetworkMagic
  -> (NodeToNodeVersion -> BlockNodeToNodeVersion blk -> r)
  -> Versions NodeToNodeVersion NodeToNodeVersionData r
forallVersionsN2N blk networkMagic mkR =
  Versions $
    flip Map.mapWithKey (supportedNodeToNodeVersions blk) $ \version blockVersion ->
      Version
        { versionApplication = const $ mkR version blockVersion
        , versionData =
            stdVersionDataNTN
              networkMagic
              N2N.InitiatorOnlyDiffusionMode
              PeerSharingDisabled
        }

