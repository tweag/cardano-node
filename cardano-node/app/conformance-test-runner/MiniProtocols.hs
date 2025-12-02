{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
module MiniProtocols (peerSimServer, queryClient) where

import           Cardano.Api.Internal.Block
import           Cardano.Api.Internal.IPC (ChainSyncClient (..), LocalChainSyncClient (..),
                   LocalNodeClientProtocols (..))

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Block as Consensus
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import           Ouroboros.Consensus.Ledger.SupportsMempool
import qualified Ouroboros.Consensus.Network.NodeToClient as Consensus
import qualified Ouroboros.Consensus.Network.NodeToClient as Consensus.N2C
import           Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import           Ouroboros.Consensus.Node (stdVersionDataNTC, stdVersionDataNTN)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import qualified Ouroboros.Consensus.Node.NetworkProtocolVersion as Consensus
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints,
                   SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block
import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.KeepAlive (keepAliveServer)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
                   OuroborosApplication (..), OuroborosApplicationWithMinimalCtx,
                   RunMiniProtocol (..), mkMiniProtocolCbFromPeer,
                   mkMiniProtocolCbFromPeerPipelined, mkMiniProtocolCbFromPeerSt)
import           Ouroboros.Network.NodeToClient (NodeToClientProtocols (..),
                   NodeToClientVersionData (..), chainSyncPeerNull, localStateQueryPeerNull,
                   localTxMonitorPeerNull, localTxSubmissionPeerNull, nodeToClientProtocols)
import qualified Ouroboros.Network.NodeToClient as N2C
import           Ouroboros.Network.NodeToNode (NodeToNodeVersionData (..), Versions (..))
import qualified Ouroboros.Network.NodeToNode as N2N
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.Protocol.BlockFetch.Server
import qualified Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined as Net.SyncP
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.Handshake.Version (Version (..))
import           Ouroboros.Network.Protocol.KeepAlive.Server (keepAliveServerPeer)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query
import           Ouroboros.Network.Protocol.LocalTxMonitor.Client (localTxMonitorClientPeer)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise)
import           Control.Monad (forever, void)
import           Control.Monad.Class.MonadSay
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import           Data.Void (Void)
import           GHC.Generics (Generic)
import qualified Network.Mux as Mux

import           Test.Consensus.PeerSimulator.Resources (BlockFetchResources (..),
                   ChainSyncResources (..), PeerResources (..))
import           Test.Util.TestBlock (TestBlock)

queryClient
  :: ( SupportedNetworkProtocolVersion blk
     , Consensus.BlockSupportsLedgerQuery blk
  , SerialiseNodeToClientConstraints blk
     , MonadST m
     , StandardHash blk
     , Serialise (HeaderHash blk)
     , Show (BlockNodeToClientVersion blk)
     , ShowProxy blk
     , MonadDelay m
     , ShowProxy (GenTx blk)
     , ShowProxy (ApplyTxErr blk)
     , ShowProxy (TxId (GenTx blk))
     , MonadAsync m
     , MonadMask m
     , ShowProxy (Consensus.BlockQuery blk)
     )
  => Proxy blk
  -> CodecConfig blk
  -> LocalNodeClientProtocols blk (Point blk) (Tip blk) SlotNo (GenTx blk) (GenTxId blk) (ApplyTxErr blk) (Consensus.Query blk) m
  -> NetworkMagic
  -> Versions
    NodeToClientVersion
    NodeToClientVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.InitiatorMode addr BL.ByteString m () Void)
queryClient blk codecCfg clients networkMagic =
  forallVersionsN2C blk networkMagic $ \version blockVersion -> do
    nodeToClientProtocols (protocols codecCfg clients blockVersion version) version $
      NodeToClientVersionData
        { networkMagic = networkMagic
        , query = True
        }


protocols
  :: ( Consensus.BlockSupportsLedgerQuery blk
     , MonadST m
     , SerialiseNodeToClientConstraints blk
     , StandardHash blk
     , Serialise (HeaderHash blk)
     , Show (BlockNodeToClientVersion blk)
     , ShowProxy blk
     , MonadDelay m
     , ShowProxy (GenTx blk)
     , ShowProxy (ApplyTxErr blk)
     , ShowProxy (TxId (GenTx blk))
     , MonadAsync m
     , MonadMask m
     , ShowProxy (Consensus.BlockQuery blk)
     )
  => CodecConfig blk
  -> LocalNodeClientProtocols blk (Point blk) (Tip blk) SlotNo (GenTx blk) (GenTxId blk) (ApplyTxErr blk) (Consensus.Query blk) m
  -> BlockNodeToClientVersion blk
  -> NodeToClientVersion
  -> NodeToClientProtocols Mux.InitiatorMode addr BL.ByteString m () Void
protocols codecCfg clients blockVersion version = do
    let Consensus.N2C.Codecs
          { cChainSyncCodec
          , cTxMonitorCodec
          , cStateQueryCodec
          , cTxSubmissionCodec
          } =
            Consensus.N2C.clientCodecs codecCfg blockVersion version


    NodeToClientProtocols
      { localChainSyncProtocol    =
          InitiatorProtocolOnly $
            case localChainSyncClient clients of
              NoLocalChainSyncClient ->
                mkMiniProtocolCbFromPeer $
                  const
                    (nullTracer, cChainSyncCodec, chainSyncPeerNull)
              LocalChainSyncClient client ->
                mkMiniProtocolCbFromPeer $
                  const
                    (nullTracer, cChainSyncCodec, Net.Sync.chainSyncClientPeer client)
              LocalChainSyncClientPipelined clientPipelined ->
                mkMiniProtocolCbFromPeerPipelined $
                  const
                    (nullTracer, cChainSyncCodec, Net.SyncP.chainSyncClientPeerPipelined clientPipelined)
      , localTxSubmissionProtocol =
          InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer $ const
            ( nullTracer
            , cTxSubmissionCodec
            , maybe localTxSubmissionPeerNull Net.Tx.localTxSubmissionClientPeer $ localTxSubmissionClient clients
            )
      , localStateQueryProtocol   =
            InitiatorProtocolOnly $
              mkMiniProtocolCbFromPeerSt $
                const
                  ( nullTracer
                  , cStateQueryCodec
                  , Net.Query.StateIdle
                  , maybe localStateQueryPeerNull

                      Net.Query.localStateQueryClientPeer $
                        localStateQueryClient clients
                  )
      , localTxMonitorProtocol    =
            InitiatorProtocolOnly $
              mkMiniProtocolCbFromPeer $
                const
                  ( nullTracer
                  , cTxMonitorCodec
                  , maybe localTxMonitorPeerNull
                      localTxMonitorClientPeer
                      $ localTxMonitoringClient clients
                  )
      }

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
      [ mkMiniProtocol
          Mux.StartOnDemandAny
          N2N.keepAliveMiniProtocolNum
          N2N.keepAliveProtocolLimits
          $ MiniProtocolCb
          $ \_ctx channel ->
            runPeer nullTracer cKeepAliveCodec channel $
              keepAliveServerPeer keepAliveServer
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.chainSyncMiniProtocolNum
          N2N.chainSyncProtocolLimits
          $ MiniProtocolCb
          $ \_ctx channel -> do
            atomically $ writeTVar csChanTMV True
            runPeer nullTracer cChainSyncCodec channel
              $ chainSyncServerPeer $ csrServer $ prChainSync res
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.blockFetchMiniProtocolNum
          N2N.blockFetchProtocolLimits
          $ MiniProtocolCb
          $ \_ctx channel -> do
            atomically $ writeTVar bfChanTMV True
            runPeer nullTracer cBlockFetchCodec channel
              $ blockFetchServerPeer $ bfrServer $ prBlockFetch res
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

-- | The ChainSync specification requires sending a rollback instruction to the
-- intersection point right after an intersection has been negotiated. (Opening
-- a connection implicitly negotiates the Genesis point as the intersection.)
data ChainSyncIntersection blk
  = JustNegotiatedIntersection !(Point blk)
  | AlreadySentRollbackToIntersection
  deriving stock Generic
  deriving anyclass NoThunks

forallVersionsN2N
  :: SupportedNetworkProtocolVersion blk
   => Proxy blk
  -> NetworkMagic
   ->
  (NodeToNodeVersion -> BlockNodeToNodeVersion blk -> r) ->
  Versions NodeToNodeVersion NodeToNodeVersionData r
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


forallVersionsN2C
  :: SupportedNetworkProtocolVersion blk
   => Proxy blk
  -> NetworkMagic
   ->
  (NodeToClientVersion -> BlockNodeToClientVersion blk -> r) ->
  Versions NodeToClientVersion NodeToClientVersionData r
forallVersionsN2C blk networkMagic mkR =
  Versions $
    flip Map.mapWithKey (supportedNodeToClientVersions blk) $ \version blockVersion ->
      Version
        { versionApplication = const $ mkR version blockVersion
        , versionData = stdVersionDataNTC networkMagic
        }
