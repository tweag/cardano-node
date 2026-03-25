{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Query
  ( getRemoteChainTip
  ) where

import           Cardano.Node.Run ()
import           Ouroboros.Consensus.Block (CodecConfig, Header, Point, Proxy (Proxy))
import           Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (SupportedNetworkProtocolVersion)
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Util.IOLike (MonadThrow (throwIO))
import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
                   OuroborosApplication (..), RunMiniProtocol (..))
import qualified Ouroboros.Network.NodeToNode as N2N
import           Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
                   encodeRemoteAddress)
import           Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient (..),
                   ClientStIdle (..), ClientStIntersect (..), chainSyncClientPeer)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)

import           Control.Tracer (nullTracer)
import qualified Network.Mux as Mux
import qualified Network.Socket as Socket

import           Test.Consensus.OrphanInstances ()
import           Test.Consensus.PeerSimulator.Config ()

import           MiniProtocols (forallVersionsN2N)

-- | Query the current chain tip of a remote node via the node-to-node (N2N)
-- ChainSync mini-protocol. Blocks until the tip is received.
--
-- This function replaces an earlier approach based on 'getLocalChainTip' from
-- @cardano-api@, which connected via a Unix domain socket using the
-- node-to-client (N2C) protocol. N2C is an implementation-specific local
-- interface unique to Haskell @cardano-node@; alternative node
-- implementations are not required to expose it. By contrast, N2N ChainSync
-- is mandatory for any node participating in the Cardano peer-to-peer network,
-- making this function work against any conforming implementation.
getRemoteChainTip
  :: forall blk.
  ( SerialiseNodeToNodeConstraints blk
  , ShowProxy blk
  , ShowProxy (Header blk)
  , SupportedNetworkProtocolVersion blk
  )
  => CodecConfig blk
  -> NetworkMagic
  -> Socket.SockAddr
  -> IO (Tip blk)
getRemoteChainTip codecCfg networkMagic nutAddress =
  withIOManager $ \iocp -> do
    let sn = Snocket.socketSnocket iocp
    r <- N2N.connectTo sn N2N.nullNetworkConnectTracers
           (forallVersionsN2N (Proxy @blk) networkMagic $ \version blockVersion ->
             let Consensus.N2N.Codecs { cChainSyncCodec } =
                   Consensus.N2N.defaultCodecs codecCfg blockVersion encodeRemoteAddress decodeRemoteAddress version
             in OuroborosApplication
                  [ MiniProtocol
                      { miniProtocolNum    = N2N.chainSyncMiniProtocolNum
                      , miniProtocolStart  = Mux.StartEagerly
                      , miniProtocolLimits = N2N.chainSyncProtocolLimits N2N.defaultMiniProtocolParameters
                      , miniProtocolRun    = InitiatorProtocolOnly $ MiniProtocolCb $ \_ channel ->
                          runPeer nullTracer cChainSyncCodec channel
                            $ chainSyncClientPeer chainSyncGetTip
                      }
                  ])
           Nothing
           nutAddress
    case r of
      Left e          -> throwIO e
      Right (Left tip) -> pure tip
      Right (Right _) -> error "getRemoteChainTip: unexpected responder result"

-- | A one-shot ChainSync client that returns the server's current chain tip.
--
-- Sends @MsgFindIntersect []@ — an intersection request with an empty point
-- list. Because no intersection with an empty set can exist, the server always
-- replies immediately with @MsgIntersectNotFound tip@, where @tip@ is its
-- current chain tip.
--
-- @MsgFindIntersect []@ is preferred over @MsgRequestNext@ for this purpose
-- because @MsgRequestNext@ asks the server to stream the next block, which may
-- block if the server is already at its tip, and implies we intend to start
-- syncing. @MsgFindIntersect []@ is semantically a pure tip query and
-- guarantees an immediate response regardless of the server's state.
chainSyncGetTip
  :: ChainSyncClient
       (Header blk)
       (Point blk)
       (Tip blk)
       IO
       (Tip blk)
chainSyncGetTip = ChainSyncClient . pure $
  SendMsgFindIntersect [] ClientStIntersect
    { recvMsgIntersectFound    = \_ tip -> ChainSyncClient . pure $ SendMsgDone tip
    , recvMsgIntersectNotFound = ChainSyncClient . pure . SendMsgDone
    }
