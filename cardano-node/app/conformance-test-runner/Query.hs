{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Query
  ( getLocalChainTip
  , connectToLocalNode
  , LocalNodeConnectInfo (..)
  , LocalNodeClientProtocols (..)
  ) where

import           Cardano.Api.Internal.Block
import           Cardano.Api.Internal.IO
import           Cardano.Api.Internal.IPC (ChainSyncClient (..), LocalChainSyncClient (..),
                   LocalNodeClientProtocols (..), LocalNodeConnectInfo (..))

import           Cardano.Node.Run ()
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import           Ouroboros.Consensus.Ledger.SupportsMempool
import qualified Ouroboros.Consensus.Network.NodeToClient as Consensus
import qualified Ouroboros.Consensus.Network.NodeToClient as Consensus.N2C
import           Ouroboros.Consensus.Node (stdVersionDataNTC)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Net
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..),
                   mkMiniProtocolCbFromPeer, mkMiniProtocolCbFromPeerPipelined,
                   mkMiniProtocolCbFromPeerSt)
import           Ouroboros.Network.NodeToClient (NodeToClientProtocols (..),
                   NodeToClientVersionData (..), chainSyncPeerNull, localStateQueryPeerNull,
                   localTxMonitorPeerNull, localTxSubmissionPeerNull, nodeToClientProtocols)
import qualified Ouroboros.Network.NodeToClient as Net
import           Ouroboros.Network.NodeToNode (Versions (..))
import qualified Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined as Net.SyncP
import           Ouroboros.Network.Protocol.Handshake.Version (Version (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query
import           Ouroboros.Network.Protocol.LocalTxMonitor.Client (localTxMonitorClientPeer)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)

import           Codec.Serialise (Serialise)
import           Control.Monad (void)
import           Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import           Data.Void (Void)
import qualified Network.Mux as Mux

import qualified Test.Util.TestBlock as TB
import           Test.Util.TestBlock (TestBlock)


-- | Query the current tip of chainsync on a local cardano node.
getLocalChainTip
  :: LocalNodeConnectInfo
  -> IO (Net.Tip TestBlock)
getLocalChainTip localNodeConInfo = do
  resultVar <- newEmptyTMVarIO
  connectToLocalNode
    localNodeConInfo
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClient $ chainSyncGetCurrentTip resultVar
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }
  atomically $ takeTMVar resultVar


-- | Connect to (and query against) a local cardano node.
connectToLocalNode
  :: (blk ~ TestBlock)
  => LocalNodeConnectInfo
  -> LocalNodeClientProtocols blk (Point blk) (Net.Tip blk) SlotNo (GenTx blk) (GenTxId blk) (ApplyTxErr blk) (Consensus.Query blk) IO
  -> IO ()
connectToLocalNode LocalNodeConnectInfo
    { localNodeSocketPath
    }
  clients = let tbcg = TB.TestBlockConfig $ NumCoreNodes 0 in
    Net.withIOManager $ \iomgr -> do
      r <-
        Net.connectTo
          (Net.localSnocket iomgr)
          Net.NetworkConnectTracers
            { Net.nctMuxTracer = nullTracer
            , Net.nctHandshakeTracer = nullTracer
            }
          (queryClient (Proxy @TestBlock) TB.TestBlockCodecConfig clients (getNetworkMagic tbcg))
          (unFile localNodeSocketPath)
      case r of
        Left e -> throwIO e
        Right _ -> pure ()


-- | ChainSyncClient implementation of 'getLocalChainTip'.
chainSyncGetCurrentTip
  :: StrictTMVar IO (Net.Tip TestBlock)
  -> ChainSyncClient TestBlock (Point TestBlock) (Net.Tip TestBlock) IO ()
chainSyncGetCurrentTip tipVar = ChainSyncClient $ pure $
  Net.Sync.SendMsgRequestNext (pure ()) $
    Net.Sync.ClientStNext
      { Net.Sync.recvMsgRollForward = \_block tip -> ChainSyncClient $ do
          void $ atomically $ tryPutTMVar tipVar tip
          pure $ Net.Sync.SendMsgDone ()
      , Net.Sync.recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
          void $ atomically $ tryPutTMVar tipVar tip
          pure $ Net.Sync.SendMsgDone ()
      }


-- | Construct a versioned application capable of querying a local cardano
-- node.
queryClient
  :: ( SupportedNetworkProtocolVersion blk
     , Consensus.BlockSupportsLedgerQuery blk
  , SerialiseNodeToClientConstraints blk
     , MonadST m
     , StandardHash blk
     , Serialise (HeaderHash blk)
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


-- | Run the given code for every version available to the block.
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


-- | Transform a 'LocalNodeClientProtocols' into a 'NodeToClientProtocols'.
-- It's unclear to isovector exactly what is going on here, but this code is
-- copied with minor changes (in particular, loosening the constraints on
-- @blk@) from "Cardano.Api.Internal.IPC".
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
