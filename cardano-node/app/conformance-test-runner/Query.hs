{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Query
  ( getLocalChainTip
  , LocalNodeConnectInfo (..)
  ) where

import           Cardano.Api.Internal.Block
import           Cardano.Api.Internal.IO
import           Cardano.Api.Internal.IPC (ChainSyncClient (..), EpochSlots (..),
                   LocalChainSyncClient (..), LocalNodeClientParams (..),
                   LocalNodeClientProtocols (..), LocalNodeConnectInfo (..), connectToLocalNode)

import           Cardano.Node.Run ()
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.Block as Net
import qualified Ouroboros.Network.Mux as Net
import           Ouroboros.Network.NodeToClient (NodeToClientProtocols (..),
                   NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as Net
import qualified Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync

import           Control.Monad
import           Control.Tracer (nullTracer)
import           Data.Proxy

import qualified Test.Util.TestBlock as TB
import           Test.Util.TestBlock (TestBlock)

import           MiniProtocols (queryClient)


getLocalChainTip
  :: LocalNodeConnectInfo
  -> IO (Net.Tip TestBlock)
getLocalChainTip localNodeConInfo = do
  resultVar <- newEmptyTMVarIO
  connectToLocalNode'
    localNodeConInfo
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClient $ chainSyncGetCurrentTip resultVar
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }
  atomically $ takeTMVar resultVar


connectToLocalNode'
  :: (blk ~ TestBlock)
  => LocalNodeConnectInfo
  -> LocalNodeClientProtocols blk (Point blk) (Net.Tip blk) SlotNo (GenTx blk) (GenTxId blk) (ApplyTxErr blk) (Consensus.Query blk) IO
  -> IO ()
connectToLocalNode' LocalNodeConnectInfo
    { localNodeSocketPath
    , localNodeNetworkId
    , localConsensusModeParams
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
