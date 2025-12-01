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
import           Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
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
  -> IO ChainTip
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


connectToLocalNode' :: LocalNodeConnectInfo -> LocalNodeClientProtocols blk a b c d e f g IO -> IO ()
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
          (queryClient (Proxy @TestBlock) TB.TestBlockCodecConfig (getNetworkMagic tbcg))
          (unFile localNodeSocketPath)
      case r of
        Left e -> throwIO e
        Right _ -> pure ()


chainSyncGetCurrentTip
  :: StrictTMVar IO ChainTip
  -> ChainSyncClient TestBlock ChainPoint ChainTip IO ()
chainSyncGetCurrentTip tipVar = ChainSyncClient $ pure clientStIdle
 where
  clientStIdle :: Net.Sync.ClientStIdle TestBlock ChainPoint ChainTip IO ()
  clientStIdle =
    Net.Sync.SendMsgRequestNext (pure ()) clientStNext

  clientStNext :: Net.Sync.ClientStNext TestBlock ChainPoint ChainTip IO ()
  clientStNext =
    Net.Sync.ClientStNext
      { Net.Sync.recvMsgRollForward = \_block tip -> ChainSyncClient $ do
          void $ atomically $ tryPutTMVar tipVar tip
          pure $ Net.Sync.SendMsgDone ()
      , Net.Sync.recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
          void $ atomically $ tryPutTMVar tipVar tip
          pure $ Net.Sync.SendMsgDone ()
      }
