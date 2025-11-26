{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main (main) where

import Ouroboros.Consensus.Storage.ChainDB.API
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..), prettyBlockTree)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     toOldestFirst)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (peersOnlyHonest)
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..),
                     scheduleBlockPoint, scheduleHeaderPoint, scheduleTipPoint)
import Test.QuickCheck (generate)
import Test.Consensus.Genesis.Setup.GenChains
import Test.Consensus.PeerSimulator.NodeLifecycle
import Test.Consensus.PeerSimulator.StateView
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
import Test.Consensus.PeerSimulator.Trace
import Test.Consensus.PeerSimulator.Run
import Control.Monad (unless)
import qualified Data.Map.Merge.Lazy as M
import Test.Consensus.PeerSimulator.Resources (PeerSimulatorResources(..), makePeerSimulatorResources)
import Control.Tracer (nullTracer, traceWith, Tracer(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Aeson (encode, encodeFile, throwDecode, Value, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Coerce
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable
import qualified Network.Socket as Socket
import Options (Options (..), parseOptions)
import Ouroboros.Consensus.Util.IOLike
-- import Ouroboros.Network.Diffusion.Topology
-- import Ouroboros.Network.PeerSelection.State.LocalRootPeers
--   ( LocalRootPeersGroup (..)
--   , LocalRootPeersGroups (..)
--   , NetworkTopology (..)
--   , RootConfig (..)
--   )
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
-- import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection.LedgerPeers (RelayAccessPoint (..), UseLedgerPeers (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..), WarmValency (..))
import Server (run)
import System.Environment (getArgs)
import Test.Consensus.OrphanInstances ()
import Test.Consensus.PointSchedule (PointSchedule (..))
import Test.Consensus.PointSchedule.Peers (PeerId (..), Peers (Peers), getPeerIds)
import Ouroboros.Network.NodeToNode (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.RelayAccessPoint (PortNumber)
import Cardano.Node.Run ()

testPointSchedule :: PointSchedule blk
testPointSchedule =
  PointSchedule
    { psSchedule =
        Peers
          (M.fromList [(1, undefined)])
          mempty
          -- (M.fromList [(1, undefined)])
    , psStartOrder = []
    , psMinEndTime = error "min end time"
    }

buildPeerMap :: PortNumber -> PointSchedule blk -> Map PeerId PortNumber
buildPeerMap firstPort = M.fromList . flip zip [firstPort ..] . getPeerIds . psSchedule

toRelayAP :: PortNumber -> RelayAccessPoint
toRelayAP = RelayAccessAddress (read "127.0.0.1")

makeTopology :: Foldable t => t PortNumber -> Value
makeTopology ports = object
  [ "localRoots" .=
      [ object
        [ "accessPoints" .= fmap toRelayAP (toList ports)
        , "advertise" .= True
        , "valency" .= num_peers
        , "warmValency" .= (num_peers + 1)
        , "diffusionMode" .= id @String "InitiatorAndResponder"
        ]
      ]
  , "useLedgerAfterSlot" .= id @Int (-1)
  , "publicRoots" .= id @[()] []
  , "bootstrapPeers" .= Nothing @String
  ]
  -- NetworkTopology
  --   { localRootPeersGroups =
  --       LocalRootPeersGroups $
  --         pure $
  --           LocalRootPeersGroup
  --             { localRoots =
  --                 RootConfig
  --                   { rootAccessPoints = fmap toRelayAP $ toList ports
  --                   , rootAdvertise = DoAdvertisePeer -- is this the right value?
  --                   }
  --             , hotValency = coerce num_peers
  --             , warmValency = coerce $ num_peers + 1
  --             , rootDiffusionMode = InitiatorOnlyDiffusionMode -- is this the right value?
  --             , extraFlags = ()
  --             }
  --   , publicRootPeers = []
  --   , useLedgerPeers = DontUseLedgerPeers -- is this the right value?
  --   , peerSnapshotPath = Nothing
  --   , extraConfig = ()
  --   }
 where
  num_peers = length ports

main :: IO ()
main = do
  runServer
  -- args <- getArgs
  -- opts <- parseOptions args
  -- contents <- BSL8.readFile (optTestFile opts)
  -- pointSchedule <- throwDecode contents :: IO (PointSchedule Bool)
  -- let simPeerMap = buildPeerMap (optPort opts) pointSchedule
  -- BSL8.writeFile (optOutputTopologyFile opts) (encode $ makeTopology simPeerMap)

zipMaps :: Ord k => Map k a -> Map k b -> Map k (a, b)
zipMaps = M.merge M.dropMissing M.dropMissing $ M.zipWithMatched $ const (,)

runServer :: IO ()
runServer = do
  gt <- generate $ genChains $ pure 1
  let chain = gt {gtSchedule = rollbackSchedule 1 $ gtBlockTree gt}
      ps = gtSchedule chain

  Prelude.putStrLn $ unlines $ prettyBlockTree $ gtBlockTree chain
  encodeFile "/tmp/topology.file" $ makeTopology $ buildPeerMap 6000 ps


  let peerMap = buildPeerMap 6000 ps

  peerSim <- makePeerSimulatorResources nullTracer (gtBlockTree chain) $ NonEmpty.fromList $ M.keys peerMap

  incomingTMV <- newEmptyTMVarIO

  peerServers <-
    for (zipMaps peerMap $ psrPeers peerSim) $ \(port, res) -> do
      -- Make a TMVar for the chainsync and blockfetch channels exposed through
      -- the miniprotocols. These get threaded into the server, which will fill
      -- them once the NUT has connected.
      csChannelTMV <- newTVarIO False
      bfChannelTMV <- newTVarIO False

      putStrLn $ "starting server on " <> show port
      let sockAddr = Socket.SockAddrInet port $ Socket.tupleToHostAddress (127, 0, 0, 1)
      thread <- async $ run res incomingTMV csChannelTMV bfChannelTMV sockAddr
      pure ((csChannelTMV, bfChannelTMV), thread)

  -- Now, take each of the resulting TMVars. This effectively blocks until the
  -- NUT has connected.
  _peerChannels <- atomically $ do
    for peerServers $ \((csChanTMV, bfChanTMV), _thread) -> do
      csChan <- readTVar csChanTMV
      bfChan <- readTVar bfChanTMV
      unless (csChan && bfChan) retry
      pure (csChan, bfChan)

  let lifecycle = NodeLifecycle (Just 1000000) (\lir -> pure $ LiveNode { lnChainDb = ChainDB { getCurrentChain = pure $ AF.Empty AF.AnchorGenesis }, lnStateTracer = nullTracer }) (\ln -> pure (LiveIntervalResult {}))

  (chainDb, stateViewTracers) <- runScheduler
    (Tracer $ traceWith nullTracer . TraceSchedulerEvent)
    (cschcMap (psrHandles peerSim))
    ps
    (psrPeers peerSim)
    lifecycle
  stView <- snapshotStateView stateViewTracers chainDb

  let StateView svSelectedChain _svPeerSimulatorResults svTipBlock _svTrace = stView

  putStrLn "took everything"
  threadDelay 60

  for_ peerServers $ uninterruptibleCancel . snd

  pure ()


-- | A schedule that advertises all the points of the trunk up until the nth
-- block after the intersection, then switches to the first alternative
-- chain of the given block tree.
--
-- PRECONDITION: Block tree with at least one alternative chain.
rollbackSchedule :: AF.HasHeader blk => Int -> BlockTree blk -> PointSchedule blk
rollbackSchedule n blockTree =
    let branch = case btBranches blockTree of
          [b] -> b
          _   -> error "The block tree must have exactly one alternative branch"
        trunkSuffix = AF.takeOldest n (btbTrunkSuffix branch)
        schedulePoints = concat
          [ banalSchedulePoints (btbPrefix branch)
          , banalSchedulePoints trunkSuffix
          , banalSchedulePoints (btbSuffix branch)
          ]
    in PointSchedule {
         psSchedule = peersOnlyHonest $ zip (map (Time . (/30)) [0..]) schedulePoints,
         psStartOrder = [],
         psMinEndTime = Time 0
       }
  where
    banalSchedulePoints :: AnchoredFragment blk -> [SchedulePoint blk]
    banalSchedulePoints = concatMap banalSchedulePoints' . toOldestFirst
    banalSchedulePoints' :: blk -> [SchedulePoint blk]
    banalSchedulePoints' block = [scheduleTipPoint block, scheduleHeaderPoint block, scheduleBlockPoint block]
