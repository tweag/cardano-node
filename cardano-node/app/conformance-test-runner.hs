{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import           Cardano.Api (ConsensusModeParams (..), EpochSlots (..), File (..), NetworkId (..))

import           Cardano.Node.Run ()
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
import           Ouroboros.Consensus.Storage.ChainDB.API hiding (getTipPoint)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment, toOldestFirst)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.NodeToNode (PeerAdvertise (..))
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (RelayAccessPoint (..),
                   UseLedgerPeers (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (PortNumber)
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   WarmValency (..))

import           Control.Monad (unless, when)
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Data.Aeson (Value, encode, encodeFile, object, throwDecode, (.=))
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Foldable
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as M
import           Data.Maybe (fromJust, isJust, isNothing)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Traversable
import qualified Network.Socket as Socket
import           Options (Options (..), parseOptions)
import           System.Environment (getArgs)

import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..), prettyBlockTree)
import           Test.Consensus.Genesis.Setup.GenChains
import           Test.Consensus.OrphanInstances ()
import           Test.Consensus.PeerSimulator.NodeLifecycle
import           Test.Consensus.PeerSimulator.Resources (PeerSimulatorResources (..),
                   makePeerSimulatorResources)
import           Test.Consensus.PeerSimulator.Run
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (PeerId (..), getPeerIds, peersOnlyHonest)
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..), scheduleBlockPoint,
                   scheduleHeaderPoint, scheduleTipPoint)
import           Test.QuickCheck (generate, scale)
import           Test.Util.TestBlock (TestBlock, unTestHash)

import           ExitCodes
import           Query
import           Server (run)
import           ShrinkIndex (ShrinkIndex, ShrinkTree, makeShrinkTree)
import qualified ShrinkIndex as Ix

-- | Dummy shrinker for all 'GenesisTest'.
--
-- NOTE: Shrinkers for 'GenesisTests' are allowed to inspect the output value
-- (the resulting 'StateView') to increase their expressivity.
-- It is argued that this makes sense if the “run” phase is particularly
-- expensive. See:
-- ouroboros-consensus/ouroboros-consensus/src/unstable-testlib/Test/Util/QuickCheck.hs
--
-- However, this considerations seem to not apply in this setting as
-- such a state view is derived from protocol messages. 
shrinkGenesisTest :: GenesisTestFull blk -> [GenesisTestFull blk]
shrinkGenesisTest _ = []

data TestResult = TestSuccess | TestFailure deriving Eq

-- | This function makes implicit reference to the fact that 'ExitCodes.Success'
-- is defined as the empty status flag pattern.
testResultToFlag :: TestResult -> Set StatusFlag
testResultToFlag result = case result of
  TestFailure -> S.singleton TestFailed
  TestSuccess -> mempty

boolToTestResult :: Bool -> TestResult
boolToTestResult True = TestSuccess
boolToTestResult False = TestFailure

-- | A 'ShrinkIndex' signaling whether shrinking should proceed.
data ContinuationIndex = ContinueShrinkingWith ShrinkIndex
                       | ShrinkNoMore ShrinkIndex

-- | Try to get the following index from the updating function to continue
-- shrinking. On failure, signal the end of shrinking and apply the given
-- transformation to the input index.
tryContinueIndex :: (ShrinkIndex -> Maybe ShrinkIndex) -- ^ Updating function.
                 -> (ShrinkIndex -> ShrinkIndex) -- ^ Failed update index transformation.
                 -> ShrinkIndex
                 -> ContinuationIndex
tryContinueIndex upd f ix = case upd ix of
  Nothing -> ShrinkNoMore $ f ix
  Just ix' -> ContinueShrinkingWith ix'

-- | Update a possibly absent 'ShrinkIndex' according to the 'TestResult'
-- and signal if shrinking should proceed.
-- POSTCONDITION: Any 'ShrinkIndex' within a 'ContinuationIndex'
-- corresponds to a node on the given 'ShrinkTree'.
indexUpdate :: TestResult
            -> ShrinkTree a
            -> Maybe ShrinkIndex
            -> ContinuationIndex
indexUpdate res tree inputIndex = case (res, inputIndex) of
  -- A direct (global) test pass.
  (TestSuccess, Nothing) -> ShrinkNoMore mempty
  -- Test pass with a shrink index.
  (TestSuccess, Just ix)
    -- Global test pass (in disguise).
    | ix == mempty -> ShrinkNoMore mempty
    -- Local test pass (current node is not a property counterexample).
    -- If sibling nodes have been exhausted, rollback to the parent index.
    -- 'fromJust' is safe here because the only index without parent
    -- is the empty index.
    | otherwise -> tryContinueIndex (Ix.succ tree) (fromJust . Ix.parent) ix
  -- When the test fails, try to stretch.
  (TestFailure, Nothing) -> tryContinueIndex (Ix.stretch tree) id mempty
  (TestFailure, Just ix) -> tryContinueIndex (Ix.stretch tree) id ix

buildPeerMap :: PortNumber -> PointSchedule blk -> Map PeerId PortNumber
buildPeerMap firstPort = M.fromList . flip zip [firstPort ..] . getPeerIds . psSchedule

toRelayAP :: PortNumber -> RelayAccessPoint
toRelayAP = RelayAccessAddress $ read "127.0.0.1"

instance Foldable BlockTreeBranch where
  foldMap f (BlockTreeBranch _ _ _ full) = foldMap f $ toOldestFirst full

instance Foldable BlockTree where
  foldMap f (BlockTree a b) = foldMap f (toOldestFirst a) <> foldMap (foldMap f) b

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
  args <- getArgs
  opts <- parseOptions args

  -- The test should be parsed from `optTestFile`, but its chain and acceptance
  -- criteria are hard coded for convenience until the @testgen@ utility is implemented.
  -- Generate a random RollBack test chain. We divide the test size by 10 here
  -- because 'TestBlock's have a hardcoded size of 100---anything longer will
  -- crash when being deserialized.
  chain0 <- generate $ scale (flip div 10) $ do
    gt <- genChains $ pure 1
    pure $ gt {gtSchedule = rollbackSchedule 1 $ gtBlockTree gt}

  let tree = makeShrinkTree shrinkGenesisTest chain0
      inputIndex = optShrinkIndex opts
  -- Note that both no index and the empty index must
  -- return the original chain. See [NOTE: shrink-index-properties]
  chain <- case Ix.lookup (fold inputIndex) tree of
    Nothing -> do
      putStrLn "Incorrect shrink index"
      exitWithStatus BadUsage
    Just chain' -> pure chain'

  res <-
    try @_ @SomeException $
      runServer
        (optPort opts)
        (optSocketPath opts)
        (optOutputTopologyFile opts)
        chain
  case res of
    Left _ -> exitWithStatus InternalError
    Right testRes -> do
      mightContinueShrinking <- case indexUpdate testRes tree inputIndex of
        ContinueShrinkingWith ix -> do
          print ix
          pure $ S.singleton ContinueShrinking
        -- This following case includes a minimal counterexample being found or
        -- a global test success. A check for the latter case is needed to
        -- account for the edge case were the root node is a minimal
        -- counterexample.
        ShrinkNoMore ix -> do
          let isGlobalSuccess =
                testRes == TestSuccess &&
                 (isNothing inputIndex || inputIndex == Just mempty)
          case (optMinimalTestOutput opts, not isGlobalSuccess) of
            -- 'fromJust' is safe here because an index generated
            -- by 'indexUpdate' is always on the tree.
            (Just minimalTestFilePath, True) -> encodeFile minimalTestFilePath (fromJust (Ix.lookup ix tree))
            _ -> print ix
          pure mempty
      exitWithStatus . Flags $ testResultToFlag testRes <> mightContinueShrinking

zipMaps :: Ord k => Map k a -> Map k b -> Map k (a, b)
zipMaps = M.merge M.dropMissing M.dropMissing $ M.zipWithMatched $ const (,)

runServer :: PortNumber
          -> FilePath
          -> FilePath
          -> GenesisTest TestBlock (PointSchedule TestBlock)
          -> IO TestResult
runServer firstPort socketPath outputTopologyPath chain = do
  let ps = gtSchedule chain
      peerMap = buildPeerMap firstPort ps

  -- Print out the generated block tree so that the person running the test
  -- knows what's going on. We probably don't want to do this in real code.
  Prelude.putStrLn $ unlines $ prettyBlockTree $ gtBlockTree chain

  -- Write out the generated topology file.
  encodeFile outputTopologyPath $ makeTopology peerMap

  -- Make a new peer simulator, and then for each peer in it, spin up a new
  -- ChainSync and BlockFetch server.
  peerSim <-
    makePeerSimulatorResources nullTracer (gtBlockTree chain) $
      NonEmpty.fromList $ M.keys peerMap
  peerServers <-
    for (zipMaps peerMap $ psrPeers peerSim) $ \(port, res) -> do
      -- Make a TMVar for the chainsync and blockfetch channels exposed through
      -- the miniprotocols. These get threaded into the server, which will fill
      -- them once the NUT has connected.
      csChannelTMV <- newTVarIO False
      bfChannelTMV <- newTVarIO False

      putStrLn $ "Starting server on " <> show port
      let sockAddr =
            Socket.SockAddrInet port $
              Socket.tupleToHostAddress (127, 0, 0, 1)
      thread <- async $ run res csChannelTMV bfChannelTMV sockAddr
      pure ((csChannelTMV, bfChannelTMV), thread)

  -- Now, take each of the resulting TMVars. This blocks until the NUT has
  -- connected to all of our simulated peers.
  atomically $ do
    for_ peerServers $ \((csChanTMV, bfChanTMV), _thread) -> do
      csChan <- readTVar csChanTMV
      bfChan <- readTVar bfChanTMV
      unless (csChan && bfChan) retry

  putStrLn "Connected!"

  -- Build up a fake 'NodeLifecycle' we can pass to the point schedule runner.
  -- We should refactor that code so as to not require this, but this is good
  -- enough for an MVP.
  svts <- defaultStateViewTracers
  let lifecycle =
        NodeLifecycle
          (Just 1000000)
          (const $ pure $ LiveNode
            { lnChainDb = ChainDB
                { getCurrentChain = pure $ AF.Empty AF.AnchorGenesis
                }
            , lnStateTracer = nullTracer
            , lnStateViewTracers = svts
            , lnCopyToImmDb = pure $ error "lnCopyToImmDb"
            , lnPeers = M.keysSet peerMap
            })
          $ const $ pure $ LiveIntervalResult
            { lirPeerResults = []
            , lirActive = mempty
            }

  (_, stateViewTracers) <- runScheduler
    (Tracer $ traceWith nullTracer . TraceSchedulerEvent)
    (cschcMap (psrHandles peerSim))
    ps
    (psrPeers peerSim)
    lifecycle

  -- Give the NUT a chance to catch up to all the messages coming from the
  -- simulated peers.
  threadDelay 2

  -- Ask the NUT what chain tip it ended up at.
  tip@(Tip _ hash _) <-
    getLocalChainTip $
      LocalNodeConnectInfo (CardanoModeParams $ EpochSlots 0) Mainnet $
        File socketPath

  -- Reconstruct the "selected chain" that the NUT ended up on. We can do this
  -- as an oracle, because we know what the block tree was. Thus, we can just
  -- check the NUT's tip against every possible branch in the tree.
  --
  -- The use of 'fromJust' here ought to be safe, assuming the NUT started from
  -- genesis and saw all of the blocks from the simulated peers.
  let selchain =
        fromJust $ asum $ do
          let bt = gtBlockTree chain
          pchain <- btTrunk bt : fmap btbFull (btBranches bt)
          pure $ do
            (c, _) <- AF.splitBeforePoint pchain $ getTipPoint tip
            pure c

  -- Construct a map from hashes to blocks. Again, we should be able to ask the
  -- NUT for which block they ended up on, but Sandy's 'TestBlock' IPC
  -- implementation fails to decode the relevant message coming from the NUT.
  -- Unclear if this is a bug in Sandy's code, or something further upstream.
  let blocks = foldMap (\blk -> M.singleton (blockHash blk) blk) $
                 gtBlockTree chain

  -- Finally, build a 'StateView' we can use to evaluate the test's acceptance
  -- criteria.
  ts <- svtGetPeerSimulatorResults stateViewTracers
  let sv = StateView
        { svSelectedChain = AF.mapAnchoredFragment getHeader selchain
        , svPeerSimulatorResults = ts
        , svTipBlock = Just $ blocks M.! hash
        , svTrace =
            -- 'svTrace' is a trace of what the NUT actually did. Such a thing
            -- makes sense when we observing the internal state of
            -- @cardano-node@ (like the tests were originally designed for),
            -- but much less sense for alternative, black-box implementations.
            -- Thankfully, almost no tests actually inspect this field.
            error "conformance-test can't inspect svTrace"
        }

  -- Kill all of the simulated peers.
  for_ peerServers $ uninterruptibleCancel . snd

  -- Return the test's acceptance criteria.
  -- This should be parsed out of the test file parameter, but is currently
  -- hard coded for convenience.
  pure . boolToTestResult $ not . hashOnTrunk . AF.headHash $ svSelectedChain sv


--------------------------------------------------------------------------------
-- The remainder of this file is copied from the ouroboros-consensus
-- PeerSimulator RollBack test, because it's not yet convenient to import it.

hashOnTrunk :: ChainHash (Header TestBlock) -> Bool
hashOnTrunk GenesisHash      = True
hashOnTrunk (BlockHash hash) = all (== 0) $ unTestHash hash

-- | A schedule that advertises all the points of the trunk up until the nth
-- block after the intersection, then switches to the first alternative
-- chain of the given block tree.
--
-- PRECONDITION: Block tree with at least one alternative chain.
-- NOTE: This function is currently used to hard code the test case generation,
-- which is not part of the @test-runner@ functionality.
-- TODO: Make sure to handle the previous precondition appropriately when
-- implementing @testgen@.
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
