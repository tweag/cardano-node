-- | Support for shrinking 'ConformanceTest's without access to a 'StateView'.
module StateViewUnavailable
  ( StateViewUnavailable (..)
  , makeConformanceShrinkTree
  , handleStateViewUnavailable
  ) where

import           Control.Exception (Exception, handle, throw)

import           Test.Consensus.Genesis.Setup (ConformanceTest (..))
import           Test.Consensus.Genesis.ShrinkIndex (ShrinkTree, makeShrinkTree)
import           Test.Consensus.PointSchedule (GenesisTestFull)

-- | Thrown when a shrinker inspects the 'StateView' it was given.
--
-- 'ConformanceTest' shrinkers may inspect the output of a test run, but the
-- runner only executes a single node of the shrink tree per invocation, so
-- the 'StateView's of its ancestors are not available. For now we pass this
-- exception in their place, and report an internal error if it is forced.
data StateViewUnavailable = StateViewUnavailable
  deriving Show

instance Exception StateViewUnavailable

-- | Build the 'ShrinkTree' of a 'ConformanceTest', rooted at the given test
-- case. Forcing the 'StateView' given to the shrinker throws
-- 'StateViewUnavailable'.
makeConformanceShrinkTree
  :: ConformanceTest blk
  -> GenesisTestFull blk
  -> ShrinkTree (GenesisTestFull blk)
makeConformanceShrinkTree ct =
  makeShrinkTree $ \gt -> ctShrinker ct gt $ throw StateViewUnavailable

-- | Run the first action, falling back to the second if the first throws
-- 'StateViewUnavailable'.
handleStateViewUnavailable :: IO a -> IO a -> IO a
handleStateViewUnavailable action onUnavailable =
  handle (\StateViewUnavailable -> onUnavailable) action
