{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Indexing the shrinking tree
module ShrinkIndex
  ( ShrinkTree,
    ShrinkIndex,
    makeShrinkTree,
    node,
    arbitraryShrinkTree,
    lookup,
    extend,
    succ,
    next,
    child,
    narrowShrinkTreeDown,
  )
where

import           Prelude hiding (lookup, succ)

import           Data.Foldable (toList)
import           Data.Maybe (listToMaybe)
import           Data.Sequence (Seq (..), fromList)

import           Test.QuickCheck (Arbitrary (shrink))

-- | Each index represents a unique path along a 'ShrinkTree'. The monoidal
-- operation corresponds to extending by the corresponding tree path, and the
-- neutral element to the current node (representing a test case).
newtype ShrinkIndex = Ix {getIndex :: Seq Int} deriving (Eq, Semigroup, Monoid)

instance Show ShrinkIndex where
  show (Ix s) = "path " <> show (toList s)

data ShrinkTree a = Node a [ShrinkTree a] deriving stock (Eq, Show, Functor, Foldable, Traversable)

path :: [Int] -> ShrinkIndex
path = foldMap child

node :: ShrinkTree a -> a
node (Node x _) = x

-- | Child branches of a 'ShrinkTree'.
branches :: ShrinkTree a -> [ShrinkTree a]
branches (Node _ bs) = bs

-- | Unfold a 'ShrinkTree' using the given shirking function.
makeShrinkTree :: (a -> [a]) -> a -> ShrinkTree a
makeShrinkTree f x = Node x $ fmap (makeShrinkTree f) $ f x

arbitraryShrinkTree :: (Arbitrary a) => a -> ShrinkTree a
arbitraryShrinkTree = makeShrinkTree shrink

lookup :: ShrinkIndex -> ShrinkTree a -> Maybe a
lookup ix tree = node <$> narrowShrinkTreeDown ix tree

narrowShrinkTreeDown :: ShrinkIndex -> ShrinkTree a -> Maybe (ShrinkTree a)
narrowShrinkTreeDown (Ix Empty) tree = Just tree
narrowShrinkTreeDown (Ix (n :<| ns)) tree = (listToMaybe . drop n . branches) tree >>= narrowShrinkTreeDown (Ix ns)

-- | Confines an index transformation /within/ a 'ShrinkTree'
withinTree :: (ShrinkIndex -> ShrinkIndex) -> ShrinkTree a -> ShrinkIndex -> Maybe ShrinkIndex
withinTree f tree ix = f ix <$ lookup (f ix) tree

-- | Extend the 'ShrinkIndex' into the first child 'ShrinkTree'.
extend :: ShrinkTree a -> ShrinkIndex -> Maybe ShrinkIndex
extend = withinTree (<> child 0)

-- | Move the 'ShrinkIndex' tip to the next sibling 'ShrinkTree' branch.
succ :: ShrinkTree a -> ShrinkIndex -> Maybe ShrinkIndex
succ = withinTree next

-- | The immediate nth child 'ShrinkTree' index.
child :: Int -> ShrinkIndex
child n = Ix $ fromList [n]

-- | The next sibling 'ShrinkTree' branch index.
next :: ShrinkIndex -> ShrinkIndex
next (Ix Empty) = mempty
next (Ix (xs :|> x)) = Ix (xs :|> (x + 1))
