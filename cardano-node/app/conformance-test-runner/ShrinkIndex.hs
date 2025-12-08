{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
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
    narrowShrinkTree,
    path,
  )
where

import           Prelude hiding (lookup, succ)

import           Control.Monad ((>=>))
import           Data.Foldable (toList)
import           Data.Maybe (listToMaybe)
import           Data.Sequence (Seq (..), fromList)

import           Test.QuickCheck (Arbitrary (..), Testable (property), frequency)
import           Test.QuickCheck.Checkers (EqProp (..), eq)

-- | Each index represents a unique path along a 'ShrinkTree'. The monoidal
-- operation corresponds to extending by the corresponding tree path, and the
-- neutral element to the current node (representing a test case).
newtype ShrinkIndex = Ix {getIndex :: Seq Int} deriving (Eq, Semigroup, Monoid)

instance Show ShrinkIndex where
  show (Ix s) = "path " <> show (toList s)

instance Arbitrary ShrinkIndex where
  arbitrary = frequency [(4, child <$> arbitrary), (1, pure mempty)]
  shrink (Ix s) = Ix <$> shrink s

data ShrinkTree a = Node a [ShrinkTree a] deriving stock (Functor, Foldable, Traversable)

instance (Arbitrary a) => Arbitrary (ShrinkTree a) where
  arbitrary = fmap arbitraryShrinkTree arbitrary

  -- Note that a 'ShrinkTree' shrinks to its node children, i.e.
  -- @shrink tree = branches tree@
  shrink = fmap arbitraryShrinkTree . shrink . node

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

-- | Find the 'ShrinkTree' node a 'ShrinkIndex' points to.
lookup :: ShrinkIndex -> ShrinkTree a -> Maybe a
lookup ix tree = node <$> runKleisli (narrowShrinkTree ix) tree

-- | A 'ShrinkTree' traversal by the given index's path.
narrowShrinkTree :: ShrinkIndex -> Kleisli Maybe (ShrinkTree a) (ShrinkTree a)
narrowShrinkTree = foldMap (\n -> Kleisli (listToMaybe . drop n . branches)) . getIndex

-- | A local definition of 'Control.Arrow.Kleisli' to provide a non-orphan
-- 'Monoid' instance. With this, the algebraic structure of the tree path
-- traversal by `narrowShrinkTree` is made explicit.
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Semigroup (Kleisli m a a) where
  Kleisli f <> Kleisli g = Kleisli $ f >=> g

instance Monad m => Monoid (Kleisli m a a) where
  mempty = Kleisli pure

-- | The testing notion of 'ShrinkTree' path equality is given by the observation
-- of the current (top) 'node'.
instance (Arbitrary a, Eq b) => EqProp (Kleisli Maybe (ShrinkTree a) (ShrinkTree b)) where
  Kleisli f =-= Kleisli g = property $ do
    x <- arbitrary
    pure $ eq (fmap node $ f x) (fmap node $ g x)

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
