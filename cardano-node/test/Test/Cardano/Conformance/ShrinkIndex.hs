{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Conformance.ShrinkIndex (tests) where


import           Control.Monad ((>=>))
import           Data.Kind (Constraint, Type)
import           Data.Proxy

import           Test.Tasty
import           Test.Tasty.QuickCheck (Arbitrary (..), Property, elements, oneof, property,
                   testProperty)

import qualified ShrinkIndex as Ix
import           ShrinkIndex (ShrinkIndex, ShrinkTree, narrowShrinkTreeDown)

tests :: TestTree
tests =
  testGroup
    "Shrink index properties"
    [ testProperty "Empty index always points to the current node" prop_emptyIndexLookup
    , testProperty "Empty index has no successor" prop_emptySucc
    , testProperty "Neighbor index picks the next sibling" prop_next
    , testProperty "The index monoidal operation composes on tree paths" prop_indexHomomorphism
    ]

type Some :: (Type -> Constraint) -> Type
data Some c where
  Some ::
    forall (c :: Type -> Constraint) (a :: Type).
    (c a, Show a, Eq a) =>
    Proxy a ->
    Some c

instance Eq (Some c) where
  Some Proxy == Some Proxy = Proxy == Proxy

instance Show (Some c) where
  show (Some Proxy) = show Proxy

instance Arbitrary (Some Arbitrary) where
  arbitrary =
    oneof
      [ pure $ Some $ Proxy @Int,
        pure $ Some $ Proxy @Bool,
        pure $ Some $ Proxy @Char,
        do
          some1 <- arbitrary @(Some Arbitrary)
          some2 <- arbitrary @(Some Arbitrary)
          case (some1, some2) of
            (Some (_ :: Proxy a), Some (_ :: Proxy b)) ->
              elements
                [ Some $ Proxy @[a],
                  Some $ Proxy @(a, b),
                  Some $ Proxy @(Either a b)
                ]
      ]

-- | The 'next' index is the index of the next sibling.
prop_next :: ShrinkIndex -> Int -> Bool
prop_next ix n = Ix.next (ix <> Ix.child n) == ix <> Ix.child (n + 1)

-- | The 'succ' of an empty index in a 'ShrinkTree' is the empty index.
prop_emptySucc :: Some Arbitrary -> Property
prop_emptySucc (Some (_ :: Proxy a)) = property $ do
  tree <- arbitrary @(ShrinkTree a)
  pure $ Ix.succ tree mempty == Just mempty

-- | The empty index picks the current (top) node value.
prop_emptyIndexLookup :: Some Arbitrary -> Property
prop_emptyIndexLookup (Some (_ :: Proxy a)) = property $ do
  tree <- arbitrary @(ShrinkTree a)
  pure $ Ix.lookup mempty tree == Just (Ix.node tree)

prop_indexHomomorphism :: ShrinkIndex -> ShrinkIndex -> Some Arbitrary -> Property
prop_indexHomomorphism ix1 ix2 (Some (_ :: Proxy a)) = property $ do
  tree <- arbitrary @(ShrinkTree a)
  pure $ (==) <$> narrowShrinkTreeDown (ix1 <> ix2) <*> (narrowShrinkTreeDown ix1 >=> narrowShrinkTreeDown ix2) $ tree
