{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Conformance.ShrinkIndex (tests) where


import           Control.Comonad (Comonad (extract))
import           Data.Kind (Type)
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable, eqT, typeRep)

import           Test.QuickCheck.Checkers (EqProp)
import           Test.QuickCheck.Classes (monoidMorphism)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary (..), Property, conjoin, elements, oneof,
                   property, testProperty)

import qualified ShrinkIndex as Ix
import           ShrinkIndex (ShrinkIndex, ShrinkTree)

tests :: TestTree
tests =
  testGroup
    "Shrink index properties"
    [ testProperty "Empty index always points to the current (top) node of a tree" prop_emptyIndexLookup
    , testProperty "Empty index has no successor on a tree" prop_emptySucc
    , testProperty "Neighbor index picks the next sibling" prop_next
    , testProperty "Shrink tree traversal by and index path is a monoid homomorphism" prop_monoidHomomorphism
    ]

-- | A data representation of 'SomeType' to generate 'Arbitrary' types for this
-- module tests.
data SomeType where
  SomeType ::
    forall (a :: Type).
    (Arbitrary a, Eq a, EqProp a, Typeable a, Show a) =>
    Proxy a ->
    SomeType

instance Eq SomeType where
  SomeType (_ :: Proxy a) == SomeType (_ :: Proxy b) =
    case eqT @a @b of
      Nothing -> False
      Just _ -> True

instance Show SomeType where
  show (SomeType (proxy :: Proxy a)) = show (typeRep proxy)

-- | Generation of (some) arbitrary types for 'ShrinkTree' (mock) values.
instance Arbitrary SomeType where
  arbitrary =
    oneof
      [ pure $ SomeType $ Proxy @Int,
        pure $ SomeType $ Proxy @Bool,
        pure $ SomeType $ Proxy @Char,
        do
          some1 <- arbitrary @SomeType
          some2 <- arbitrary @SomeType
          case (some1, some2) of
            (SomeType (_ :: Proxy a), SomeType (_ :: Proxy b)) ->
              elements
                [ SomeType $ Proxy @[a],
                  SomeType $ Proxy @(a, b),
                  SomeType $ Proxy @(Either a b)
                ]
      ]

-- | The 'next' index is the index of the next sibling.
prop_next :: ShrinkIndex -> Int -> Bool
prop_next ix n = Ix.next (ix <> Ix.child n) == ix <> Ix.child (n + 1)

-- | The 'succ' of an empty index in a 'ShrinkTree' is the empty index.
prop_emptySucc :: SomeType -> Property
prop_emptySucc (SomeType (_ :: Proxy a)) = property $ do
  tree <- arbitrary @(ShrinkTree a)
  pure $ Ix.succ tree mempty == Just mempty

-- | The empty index picks the current (top) node value.
prop_emptyIndexLookup :: SomeType -> Property
prop_emptyIndexLookup (SomeType (_ :: Proxy a)) = property $ do
  tree <- arbitrary @(ShrinkTree a)
  pure $ Ix.lookup mempty tree == Just (extract tree)

-- | 'narrowShrinkTree' induces a monoid homomorphism
-- of 'ShrinkIndex' into 'Kleisli Maybe (ShrinkTree a) (ShrinkTree a)'.
-- In other words, indexes compose by their monoidal operation as paths
-- traversing down a 'ShrinkTree'.
prop_monoidHomomorphism :: SomeType -> Property
prop_monoidHomomorphism (SomeType (_ :: Proxy a)) =
    let (_, testList) = monoidMorphism (Ix.narrowShrinkTree @a)
     in conjoin $ fmap snd testList

