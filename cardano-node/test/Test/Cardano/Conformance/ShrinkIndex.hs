{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Conformance.ShrinkIndex (tests) where


import           Data.Function (on)
import           Data.Kind (Constraint, Type)
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable, eqT, typeRep)

import           Test.Tasty
import           Test.Tasty.QuickCheck (Arbitrary (..), Property, elements, oneof, property,
                   testProperty)

import qualified ShrinkIndex as Ix
import           ShrinkIndex (Kleisli (runKleisli), ShrinkIndex, ShrinkTree)

tests :: TestTree
tests =
  testGroup
    "Shrink index properties"
    [ testProperty "Empty index always points to the current (top) node of a tree" prop_emptyIndexLookup
    , testProperty "Empty index has no successor on a tree" prop_emptySucc
    , testProperty "Neighbor index picks the next sibling" prop_next
    , testProperty "The index monoidal operation distributes over shrink tree narrowing" prop_indexDistributes
    , testProperty "The empty index produces the identity when narrowing shrink trees" prop_indexIdentity
    ]

-- | A data representation of 'Some' type class constraint.
-- It bears withness of its 'Proxy' implementing said class.
type Some :: (Type -> Constraint) -> Type
data Some c where
  Some ::
    forall (c :: Type -> Constraint) (a :: Type).
    (c a, Eq a, Typeable a) =>
    Proxy a ->
    Some c

instance Eq (Some c) where
  Some (_ :: Proxy a) == Some (_ :: Proxy b) =
    case eqT @a @b of
      Nothing -> False
      Just _ -> True

instance Show (Some c) where
  show (Some (proxy :: Proxy a)) = show (typeRep proxy)

-- | Generation of (some) arbitrary types for 'ShrinkTree' (mock) values.
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

-- | Narrowing a 'ShrinkTree' by a 'ShrinkIndex' distributes over
-- the index monoidal operation. In other words, indexes compose as paths
-- traversing down a 'ShrinkTree'. Defined by testing for equality on the
-- result's current node, because comparing full 'ShirnkTree's is untenable as
-- they are exponentially big.
-- Along with 'prop_indexIndentity', proves that 'narrowShrinkTree' is a
-- monoid homomorphism.
prop_indexDistributes :: ShrinkIndex -> ShrinkIndex -> Some Arbitrary -> Property
prop_indexDistributes ix1 ix2 (Some (_ :: Proxy a)) = property $ do
  tree <- arbitrary @(ShrinkTree a)
  pure $ on (==) (fmap Ix.node . ($ tree) . runKleisli )
           (Ix.narrowShrinkTree (ix1 <> ix2))
           (Ix.narrowShrinkTree ix1 <> Ix.narrowShrinkTree ix2)

-- | Narrowing a 'ShrinkTree' by an empty index is equal to identity.
-- Defined by testing for equality on the result's current node, because
-- comparing full 'ShrinkTree's is untenable as they are exponentially big.
-- Along with 'prop_indexDistributes', proves that 'narrowShrinkTree' is a
-- monoid homomorphism.
prop_indexIdentity :: Some Arbitrary -> Property
prop_indexIdentity (Some (_ :: Proxy a)) = property $ do
  tree <- arbitrary @(ShrinkTree a)
  pure $ on (==) (fmap Ix.node . ($ tree) . runKleisli )
           (Ix.narrowShrinkTree mempty)
           mempty
