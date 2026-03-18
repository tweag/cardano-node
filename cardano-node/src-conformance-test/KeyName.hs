{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic rendering of test keys as dot-separated constructor paths.
--
-- Given a key type with a 'Generic' instance (which all 'SmallKey' types
-- already derive), 'keyName' produces a stable string representation of
-- the form @\"DatatypeName.Constructor.NestedConstructor...\"@.
--
-- The 'SmallKey' restrictions (no product types, no recursion) guarantees
-- that the constructor path is always a simple chain of sum constructor
-- names, making the rendering injective and trivially invertible via
-- 'parseKeyName'.
module KeyName (
    -- * Rendering
    keyName
    -- * Reverse lookup
  , keyNameMap
  , parseKeyName
    -- * Re-exported for type signatures
  , GKeyName
  ) where

import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics

import           Test.Consensus.Genesis.TestSuite.SmallKey (SmallKey, getAllKeys)

-- | Render a key as a dot-separated path derived from its generic
-- representation. The path starts with the datatype name, followed by
-- the constructor chain.
--
-- For example, given:
--
-- @
-- data GenesisTestKey = Uniform !Uniform.TestKey | ...
-- data TestKey = BlockFetchLeashingAttack | ...
-- @
--
-- >>> keyName (Uniform BlockFetchLeashingAttack)
-- "GenesisTestKey.Uniform.BlockFetchLeashingAttack"
keyName :: (Generic a, GKeyName (Rep a)) => a -> String
keyName = gKeyName . from

-- | Build a map from rendered key names to key values.
-- Useful for parsing command-line key names back to typed values.
keyNameMap ::
  (SmallKey a, Generic a, GKeyName (Rep a)) =>
  Map String a
keyNameMap = Map.fromList $ fmap (\k -> (keyName k, k)) getAllKeys

-- | Parse a rendered key name back to a key value.
parseKeyName ::
  (SmallKey a, Generic a, GKeyName (Rep a)) =>
  String ->
  Maybe a
parseKeyName name = Map.lookup name keyNameMap

-- * Generic machinery
--
-- Three typeclasses, mirroring the structure of GHC.Generics
-- representations:
--
--   * 'GKeyName'   — top level: emits the datatype name as the path root.
--   * 'GKeyPath'   — sum / constructor level: traverses @(':+:')@ and
--                    emits constructor names.
--   * 'GKeyFields' — field level: handles nullary constructors ('U1') and
--                    recursion into nested key types via 'K1'.

-- | Top-level walk. Emits the datatype name, then delegates to
-- 'GKeyPath' for the constructor chain.
class GKeyName f where
  gKeyName :: f a -> String

instance (Datatype d, GKeyPath f) => GKeyName (M1 D d f) where
  gKeyName m@(M1 x) = intercalate "." (datatypeName m : gKeyPath x)

-- | Constructor-path walk. Traverses sums and emits constructor names.
class GKeyPath f where
  gKeyPath :: f a -> [String]

instance (GKeyPath f, GKeyPath g) => GKeyPath (f :+: g) where
  gKeyPath (L1 x) = gKeyPath x
  gKeyPath (R1 x) = gKeyPath x

-- | Skip the datatype metadata for nested keys (the datatype name is
-- only emitted at the top level by 'GKeyName').
instance GKeyPath f => GKeyPath (M1 D d f) where
  gKeyPath (M1 x) = gKeyPath x

instance (Constructor c, GKeyFields f) => GKeyPath (M1 C c f) where
  gKeyPath m@(M1 x) = conName m : gKeyFields x

-- | Field walk. Handles nullary constructors and recurses into nested
-- key types.
class GKeyFields f where
  gKeyFields :: f a -> [String]

instance GKeyFields U1 where
  gKeyFields U1 = []

instance GKeyFields f => GKeyFields (M1 S c f) where
  gKeyFields (M1 x) = gKeyFields x

-- | Recurse into a nested key type by entering its generic
-- constructor path. This skips the nested type's datatype name
-- (handled by the 'M1 D' instance of 'GKeyPath').
instance (Generic key, GKeyPath (Rep key)) => GKeyFields (K1 i key) where
  gKeyFields (K1 x) = gKeyPath (from x)
