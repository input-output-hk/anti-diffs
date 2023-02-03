{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Map.Diff.Strict.Internal (
    -- * Types
    Diff (..)
  , DiffEntry (..)
    -- * Conversion
  , keysSet
    -- * Construction
  , diff
    -- ** Maps
  , fromMap
  , fromMapDeletes
  , fromMapInserts
    -- ** Lists
  , fromList
  , fromListDeletes
  , fromListInserts
    -- * Query
    -- ** Size
  , null
  , size
    -- * Applying diffs
  , applyDiff
  , applyDiffForKeys
    -- * Traversal
  , traverseDiffEntryWithKey_
    -- * Folds
  , foldMapDiffEntry
    -- * Filter
  , filterOnlyKey
  ) where

import           Prelude hiding (null)

import           Control.Monad (void)
import           Data.Bifunctor
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence.NonEmpty.Extra ()
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

newtype Diff k v = Diff (Map k (DiffEntry v))
  deriving stock (Generic, Show, Eq, Functor, Foldable, Traversable)
  deriving anyclass (NoThunks)

-- | A change to a value in a key-value store.
data DiffEntry v =
      Insert !v
    | Delete
  deriving stock (Generic, Show, Eq, Functor, Foldable, Traversable)
  deriving anyclass (NoThunks)

instance Ord k => Semigroup (Diff k v) where
  Diff m1 <> Diff m2 = Diff $
    Merge.merge
      Merge.preserveMissing
      Merge.preserveMissing
      (Merge.zipWithMaybeMatched(\_k _h1 h2 -> Just h2
      ))
      m1
      m2

instance Ord k => Monoid (Diff k v) where
  mempty = Diff mempty

{------------------------------------------------------------------------------
  Conversion
------------------------------------------------------------------------------}

keysSet :: Diff k v -> Set k
keysSet (Diff m) = Map.keysSet m

{------------------------------------------------------------------------------
  Construction
------------------------------------------------------------------------------}

-- | Compute the difference between @'Map'@s.
diff :: (Ord k, Eq v) => Map k v -> Map k v -> Diff k v
diff m1 m2 = Diff $
    Merge.merge
      (Merge.mapMissing $ \_k _v -> Delete)
      (Merge.mapMissing $ \_k v  -> Insert v)
      (Merge.zipWithMaybeMatched $ \ _k v1 v2 ->
        if v1 == v2 then
          Nothing
        else
          Just $ Insert v2
      )
      m1
      m2

-- | @'fromMap' m@ creates a @'Diff'@ from the inserts and deletes in @m@.
fromMap :: Map k (DiffEntry v) -> Diff k v
fromMap = Diff

-- | @'fromMapInserts' m@ creates a @'Diff'@ that inserts all values in @m@.
fromMapInserts :: Map k v -> Diff k v
fromMapInserts = Diff . fmap Insert

-- | @'fromMapDeletes' m@ creates a @'Diff'@ that deletes all values in @m@.
fromMapDeletes :: Map k v -> Diff k v
fromMapDeletes = Diff . fmap (const Delete)

-- | @'fromList' xs@ creates a @'Diff'@ from the inserts and deletes in @xs@.
fromList :: Ord k => [(k, DiffEntry v)] -> Diff k v
fromList = Diff . Map.fromList

-- | @'fromListInserts' xs@ creates a @'Diff'@ that inserts all values in @xs@.
fromListInserts :: Ord k => [(k, v)] -> Diff k v
fromListInserts = fromList . fmap (second Insert)

-- | @'fromListDeletes' xs@ creates a @'Diff'@ that deletes all values in @xs@.
fromListDeletes :: Ord k => [(k, v)] -> Diff k v
fromListDeletes = fromList . fmap (second (const Delete))

{------------------------------------------------------------------------------
  Query
------------------------------------------------------------------------------}

null :: Diff k v -> Bool
null (Diff m) = Map.null m

size :: Diff k v -> Int
size (Diff m) = Map.size m

{------------------------------------------------------------------------------
  Applying diffs
------------------------------------------------------------------------------}

-- | Applies a diff to a @'Map'@.
applyDiff ::
     Ord k
  => Map k v
  -> Diff k v
  -> Map k v
applyDiff m (Diff diffs) =
    Merge.merge
      Merge.preserveMissing
      (Merge.mapMaybeMissing newKeys)
      (Merge.zipWithMaybeMatched oldKeys)
      m
      diffs
  where
    newKeys :: k -> DiffEntry v -> Maybe v
    newKeys _k de = case de of
      Insert x -> Just x
      Delete   -> Nothing

    oldKeys :: k -> v -> DiffEntry v-> Maybe v
    oldKeys _k _v1 de = case de of
      Insert x -> Just x
      Delete   -> Nothing

-- | Applies a diff to a @'Map'@ for a specific set of keys.
applyDiffForKeys ::
     Ord k
  => Map k v
  -> Set k
  -> Diff k v
  -> Map k v
applyDiffForKeys m ks (Diff diffs) =
  applyDiff
    m
    (Diff $ diffs `Map.restrictKeys` (Map.keysSet m `Set.union` ks))

{------------------------------------------------------------------------------
  Folds and traversals
------------------------------------------------------------------------------}

--- | @'foldMap'@ over the last diff entry in each diff history.
foldMapDiffEntry :: (Monoid m) => (DiffEntry v -> m) -> Diff k v -> m
foldMapDiffEntry f (Diff m) =
  foldMap f m

-- | Traversal with keys over the last diff entry in each diff history.
traverseDiffEntryWithKey_ ::
     Applicative t
  => (k -> DiffEntry v -> t a)
  -> Diff k v
  -> t ()
traverseDiffEntryWithKey_ f (Diff m) = void $ Map.traverseWithKey f m

{-------------------------------------------------------------------------------
  Filter
-------------------------------------------------------------------------------}

filterOnlyKey :: (k -> Bool) -> Diff k v -> Diff k v
filterOnlyKey f (Diff m) = Diff $ Map.filterWithKey (const . f) m
