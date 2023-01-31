{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Map.Diff.Strict (
    Diff (..)
  , DiffEntry (..)
  , DiffHistory (..)
  , NEDiffHistory (..)
    -- * Conversions between empty and non-empty diff histories
  , nonEmptyDiffHistory
  , toDiffHistory
  , unsafeFromDiffHistory
    -- * Construction
  , diff
  , fromList
  , fromListDeletes
  , fromListEntries
  , fromListInserts
  , singleton
  , singletonDelete
  , singletonInsert
    -- * Predicates
  , null
    -- * Class instances for @'DiffHistory'@
  , areInverses
    -- * Applying diffs
  , applyDiff
  , applyDiffForKeys
    -- * Folds and traversals
  , traverseLastDiffEntries
  , unsafeFoldMapDiffEntry
  ) where

import           Prelude hiding (last, length, null, splitAt)

import           Control.Monad (void)
import           Data.Bifunctor
import           Data.Group
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import           Data.Sequence.NonEmpty.Extra ()
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- | A diff for key-value stores.
newtype Diff k v = Diff (Map k (NEDiffHistory v))
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

-- | A history of changes to a value in a key-value store.
--
-- A history has an implicit sense of ordering according to time: from left to
-- right. This means that the leftmost element in the history is the /earliest/
-- change, while the rightmost element in the history is the /latest/ change.
newtype DiffHistory v = DiffHistory { getDiffHistory :: Seq (DiffEntry v) }
  deriving stock (Generic, Show, Eq, Functor, Foldable)
  deriving newtype (NoThunks)

-- | A non-empty @'DiffHistory'@.
newtype NEDiffHistory v = NEDiffHistory { getNEDiffHistory :: NESeq (DiffEntry v) }
  deriving stock (Generic, Show, Eq, Functor, Foldable)
  deriving newtype (NoThunks)

-- | A change to a value in a key-value store.
data DiffEntry v =
      Insert !v
    | Delete !v
  deriving stock (Generic, Show, Eq, Functor, Foldable)
  deriving anyclass (NoThunks)

{------------------------------------------------------------------------------
  Conversions between empty and non-empty diff histories
------------------------------------------------------------------------------}

toDiffHistory :: NEDiffHistory v -> DiffHistory v
toDiffHistory (NEDiffHistory sq) = DiffHistory $ NESeq.toSeq sq

unsafeFromDiffHistory :: DiffHistory v -> NEDiffHistory v
unsafeFromDiffHistory (DiffHistory sq) = NEDiffHistory $ NESeq.unsafeFromSeq sq

nonEmptyDiffHistory :: DiffHistory v -> Maybe (NEDiffHistory v)
nonEmptyDiffHistory (DiffHistory sq) = NEDiffHistory <$> NESeq.nonEmptySeq sq

{------------------------------------------------------------------------------
  Construction
------------------------------------------------------------------------------}

-- | Compute the difference between @'Map'@s.
diff :: (Ord k, Eq v) => Map k v -> Map k v -> Diff k v
diff m1 m2 = Diff $
    Merge.merge
      (Merge.mapMissing $ \_k v -> singletonDelete v)
      (Merge.mapMissing $ \_k v -> singletonInsert v)
      (Merge.zipWithMaybeMatched $ \ _k v1 v2 ->
        if v1 == v2 then
          Nothing
        else
          Just $ singletonDelete v1 `unsafeAppend` singletonInsert v2
      )
      m1
      m2
  where
    unsafeAppend (NEDiffHistory h1) (NEDiffHistory h2) =
      NEDiffHistory $ h1 <> h2

fromList :: Ord k => [(k, NEDiffHistory v)] -> Diff k v
fromList = Diff . Map.fromList

fromListEntries :: Ord k => [(k, DiffEntry v)] -> Diff k v
fromListEntries = fromList . fmap (second singleton)

fromListInserts :: Ord k => [(k, v)] -> Diff k v
fromListInserts = fromList . fmap (second singletonInsert)

fromListDeletes :: Ord k => [(k, v)] -> Diff k v
fromListDeletes = fromList . fmap (second singletonDelete)

singleton :: DiffEntry v -> NEDiffHistory v
singleton = NEDiffHistory . NESeq.singleton

singletonInsert :: v -> NEDiffHistory v
singletonInsert = singleton . Insert

singletonDelete :: v -> NEDiffHistory v
singletonDelete = singleton . Delete

{------------------------------------------------------------------------------
  Deconstruction
------------------------------------------------------------------------------}

last :: NEDiffHistory v -> DiffEntry v
last (getNEDiffHistory -> _ NESeq.:||> e) = e

{------------------------------------------------------------------------------
  Predicates
------------------------------------------------------------------------------}

null :: Diff k v -> Bool
null (Diff m) = Map.null m


{------------------------------------------------------------------------------
  Class instances for @'Diff'@
------------------------------------------------------------------------------}

instance (Ord k, Eq v) => Semigroup (Diff k v) where
  Diff m1 <> Diff m2 = Diff $
    Merge.merge
      Merge.preserveMissing
      Merge.preserveMissing
      (Merge.zipWithMaybeMatched(\_k h1 h2 ->
        nonEmptyDiffHistory (toDiffHistory h1 <> toDiffHistory h2)
      ))
      m1
      m2

instance (Ord k, Eq v) => Monoid (Diff k v) where
  mempty = Diff mempty

instance (Ord k, Eq v) => Group (Diff k v) where
  invert (Diff m) = Diff $
    fmap (unsafeFromDiffHistory . invert . toDiffHistory) m

{------------------------------------------------------------------------------
  Class instances for @'DiffHistory'@
------------------------------------------------------------------------------}

-- | @h1 <> h2@ sums @h1@ and @h2@ by cancelling out as many consecutive diff
-- entries as possible, and appending the remainders.
--
-- Diff entries that are each other's inverse can cancel out: @'Delete'@ cancels
-- out any @'Insert' x@, and vice versa.
--
-- Note: We do not cancel out consecutive elements in @h1@ and @h2@
-- individually. It is only at the border between @h1@ and @h2@ that we cancel
-- out elements.
instance Eq v => Semigroup (DiffHistory v) where
  DiffHistory s1 <> DiffHistory s2 = DiffHistory $ s1 `mappend'` s2
    where
      -- At the ``touching'' ends of the sequences, take off diff entries that
      -- are each other's inverse until we find two non-inverse entries. In this
      -- case, we can not continue so we return the concatenated remainders.
      mappend' (xs :|> x) (y :<| ys)
        | areInverses x y                    = mappend' xs ys
      mappend' xs ys                         = xs Seq.>< ys

instance Eq v => Monoid (DiffHistory v) where
  mempty = DiffHistory mempty

instance Eq v => Group (DiffHistory v) where
  invert (DiffHistory s) = DiffHistory $ Seq.reverse . fmap invertDiffEntry $ s

-- | @`invertDiffEntry` e@ inverts a @'DiffEntry' e@ to its counterpart.
--
-- Note: We invert @DiffEntry@s, but it is not a @Group@: We do not have an
-- identity element, so it is not a @Monoid@ or @Semigroup@.
invertDiffEntry :: DiffEntry v -> DiffEntry v
invertDiffEntry = \case
  Insert x           -> Delete x
  Delete x           -> Insert x

-- | @'areInverses e1 e2@ checks whether @e1@ and @e2@ are each other's inverse.
--
-- For simplicity, we simply compare the inverse of the first argument to the
-- second argument. That is, inversion should be invertible.
areInverses :: Eq v => DiffEntry v -> DiffEntry v -> Bool
areInverses e1 e2 = invertDiffEntry e1 == e2

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
      (Merge.mapMaybeMissing     newKeys)
      (Merge.zipWithMaybeMatched oldKeys)
      m
      diffs
  where
    newKeys :: k -> NEDiffHistory v -> Maybe v
    newKeys _k h = case last h of
      Insert x  -> Just x
      Delete _x -> Nothing

    oldKeys :: k -> v -> NEDiffHistory v -> Maybe v
    oldKeys _k _v1 h = case last h of
      Insert x  -> Just x
      Delete _x -> Nothing

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

-- | @'foldMap'@ over the last diff entry in each diff history.
unsafeFoldMapDiffEntry :: (Monoid m) => (DiffEntry v -> m) -> Diff k v -> m
unsafeFoldMapDiffEntry f (Diff m) =
  foldMap (f . NESeq.last . getNEDiffHistory) m

-- | Traversal with keys over the last diff entry in each diff history.
traverseLastDiffEntries ::
     Applicative t
  => (k -> DiffEntry v -> t a)
  -> Diff k v
  -> t ()
traverseLastDiffEntries f (Diff m) = void $ Map.traverseWithKey g m
  where
    g k dh = f k (last dh)
