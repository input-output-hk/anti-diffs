{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Map.AntiDiff.Strict.Internal (
    -- * Anti-diffs
    AntiDiff (..)
  , AntiDiffEntry (..)
  , DiffHistory (..)
  , NEDiffHistory (..)
  , areInverses
  , fromAnti
  , fromListDiffHistories
  , invertDiffEntry
  , last
  , nonEmptyDiffHistory
  , singleton
  , singletonDelete
  , singletonInsert
  , toAnti
  , toDiffHistory
  , unsafeFromDiffHistory
  ) where

import           Prelude hiding (last, length, null, splitAt)

import           Data.Group
import           Data.Map.Diff.Strict
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import           Data.Sequence.NonEmpty.Extra ()
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

toAnti :: Diff k v -> AntiDiff k v
toAnti (Diff m) = AntiDiff $ fmap singleton m

fromAnti :: AntiDiff k v -> Maybe (Diff k v)
fromAnti (AntiDiff m) = Diff <$> traverse f m
  where
    f dh = case last dh of
      Positive de -> Just de
      Negative _  -> Nothing

-- | A diff for key-value stores.
newtype AntiDiff k v = AntiDiff (Map k (NEDiffHistory v))
  deriving stock (Generic, Show, Eq, Functor)
  deriving newtype (NoThunks)

-- | A history of changes to a value in a key-value store.
--
-- A history has an implicit sense of ordering according to time: from left to
-- right. This means that the leftmost element in the history is the /earliest/
-- change, while the rightmost element in the history is the /latest/ change.
newtype DiffHistory v = DiffHistory { getDiffHistory :: Seq (AntiDiffEntry v) }
  deriving stock (Generic, Show, Eq, Functor, Foldable, Traversable)
  deriving newtype (NoThunks)

-- | A non-empty @'DiffHistory'@.
newtype NEDiffHistory v = NEDiffHistory { getNEDiffHistory :: NESeq (AntiDiffEntry v) }
  deriving stock (Generic, Show, Eq, Functor, Foldable, Traversable)
  deriving newtype (NoThunks)

data AntiDiffEntry v =
    Positive !(DiffEntry v)
  | Negative !(DiffEntry v)
  deriving stock (Generic, Show, Eq, Functor, Foldable, Traversable)
  deriving anyclass (NoThunks)

instance (Ord k, Eq v) => Semigroup (AntiDiff k v) where
  AntiDiff m1 <> AntiDiff m2 = AntiDiff $
    Merge.merge
      Merge.preserveMissing
      Merge.preserveMissing
      (Merge.zipWithMaybeMatched(\_k h1 h2 ->
        nonEmptyDiffHistory (toDiffHistory h1 <> toDiffHistory h2)
      ))
      m1
      m2

instance (Ord k, Eq v) => Monoid (AntiDiff k v) where
  mempty = AntiDiff mempty

instance (Ord k, Eq v) => Group (AntiDiff k v) where
  invert (AntiDiff m) = AntiDiff $
    fmap (unsafeFromDiffHistory . invert . toDiffHistory) m

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
invertDiffEntry :: AntiDiffEntry v -> AntiDiffEntry v
invertDiffEntry = \case
  Positive de -> Negative de
  Negative de -> Positive de

-- * Conversion

-- | @'areInverses e1 e2@ checks whether @e1@ and @e2@ are each other's inverse.
--
-- For simplicity, we simply compare the inverse of the first argument to the
-- second argument. That is, inversion should be invertible.
areInverses :: Eq v => AntiDiffEntry v -> AntiDiffEntry v -> Bool
areInverses e1 e2 = invertDiffEntry e1 == e2

toDiffHistory :: NEDiffHistory v -> DiffHistory v
toDiffHistory (NEDiffHistory sq) = DiffHistory $ NESeq.toSeq sq

unsafeFromDiffHistory :: DiffHistory v -> NEDiffHistory v
unsafeFromDiffHistory (DiffHistory sq) = NEDiffHistory $ NESeq.unsafeFromSeq sq

nonEmptyDiffHistory :: DiffHistory v -> Maybe (NEDiffHistory v)
nonEmptyDiffHistory (DiffHistory sq) = NEDiffHistory <$> NESeq.nonEmptySeq sq

-- * Construction

fromListDiffHistories :: Ord k => [(k, NEDiffHistory v)] -> AntiDiff k v
fromListDiffHistories = AntiDiff . Map.fromList

singleton :: DiffEntry v -> NEDiffHistory v
singleton = NEDiffHistory . NESeq.singleton . Positive

singletonInsert :: v -> NEDiffHistory v
singletonInsert = singleton . Insert

singletonDelete :: v -> NEDiffHistory v
singletonDelete = singleton . const Delete

-- * Deconstruction

last :: NEDiffHistory v -> AntiDiffEntry v
last (getNEDiffHistory -> _ NESeq.:||> e) = e
