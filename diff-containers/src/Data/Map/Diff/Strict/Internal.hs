{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | See the module documentation for "Data.Map.Diff.Strict".
module Data.Map.Diff.Strict.Internal (
    -- * Types
    Diff (..)
  , DiffEntry (..)
  , DiffHistory (..)
  , NEDiffHistory (..)
    -- * Conversion
  , keysSet
    -- ** Diff histories
  , nonEmptyDiffHistory
  , toDiffHistory
    -- * Construction
  , diff
    -- ** Maps
  , fromMap
  , fromMapDeletes
  , fromMapInserts
    -- ** Lists
  , fromList
  , fromListDeletes
  , fromListDiffHistories
  , fromListInserts
    -- ** Diff histories
  , singleton
  , singletonDelete
  , singletonInsert
    -- * Deconstruction
    -- ** Diff histories
  , last
    -- * Query
    -- ** Size
  , null
  , size
    -- * Applying diffs
  , applyDiff
  , applyDiffForKeys
    -- * Folds and traversals
  , foldMapDiffEntry
  , traverseDiffEntryWithKey_
    -- * Filter
  , filterOnlyKey
  ) where

import           Control.Monad (void)
import           Data.Bifunctor (Bifunctor (second))
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup.Cancellative (LeftCancellative,
                     LeftReductive (..), RightCancellative, RightReductive (..))
import           Data.Sequence (Seq (..))
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import           Data.Sequence.NonEmpty.Extra ()
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))
import           Prelude hiding (last, length, null, splitAt)

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
  deriving stock (Generic, Show, Eq, Functor)
  deriving newtype (NoThunks)

-- | A non-empty @'DiffHistory'@.
newtype NEDiffHistory v = NEDiffHistory { getNEDiffHistory :: NESeq (DiffEntry v) }
  deriving stock (Generic, Show, Eq, Functor)
  deriving newtype (NoThunks)

-- | A change to a value in a key-value store.
data DiffEntry v =
      Insert !v
    | Delete !v
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

{------------------------------------------------------------------------------
  Conversion
------------------------------------------------------------------------------}

keysSet :: Diff k v -> Set k
keysSet (Diff m) = Map.keysSet m

toDiffHistory :: NEDiffHistory v -> DiffHistory v
toDiffHistory (NEDiffHistory sq) = DiffHistory $ NESeq.toSeq sq

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

-- | @'fromMap' m@ creates a @'Diff'@ from the inserts and deletes in @m@.
fromMap :: Map k (DiffEntry v) -> Diff k v
fromMap = Diff . fmap singleton

-- | @'fromMapInserts' m@ creates a @'Diff'@ that inserts all values in @m@.
fromMapInserts :: Map k v -> Diff k v
fromMapInserts = Diff . fmap singletonInsert

-- | @'fromMapDeletes' m@ creates a @'Diff'@ that deletes all values in @m@.
fromMapDeletes :: Map k v -> Diff k v
fromMapDeletes = Diff . fmap singletonDelete

fromListDiffHistories :: Ord k => [(k, NEDiffHistory v)] -> Diff k v
fromListDiffHistories = Diff . Map.fromList

-- | @'fromList' xs@ creates a @'Diff'@ from the inserts and deletes in @xs@.
fromList :: Ord k => [(k, DiffEntry v)] -> Diff k v
fromList = fromListDiffHistories . fmap (second singleton)

-- | @'fromListInserts' xs@ creates a @'Diff'@ that inserts all values in @xs@.
fromListInserts :: Ord k => [(k, v)] -> Diff k v
fromListInserts = fromListDiffHistories . fmap (second singletonInsert)

-- | @'fromListDeletes' xs@ creates a @'Diff'@ that deletes all values in @xs@.
fromListDeletes :: Ord k => [(k, v)] -> Diff k v
fromListDeletes = fromListDiffHistories . fmap (second singletonDelete)

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
last (NEDiffHistory (_ NESeq.:||> e)) = e

{------------------------------------------------------------------------------
  Query
------------------------------------------------------------------------------}

null :: Diff k v -> Bool
null (Diff m) = Map.null m

size :: Diff k v -> Int
size (Diff m) = Map.size m

{------------------------------------------------------------------------------
  Instances
------------------------------------------------------------------------------}

instance Ord k => Semigroup (Diff k v) where
  (<>) :: Diff k v -> Diff k v -> Diff k v
  (Diff m1) <> (Diff m2) = Diff $ Map.unionWith (<>) m1 m2

instance Ord k => Monoid (Diff k v) where
  mempty :: Diff k v
  mempty = Diff mempty

instance (Ord k, Eq v) => LeftReductive (Diff k v) where
  stripPrefix :: Diff k v -> Diff k v -> Maybe (Diff k v)
  stripPrefix (Diff m1) (Diff m2) = Diff <$>
      Merge.mergeA
        (Merge.traverseMissing $ \_ _ -> Nothing)
        Merge.preserveMissing
        (Merge.zipWithMaybeAMatched f)
        m1
        m2
    where
      f _ h1 h2 = nonEmptyDiffHistory <$>
          stripPrefix (toDiffHistory h1) (toDiffHistory h2)

instance (Ord k, Eq v) => RightReductive (Diff k v) where
  stripSuffix :: Diff k v -> Diff k v -> Maybe (Diff k v)
  stripSuffix (Diff m1) (Diff m2) = Diff <$>
      Merge.mergeA
        (Merge.traverseMissing $ \_ _ -> Nothing)
        Merge.preserveMissing
        (Merge.zipWithMaybeAMatched f)
        m1
        m2
    where
      f _ h1 h2 = nonEmptyDiffHistory <$>
          stripSuffix (toDiffHistory h1) (toDiffHistory h2)

instance (Ord k, Eq v) => LeftCancellative (Diff k v)
instance (Ord k, Eq v) => RightCancellative (Diff k v)

deriving newtype instance Semigroup (NEDiffHistory v)

deriving newtype instance Semigroup (DiffHistory v)
deriving newtype instance Monoid (DiffHistory v)
deriving newtype instance Eq v => LeftReductive (DiffHistory v)
deriving newtype instance Eq v => RightReductive (DiffHistory v)
deriving newtype instance Eq v => LeftCancellative (DiffHistory v)
deriving newtype instance Eq v => RightCancellative(DiffHistory v)

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
    newKeys :: k -> NEDiffHistory v -> Maybe v
    newKeys _k h = case last h of
      Insert x -> Just x
      Delete _ -> Nothing

    oldKeys :: k -> v -> NEDiffHistory v -> Maybe v
    oldKeys _k _v1 h = case last h of
      Insert x -> Just x
      Delete _ -> Nothing

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
foldMapDiffEntry :: (Monoid m) => (DiffEntry v -> m) -> Diff k v -> m
foldMapDiffEntry f (Diff m) =
  foldMap (f . NESeq.last . getNEDiffHistory) m

-- | Traversal with keys over the last diff entry in each diff history.
traverseDiffEntryWithKey_ ::
     Applicative t
  => (k -> DiffEntry v -> t a)
  -> Diff k v
  -> t ()
traverseDiffEntryWithKey_ f (Diff m) = void $ Map.traverseWithKey g m
  where
    g k dh = f k (last dh)

{-------------------------------------------------------------------------------
  Filter
-------------------------------------------------------------------------------}

filterOnlyKey :: (k -> Bool) -> Diff k v -> Diff k v
filterOnlyKey f (Diff m) = Diff $ Map.filterWithKey (const . f) m
