{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

-- | See the module documentation for "Data.Map.Diff.Strict".
module Data.Map.Diff.Strict.Internal (
    -- * Types
    Delta (..)
  , DeltaHistory (..)
  , Diff (..)
    -- * Conversion
  , keysSet
    -- * Construction
  , diff
  , empty
    -- ** Maps
  , fromMap
  , fromMapDeletes
  , fromMapInserts
    -- ** Lists
  , fromList
  , fromListDeletes
  , fromListDeltaHistories
  , fromListInserts
    -- ** Delta history
  , singleton
  , singletonDelete
  , singletonInsert
    -- * Deconstruction
    -- ** Delta history
  , last
    -- * Query
    -- ** Size
  , null
  , numDeletes
  , numInserts
  , size
    -- * Applying diffs
  , applyDiff
  , applyDiffForKeys
    -- * Folds and traversals
  , foldMapDelta
  , mapMaybeDiff
  , traverseDeltaWithKey_
    -- * Filter
  , filterOnlyKey
  ) where

import           Control.Monad (void)
import           Data.Bifunctor (Bifunctor (second))
import           Data.Foldable (foldMap', toList)
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import           Data.Monoid (Sum (..))
import           Data.Semigroup.Cancellative (LeftCancellative,
                     LeftReductive (..), RightCancellative, RightReductive (..))
import qualified Data.Sequence as Seq
import           Data.Sequence.NonEmpty (NESeq (..), nonEmptySeq)
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
newtype Diff k v = Diff (Map k (DeltaHistory v))
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NoThunks)

-- | Custom 'Functor' instance, since @'Functor' ('Map' k)@ is actually the
-- 'Functor' instance for a lazy Map.
instance Functor (Diff k) where
  fmap f (Diff m) = Diff $ Map.map (fmap f) m

-- | A non-empty history of changes to a value in a key-value store.
--
-- A history has an implicit sense of ordering according to time: from left to
-- right. This means that the leftmost element in the history is the /earliest/
-- change, while the rightmost element in the history is the /latest/ change.
newtype DeltaHistory v = DeltaHistory { getDeltaHistory :: NESeq (Delta v) }
  deriving stock (Generic, Show, Eq, Functor)
  deriving newtype (NoThunks)

-- | A change to a value in a key-value store.
data Delta v =
      Insert !v
    | Delete
  deriving stock (Generic, Show, Eq, Functor, Foldable, Traversable)
  deriving anyclass (NoThunks)

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
      (Merge.mapMissing $ \_k _v -> singletonDelete)
      (Merge.mapMissing $ \_k v -> singletonInsert v)
      (Merge.zipWithMaybeMatched $ \ _k v1 v2 ->
        if v1 == v2 then
          Nothing
        else
          Just $ singletonDelete <> singletonInsert v2
      )
      m1
      m2

empty :: Diff k v
empty = Diff Map.empty

-- | @'fromMap' m@ creates a @'Diff'@ from the inserts and deletes in @m@.
fromMap :: Map k (Delta v) -> Diff k v
fromMap = Diff . Map.map singleton

-- | @'fromMapInserts' m@ creates a @'Diff'@ that inserts all values in @m@.
fromMapInserts :: Map k v -> Diff k v
fromMapInserts = Diff . Map.map singletonInsert

-- | @'fromMapDeletes' m@ creates a @'Diff'@ that deletes all values in @m@.
fromMapDeletes :: Map k v -> Diff k v
fromMapDeletes = Diff . Map.map (const singletonDelete)

fromListDeltaHistories :: Ord k => [(k, DeltaHistory v)] -> Diff k v
fromListDeltaHistories = Diff . Map.fromList

-- | @'fromList' xs@ creates a @'Diff'@ from the inserts and deletes in @xs@.
fromList :: Ord k => [(k, Delta v)] -> Diff k v
fromList = fromListDeltaHistories . fmap (second singleton)

-- | @'fromListInserts' xs@ creates a @'Diff'@ that inserts all values in @xs@.
fromListInserts :: Ord k => [(k, v)] -> Diff k v
fromListInserts = fromListDeltaHistories . fmap (second singletonInsert)

-- | @'fromListDeletes' xs@ creates a @'Diff'@ that deletes all values in @xs@.
fromListDeletes :: Ord k => [k] -> Diff k v
fromListDeletes = fromListDeltaHistories . fmap (,singletonDelete)

singleton :: Delta v -> DeltaHistory v
singleton = DeltaHistory . NESeq.singleton

singletonInsert :: v -> DeltaHistory v
singletonInsert = singleton . Insert

singletonDelete :: DeltaHistory v
singletonDelete = singleton Delete

{------------------------------------------------------------------------------
  Deconstruction
------------------------------------------------------------------------------}

last :: DeltaHistory v -> Delta v
last (DeltaHistory (_ NESeq.:||> e)) = e

{------------------------------------------------------------------------------
  Query
------------------------------------------------------------------------------}

null :: Diff k v -> Bool
null (Diff m) = Map.null m

size :: Diff k v -> Int
size (Diff m) = Map.size m

-- | @'numInserts' d@ returns the number of inserts in the diff @d@.
--
-- Note: that is, the number of diff histories that have inserts as their last
-- change.
numInserts :: Diff k v -> Int
numInserts (Diff m) = getSum $ foldMap' f m
  where
    f h = case last h of
      Insert _ -> 1
      Delete   -> 0

-- | @'numDeletes' d@ returns the number of deletes in the diff @d@.
--
-- Note: that is, the number of diff histories that have deletes as their last
-- change.
numDeletes :: Diff k v -> Int
numDeletes (Diff m) = getSum $ foldMap' f m
  where
    f h = case last h of
      Insert _ -> 0
      Delete   -> 1

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
      f :: k
        -> DeltaHistory v
        -> DeltaHistory v
        -> Maybe (Maybe (DeltaHistory v))
      f _ h1 h2 = fmap DeltaHistory . NESeq.nonEmptySeq <$>
          stripPrefix
            (NESeq.toSeq $ getDeltaHistory h1)
            (NESeq.toSeq $ getDeltaHistory h2)

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
      f :: k
        -> DeltaHistory v
        -> DeltaHistory v
        -> Maybe (Maybe (DeltaHistory v))
      f _ h1 h2 = fmap DeltaHistory . NESeq.nonEmptySeq <$>
          stripSuffix
            (NESeq.toSeq $ getDeltaHistory h1)
            (NESeq.toSeq $ getDeltaHistory h2)

instance (Ord k, Eq v) => LeftCancellative (Diff k v)
instance (Ord k, Eq v) => RightCancellative (Diff k v)

deriving newtype instance Semigroup (DeltaHistory v)

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
    newKeys :: k -> DeltaHistory v -> Maybe v
    newKeys _k h = case last h of
      Insert x -> Just x
      Delete   -> Nothing

    oldKeys :: k -> v -> DeltaHistory v -> Maybe v
    oldKeys _k _v1 h = case last h of
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

-- | @'foldMap'@ over the last delta in each delta history.
foldMapDelta :: (Monoid m) => (Delta v -> m) -> Diff k v -> m
foldMapDelta f (Diff m) =
  foldMap (f . NESeq.last . getDeltaHistory) m

-- | Traversal with keys over the last delta in each delta history.
traverseDeltaWithKey_ ::
     Applicative t
  => (k -> Delta v -> t a)
  -> Diff k v
  -> t ()
traverseDeltaWithKey_ f (Diff m) = void $ Map.traverseWithKey g m
  where
    g k dh = f k (last dh)

{-------------------------------------------------------------------------------
  Filter
-------------------------------------------------------------------------------}

filterOnlyKey :: (k -> Bool) -> Diff k v -> Diff k v
filterOnlyKey f (Diff m) = Diff $ Map.filterWithKey (const . f) m

mapMaybeSeq :: (v -> Maybe v') -> DeltaHistory v -> Maybe (DeltaHistory v')
mapMaybeSeq f =
    fmap DeltaHistory
  . nonEmptySeq
  . Seq.fromList
  . Maybe.mapMaybe (traverse f)
  . toList
  . getDeltaHistory

mapMaybeDiff :: (v -> Maybe v') -> Diff k v -> Diff k v'
mapMaybeDiff f (Diff d) = Diff $ Map.mapMaybe (mapMaybeSeq f) d
