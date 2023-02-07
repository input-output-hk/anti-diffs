{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Differences on @'Map'@s with a @'Group'@ instance.
--
-- === Positivity and normality
--
-- It is an unfortunate side effect of facilitating a @'Group'@ instance for
-- @'Diff'@s that we can get into situations where applying diffs will fail or
-- produce wrong results due to diffs containing internally unresolved sums and
-- subtractions. The responsibility of downstream code is to ensure that diffs
-- that are applied are both /positive/ (@'isPositive'@) and /normal/
-- @('isNormal')@. If that is the case, then applying diffs will never go wrong.
--
-- A number of definitions in this modules are annotated with PRECONDITION,
-- INVARIANT and POSTCONDITION. Use these and the type class laws for
-- @'Semigroup'@, @'Monoid'@ and @'Group'@ (which hold given preconditions) to
-- ensure that applied diffs are always both positive and normal.
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
  , unsafeFromDiffHistory
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
  , last
    -- * Query
    -- ** Size
  , null
  , size
    -- ** Positivity and normality
  , isNormal
  , isNormalDiffHistory
  , isPositive
  , isPositiveDiffHistory
    -- * @'Group'@ instances
  , areInverses
  , invertDiffEntry
    -- * Applying diffs
  , applyDiff
  , applyDiffForKeys
  , unsafeApplyDiff
  , unsafeApplyDiffForKeys
    -- * Folds and traversals
  , foldMapDiffEntry
  , traverseDiffEntryWithKey_
    -- * Filter
  , filterOnlyKey
  ) where

import           Prelude hiding (last, length, null, splitAt)

import           Control.Monad (void)
import           Data.Bifunctor
import           Data.Either (fromRight)
import           Data.Foldable hiding (null)
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
      -- | Considered unsafe to act on. Consider throwing an error when pattern
      -- matching on this constructor.
    | UnsafeAntiInsert !v
      -- | Considered unsafe to act on. Consider throwing an error when pattern
      -- matching on this constructor.
    | UnsafeAntiDelete !v
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

{------------------------------------------------------------------------------
  Conversion
------------------------------------------------------------------------------}

keysSet :: Diff k v -> Set k
keysSet (Diff m) = Map.keysSet m

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
--
-- POSTCONDITION: diffing computes a @'Diff'@ that is both normal and
-- positive.
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
last (getNEDiffHistory -> _ NESeq.:||> e) = e

{------------------------------------------------------------------------------
  Query
------------------------------------------------------------------------------}

null :: Diff k v -> Bool
null (Diff m) = Map.null m

size :: Diff k v -> Int
size (Diff m) = Map.size m

-- | A positive diff contains only @'Insert'@s and @'Delete'@s. A negative diff
-- can also contain @'UnsafeAntiInsert'@s and @'UnsafeAntiDelete'@s, which makes
-- applying diffs unsafe.
isPositive :: Diff k v -> Bool
isPositive (Diff m) = all (isPositiveDiffHistory . toDiffHistory) m

-- | Check if a diff history is positive, which means it can only contain
-- @'Insert'@s and @'Delete'@s.
isPositiveDiffHistory :: DiffHistory v -> Bool
isPositiveDiffHistory (DiffHistory vs) = all p vs
  where
    p (Insert _)           = True
    p (Delete _)           = True
    p (UnsafeAntiInsert _) = False
    p (UnsafeAntiDelete _) = False

-- | A normal diff has resolved all sums and subtractions internally. Applying a
-- non-normal diff is considered unsafe.
isNormal :: Eq v => Diff k v -> Bool
isNormal (Diff d) = all (isNormalDiffHistory . toDiffHistory) d

-- | Check if a diff history is in normal form, where no succesive elements are
-- inverses of each other.
--
-- If two succesive diff entries are inverses, they can be cancelled out. In
-- other words, we can normalise the diff history further by cancelling out the
-- diff entries. If so, we can conclude that the input diff history is not in
-- normal form.
isNormalDiffHistory :: Eq v => DiffHistory v -> Bool
isNormalDiffHistory (DiffHistory vs) =
    snd $ foldl' f (Nothing, True) vs
  where
    f (prevMay, b) cur = case prevMay of
      Nothing   -> (Just cur, b)
      Just prev -> (Just cur, b && not (areInverses prev cur))

{------------------------------------------------------------------------------
  @'Group'@ instances
------------------------------------------------------------------------------}

-- | Summing of diffs.
--
-- PRECONDITION: Normality is required for type class laws to hold.
--
-- INVARIANT: Summing preserves both positivity and normality.
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

-- | Identity diffs.
--
-- PRECONDITION: Normality is required for type class laws to hold.
--
-- INVARIANT: @'mempty'@ is both positive and normal.
instance (Ord k, Eq v) => Monoid (Diff k v) where
  mempty = Diff mempty

-- | Inverting diffs.
--
-- PRECONDITION: Normality is required for type class laws to hold.
--
-- INVARIANT: @'invert'@ preserves normality.
instance (Ord k, Eq v) => Group (Diff k v) where
  invert (Diff m) = Diff $
    fmap (unsafeFromDiffHistory . invert . toDiffHistory) m

-- | @h1 <> h2@ sums @h1@ and @h2@ by cancelling out as many consecutive diff
-- entries as possible, and appending the remainders.
--
-- PRECONDITION: Normality is required for type class laws to hold.
--
-- INVARIANT: Summing preserves both positivity and normality.
--
-- Diff entries that are each other's inverse (irrespective of their order) can
-- cancel out:
--
-- * @'UnsafeAntiInsert' x@ cancels out any @'Insert' y@ if @x == y@.
--
-- * @'UnsafeAntiDelete' x@ cancels out any @'Delete' y@ if @x == y@.
--
-- Note: We do not cancel out consecutive elements in @h1@ and @h2@
-- individually. It is only at the border between @h1@ and @h2@ that we cancel
-- out elements. This means that @'mappend'@ing two diff histories does not by
-- definition return a normal diff.
instance Eq v => Semigroup (DiffHistory v) where
  DiffHistory s1 <> DiffHistory s2 = DiffHistory $ s1 `mappend'` s2
    where
      -- At the ``touching'' ends of the sequences, take off diff entries that
      -- are each other's inverse until we find two non-inverse entries. In this
      -- case, we can not continue so we return the concatenated remainders.
      mappend' (xs :|> x) (y :<| ys)
        | areInverses x y                    = mappend' xs ys
      mappend' xs ys                         = xs Seq.>< ys

-- | Identity diff histories.
--
-- PRECONDITION: Normality is required for type class laws to hold.
--
-- INVARIANT: @'mempty'@ is both positive and normal.
instance Eq v => Monoid (DiffHistory v) where
  mempty = DiffHistory mempty

-- | Inverting diff histories.
--
-- PRECONDITION: Normality is required for type class laws to hold.
--
-- INVARIANT: @'invert'@ preserves normality.
instance Eq v => Group (DiffHistory v) where
  invert (DiffHistory s) = DiffHistory $ Seq.reverse . fmap invertDiffEntry $ s

-- | @`invertDiffEntry` e@ inverts a @'DiffEntry' e@ to its counterpart.
--
-- Note: We invert @'DiffEntry'@s, but a @'DiffEntry'@ is not a @'Group'@: we do
-- not have an identity element, so @'DiffEntry'@ is not a @'Monoid'@ or
-- @'Semigroup'@.
invertDiffEntry :: DiffEntry v -> DiffEntry v
invertDiffEntry = \case
  Insert x           -> UnsafeAntiInsert x
  Delete x           -> UnsafeAntiDelete x
  UnsafeAntiInsert x -> Insert x
  UnsafeAntiDelete x -> Delete x

-- | @'areInverses' e1 e2@ checks whether @e1@ and @e2@ are each other's
-- inverse.
areInverses :: Eq v => DiffEntry v -> DiffEntry v -> Bool
areInverses e1 e2 = invertDiffEntry e1 == e2

{------------------------------------------------------------------------------
  Applying diffs
------------------------------------------------------------------------------}

-- | Applies a diff to a @'Map'@.
--
-- PRECONDITION: The diff that is to be applied is both normal and positive.
--
-- POSTCONDITION: The result is @'Right' m@ for some @m@.
applyDiff ::
     Ord k
  => Map k v
  -> Diff k v
  -> Either () (Map k v)
applyDiff m (Diff diffs) =
    Merge.mergeA
      Merge.preserveMissing
      (Merge.traverseMaybeMissing newKeys)
      (Merge.zipWithMaybeAMatched oldKeys)
      m
      diffs
  where
    newKeys :: k -> NEDiffHistory v -> Either () (Maybe v)
    newKeys _k h = case last h of
      Insert x           -> Right $ Just x
      Delete _           -> Right Nothing
      UnsafeAntiInsert _ -> Left ()
      UnsafeAntiDelete _ -> Left ()

    oldKeys :: k -> v -> NEDiffHistory v -> Either () (Maybe v)
    oldKeys _k _v1 h = case last h of
      Insert x           -> Right $ Just x
      Delete _           -> Right Nothing
      UnsafeAntiInsert _ -> Left ()
      UnsafeAntiDelete _ -> Left ()

-- | Applies a diff to a @'Map'@ for a specific set of keys.
--
-- PRECONDITION: The diff that is to be applied is both normal and positive.
--
-- POSTCONDITION: The result is @'Right' m@ for some @m@.
applyDiffForKeys ::
     Ord k
  => Map k v
  -> Set k
  -> Diff k v
  -> Either () (Map k v)
applyDiffForKeys m ks (Diff diffs) =
  applyDiff
    m
    (Diff $ diffs `Map.restrictKeys` (Map.keysSet m `Set.union` ks))

-- | Applies a diff to a @'Map'@, throws an error if applying the diff failed.
--
-- PRECONDITION: The diff that is to be applied is both normal and positive.
--
-- POSTCONDITION: No error is thrown.
unsafeApplyDiff ::
     Ord k
  => Map k v
  -> Diff k v
  -> Map k v
unsafeApplyDiff m d = fromRight (error "applyDiff failed") $
  applyDiff m d

-- | Applies a diff to a @'Map'@ for a specific set of keys, throws an error if
-- applying the diff failed.
--
-- PRECONDITION: The diff that is to be applied is both normal and positive.
--
-- POSTCONDITION: No error is thrown.
unsafeApplyDiffForKeys ::
     Ord k
  => Map k v
  -> Set k
  -> Diff k v
  -> Map k v
unsafeApplyDiffForKeys m s d = fromRight (error "applyDiffForKeys failed") $
  applyDiffForKeys m s d

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
