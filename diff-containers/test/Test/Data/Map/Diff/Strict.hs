{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Either
import           Data.Foldable
import           Data.Group (Group (..))
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Proxy (Proxy (Proxy))
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq

import           Test.Tasty (TestTree, localOption, testGroup)
import           Test.Tasty.QuickCheck hiding (Negative, Positive)

import           Data.Map.Diff.Strict.Internal hiding (null)

import           Data.Semigroupoid.Simple.Laws

import           Test.Util

-- | Tests for "Data.Map.Diff.Strict".
--
-- === The use of @'OftenSmall'@
--
-- Throughout these tests, we often use/test the @'Group'@ instances for
-- @'DiffHistory'@ and @'Diff'@. For @'mappend'@, @'mempty'@ and @'invert'@ to
-- do interesting things, we should generate values in a small range. Examples:
--
-- * An @'Insert' x@ and @'UnsafeAntiInsert' y@ can only cancel out if @x == y@.
-- If the range that we pick @x@ and @y@ from is large, then the probability
-- that @x == y@ is small.
--
-- * Only if two @'mappend'@ed diffs contain the same key will the corresponding
-- diff histories be @'mappend'@ed. If we pick keys in diffs from a large range,
-- then the probability of matching keys is low.
--
-- We use the @'OftenSmall'@ wrapper and its @'Arbitrary'@ instance to generate
-- small values often.
tests :: TestTree
tests = testGroup "Data.Map.Diff.Strict" [
      localOption (QuickCheckTests 10000) $
      testGroupWithProxy (Proxy @(NormalDiffHistory (OftenSmall Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , localOption (QuickCheckTests 1000) $
      testGroupWithProxy (Proxy @(NormalDiff (OftenSmall Int) (OftenSmall Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroup "diffing" [
          localOption (QuickCheckTests 10000) $
          testProperty "prop_diffingIsPositive" $
            prop_diffingIsPositive @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 10000) $
          testProperty "prop_diffingIsNormal" $
            prop_diffingIsNormal @(OftenSmall Int) @(OftenSmall Int)
        ]
    , testGroup "Preserving positivity and normality" [
          localOption (QuickCheckTests 1000) $
          testProperty "prop_summingDiffsPreservesPositivity" $
            prop_summingDiffsPreservesPositivity @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 10000) $
          testProperty "prop_summingDiffHistoriesPreservesPositivity" $
            prop_summingDiffHistoriesPreservesPositivity @(OftenSmall Int)
        , localOption (QuickCheckTests 10000) $
          testProperty "prop_summingDiffsPreservesNormality" $
            prop_summingDiffsPreservesNormality @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 100000) $
          testProperty "prop_summingDiffHistoriesPreservesNormality" $
            prop_summingDiffHistoriesPreservesNormality @(OftenSmall Int)
        , localOption (QuickCheckTests 10000) $
          testProperty "prop_invertingDiffsPreservesNormality" $
            prop_invertingDiffsPreservesNormality @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 100000) $
          testProperty "prop_invertingDiffHistoriesPreservesNormality" $
            prop_invertingDiffHistoriesPreservesNormality @(OftenSmall Int)
        ]
    , testGroup "Positivity and normality for identity elements" [
          testProperty "prop_emptyDiffIsPositive" prop_emptyDiffIsPositive
        , testProperty "prop_emptyDiffHistoryIsPositive" prop_emptyDiffHistoryIsPositive
        , testProperty "prop_emptyDiffIsNormal" prop_emptyDiffIsNormal
        , testProperty "prop_emptyDiffHistoryIsNormal" prop_emptyDiffHistoryIsNormal
        ]
    , testGroup "Applying diffs" [
          localOption (QuickCheckTests 10000) $
          testProperty "prop_diffThenApply" $
            prop_diffThenApply @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 10000) $
          testProperty "prop_applyMempty" $
            prop_applyMempty @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 100) $
          testProperty "prop_applyAllAndApplySum" $
            prop_applyAllAndApplySum @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 1000) $
          testProperty "prop_normalAndPositiveDiffsNeverFailApply" $
            prop_normalAndPositiveDiffsNeverFailApply @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 1000) $
          testProperty "prop_normalAndPositiveDiffsNeverFailUnsafeApply" $
            prop_normalAndPositiveDiffsNeverFailUnsafeApply @(OftenSmall Int) @(OftenSmall Int)
        ]
    ]

{------------------------------------------------------------------------------
  Simple properties
------------------------------------------------------------------------------}

-- | A @'Diff'@ computed from two @'Map'@s is positive.
prop_diffingIsPositive ::
     (Ord k, Eq v)
  => Map k v
  -> Map k v
  -> Property
prop_diffingIsPositive m1 m2 = property $ isPositive (diff m1 m2)

-- | A @'Diff'@ computed from two @'Map'@s is normal.
prop_diffingIsNormal ::
     (Ord k, Eq v)
  => Map k v
  -> Map k v
  -> Property
prop_diffingIsNormal m1 m2 = property $ isNormal (diff m1 m2)

-- | Test the invariant that summing @'Diff'@s preserves positivity.
prop_summingDiffsPreservesPositivity ::
     (Ord k, Eq v)
  => PositiveDiff k v
  -> PositiveDiff k v
  -> Property
prop_summingDiffsPreservesPositivity (PositiveDiff d1) (PositiveDiff d2) =
  isPositive d1 && isPositive d2 ==> isPositive (d1 <> d2)

-- | Test the invariant that summing @'DiffHistory'@s preserves positivity.
prop_summingDiffHistoriesPreservesPositivity ::
     Eq v
  => PositiveDiffHistory v
  -> PositiveDiffHistory v
  -> Property
prop_summingDiffHistoriesPreservesPositivity (PositiveDiffHistory h1) (PositiveDiffHistory h2) =
  isPositiveDiffHistory h1 && isPositiveDiffHistory h2 ==> isPositiveDiffHistory (h1 <> h2)

-- | Test the invariant that summing @'Diff'@s preserves normality.
prop_summingDiffsPreservesNormality ::
     (Ord k, Eq v)
  => NormalDiff k v
  -> NormalDiff k v
  -> Property
prop_summingDiffsPreservesNormality (NormalDiff d1) (NormalDiff d2) =
  isNormal d1 && isNormal d2 ==> isNormal (d1 <> d2)

-- | Test the invariant that summing @'DiffHistory'@s preserves normality.
prop_summingDiffHistoriesPreservesNormality ::
     Eq v
  => NormalDiffHistory v
  -> NormalDiffHistory v
  -> Property
prop_summingDiffHistoriesPreservesNormality (NormalDiffHistory h1) (NormalDiffHistory h2) =
  isNormalDiffHistory h1 && isNormalDiffHistory h2 ==> isNormalDiffHistory (h1 <> h2)

-- | Test the invariant that inverting @'Diff'@s preserves normality.
prop_invertingDiffsPreservesNormality ::
     (Ord k, Eq v)
  => NormalDiff k v
  -> Property
prop_invertingDiffsPreservesNormality (NormalDiff d) =
  isNormal d ==> isNormal (invert d)

-- | Test the invariant that inverting @'DiffHistory'@s preserves normality.
prop_invertingDiffHistoriesPreservesNormality ::
     Eq v
  => NormalDiffHistory v
  -> Property
prop_invertingDiffHistoriesPreservesNormality (NormalDiffHistory h) =
  isNormalDiffHistory h ==> isNormalDiffHistory (invert h)

-- | Test the invariant that the identity @'Diff'@ element is positive.
prop_emptyDiffIsPositive :: Property
prop_emptyDiffIsPositive = once $ isPositive (mempty :: Diff Int Int)

-- | Test the invariant that the identity @'DiffHistory'@ element is positive.
prop_emptyDiffHistoryIsPositive :: Property
prop_emptyDiffHistoryIsPositive = once $ isPositiveDiffHistory (mempty :: DiffHistory Int)

-- | Test the invariant that the identity @'Diff'@ element is normal.
prop_emptyDiffIsNormal :: Property
prop_emptyDiffIsNormal = once $ isNormal (mempty :: Diff Int Int)

-- | Test the invariant that the identity @'DiffHistory'@ element is normal.
prop_emptyDiffHistoryIsNormal:: Property
prop_emptyDiffHistoryIsNormal = once $ isNormalDiffHistory (mempty :: DiffHistory Int)

-- | Applying a @'Diff'@ computed from a source and target @'Map'@ should
-- produce the target @'Map'@.
prop_diffThenApply ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> Map k v
  -> Property
prop_diffThenApply m1 m2 = applyDiff m1 (diff m1 m2) === Right m2

-- | Applying an empty @'Diff'@ is the identity function.
prop_applyMempty ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> Property
prop_applyMempty m = applyDiff m mempty === Right m

-- | Applying a sum of normal and positive @'Diff'@s is equivalent to applying
-- each (normal and positive) @'Diff'@ separately (in order).
prop_applyAllAndApplySum ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> [NPDiff k v]
  -> Property
prop_applyAllAndApplySum m (fmap getNPDiff -> ds) =
  all isPositive ds && all isNormal ds ==> foldlM applyDiff m ds === applyDiff m (mconcat ds)

-- | Applying a diff that is both positive and normal will never fail.
prop_normalAndPositiveDiffsNeverFailApply ::
     (Ord k, Eq v)
  => Map k v
  -> NPDiff k v
  -> Property
prop_normalAndPositiveDiffsNeverFailApply m (NPDiff d) =
  isPositive d && isNormal d ==> isRight (applyDiff m d)


-- | Unsafely applying a diff that is both positive and normal will never throw
-- an error.
prop_normalAndPositiveDiffsNeverFailUnsafeApply ::
     (Ord k, Eq v)
  => Map k v
  -> NPDiff k v
  -> Property
prop_normalAndPositiveDiffsNeverFailUnsafeApply m (NPDiff d) =
  isPositive d && isNormal d ==> unsafeApplyDiff m d `seq` ()

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)

instance Arbitrary v => Arbitrary (NEDiffHistory v) where
  arbitrary = NEDiffHistory <$> ((:<||) <$> arbitrary <*> arbitrary)
  shrink (NEDiffHistory h) =
    fmap NEDiffHistory $ mapMaybe NESeq.nonEmptySeq $ shrink (NESeq.toSeq h)

instance Arbitrary v => Arbitrary (DiffHistory v) where
  arbitrary = DiffHistory <$> arbitrary
  shrink (DiffHistory s) = DiffHistory <$> shrink s

instance Arbitrary v => Arbitrary (DiffEntry v) where
  arbitrary = oneof [
      Insert <$> arbitrary
    , Delete <$> arbitrary
    , UnsafeAntiInsert <$> arbitrary
    , UnsafeAntiDelete <$> arbitrary
    ]
  shrink de = case de of
    Insert x           -> Insert <$> shrink x
    Delete x           -> Delete <$> shrink x
    UnsafeAntiInsert x -> UnsafeAntiInsert <$> shrink x
    UnsafeAntiDelete x -> UnsafeAntiDelete <$> shrink x

{------------------------------------------------------------------------------
  Modifiers: positivity
------------------------------------------------------------------------------}

-- | A @'Diff'@ for which @'isPositive'@ holds.
newtype PositiveDiff k v = PositiveDiff { getPositiveDiff :: Diff k v }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid, Group)

instance (Ord k, Arbitrary k, Arbitrary v)
      => Arbitrary (PositiveDiff k v) where
  arbitrary               = PositiveDiff <$> arbitrary `suchThat` isPositive
  shrink (PositiveDiff d) = [PositiveDiff d' | d' <- shrink d, isPositive d']

-- | A @'DiffHistory'@ for which @'isPositiveDiffHistory'@ holds.
newtype PositiveDiffHistory v = PositiveDiffHistory {
    getPositiveDiffHistory :: DiffHistory v
  }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid, Group)

instance Arbitrary v => Arbitrary (PositiveDiffHistory v) where
  arbitrary = PositiveDiffHistory <$> arbitrary `suchThat` isPositiveDiffHistory
  shrink (PositiveDiffHistory h) = [PositiveDiffHistory h' | h' <- shrink h, isPositiveDiffHistory h']

{------------------------------------------------------------------------------
  Modifiers: normality
------------------------------------------------------------------------------}

-- | A @'Diff'@ for which @'isNormal'@ holds.
newtype NormalDiff k v = NormalDiff { getNormalDiff :: Diff k v }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid, Group)

instance (Ord k, Eq v, Arbitrary k, Arbitrary v)
      => Arbitrary (NormalDiff k v) where
  arbitrary             = NormalDiff <$> arbitrary `suchThat` isNormal
  shrink (NormalDiff d) = [NormalDiff d' | d' <- shrink d, isNormal d']

-- | A @'DiffHistory'@ for which @'isNormalDiffHistory'@ holds.
newtype NormalDiffHistory v = NormalDiffHistory {
    getNormalDiffHistory :: DiffHistory v
  }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid, Group)

instance (Arbitrary v, Eq v) => Arbitrary (NormalDiffHistory v) where
  arbitrary = NormalDiffHistory <$> arbitrary `suchThat` isNormalDiffHistory
  shrink (NormalDiffHistory h) = [NormalDiffHistory h' | h' <- shrink h, isNormalDiffHistory h']

{------------------------------------------------------------------------------
  Modifiers: positivity and normality
------------------------------------------------------------------------------}

-- | A @'Diff'@ for which @'isNormal'@ and @'isPositive'@ hold.
newtype NPDiff k v = NPDiff { getNPDiff :: Diff k v }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid, Group)

instance (Ord k, Eq v, Arbitrary k, Arbitrary v)
      => Arbitrary (NPDiff k v) where
  arbitrary         = NPDiff <$> arbitrary `suchThat` (\d -> isNormal d && isPositive d)
  shrink (NPDiff d) = [NPDiff d' | d' <- shrink d, isNormal d', isPositive d']

-- | A @'DiffHistory'@ for which @'isNormalDiffHistory'@ and @'isPositiveDiffHistory'@ hold.
newtype NPDiffHistory v = NPDiffHistory {
    getNPDiffHistory :: DiffHistory v
  }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid, Group)

instance (Arbitrary v, Eq v) => Arbitrary (NPDiffHistory v) where
  arbitrary = NPDiffHistory <$> arbitrary `suchThat` (\d -> isNormalDiffHistory d && isPositiveDiffHistory d)
  shrink (NPDiffHistory h) = [NPDiffHistory h' | h' <- shrink h, isNormalDiffHistory h', isPositiveDiffHistory h']
