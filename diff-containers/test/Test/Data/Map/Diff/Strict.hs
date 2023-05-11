{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import qualified Data.Map.Diff.Strict as Diff
import           Data.Map.Diff.Strict.Internal (Diff (..), DiffEntry (..),
                     DiffHistory (DiffHistory), NEDiffHistory (NEDiffHistory))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as Set
import           Data.Typeable
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Classes.Semigroup.Cancellative
import           Test.Tasty (TestTree, localOption, testGroup)
import           Test.Tasty.QuickCheck hiding (Negative, Positive)
import           Test.Util

-- | Tests for "Data.Map.Diff.Strict".
--
-- Note: the laws from "Test.QuickCheck.Classes.Semigroup.Cancellative" are not
-- affected by @'localOption' ('QuickCheckTests' n)@
tests :: TestTree
tests = testGroup "Data.Map.Diff.Strict" [
      localOption (QuickCheckTests 10000) $
      testGroup "quickcheck-classes" [
          lawsTestOne  (Proxy @(NEDiffHistory (OftenSmall Int))) [
              semigroupLaws
            ]
        , lawsTestOne  (Proxy @(DiffHistory (OftenSmall Int))) [
              semigroupLaws
            , monoidLaws
            , leftReductiveLaws
            , rightReductiveLaws
            , leftCancellativeLaws
            , rightCancellativeLaws
            ]
        , lawsTestOne  (Proxy @(Diff (OftenSmall Int) (OftenSmall Int))) [
              semigroupLaws
            , monoidLaws
            , leftReductiveLaws
            , rightReductiveLaws
            , leftCancellativeLaws
            , rightCancellativeLaws
            ]
        ]
    , testGroup "Applying diffs" [
          localOption (QuickCheckTests 10000) $
          testProperty "prop_diffThenApply" $
            prop_diffThenApply @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 10000) $
          testProperty "prop_applyMempty" $
            prop_applyMempty @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 1000) $
          testProperty "prop_applyAllAndApplySum" $
            prop_applyAllAndApplySum @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 10000) $
          testProperty "prop_applyDiffNumInsertsDeletes" $
            prop_applyDiffNumInsertsDeletes @(OftenSmall Int) @(OftenSmall Int)
        , localOption (QuickCheckTests 10000) $
          testProperty "prop_applyDiffNumInsertsDeletesExact" $
            prop_applyDiffNumInsertsDeletesExact @Int @Int
        ]
    ]

{------------------------------------------------------------------------------
  Running laws in test trees
------------------------------------------------------------------------------}

lawsTest :: Laws -> TestTree
lawsTest Laws{lawsTypeclass, lawsProperties} = testGroup lawsTypeclass $
    fmap (uncurry testProperty) lawsProperties

lawsTestOne :: Typeable a => Proxy a -> [Proxy a -> Laws] -> TestTree
lawsTestOne p tts =
    testGroup (show $ typeOf p) (fmap (\f -> lawsTest $ f p) tts)

{------------------------------------------------------------------------------
  Applying diffs
------------------------------------------------------------------------------}

-- | Applying a @'Diff'@ computed from a source and target @'Map'@ should
-- produce the target @'Map'@.
prop_diffThenApply ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> Map k v
  -> Property
prop_diffThenApply m1 m2 = Diff.applyDiff m1 (Diff.diff m1 m2) === m2

-- | Applying an empty @'Diff'@ is the identity function.
prop_applyMempty ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> Property
prop_applyMempty m = Diff.applyDiff m mempty === m

-- | Applying a sum of @'Diff'@s is equivalent to applying each @'Diff'@
-- separately (in order).
prop_applyAllAndApplySum ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> [Diff k v]
  -> Property
prop_applyAllAndApplySum m ds =
  foldl Diff.applyDiff m ds === Diff.applyDiff m (mconcat ds)

-- | Applying a @'Diff' d@ to a @'Map' m@ increases the size of @m@ by exactly
-- @numInserts d - numDeletes d@ if @d@ inserts only new keys and @d@ only
-- deletes existing keys.
--
-- Diffing two 'Map's that have disjoint keysets creates exactly a diff @d@ that
-- only inserts new keys and deletes existing keys.
prop_applyDiffNumInsertsDeletesExact ::
     (Ord k, Eq v)
  => Map k v
  -> Map k v
  -> Property
prop_applyDiffNumInsertsDeletesExact m1 m2 =
    Map.keysSet m1 `Set.disjoint` Map.keysSet m2 ==>
      Map.size (Diff.applyDiff m1 d) ===
        Map.size m1 + Diff.numInserts d - Diff.numDeletes d
  where
    d = Diff.diff m1 m2

-- | Applying a @'Diff' d@ to a @'Map' m@ may increase/decrease the size of @m@
-- up to bounds depending on the number of inserts and deletes in @d@.
--
-- * The size of @m@ may /decrease/ by up to the number of deletes in @d@. This
--   happens if @d@ does not insert any new keys.
-- * The size of @m@ may /increase/ by up to the number of inserts in @d@. This
--   if @d@ does not delete any existing keys.
prop_applyDiffNumInsertsDeletes ::
     Ord k
  => Map k v -> Diff k v -> Property
prop_applyDiffNumInsertsDeletes m d = property $
    lb <= n' && n' <= ub
  where
    n        = Map.size m
    nInserts = Diff.numInserts d
    nDeletes = Diff.numDeletes d
    n'  = Map.size (Diff.applyDiff m d)
    lb = n - nDeletes
    ub = n + nInserts

{------------------------------------------------------------------------------
  Plain @'Arbitrary'@ instances
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)
deriving newtype instance Arbitrary v => Arbitrary (DiffHistory v)

instance Arbitrary v => Arbitrary (NEDiffHistory v) where
  arbitrary = NEDiffHistory <$> ((:<||) <$> arbitrary <*> arbitrary)
  shrink (NEDiffHistory h) =
    fmap NEDiffHistory $ mapMaybe NESeq.nonEmptySeq $ shrink (NESeq.toSeq h)

instance Arbitrary v => Arbitrary (DiffEntry v) where
  arbitrary = oneof [
      Insert <$> arbitrary
    , Delete <$> arbitrary
    ]
  shrink de = case de of
    Insert x -> Insert <$> shrink x
    Delete x -> Delete <$> shrink x
