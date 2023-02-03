{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Foldable
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

tests :: TestTree
tests = testGroup "Data.Map.Diff.Strict" [
      localOption (QuickCheckTests 1000) $
      testGroupWithProxy (Proxy @(DiffHistory (OftenSmall Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , localOption (QuickCheckTests 1000) $
      testGroupWithProxy (Proxy @(Diff (OftenSmall Int) (OftenSmall Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , localOption (QuickCheckTests 10000) $
      testProperty "prop_diffingIsPositive" $
        prop_diffingIsPositive @(OftenSmall Int) @(OftenSmall Int)
    , localOption (QuickCheckTests 10000) $
      testProperty "prop_diffThenApply" $
        prop_diffThenApply @(OftenSmall Int) @(OftenSmall Int)
    , localOption (QuickCheckTests 10000) $
      testProperty "prop_applyMempty" $
        prop_applyMempty @(OftenSmall Int) @(OftenSmall Int)
    , localOption (QuickCheckMaxRatio 100) $
      localOption (QuickCheckTests 1000) $
      testProperty "prop_applyAllAndApplySum" $
        prop_applyAllAndApplySum @(OftenSmall Int) @(OftenSmall Int)
    , localOption (QuickCheckMaxRatio 100) $
      localOption (QuickCheckTests 1000) $
      testProperty "prop_unsafeApplyAllAndUnsafeApplySum" $
        prop_unsafeApplyAllAndUnsafeApplySum @(OftenSmall Int) @(OftenSmall Int)
    ]

{------------------------------------------------------------------------------
  Simple properties
------------------------------------------------------------------------------}

prop_diffingIsPositive ::
     (Ord k, Eq v)
  => Map k v
  -> Map k v
  -> Property
prop_diffingIsPositive m1 m2 = property $ isPositive (diff m1 m2)

prop_diffThenApply ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> Map k v
  -> Property
prop_diffThenApply m1 m2 = applyDiff m1 (diff m1 m2) === Right m2

prop_applyMempty ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> Property
prop_applyMempty m = applyDiff m mempty === Right m

prop_applyAllAndApplySum ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> [Diff k v]
  -> Property
prop_applyAllAndApplySum m ds =
  all isPositive ds ==> foldlM applyDiff m ds === applyDiff m (mconcat ds)

prop_unsafeApplyAllAndUnsafeApplySum ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> [Diff k v]
  -> Property
prop_unsafeApplyAllAndUnsafeApplySum m ds =
  all isPositive ds ==> foldl' unsafeApplyDiff m ds === unsafeApplyDiff m (mconcat ds)

{------------------------------------------------------------------------------
  Preconditions
------------------------------------------------------------------------------}

-- | Check if a diff history is in normal form, where no succesive elements are
-- inverses of each other.
--
-- If two succesive diff entries are inverses, they can be cancelled out. In
-- other words, we can normalise the diff history further by cancelling out the
-- diff entries. If so, we can conclude that the input diff history is not in
-- normal form.
isNormal :: Eq v => DiffHistory v -> Bool
isNormal (DiffHistory vs) =
    snd $ foldl' f (Nothing, True) vs
  where
    f (prevMay, b) cur = case prevMay of
      Nothing   -> (Just cur, b)
      Just prev -> (Just cur, b && not (areInverses prev cur))

isPositive :: Diff k v -> Bool
isPositive (Diff m) = all (isPositiveDiffHistory . toDiffHistory) m

isPositiveDiffHistory :: DiffHistory v -> Bool
isPositiveDiffHistory (DiffHistory vs) = all p vs
  where
    p (Insert _)           = True
    p (Delete _)           = True
    p (UnsafeAntiInsert _) = False
    p (UnsafeAntiDelete _) = False

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Eq v, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)

instance (Arbitrary v, Eq v) => Arbitrary (NEDiffHistory v) where
  arbitrary = (NEDiffHistory <$> ((:<||) <$> arbitrary <*> arbitrary))
    `suchThat` (isNormal . toDiffHistory)
  shrink (NEDiffHistory h) =
    fmap NEDiffHistory $ mapMaybe NESeq.nonEmptySeq $ shrink (NESeq.toSeq h)

instance (Arbitrary v, Eq v) => Arbitrary (DiffHistory v) where
  arbitrary = (DiffHistory <$> arbitrary)
    `suchThat` isNormal
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
