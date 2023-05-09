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
import           Data.Maybe
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
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
