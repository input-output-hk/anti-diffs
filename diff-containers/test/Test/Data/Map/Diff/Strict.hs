{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Foldable
import           Data.Map.Strict (Map)
import           Data.Proxy (Proxy (Proxy))

import           Test.Tasty (TestTree, localOption, testGroup)
import           Test.Tasty.QuickCheck hiding (Negative, Positive)

import           Data.Map.Diff.Strict.Internal
import           Data.Semigroupoid.Simple.Laws

import           Test.Util

tests :: TestTree
tests = testGroup "Data.Map.Diff.Strict" [
      localOption (QuickCheckTests 1000) $
      testGroupWithProxy (Proxy @(Diff (OftenSmall Int) (OftenSmall Int))) [
          testSemigroupLaws
        , testMonoidLaws
        ]
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
    ]

{------------------------------------------------------------------------------
  Simple properties
------------------------------------------------------------------------------}

prop_diffThenApply ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> Map k v
  -> Property
prop_diffThenApply m1 m2 = applyDiff m1 (diff m1 m2) === m2

prop_applyMempty ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> Property
prop_applyMempty m = applyDiff m mempty === m

prop_applyAllAndApplySum ::
     (Show k, Show v, Ord k, Eq v)
  => Map k v
  -> [Diff k v]
  -> Property
prop_applyAllAndApplySum m ds =
  foldl' applyDiff m ds === applyDiff m (mconcat ds)

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)

instance Arbitrary v => Arbitrary (DiffEntry v) where
  arbitrary = oneof [
      Insert <$> arbitrary
    , Delete <$> arbitrary
    ]
  shrink = traverse shrink
