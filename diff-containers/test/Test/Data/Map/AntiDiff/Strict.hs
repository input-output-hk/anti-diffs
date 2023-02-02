{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.AntiDiff.Strict (tests) where

import           Data.Foldable
import           Data.Maybe
import           Data.Proxy (Proxy (Proxy))
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq

import           Test.Tasty (TestTree, localOption, testGroup)
import           Test.Tasty.QuickCheck hiding (Negative, Positive)

import           Data.Map.AntiDiff.Strict.Internal
import           Data.Map.Diff.Strict
import           Data.Semigroupoid.Simple.Laws

import           Test.Data.Map.Diff.Strict ()

import           Test.Util

tests :: TestTree
tests = testGroup "Data.Map.AntiDiff.Strict" [
      localOption (QuickCheckTests 1000) $
      testGroupWithProxy (Proxy @(DiffHistory (OftenSmall Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , localOption (QuickCheckTests 1000) $
      testGroupWithProxy (Proxy @(AntiDiff (OftenSmall Int) (OftenSmall Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , localOption (QuickCheckTests 10000) $
      testProperty "prop_toAntiIsPositive" $
        prop_toAntiIsPositive @(OftenSmall Int) @(OftenSmall Int)
    , localOption (QuickCheckTests 10000) $
      testProperty "prop_toAntiFromAntiId" $
        prop_toAntiFromAntiId @(OftenSmall Int) @(OftenSmall Int)
    ]

{------------------------------------------------------------------------------
  Simple properties
------------------------------------------------------------------------------}

prop_toAntiIsPositive ::
     Diff k v
  -> Property
prop_toAntiIsPositive d = property $ isPositive (toAnti d)

prop_toAntiFromAntiId ::
     (Eq k, Eq v)
  => Diff k v
  -> Property
prop_toAntiFromAntiId d = property $ case fromAnti (toAnti d) of
  Nothing -> False
  Just d' -> d == d'

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

isPositive :: AntiDiff k v -> Bool
isPositive (AntiDiff m) = all (isPositiveDiffHistory . toDiffHistory) m

isPositiveDiffHistory :: DiffHistory v -> Bool
isPositiveDiffHistory (DiffHistory vs) = all p vs
  where
    p (Positive _) = True
    p (Negative _) = False

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Eq v, Arbitrary k, Arbitrary v)
                       => Arbitrary (AntiDiff k v)


instance (Arbitrary v, Eq v) => Arbitrary (NEDiffHistory v) where
  arbitrary = (NEDiffHistory <$> ((:<||) <$> arbitrary <*> arbitrary))
    `suchThat` (isNormal . toDiffHistory)
  shrink (NEDiffHistory h) =
    fmap NEDiffHistory $ mapMaybe NESeq.nonEmptySeq $ shrink (NESeq.toSeq h)

instance (Arbitrary v, Eq v) => Arbitrary (DiffHistory v) where
  arbitrary = (DiffHistory <$> arbitrary) `suchThat` isNormal
  shrink = traverse shrink

instance Arbitrary v => Arbitrary (AntiDiffEntry v) where
  arbitrary = elements [Positive, Negative] <*> arbitrary
  shrink = traverse shrink
