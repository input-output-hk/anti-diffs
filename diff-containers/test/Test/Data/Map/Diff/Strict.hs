{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Foldable (foldl')
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Proxy (Proxy (Proxy))
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import           Data.Map.Diff.Strict

import           Data.Semigroupoid.Simple.Auto
import           Data.Semigroupoid.Simple.Laws

tests :: TestTree
tests = testGroup "Data.Map.Diff.Strict" [
      testGroupWithProxy (Proxy @(DiffEntry (Smaller Int))) [
      ]
    , testGroupWithProxy (Proxy @(DiffHistory (Smaller Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroupWithProxy (Proxy @(Auto (DiffHistory (Smaller Int)))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    , testGroupWithProxy (Proxy @(Diff (Smaller Int) (Smaller Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroupWithProxy (Proxy @(Auto (Diff (Smaller Int) (Smaller Int)))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    , testGroupWithProxy (Proxy @(Act (Smaller Int))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    , testProperty "prop_diffThenApply @(Smaller Int)" $
        prop_diffThenApply @(Smaller Int) @(Smaller Int)
    , testProperty "prop_diffThenApply @Int" $
        prop_diffThenApply @Int @Int
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

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

newtype Smaller a = Smaller a
  deriving newtype (Show, Eq, Ord)

instance Integral a => Arbitrary (Smaller a) where
  arbitrary = Smaller . fromIntegral <$> chooseInt (-5, 5)
  shrink (Smaller x) = Smaller . fromIntegral <$> shrink @Int (fromIntegral x)

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
  shrink = \case
    Insert x           -> Insert <$> shrink x
    Delete x           -> Delete <$> shrink x
    UnsafeAntiInsert x -> UnsafeAntiInsert <$> shrink x
    UnsafeAntiDelete x -> UnsafeAntiDelete <$> shrink x

instance Arbitrary v => Arbitrary (Act v) where
  arbitrary = oneof [
      Ins <$> arbitrary
    , Del <$> arbitrary
    , pure InsDel
    , DelIns <$> arbitrary <*> arbitrary
    ]
  shrink = \case
    Ins x      -> Ins <$> shrink x
    Del x      -> Del <$> shrink x
    InsDel     -> []
    DelIns x y -> DelIns <$> shrink x <*> shrink y
