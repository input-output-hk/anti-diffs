{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Test.Util (OftenSmall (..)) where

import           Test.QuickCheck

-- | See the @'Arbitrary'@ instance for this type.
newtype OftenSmall a = OftenSmall a
  deriving newtype (Show, Eq, Ord)

-- | This instance will generate a small @a@ with high probability, and it will
-- defer to @a@'s generator with a low probability. Shrinking is deferred to
-- @a@'s shrinker.
--
-- Generating @a@'s in a small range can sometimes be essential for hitting
-- interesting cases in property tests. See "Test.Data.Map.Diff.Strict" for
-- examples. We defer to @a@'s generator with low probability to diversify the
-- test cases we hit.
instance (Arbitrary a, Integral a) => Arbitrary (OftenSmall a) where
  arbitrary = frequency [
      (10, OftenSmall . fromIntegral <$> chooseInt (-5, 5))
    , (1, OftenSmall <$> arbitrary)
    ]
  shrink (OftenSmall x) = OftenSmall <$> shrink x
