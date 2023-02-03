{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Test.Util (OftenSmall (..)) where

import           Test.QuickCheck

newtype OftenSmall a = OftenSmall a
  deriving newtype (Show, Eq, Ord)

instance (Arbitrary a, Integral a) => Arbitrary (OftenSmall a) where
  arbitrary = frequency [
      (10, OftenSmall . fromIntegral <$> chooseInt (-5, 5))
    , (1, OftenSmall <$> arbitrary)
    ]
  shrink (OftenSmall x) = OftenSmall <$> shrink x
