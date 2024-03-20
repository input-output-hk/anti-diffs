{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Map.Diff.Class (
    ApplyDiff (..)
  , prop_diffThenApply
  , prop_applyMempty
  , prop_applySum
  ) where

import           Data.Foldable as F
import qualified Data.Map.Diff.Simple.Strict as Simple
import qualified Data.Map.Diff.Strict as Anti
import           Data.Map.Strict as Map
import           Data.Proxy (Proxy)
import           Test.QuickCheck

class ApplyDiff a d where
  diff :: a -> a -> d
  applyDiff :: a -> d -> a

instance (Ord k, Eq v) => ApplyDiff (Map k v) (Simple.Diff k v) where
  diff = Simple.diff
  applyDiff = Simple.applyDiff

instance (Ord k, Eq v) => ApplyDiff (Map k v) (Anti.Diff k v) where
  diff = Anti.diff
  applyDiff = Anti.applyDiff

-- | Applying a diff computed from a source and target value should
-- produce the target value.
prop_diffThenApply ::
     forall a d. (ApplyDiff a d, Eq a, Show a)
  => Proxy a
  -> Proxy d
  -> a
  -> a
  -> Property
prop_diffThenApply _ _ x y = applyDiff @a @d x (diff x y) === y

-- | Applying an empty diff is the identity function.
prop_applyMempty ::
     forall a d. (ApplyDiff a d, Monoid d, Eq a, Show a)
  => Proxy a
  -> Proxy d
  -> a
  -> Property
prop_applyMempty _ _ x = applyDiff @a @d x (mempty @d) === x

-- | Applying a sum of diffs is equivalent to applying each @'Diff'@
-- separately (in order).
prop_applySum ::
     forall a d f. (ApplyDiff a d, Monoid d, Eq a, Show a, Foldable f)
  => Proxy a
  -> Proxy d
  -> Proxy f
  -> a
  -> f d
  -> Property
prop_applySum _ _ _ x ds =
  F.foldl' applyDiff x ds === applyDiff x (foldMap' id ds)
