{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Differences on 'Map's, represented as cancellative monoids.
module Data.Map.Diff.Strict (
    -- * Types
    Delta (..)
  , Diff
    -- * Conversion
  , keysSet
    -- * Construction
  , diff
  , empty
    -- ** Maps
  , fromMap
  , fromMapDeletes
  , fromMapInserts
    -- ** Lists
  , fromList
  , fromListDeletes
  , fromListInserts
    -- * Query
    -- ** Size
  , null
  , numDeletes
  , numInserts
  , size
    -- * Applying diffs
  , applyDiff
  , applyDiffForKeys
    -- * Folds and traversals
  , foldMapDelta
  , traverseDeltaWithKey_
    -- * Filter
  , filterOnlyKey
  ) where

import           Data.Map.Diff.Strict.Internal
import           Data.Map.Strict (Map)
import           Prelude hiding (null)
