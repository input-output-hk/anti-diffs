module Data.Map.Diff.Strict (
    -- * Types
    Diff (..)
  , DiffEntry (..)
    -- * Conversion
  , keysSet
    -- * Construction
  , diff
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
  , size
    -- * Applying diffs
  , applyDiff
  , applyDiffForKeys
    -- * Traversal
  , traverseDiffEntryWithKey_
    -- * Folds
  , foldMapDiffEntry
    -- * Filter
  , filterOnlyKey
  ) where

import           Prelude hiding (null)

import           Data.Map.Diff.Strict.Internal
