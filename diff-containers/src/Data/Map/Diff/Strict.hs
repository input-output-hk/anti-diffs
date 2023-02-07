{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Differences on @'Map'@s with a @'Group'@ instance.
--
-- === Positivity and normality
--
-- It is an unfortunate side effect of facilitating a @'Group'@ instance for
-- @'Diff'@s that we can get into situations where applying diffs will fail or
-- produce wrong results due to diffs containing internally unresolved sums and
-- subtractions. The responsibility of downstream code is to ensure that diffs
-- that are applied are both /positive/ (@'isPositive'@) and /normal/
-- @('isNormal')@. If that is the case, then applying diffs will never go wrong.
--
-- A number of definitions in this modules are annotated with PRECONDITION,
-- INVARIANT and POSTCONDITION. Use these and the type class laws for
-- @'Semigroup'@, @'Monoid'@ and @'Group'@ (which hold given preconditions) to
-- ensure that applied diffs are always both positive and normal.
module Data.Map.Diff.Strict (
    -- * Types
    Diff
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
    -- ** Positivity and normality
  , isNormal
  , isPositive
    -- * Applying diffs
  , applyDiff
  , applyDiffForKeys
    -- * Folds and traversals
  , foldMapDiffEntry
  , traverseDiffEntryWithKey_
    -- * Filter
  , filterOnlyKey
  ) where

import           Prelude hiding (null)

import           Data.Group (Group)
import           Data.Map.Strict (Map)

import           Data.Map.Diff.Strict.Internal
