{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Sequence.NonEmpty.Extra () where

import           Data.Foldable
import           Data.Sequence.NonEmpty
import           NoThunks.Class

-- | Instance for @'NESeq'@ checks elements only
--
-- The internal fingertree in @'NESeq'@ might have thunks, which is essential for
-- its asymptotic complexity.
--
-- Note: see documentation of @'NoThunks' ('Seq' a)@
instance NoThunks a => NoThunks (NESeq a) where
  showTypeOf _ = "NESeq"
  wNoThunks ctxt = noThunksInValues ctxt . toList
