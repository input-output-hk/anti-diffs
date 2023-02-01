{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Sequence.NonEmpty.Extra () where

import           Data.Foldable
import           Data.Sequence.NonEmpty
import           NoThunks.Class

-- | Instance for @'NESeq'@ which only checks for thunks on the elements
--
-- The internal fingertree in @'NESeq'@ might have thunks, which is essential for
-- its asymptotic complexity.
--
-- Note: see documentation of @'NoThunks' ('Seq' a)@
instance NoThunks a => NoThunks (NESeq a) where
  showTypeOf _ = "NESeq"
  wNoThunks ctxt = noThunksInValues ctxt . toList
