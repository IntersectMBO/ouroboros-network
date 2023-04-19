-- | Classification of lists of symbols
--
-- Intended for qualified import.
--
-- > import qualified Test.Util.Classify as C
module Test.Util.Classify {-# DEPRECATED "Use Test.QuickCheck.StateMachine.Labelling instead" #-} (
    Predicate (..)
  , classify
  , maximum
  , predicate
  ) where

import           Prelude hiding (maximum)

import           Test.StateMachine.Labelling
