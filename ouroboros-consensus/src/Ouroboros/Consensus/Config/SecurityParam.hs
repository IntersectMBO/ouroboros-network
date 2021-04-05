{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..)) where

import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Quiet

-- | Protocol security parameter
--
-- We interpret this as the number of rollbacks we support.
--
-- i.e., k == 0: we can't roll back at all
--       k == 1: we can roll back at most one block, etc
--
-- NOTE: This talks about the number of /blocks/ we can roll back, not
-- the number of /slots/.
newtype SecurityParam = SecurityParam { maxRollbacks :: Word64 }
  deriving (Eq, Generic, NoThunks)
  deriving Show via Quiet SecurityParam
