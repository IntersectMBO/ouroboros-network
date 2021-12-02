{-# LANGUAGE DeriveGeneric #-}
module Ouroboros.Consensus.Storage.LedgerDB.Types (UpdateLedgerDbTraceEvent (..)) where


import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block.RealPoint (RealPoint)

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}
data UpdateLedgerDbTraceEvent blk =
    -- | Event fired when we are about to push a block to the LedgerDB
      StartedPushingBlockToTheLedgerDb !(RealPoint blk)
  deriving (Show, Eq, Generic)
