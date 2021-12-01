{-# LANGUAGE DeriveGeneric #-}
module Ouroboros.Consensus.Storage.LedgerDB.Types (UpdateLedgerDbTraceEvent (..)) where

import           GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}
data UpdateLedgerDbTraceEvent blk =
    -- | Event fired when we are about to push a block to the LedgerDB
      StartedPushingBlockToTheLedgerDb
    -- | Event fired when we successfully pushed a block to the LedgerDB
    | PushedBlockToTheLedgerDb
  deriving (Show, Eq, Generic)
