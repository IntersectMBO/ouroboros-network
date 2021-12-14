{-# LANGUAGE DeriveGeneric #-}
module Ouroboros.Consensus.Storage.LedgerDB.Types (
    PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , UpdateLedgerDbTraceEvent (..)
  ) where


import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block.RealPoint (RealPoint)

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}
newtype PushStart blk = PushStart { unPushStart :: RealPoint blk }
  deriving (Show, Eq)

newtype PushGoal blk = PushGoal { unPushGoal :: RealPoint blk }
  deriving (Show, Eq)

newtype Pushing blk = Pushing { unPushing :: RealPoint blk }
  deriving (Show, Eq)

data UpdateLedgerDbTraceEvent blk =
    -- | Event fired when we are about to push a block to the LedgerDB
      StartedPushingBlockToTheLedgerDb
        !(PushStart blk)
        -- ^ Point from which we started pushing new blocks
        !(Pushing blk)
        -- ^ Point which block we are about to push
        (PushGoal blk)
        -- ^ Point to which we are updating the ledger, the last event
        -- StartedPushingBlockToTheLedgerDb will have Pushing and PushGoal
        -- wrapping over the same RealPoint
  deriving (Show, Eq, Generic)
