{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.Forging (
    BlockForging (..)
  , CannotForge
  , ForgeStateInfo
  , ForgeStateUpdateError
  , ForgeStateUpdateInfo (..)
  , ShouldForge (..)
  , castForgeStateUpdateInfo
  , checkShouldForge
  , forgeStateUpdateInfoFromUpdateInfo
    -- * 'UpdateInfo'
  , UpdateInfo (..)
    -- * Selecting transaction sequence prefixes
  , computeMaxTxCapacity
  , takeLargestPrefixThatFits
  ) where

import           Control.Tracer (Tracer, traceWith)
import           Data.Kind (Type)
import           Data.Text (Text)
import           GHC.Stack


import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.TxLimits (TxLimits)
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked

-- | Information about why we /cannot/ forge a block, although we are a leader
--
-- This should happen only rarely. An example might be that our hot key
-- does not (yet/anymore) match the delegation state.
type family CannotForge blk :: Type

-- | Returned when a call to 'updateForgeState' succeeded and caused the forge
-- state to change. This info is traced.
type family ForgeStateInfo blk :: Type

-- | Returned when a call 'updateForgeState' failed, e.g., because the KES key
-- is no longer valid. This info is traced.
type family ForgeStateUpdateError blk :: Type

-- | The result of 'updateForgeState'.
--
-- Note: the forge state itself is implicit and not reflected in the types.
data ForgeStateUpdateInfo blk =
    ForgeStateUpdated          (ForgeStateInfo        blk)
    -- ^ NB The update might have not changed the forge state.
  | ForgeStateUpdateFailed     (ForgeStateUpdateError blk)
  | ForgeStateUpdateSuppressed
    -- ^ A node was prevented from forging for an artificial reason, such as
    -- testing, benchmarking, etc. It's /artificial/ in that this constructor
    -- should never occur in a production deployment.

deriving instance (Show (ForgeStateInfo blk), Show (ForgeStateUpdateError blk))
               => Show (ForgeStateUpdateInfo blk)

castForgeStateUpdateInfo ::
     ( ForgeStateInfo        blk ~ ForgeStateInfo        blk'
     , ForgeStateUpdateError blk ~ ForgeStateUpdateError blk'
     )
  => ForgeStateUpdateInfo blk -> ForgeStateUpdateInfo blk'
castForgeStateUpdateInfo = \case
    ForgeStateUpdated x        -> ForgeStateUpdated x
    ForgeStateUpdateFailed x   -> ForgeStateUpdateFailed x
    ForgeStateUpdateSuppressed -> ForgeStateUpdateSuppressed

-- | Stateful wrapper around block production
--
-- NOTE: do not refer to the consensus or ledger config in the closure of this
-- record because they might contain an @EpochInfo Identity@, which will be
-- incorrect when used as part of the hard fork combinator.
data BlockForging m blk = BlockForging {
      -- | Identifier used in the trace messages produced for this
      -- 'BlockForging' record.
      --
      -- Useful when the node is running with multiple sets of credentials.
      forgeLabel :: Text

      -- | Proof that the node can be a leader
      --
      -- NOTE: the other fields of this record may refer to this value (or a
      -- value derived from it) in their closure, which means one should not
      -- override this field independently from the others.
    , canBeLeader :: CanBeLeader (BlockProtocol blk)

      -- | Update the forge state.
      --
      -- When the node can be a leader, this will be called at the start of
      -- each slot, right before calling 'checkCanForge'.
      --
      -- When 'Updated' is returned, we trace the 'ForgeStateInfo'.
      --
      -- When 'UpdateFailed' is returned, we trace the 'ForgeStateUpdateError'
      -- and don't call 'checkCanForge'.
    , updateForgeState ::
           TopLevelConfig blk
        -> SlotNo
        -> Ticked (ChainDepState (BlockProtocol blk))
        -> m (ForgeStateUpdateInfo blk)

      -- | After checking that the node indeed is a leader ('checkIsLeader'
      -- returned 'Just') and successfully updating the forge state
      -- ('updateForgeState' did not return 'UpdateFailed'), do another check
      -- to see whether we can actually forge a block.
      --
      -- When 'CannotForge' is returned, we don't call 'forgeBlock'.
    , checkCanForge ::
           TopLevelConfig blk
        -> SlotNo
        -> Ticked (ChainDepState (BlockProtocol blk))
        -> IsLeader (BlockProtocol blk)
        -> ForgeStateInfo blk  -- Proof that 'updateForgeState' did not fail
        -> Either (CannotForge blk) ()

      -- | Forge a block
      --
      -- The function is passed the contents of the mempool; this is a set of
      -- transactions that is guaranteed to be consistent with the ledger state
      -- (also provided as an argument) and with each other (when applied in
      -- order). In principle /all/ of them could be included in the block (up
      -- to maximum block size).
      --
      -- NOTE: do not refer to the consensus or ledger config in the closure,
      -- because they might contain an @EpochInfo Identity@, which will be
      -- incorrect when used as part of the hard fork combinator. Use the
      -- given 'TopLevelConfig' instead, as it is guaranteed to be correct
      -- even when used as part of the hard fork combinator.
      --
      -- PRECONDITION: 'checkCanForge' returned @Right ()@.
    , forgeBlock ::
           TopLevelConfig blk
        -> BlockNo                      -- Current block number
        -> SlotNo                       -- Current slot number
        -> TickedLedgerState blk        -- Current ledger state
        -> [Validated (GenTx blk)]      -- Contents of the mempool
        -> IsLeader (BlockProtocol blk) -- Proof we are leader
        -> m blk
    }

-- | Computes maximum tx capacity
--
-- It queries the ledger state for the current limit and then applies the given
-- override. The result is the pointwise minimum of the ledger-specific capacity
-- and the result of the override. In other words, the override can only reduce
-- (parts of) the 'TxLimits.Measure'.
computeMaxTxCapacity ::
     TxLimits blk
  => TickedLedgerState blk
  -> TxLimits.Overrides blk
  -> TxLimits.Measure blk
computeMaxTxCapacity ledger overrides =
    TxLimits.applyOverrides overrides (TxLimits.maxCapacity ledger)

-- | Filters out all transactions that do not fit the maximum size that is
-- passed to this function as the first argument. Value of that first argument
-- will most often by calculated by calling 'computeMaxTxCapacity'
takeLargestPrefixThatFits ::
     forall blk. TxLimits blk
  => TxLimits.Measure blk
  -> [Validated (GenTx blk)]
  -> [Validated (GenTx blk)]
takeLargestPrefixThatFits computedMaxTxCapacity = go mempty
  where
    go acc = \case
      (tx : remainingTxs) | fits -> tx : go acc' remainingTxs
        where
          acc' = acc <> TxLimits.txMeasure tx
          fits = TxLimits.lessEq @blk acc' computedMaxTxCapacity
      _ -> []

data ShouldForge blk =
    -- | Before check whether we are a leader in this slot, we tried to update
    --  our forge state ('updateForgeState'), but it failed. We will not check
    --  whether we are leader and will thus not forge a block either.
    --
    -- E.g., we could not evolve our KES key.
    ForgeStateUpdateError (ForgeStateUpdateError blk)

    -- | We are a leader in this slot, but we cannot forge for a certain
    -- reason.
    --
    -- E.g., our KES key is not yet valid in this slot or we are not the
    -- current delegate of the genesis key we have a delegation certificate
    -- from.
  | CannotForge (CannotForge blk)

    -- | We are not a leader in this slot
  | NotLeader

    -- | We are a leader in this slot and we should forge a block.
  | ShouldForge (IsLeader (BlockProtocol blk))

checkShouldForge ::
     forall m blk.
     ( Monad m
     , ConsensusProtocol (BlockProtocol blk)
     , HasCallStack
     )
  => BlockForging m blk
  -> Tracer m (ForgeStateInfo blk)
  -> TopLevelConfig blk
  -> SlotNo
  -> Ticked (ChainDepState (BlockProtocol blk))
  -> m (ShouldForge blk)
checkShouldForge BlockForging{..}
                 forgeStateInfoTracer
                 cfg
                 slot
                 tickedChainDepState =
    updateForgeState cfg slot tickedChainDepState >>= \updateInfo ->
      case updateInfo of
        ForgeStateUpdated      info -> handleUpdated info
        ForgeStateUpdateFailed err  -> return $ ForgeStateUpdateError err
        ForgeStateUpdateSuppressed  -> return NotLeader
  where
    mbIsLeader :: Maybe (IsLeader (BlockProtocol blk))
    mbIsLeader =
        -- WARNING: It is critical that we do not depend on the 'BlockForging'
        -- record for the implementation of 'checkIsLeader'. Doing so would
        -- make composing multiple 'BlockForging' values responsible for also
        -- composing the 'checkIsLeader' checks, but that should be the
        -- responsibility of the 'ConsensusProtocol' instance for the
        -- composition of those blocks.
        checkIsLeader
          (configConsensus cfg)
          canBeLeader
          slot
          tickedChainDepState

    handleUpdated :: ForgeStateInfo blk -> m (ShouldForge blk)
    handleUpdated info = do
        traceWith forgeStateInfoTracer info
        return $ case mbIsLeader of
          Nothing       -> NotLeader
          Just isLeader ->
              case checkCanForge cfg slot tickedChainDepState isLeader info of
                Left cannotForge -> CannotForge cannotForge
                Right ()         -> ShouldForge isLeader

{-------------------------------------------------------------------------------
  UpdateInfo
-------------------------------------------------------------------------------}

-- | The result of updating something, e.g., the forge state.
data UpdateInfo updated failed =
    -- | NOTE: The update may have induced no change.
    Updated updated
  | UpdateFailed failed
  deriving (Show)

-- | Embed 'UpdateInfo' into 'ForgeStateUpdateInfo'
forgeStateUpdateInfoFromUpdateInfo ::
     UpdateInfo (ForgeStateInfo blk) (ForgeStateUpdateError blk)
  -> ForgeStateUpdateInfo blk
forgeStateUpdateInfoFromUpdateInfo = \case
    Updated      info -> ForgeStateUpdated      info
    UpdateFailed err  -> ForgeStateUpdateFailed err
