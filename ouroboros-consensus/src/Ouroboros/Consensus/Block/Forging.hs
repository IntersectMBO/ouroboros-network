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
    CannotForge
  , ForgeStateInfo
  , ForgeStateUpdateError
  , ForgeStateUpdateInfo(..)
  , castForgeStateUpdateInfo
  , BlockForging(..)
  , ShouldForge(..)
  , checkShouldForge
    -- * 'UpdateInfo'
  , UpdateInfo (..)
  , castUpdateInfo
  ) where

import           Control.Tracer (Tracer, traceWith)
import           GHC.Stack

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked

-- | Information about why we /cannot/ forge a block, although we are a leader
--
-- This should happen only rarely. An example might be that our hot key
-- does not (yet/anymore) match the delegation state.
type family CannotForge blk :: *

-- | Returned when a call to 'updateForgeState' succeeded and caused the forge
-- state to change. This info is traced.
type family ForgeStateInfo blk :: *

-- | Returned when a call 'updateForgeState' failed, e.g., because the KES key
-- is no longer valid. This info is traced.
type family ForgeStateUpdateError blk :: *

-- | The result of 'updateForgeState'.
--
-- Note: the forge state itself is implicit and not reflected in the types.
newtype ForgeStateUpdateInfo blk = ForgeStateUpdateInfo {
      getForgeStateUpdateInfo :: UpdateInfo
                                   (ForgeStateInfo        blk)
                                   (ForgeStateInfo        blk)
                                   (ForgeStateUpdateError blk)
    }

deriving instance (Show (ForgeStateInfo blk), Show (ForgeStateUpdateError blk))
               => Show (ForgeStateUpdateInfo blk)

castForgeStateUpdateInfo ::
     ( ForgeStateInfo        blk ~ ForgeStateInfo        blk'
     , ForgeStateUpdateError blk ~ ForgeStateUpdateError blk'
     )
  => ForgeStateUpdateInfo blk -> ForgeStateUpdateInfo blk'
castForgeStateUpdateInfo =
      ForgeStateUpdateInfo
    . castUpdateInfo
    . getForgeStateUpdateInfo

-- | Stateful wrapper around block production
--
-- NOTE: do not refer to the consensus or ledger config in the closure of this
-- record because they might contain an @EpochInfo Identity@, which will be
-- incorrect when used as part of the hard fork combinator.
data BlockForging m blk = BlockForging {
      -- | Proof that the node can be a leader
      --
      -- NOTE: the other fields of this record may refer to this value (or a
      -- value derived from it) in their closure, which means one should not
      -- override this field independently from the others.
      canBeLeader :: CanBeLeader (BlockProtocol blk)

      -- | Update the forge state.
      --
      -- When the node can be a leader, this will be called at the start of
      -- each slot, right before calling 'checkCanForge'.
      --
      -- When 'Updated' is returned, we trace the changed 'ForgeStateInfo'.
      --
      -- When 'UpdateFailed' is returned, we trace the 'ForgeStateUpdateError'
      -- and don't call 'checkCanForge'.
    , updateForgeState :: SlotNo -> m (ForgeStateUpdateInfo blk)

      -- | After checking that the node indeed is a leader ('checkIsLeader'
      -- returned 'Just') and successfully updating the forge state
      -- ('updateForgeState' did not return 'UpdateFailed'), do another check
      -- to see whether we can actually forge a block.
      --
      -- When 'CannotForge' is returned, we don't call 'forgeBlock'.
    , checkCanForge ::
           forall p. BlockProtocol blk ~ p
        => TopLevelConfig blk
        -> SlotNo
        -> Ticked (ChainDepState p)
        -> IsLeader p
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
        -> [GenTx blk]                  -- Contents of the mempool
        -> IsLeader (BlockProtocol blk) -- Proof we are leader
        -> m blk
    }

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
               tickedChainDepState = do
    eForgeStateInfo <-
      updateForgeState slot >>= \updateInfo ->
        case getForgeStateUpdateInfo updateInfo of
          Updated info -> do
            traceWith forgeStateInfoTracer info
            return $ Right info
          Unchanged info ->
            -- We intentionally do no trace the 'ForgeStateInfo' when it did not
            -- change.
            return $ Right info
          UpdateFailed err ->
            return $ Left err

    return $
      case eForgeStateInfo of
        Left  err            -> ForgeStateUpdateError err
        Right forgeStateInfo ->
          case checkIsLeader (configConsensus cfg) canBeLeader slot tickedChainDepState of
            Nothing       -> NotLeader
            Just isLeader ->
              case checkCanForge cfg slot tickedChainDepState isLeader forgeStateInfo of
                Left cannotForge -> CannotForge cannotForge
                Right ()         -> ShouldForge isLeader

{-------------------------------------------------------------------------------
  UpdateInfo
-------------------------------------------------------------------------------}

-- | The result of updating something, e.g., the forge state.
data UpdateInfo updated unchanged failed =
    Updated      updated
  | Unchanged    unchanged
  | UpdateFailed failed
  deriving (Show)

castUpdateInfo ::
     ( updated   ~ updated'
     , unchanged ~ unchanged'
     , failed    ~ failed'
     )
  => UpdateInfo updated  unchanged  failed
  -> UpdateInfo updated' unchanged' failed'
castUpdateInfo = \case
    Updated      updated   -> Updated      updated
    Unchanged    unchanged -> Unchanged    unchanged
    UpdateFailed failed    -> UpdateFailed failed
