{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | Definition is 'IsLedger'
--
-- Normally this is imported from "Ouroboros.Consensus.Ledger.Abstract". We
-- pull this out to avoid circular module dependencies.
module Ouroboros.Consensus.Ledger.Basics (
    -- * GetTip
    GetTip (..)
  , getTipHash
  , getTipSlot
    -- * Ledger Events
  , LedgerResult (..)
  , VoidLedgerEvent
  , castLedgerResult
  , embedLedgerResult
  , pureLedgerResult
    -- * Definition of a ledger independent of a choice of block
  , IsLedger (..)
  , LedgerCfg
  , applyChainTick
    -- * Link block to its ledger
  , Canonical (..)
  , LedgerConfig
  , LedgerError
  , LedgerState
  , LedgerStateKind
  , TickedLedgerState
  ) where

import           Data.Kind (Constraint, Type)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Tables (Canonical (..),
                     LedgerStateKind)
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util ((..:))

{-------------------------------------------------------------------------------
  Tip
-------------------------------------------------------------------------------}

class GetTip l where
  -- | Point of the most recently applied block
  --
  -- Should be 'genesisPoint' when no blocks have been applied yet
  getTip :: l -> Point l

type instance HeaderHash (Ticked l) = HeaderHash l
type instance HeaderHash (Ticked1 (l :: k -> Type)) = HeaderHash l
type instance HeaderHash (Ticked1 (l :: k -> Type) mk) = HeaderHash l

getTipHash :: GetTip l => l -> ChainHash l
getTipHash = pointHash . getTip

getTipSlot :: GetTip l => l -> WithOrigin SlotNo
getTipSlot = pointSlot . getTip

{-------------------------------------------------------------------------------
  Events directly from the ledger
-------------------------------------------------------------------------------}

-- | A 'Data.Void.Void' isomorph for explicitly declaring that some ledger has
-- no events
data VoidLedgerEvent l

-- | The result of invoke a ledger function that does validation
--
-- Note: we do not instantiate 'Applicative' or 'Monad' for this type because
-- those interfaces would typically incur space leaks. We encourage you to
-- process the events each time you invoke a ledger function.
data LedgerResult l a = LedgerResult
  { lrEvents :: [AuxLedgerEvent l]
  , lrResult :: !a
  }
  deriving (Foldable, Functor, Traversable)

castLedgerResult ::
     (AuxLedgerEvent l ~ AuxLedgerEvent l')
  => LedgerResult l  a
  -> LedgerResult l' a
castLedgerResult (LedgerResult x0 x1) = LedgerResult x0 x1

embedLedgerResult ::
     (AuxLedgerEvent l -> AuxLedgerEvent l')
  -> LedgerResult l  a
  -> LedgerResult l' a
embedLedgerResult inj lr = lr{lrEvents = inj `map` lrEvents lr}

pureLedgerResult :: a -> LedgerResult l a
pureLedgerResult a = LedgerResult {
    lrEvents = mempty
  , lrResult = a
  }

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
type LedgerCfg :: LedgerStateKind -> Type
type family LedgerCfg l :: Type

type IsLedger :: LedgerStateKind -> Constraint
class ( -- Requirements on the ledger state itself
        Show     (l Canonical)
      , Eq       (l Canonical)
      , NoThunks (l Canonical)
        -- Requirements on 'LedgerCfg'
      , NoThunks (LedgerCfg l)
        -- Requirements on 'LedgerErr'
      , Show     (LedgerErr l)
      , Eq       (LedgerErr l)
      , NoThunks (LedgerErr l)
        -- Get the tip
        --
        -- See comment for 'applyChainTickLedgerResult' about the tip of the
        -- ticked ledger.
      , GetTip (l Canonical)
      , GetTip (Ticked1 l Canonical)
      , HeaderHash (l Canonical) ~ HeaderHash l
      ) => IsLedger l where
  -- | Errors that can arise when updating the ledger
  --
  -- This is defined here rather than in 'ApplyBlock', since the /type/ of
  -- these errors does not depend on the type of the block.
  type family LedgerErr l :: Type

  -- | Event emitted by the ledger
  --
  -- TODO we call this 'AuxLedgerEvent' to differentiate from 'LedgerEvent' in
  -- 'InspectLedger'. When that module is rewritten to make use of ledger
  -- derived events, we may rename this type.
  type family AuxLedgerEvent l :: Type

  -- | Apply "slot based" state transformations
  --
  -- When a block is applied to the ledger state, a number of things happen
  -- purely based on the slot number of that block. For example:
  --
  -- * In Byron, scheduled updates are applied, and the update system state is
  --   updated.
  -- * In Shelley, delegation state is updated (on epoch boundaries).
  --
  -- The consensus layer must be able to apply such a "chain tick" function,
  -- primarily when validating transactions in the mempool (which, conceptually,
  -- live in "some block in the future") or when extracting valid transactions
  -- from the mempool to insert into a new block to be produced.
  --
  -- This is not allowed to throw any errors. After all, if this could fail,
  -- it would mean a /previous/ block set up the ledger state in such a way
  -- that as soon as a certain slot was reached, /any/ block would be invalid.
  --
  -- PRECONDITION: The slot number must be strictly greater than the slot at
  -- the tip of the ledger (except for EBBs, obviously..).
  --
  -- NOTE: 'applyChainTickLedgerResult' should /not/ change the tip of the
  -- underlying ledger state, which should still refer to the most recent
  -- applied /block/. In other words, we should have
  --
  -- >    ledgerTipPoint (applyChainTick cfg slot st)
  -- > == ledgerTipPoint st
  applyChainTickLedgerResult ::
       LedgerCfg l
    -> SlotNo
    -> l Canonical
    -> LedgerResult l (Ticked1 l Canonical)

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick :: IsLedger l => LedgerCfg l -> SlotNo -> l Canonical -> Ticked1 l Canonical
applyChainTick = lrResult ..: applyChainTickLedgerResult

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

-- | Ledger state associated with a block
--
-- The @blk@ parameter will be instantiated with the block used in the
-- blockchain, and @mk@ will be instantiated to some MapKind. See
-- "Ouroboros.Consensus.Ledger.Tables" for a more thorough discussion on this
-- one.
type LedgerState :: Type -> LedgerStateKind
data family LedgerState blk mk

type instance HeaderHash (LedgerState blk)    = HeaderHash blk
type instance HeaderHash (LedgerState blk mk) = HeaderHash blk

instance StandardHash blk => StandardHash (LedgerState blk)
instance StandardHash blk => StandardHash (LedgerState blk mk)

type LedgerConfig      blk    = LedgerCfg (LedgerState blk)
type LedgerError       blk    = LedgerErr (LedgerState blk)
type TickedLedgerState blk mk = Ticked1   (LedgerState blk) mk
