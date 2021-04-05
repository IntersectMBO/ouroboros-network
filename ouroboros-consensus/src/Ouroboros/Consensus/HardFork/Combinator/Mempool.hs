{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Mempool (
    GenTx (..)
  , HardForkApplyTxErr (..)
  , TxId (..)
  , hardForkApplyTxErrFromEither
  , hardForkApplyTxErrToEither
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.SOP.Strict
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import           Ouroboros.Consensus.HardFork.Combinator.Ledger (Ticked (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

data HardForkApplyTxErr xs =
    -- | Validation error from one of the eras
    HardForkApplyTxErrFromEra !(OneEraApplyTxErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkApplyTxErrWrongEra !(MismatchEraInfo xs)
  deriving (Generic)

instance Typeable xs => ShowProxy (HardForkApplyTxErr xs) where

hardForkApplyTxErrToEither :: HardForkApplyTxErr xs
                           -> Either (MismatchEraInfo xs) (OneEraApplyTxErr xs)
hardForkApplyTxErrToEither (HardForkApplyTxErrFromEra  err) = Right err
hardForkApplyTxErrToEither (HardForkApplyTxErrWrongEra err) = Left  err

hardForkApplyTxErrFromEither :: Either (MismatchEraInfo xs) (OneEraApplyTxErr xs)
                             -> HardForkApplyTxErr xs
hardForkApplyTxErrFromEither (Right err) = HardForkApplyTxErrFromEra  err
hardForkApplyTxErrFromEither (Left  err) = HardForkApplyTxErrWrongEra err

deriving stock instance CanHardFork xs => Show (HardForkApplyTxErr xs)

deriving stock instance CanHardFork xs => Eq (HardForkApplyTxErr xs)

newtype instance GenTx (HardForkBlock xs) = HardForkGenTx {
      getHardForkGenTx :: OneEraGenTx xs
    }
  deriving (Eq, Show, NoThunks)

instance Typeable xs => ShowProxy (GenTx (HardForkBlock xs)) where

type instance ApplyTxErr (HardForkBlock xs) = HardForkApplyTxErr xs

instance CanHardFork xs => LedgerSupportsMempool (HardForkBlock xs) where
  applyTx   = applyHelper applyTx
  reapplyTx = applyHelper reapplyTx

  maxTxCapacity =
        hcollapse
      . hcmap proxySingle (K . maxTxCapacity . unComp)
      . State.tip
      . tickedHardForkLedgerStatePerEra

  txInBlockSize =
        hcollapse
      . hcmap proxySingle (K . txInBlockSize)
      . getOneEraGenTx
      . getHardForkGenTx

applyHelper
  :: forall xs. CanHardFork xs
  => (    forall blk. (SingleEraBlock blk, HasCallStack)
       => LedgerConfig blk
       -> SlotNo
       -> GenTx blk
       -> TickedLedgerState blk
       -> Except (ApplyTxErr blk) (TickedLedgerState blk)
     )
  -> LedgerConfig (HardForkBlock xs)
  -> SlotNo
  -> GenTx (HardForkBlock xs)
  -> TickedLedgerState (HardForkBlock xs)
  -> Except (HardForkApplyTxErr xs) (TickedLedgerState (HardForkBlock xs))
applyHelper apply
            HardForkLedgerConfig{..}
            slot
            (HardForkGenTx (OneEraGenTx hardForkTx))
            (TickedHardForkLedgerState transition hardForkState) =
    case matchTx injectTxs hardForkTx hardForkState of
      Left mismatch ->
        throwError $ HardForkApplyTxErrWrongEra . MismatchEraInfo $
          Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
      Right matched ->
        -- We are updating the ticked ledger state by applying a transaction,
        -- but for the HFC that ledger state contains a bundled
        -- 'TransitionInfo'. We don't change that 'TransitionInfo' here, which
        -- requires justification. Three cases:
        --
        -- o 'TransitionUnknown'. Transitions become known only when the
        --    transaction that confirms them becomes stable, so this cannot
        --    happen simply by applying a transaction. In this case we record
        --    the tip of the ledger, which is also not changed halfway a block.
        -- o 'TransitionKnown'. In this case, we record the 'EpochNo' of the
        --    epoch that starts the new era; this information similarly won't
        --    halfway a block (it can only change, in fact, when we do transition
        --    to that new era).
        -- o 'TransitionImpossible'. Two subcases: we are in the final era (in
        --    which we will remain to be) or we are forecasting, which is not
        --    applicable here.
        fmap (TickedHardForkLedgerState transition) $ hsequence' $
          hcizipWith proxySingle applyCurrent cfgs matched
  where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei    = State.epochInfoPrecomputedTransitionInfo
              hardForkLedgerConfigShape
              transition
              hardForkState

    injectTxs :: InPairs InjectTx xs
    injectTxs = InPairs.requiringBoth cfgs hardForkInjectTxs

    applyCurrent
      :: forall blk. SingleEraBlock blk
      => Index xs                                                      blk
      -> WrapLedgerConfig                                              blk
      -> Product GenTx (Ticked :.: LedgerState)                        blk
      -> (Except (HardForkApplyTxErr xs) :.: (Ticked :.: LedgerState)) blk
    applyCurrent index cfg (Pair tx (Comp st)) = Comp $ fmap Comp $
      withExcept (injectApplyTxErr index) $
        apply (unwrapLedgerConfig cfg) slot tx st

newtype instance TxId (GenTx (HardForkBlock xs)) = HardForkGenTxId {
      getHardForkGenTxId :: OneEraGenTxId xs
    }
  deriving (Show, Eq, Ord, NoThunks)

instance Typeable xs => ShowProxy (TxId (GenTx (HardForkBlock xs))) where

instance CanHardFork xs => HasTxId (GenTx (HardForkBlock xs)) where
  txId = HardForkGenTxId . OneEraGenTxId
       . hcmap proxySingle (WrapGenTxId . txId)
       . getOneEraGenTx . getHardForkGenTx

{-------------------------------------------------------------------------------
  HasTxs

  This is not required by consensus itself, but is required by RunNode.
-------------------------------------------------------------------------------}

instance All HasTxs xs => HasTxs (HardForkBlock xs) where
  extractTxs =
        hcollapse
      . hcimap (Proxy @HasTxs) aux
      . getOneEraBlock
      . getHardForkBlock
    where
      aux ::
           HasTxs blk
        => Index xs blk
        -> I blk
        -> K [GenTx (HardForkBlock xs)] blk
      aux index = K . map (injectNS' (Proxy @GenTx) index) . extractTxs . unI

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => State.Current (Ticked :.: LedgerState) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectApplyTxErr :: Index xs blk -> ApplyTxErr blk -> HardForkApplyTxErr xs
injectApplyTxErr index =
      HardForkApplyTxErrFromEra
    . OneEraApplyTxErr
    . injectNS index
    . WrapApplyTxErr
