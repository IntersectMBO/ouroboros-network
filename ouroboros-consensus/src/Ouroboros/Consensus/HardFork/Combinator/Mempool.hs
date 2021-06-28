{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
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
  , Validated (..)
  , hardForkApplyTxErrFromEither
  , hardForkApplyTxErrToEither
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.SOP.Strict
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
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
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Product2 (..))
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
  deriving (Eq, Generic, Show)
  deriving anyclass (NoThunks)

newtype instance Validated (GenTx (HardForkBlock xs)) = HardForkValidatedGenTx {
      getHardForkValidatedGenTx :: OneEraValidatedGenTx xs
    }
  deriving (Eq, Generic, Show)
  deriving anyclass (NoThunks)

instance Typeable xs => ShowProxy (GenTx (HardForkBlock xs)) where

type instance ApplyTxErr (HardForkBlock xs) = HardForkApplyTxErr xs

instance CanHardFork xs => LedgerSupportsMempool (HardForkBlock xs) where
  applyTx   = applyHelper ModeApply

  reapplyTx = \cfg slot vtx tls ->
        fmap (\(tls', _vtx) -> tls')
      $ applyHelper
          ModeReapply
          cfg
          DoNotIntervene
          slot
          (WrapValidatedGenTx vtx)
          tls

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

  txForgetValidated =
        HardForkGenTx
      . OneEraGenTx
      . hcmap proxySingle (txForgetValidated . unwrapValidatedGenTx)
      . getOneEraValidatedGenTx
      . getHardForkValidatedGenTx

-- | A private type used only to clarify the parameterization of 'applyHelper'
data ApplyHelperMode :: (Type -> Type) -> Type where
  ModeApply   :: ApplyHelperMode GenTx
  ModeReapply :: ApplyHelperMode WrapValidatedGenTx

-- | A private type used only to clarify the definition of 'applyHelper'
data ApplyResult xs blk = ApplyResult {
    arState       :: Ticked (LedgerState blk)
  , arValidatedTx :: Validated (GenTx (HardForkBlock xs))
  }

-- | The shared logic between 'applyTx' and 'reapplyTx' for 'HardForkBlock'
--
-- The @txIn@ variable is 'GenTx' or 'WrapValidatedGenTx', respectively. See
-- 'ApplyHelperMode'.
applyHelper :: forall xs txIn. CanHardFork xs
  => ApplyHelperMode txIn
  -> LedgerConfig (HardForkBlock xs)
  -> WhetherToIntervene
  -> SlotNo
  -> txIn (HardForkBlock xs)
  -> TickedLedgerState (HardForkBlock xs)
  -> Except
      (HardForkApplyTxErr xs)
      ( TickedLedgerState (HardForkBlock xs)
      , Validated (GenTx (HardForkBlock xs))
      )
applyHelper mode
            HardForkLedgerConfig{..}
            wti
            slot
            tx
            (TickedHardForkLedgerState transition hardForkState) =
    case matchPolyTx injs (modeGetTx tx) hardForkState of
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
        do
          result <-
              hsequence'
            $ hcizipWith proxySingle modeApplyCurrent cfgs matched
          let _ = result :: State.HardForkState (ApplyResult xs) xs

              st' :: State.HardForkState (Ticked :.: LedgerState) xs
              st' = (Comp . arState) `hmap` result

              vtx :: Validated (GenTx (HardForkBlock xs))
              vtx = hcollapse $ (K . arValidatedTx) `hmap` result

          return (TickedHardForkLedgerState transition st', vtx)
  where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei    = State.epochInfoPrecomputedTransitionInfo
              hardForkLedgerConfigShape
              transition
              hardForkState

    injs :: InPairs (InjectPolyTx txIn) xs
    injs =
        InPairs.hmap
          modeGetInjection
          (InPairs.requiringBoth cfgs hardForkInjectTxs)

    modeGetTx :: txIn (HardForkBlock xs) -> NS txIn xs
    modeGetTx = case mode of
        ModeApply   ->
              getOneEraGenTx
            . getHardForkGenTx
        ModeReapply ->
              getOneEraValidatedGenTx
            . getHardForkValidatedGenTx
            . unwrapValidatedGenTx

    modeGetInjection :: forall blk1 blk2.
         Product2 InjectTx InjectValidatedTx blk1 blk2
      -> InjectPolyTx txIn                   blk1 blk2
    modeGetInjection (Pair2 injTx injValidatedTx) = case mode of
        ModeApply   -> injTx
        ModeReapply -> injValidatedTx

    modeApplyCurrent :: forall blk.
         SingleEraBlock                        blk
      => Index xs                              blk
      -> WrapLedgerConfig                      blk
      -> Product txIn (Ticked :.: LedgerState) blk
      -> (     Except (HardForkApplyTxErr xs)
           :.: ApplyResult xs
         ) blk
    modeApplyCurrent index cfg (Pair tx' (Comp st)) =
          Comp
        $ withExcept (injectApplyTxErr index)
        $ do
            let lcfg = unwrapLedgerConfig cfg
            (st', vtx) <- case mode of
              ModeApply   -> applyTx lcfg wti slot tx' st
              ModeReapply -> do
                  let vtx' = unwrapValidatedGenTx tx'
                  st' <- reapplyTx lcfg slot vtx' st
                  -- provide the given transaction, which was already validated
                  pure (st', vtx')
            pure ApplyResult {
                arValidatedTx = injectValidatedGenTx index vtx
              , arState       = st'
              }

newtype instance TxId (GenTx (HardForkBlock xs)) = HardForkGenTxId {
      getHardForkGenTxId :: OneEraGenTxId xs
    }
  deriving (Eq, Generic, Ord, Show)
  deriving anyclass (NoThunks)

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
        -> K [Validated (GenTx (HardForkBlock xs))] blk
      aux index = K . map (injectNS' (Proxy @WrapValidatedGenTx) index) . extractTxs . unI

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

injectValidatedGenTx :: Index xs blk -> Validated (GenTx blk) -> Validated (GenTx (HardForkBlock xs))
injectValidatedGenTx index =
      HardForkValidatedGenTx
    . OneEraValidatedGenTx
    . injectNS index
    . WrapValidatedGenTx
