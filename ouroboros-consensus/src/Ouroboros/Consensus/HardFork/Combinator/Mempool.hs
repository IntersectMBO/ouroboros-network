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
    HardForkApplyTxErr(..)
  , hardForkApplyTxErrToEither
  , hardForkApplyTxErrFromEither
  , GenTx(..)
  , TxId(..)
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.SOP.Strict
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

data HardForkApplyTxErr xs =
    -- | Validation error from one of the eras
    HardForkApplyTxErrFromEra !(OneEraApplyTxErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkApplyTxErrWrongEra !(MismatchEraInfo xs)
  deriving (Generic)

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

instance CanHardFork xs => LedgerSupportsMempool (HardForkBlock xs) where
  newtype GenTx (HardForkBlock xs) = HardForkGenTx {
        getHardForkGenTx :: OneEraGenTx xs
      }
    deriving (Eq, Show, NoUnexpectedThunks)

  type ApplyTxErr (HardForkBlock xs) = HardForkApplyTxErr xs

  applyTx   = applyHelper applyTx
  reapplyTx = applyHelper reapplyTx

  maxTxCapacity (Ticked slot st) =
      hcollapse
    . hcmap proxySingle (K . maxTxCapacity . Ticked slot)
    . State.tip
    . getHardForkLedgerState
    $ st

  maxTxSize =
      hcollapse
    . hcmap proxySingle (K . maxTxSize)
    . State.tip
    . getHardForkLedgerState

  txInBlockSize =
      hcollapse
    . hcmap proxySingle (K . txInBlockSize)
    . getOneEraGenTx
    . getHardForkGenTx

applyHelper
  :: forall xs. CanHardFork xs
  => (    forall blk. (SingleEraBlock blk, HasCallStack)
       => LedgerConfig blk
       -> GenTx blk
       -> TickedLedgerState blk
       -> Except (ApplyTxErr blk) (TickedLedgerState blk)
     )
  -> LedgerConfig (HardForkBlock xs)
  -> GenTx (HardForkBlock xs)
  -> TickedLedgerState (HardForkBlock xs)
  -> Except (HardForkApplyTxErr xs) (TickedLedgerState (HardForkBlock xs))
applyHelper apply
            hardForkConfig@HardForkLedgerConfig{..}
            (HardForkGenTx (OneEraGenTx hardForkTx))
            (Ticked slot (HardForkLedgerState hardForkState)) =
    case State.match hardForkTx (hmap (Comp . Ticked slot) hardForkState) of
      Left mismatch ->
        throwError $ HardForkApplyTxErrWrongEra . MismatchEraInfo $
          Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
      Right matched ->
        fmap (fmap HardForkLedgerState . State.sequence) $ hsequence' $
          hczipWith3 proxySingle applyCurrent cfgs injections matched
  where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    ei   = State.epochInfoLedger hardForkConfig hardForkState

    applyCurrent
      :: forall blk. SingleEraBlock blk
      => WrapPartialLedgerConfig                                       blk
      -> Injection WrapApplyTxErr xs                                   blk
      -> Product GenTx (Ticked :.: LedgerState)                        blk
      -> (Except (HardForkApplyTxErr xs) :.: (Ticked :.: LedgerState)) blk
    applyCurrent cfg injectErr (Pair tx (Comp st)) = Comp $ fmap Comp $
      withExcept (injectApplyTxErr injectErr) $
        apply (completeLedgerConfig' ei cfg) tx st

instance CanHardFork xs => HasTxId (GenTx (HardForkBlock xs)) where
  newtype TxId (GenTx (HardForkBlock xs)) = HardForkGenTxId {
        getHardForkGenTxId :: OneEraGenTxId xs
      }
    deriving (Show, Eq, Ord, NoUnexpectedThunks)

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
      . hzipWith (\inj -> K . map (mkTx inj) . unComp) injections
      . hcmap (Proxy @HasTxs) (Comp . extractTxs . unI)
      . getOneEraBlock
      . getHardForkBlock
    where
      mkTx :: Injection GenTx xs blk -> GenTx blk -> GenTx (HardForkBlock xs)
      mkTx inj = HardForkGenTx . OneEraGenTx . unK . apFn inj

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => State.Current (Ticked :.: LedgerState) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectApplyTxErr :: Injection WrapApplyTxErr xs blk
                 -> ApplyTxErr blk
                 -> HardForkApplyTxErr xs
injectApplyTxErr inj =
      HardForkApplyTxErrFromEra
    . OneEraApplyTxErr
    . unK
    . apFn inj
    . WrapApplyTxErr
