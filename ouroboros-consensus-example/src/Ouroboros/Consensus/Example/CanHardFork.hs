{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Example.CanHardFork (
    TriggerHardFork (..)
  , ShelleyPartialLedgerConfig (..)
  , ExampleHardForkConstraints
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
  ) where

import           Control.Monad
import           Control.Monad.Except (Except, runExcept, throwError)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Proxy
import           Data.SOP.Strict ((:.:) (..), NP (..), unComp)
import           Data.Void (Void)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot),
                     addSlots)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.RedundantConstraints

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails (..))

import           Ouroboros.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState

import           Ouroboros.Consensus.Cardano.CanHardFork

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.Inspect as Shelley.Inspect
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol

import           Cardano.Ledger.Crypto (ADDRHASH, DSIGN, HASH)
import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Example
import           Cardano.Ledger.Example.Translation
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Example.Block

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type ExampleHardForkConstraints c =
  ( PraosCrypto c
  , ShelleyBasedEra (ShelleyEra c)
  , ShelleyBasedEra (ExampleEra c)
    -- These equalities allow the transition from Byron to Shelley, since
    -- @shelley-spec-ledger@ requires Ed25519 for Byron bootstrap addresses and
    -- the current Byron-to-Shelley translation requires a 224-bit hash for
    -- address and a 256-bit hash for header hashes.
  , HASH     c ~ Blake2b_256
  , ADDRHASH c ~ Blake2b_224
  , DSIGN    c ~ Ed25519DSIGN
  )

instance ExampleHardForkConstraints c => CanHardFork (ExampleEras c) where
  hardForkEraTranslation = EraTranslation {
      translateLedgerState   =
          PCons translateLedgerStateShelleyToExampleWrapper
        $ PNil
    , translateChainDepState =
          PCons translateChainDepStateAcrossShelley
        $ PNil
    , translateLedgerView    =
          PCons translateLedgerViewAcrossShelley
        $ PNil
    }
  hardForkChainSel =
        -- Shelley <-> Example, ...
        TCons (SelectSameProtocol :* Nil)
        -- Example <-> ...
      $ TCons Nil
      $ TNil
  hardForkInjectTxs =
        PCons (ignoringBoth translateTxShelleyToExampleWrapper)
      $ PNil

{-------------------------------------------------------------------------------
  Translation from Shelley to Example
-------------------------------------------------------------------------------}

translateLedgerStateShelleyToExampleWrapper ::
     PraosCrypto c
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (ExampleEra c))
translateLedgerStateShelleyToExampleWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' () . Comp

translateTxShelleyToExampleWrapper ::
     PraosCrypto c
  => InjectTx
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (ExampleEra c))
translateTxShelleyToExampleWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp
