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
    ExampleHardForkConstraints
  , ShelleyPartialLedgerConfig (..)
  , TriggerHardFork (..)
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
  ) where

import           Control.Monad.Except (runExcept)
import           Data.SOP.Strict (NP (..), unComp, (:.:) (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails (..))
import           Ouroboros.Consensus.HardFork.Simple

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.ShelleyHFC

import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Example.Translation ()

import           Ouroboros.Consensus.Example.Block

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type ExampleHardForkConstraints c =
  ( PraosCrypto c
  , ShelleyBasedEra (ShelleyEra c)
  , ShelleyBasedEra (ExampleEra c)
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
