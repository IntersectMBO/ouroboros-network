{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Cardano.ShelleyBased (overShelleyBasedLedgerState) where

import           Data.SOP.Strict hiding (All2)
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible)

-- | When the given ledger state corresponds to a Shelley-based era, apply the
-- given function to it.
overShelleyBasedLedgerState ::
     forall c.
     (TPraos.PraosCrypto c, Praos.PraosCrypto c)
  => (   forall era proto. (EraCrypto era ~ c, ShelleyCompatible proto era)
      => LedgerState (ShelleyBlock proto era) Canonical
      -> LedgerState (ShelleyBlock proto era) Canonical
     )
  -> LedgerState (CardanoBlock c) Canonical
  -> LedgerState (CardanoBlock c) Canonical
overShelleyBasedLedgerState f (HardForkLedgerState st) =
    HardForkLedgerState $ hap fs st
  where
    fs :: NP (Flip LedgerState Canonical -.-> Flip LedgerState Canonical)
             (CardanoEras c)
    fs = fn id
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEra
        :* Nil

    injectSingleEra ::
      ( ShelleyCompatible proto era, EraCrypto era ~ c
      , shelleyEra ~ ShelleyBlock proto era
      )
      => (Flip LedgerState Canonical -.-> Flip LedgerState Canonical) shelleyEra
    injectSingleEra = fn (Flip . f . unFlip)
