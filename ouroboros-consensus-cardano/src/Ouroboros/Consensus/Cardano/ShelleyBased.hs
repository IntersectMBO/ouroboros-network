{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Ouroboros.Consensus.Cardano.ShelleyBased (overShelleyBasedLedgerState) where

import           Data.SOP.Strict hiding (All2)

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Util.NP2

import           Cardano.Ledger.Hashes (EraIndependentTxBody)
import           Cardano.Ledger.Keys (DSignable, Hash)
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Protocol.TPraos (PraosCrypto)
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible)
import           Ouroboros.Consensus.Shelley.ShelleyBased

-- | When the given ledger state corresponds to a Shelley-based era, apply the
-- given function to it.
overShelleyBasedLedgerState ::
     forall c.
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => (   forall era proto. (EraCrypto era ~ c, ShelleyCompatible proto era)
      => LedgerState (ShelleyBlock proto era)
      -> LedgerState (ShelleyBlock proto era)
     )
  -> LedgerState (CardanoBlock c)
  -> LedgerState (CardanoBlock c)
overShelleyBasedLedgerState f (HardForkLedgerState st) =
    HardForkLedgerState $ hap fs st
  where
    fs :: NP (LedgerState -.-> LedgerState)
             (CardanoEras c)
    fs = fn id
        :* injectShelleyNP2
            reassoc
            f'

    f' ::
      NP2 (LedgerState :..: ShelleyBlock
            -..-> LedgerState :..: ShelleyBlock
          )
          (ShelleyErasAndProtos c)
    f' = cpure_NP2
               (Proxy @(And2 (HasCrypto c) ShelleyCompatible))
               (Fn2 (Comp2 . f . unComp2))

    reassoc ::
        (      LedgerState :..: ShelleyBlock
        -..-> LedgerState :..: ShelleyBlock
        ) proto shelleyEra
      -> (     LedgerState
        -.-> LedgerState
        ) (ShelleyBlock proto shelleyEra)
    reassoc g = fn $ unComp2 . apFn2 g . Comp2

    unComp2 (Comp2 x) = x
