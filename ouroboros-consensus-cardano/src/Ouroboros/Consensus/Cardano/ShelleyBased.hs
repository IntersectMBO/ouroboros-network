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
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible)
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import           Ouroboros.Consensus.Util.NP2

{-------------------------------------------------------------------------------
  Transform Shelley-based types
-------------------------------------------------------------------------------}

-- | Witness the relation between the crypto used by a Shelley-based era and
-- protocol.
--
-- Can be partially applied while an equality constraint cannot.
class (EraCrypto era ~ c, ProtoCrypto proto ~ c) => HasCrypto c proto era
instance (EraCrypto era ~ c, ProtoCrypto proto ~ c) => HasCrypto c proto era

-- | The Shelley-based eras in the Cardano chain
type ShelleyErasAndProtos c =
  [ '(TPraos c, ShelleyEra c)
  , '(TPraos c, AllegraEra c)
  , '(TPraos c, MaryEra c)
  , '(TPraos c, AlonzoEra c)
  , '(Praos c,  BabbageEra c)
  ]

{-------------------------------------------------------------------------------
  Injection from Shelley-based eras into consensus mode eras
-------------------------------------------------------------------------------}

-- | Witness the relation between consensus mode (e.g. Cardano) eras and the Shelley-based eras.
class    (consensusModeEra ~ ShelleyBlock (Fst protoAndEra) (Snd protoAndEra))
  => InjectShelley protoAndEra consensusModeEra
instance (consensusModeEra ~ ShelleyBlock (Fst protoAndEra) (Snd protoAndEra))
  => InjectShelley protoAndEra consensusModeEra

injectShelleyNP2 ::
     AllZip InjectShelley shelleyEras consensusModeEras
  => (   forall era proto protoAndEra consensusModeEra.
         (InjectShelley protoAndEra consensusModeEra
         , protoAndEra ~ '(proto, era)
         )
      => f proto era -> g consensusModeEra
     )
  -> NP2 f shelleyEras -> NP g consensusModeEras
injectShelleyNP2 _ Nil2         = Nil
injectShelleyNP2 f (Cons2 x xs) = f x :* injectShelleyNP2 f xs


-- | When the given ledger state corresponds to a Shelley-based era, apply the
-- given function to it.
overShelleyBasedLedgerState ::
     forall c.
     (TPraos.PraosCrypto c, Praos.PraosCrypto c)
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
