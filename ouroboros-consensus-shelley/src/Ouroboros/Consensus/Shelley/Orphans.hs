{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans            #-}
module Ouroboros.Consensus.Shelley.Orphans where

import           Cardano.Ledger.Era (TranslateEra (..), PreviousEra, TranslationContext)
import           Cardano.Ledger.Alonzo (AlonzoEra)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Ledger.Crypto (Crypto)
import qualified Shelley.Spec.Ledger.API as SL

{-------------------------------------------------------------------------------
  Orphaned instance, TODO: remove this once correct definitions are available
  on the ledger-spec side
-------------------------------------------------------------------------------}
type instance PreviousEra (AlonzoEra c) = MaryEra c
-- | We don't know yet what this type will look like but for now we are
-- setting it to Int as an arbitrary type that is different
-- than unit (unit was used as TranslationContext for all other shelley
-- based eras)
type instance TranslationContext (AlonzoEra c) = Int

instance Crypto c => TranslateEra (AlonzoEra c) SL.ShelleyGenesis where
  translateEra _ctxt _genesis = undefined

instance Crypto c => TranslateEra (AlonzoEra c) SL.Tx where
  translateEra _ctxt _genesis = undefined

instance Crypto c => TranslateEra (AlonzoEra c) SL.NewEpochState where
  translateEra _ctxt _genesis = undefined

{-------------------------------------------------------------------------------
  TODO: Should it be us or ledger-spec
-------------------------------------------------------------------------------}
type instance TranslationContext (ShelleyEra c) = ()
