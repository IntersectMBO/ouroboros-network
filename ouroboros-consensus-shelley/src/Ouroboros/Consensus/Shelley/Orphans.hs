{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Orphans () where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Ledger.Alonzo.Translation (AlonzoGenesis (..))
import qualified Cardano.Ledger.Era as SL (TranslationContext)
import           Cardano.Ledger.Shelley (ShelleyEra)

deriving instance Generic AlonzoGenesis

deriving instance NoThunks AlonzoGenesis

type instance SL.TranslationContext (ShelleyEra c) = ()
