{-# LANGUAGE GADTs #-}

module Cardano.Tools.CardanoLedgerStateConverter.Types (
    AnyShelleyEra (..)
  , Config (..)
  , SomeNewEpochState (..)
  ) where

import qualified Cardano.Ledger.Allegra as SL
import qualified Cardano.Ledger.Alonzo as SL
import qualified Cardano.Ledger.Babbage as SL
import           Cardano.Ledger.Binary (ToCBOR (..))
import qualified Cardano.Ledger.Conway as SL
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Mary as SL
import qualified Cardano.Ledger.Shelley as SL
import qualified Cardano.Ledger.Shelley.API as SL
import           Ouroboros.Consensus.Storage.LedgerDB (DiskSnapshot)

data AnyShelleyEra c era where
    ShelleyEra :: AnyShelleyEra c (SL.ShelleyEra c)
    AllegraEra :: AnyShelleyEra c (SL.AllegraEra c)
    MaryEra    :: AnyShelleyEra c (SL.MaryEra    c)
    AlonzoEra  :: AnyShelleyEra c (SL.AlonzoEra  c)
    BabbageEra :: AnyShelleyEra c (SL.BabbageEra c)
    ConwayEra  :: AnyShelleyEra c (SL.ConwayEra  c)

data SomeNewEpochState where
    SomeNewEpochState ::
         !(AnyShelleyEra StandardCrypto era)
      -> !(SL.NewEpochState era)
      -> SomeNewEpochState

instance ToCBOR SomeNewEpochState where
  toCBOR (SomeNewEpochState ShelleyEra nes) = toCBOR nes
  toCBOR (SomeNewEpochState AllegraEra nes) = toCBOR nes
  toCBOR (SomeNewEpochState MaryEra    nes) = toCBOR nes
  toCBOR (SomeNewEpochState AlonzoEra  nes) = toCBOR nes
  toCBOR (SomeNewEpochState BabbageEra nes) = toCBOR nes
  toCBOR (SomeNewEpochState ConwayEra  nes)  = toCBOR nes


data Config = Config {
    dbDir    :: FilePath
  , snapShot :: DiskSnapshot
  }
