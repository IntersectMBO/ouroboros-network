{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tools.CardanoLedgerStateConverter.Run (analyse) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.Serialise (Serialise (decode))
import           Control.Monad.Except (runExceptT)

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Node as Node
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..))
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (SnapshotInterval (..), defaultDiskPolicy)
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (readSnapshot)

import           Cardano.Tools.CardanoLedgerStateConverter.Types

import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Allegra as SL
import qualified Cardano.Ledger.Alonzo as SL
import qualified Cardano.Ledger.Babbage as SL
import qualified Cardano.Ledger.Mary as SL
import qualified Cardano.Ledger.Shelley as SL
import qualified Cardano.Ledger.Shelley.API as SL
import           Ouroboros.Consensus.Cardano (CardanoBlock)
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import qualified Ouroboros.Consensus.Ledger.Basics as Consensus
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import           Ouroboros.Consensus.Shelley.Ledger (shelleyLedgerState)
import           Ouroboros.Consensus.Shelley.Ledger.Config (CodecConfig (ShelleyCodecConfig))
import           Ouroboros.Consensus.Byron.Ledger.Config (CodecConfig (ByronCodecConfig))
import           Cardano.Binary (ToCBOR (..), serialize')
import           Cardano.Chain.Slotting (EpochSlots(..))
import qualified Data.ByteString as BS
import           Cardano.Tools.DBAnalyser.Block.Cardano ()


data AnyShelleyEra c era where
    ShelleyEra :: AnyShelleyEra c (SL.ShelleyEra c)
    AllegraEra :: AnyShelleyEra c (SL.AllegraEra c)
    MaryEra    :: AnyShelleyEra c (SL.MaryEra    c)
    AlonzoEra  :: AnyShelleyEra c (SL.AlonzoEra  c)
    BabbageEra :: AnyShelleyEra c (SL.BabbageEra c)

data SomeNewEpochState where
    SomeNewEpochState ::
         !(AnyShelleyEra StandardCrypto era)
      -> !(SL.NewEpochState era)
      -> SomeNewEpochState

instance ToCBOR SomeNewEpochState where
  toCBOR (SomeNewEpochState AlonzoEra nes) = toCBOR nes
  toCBOR _ = error "dont look back"

-- | Returns @Nothing@ only for a Cardano ledger state in the Byron era
projectNewEpochState ::
     Consensus.ExtLedgerState (CardanoBlock StandardCrypto)
  -> Maybe SomeNewEpochState
projectNewEpochState extLedgerState =
    case ledgerState of
      Cardano.LedgerStateByron{}   -> Nothing
      Cardano.LedgerStateShelley x -> f ShelleyEra x
      Cardano.LedgerStateAllegra x -> f AllegraEra x
      Cardano.LedgerStateMary    x -> f MaryEra    x
      Cardano.LedgerStateAlonzo  x -> f AlonzoEra  x
      Cardano.LedgerStateBabbage x -> f BabbageEra x
  where
    ledgerState :: Consensus.LedgerState (CardanoBlock StandardCrypto)
    Consensus.ExtLedgerState {Consensus.ledgerState} = extLedgerState

    f era = Just . SomeNewEpochState era . shelleyLedgerState

analyse :: Config -> IO ()
analyse Config{dbDir, selectDB} = do
      let diskPolicy = defaultDiskPolicy (SecurityParam 0) DefaultSnapshotInterval
          args' = ChainDB.defaultArgs (Node.stdMkChainDbHasFS dbDir) diskPolicy
          ledgerDbFS = ChainDB.cdbHasFSLgrDB args'

      case selectDB of
        SelectImmutableDB initializeFrom -> do
          initLedgerErr <- runExceptT $ case initializeFrom of
            Nothing       -> error "no snapshot given"
            Just snapshot -> readSnapshot ledgerDbFS decodeExtLedgerState' decode snapshot
          initLedger <- either (error . show) pure initLedgerErr
          case projectNewEpochState initLedger of
            Nothing -> error "failed to get ledger state from extended ledger state"
            Just ls -> BS.writeFile "newEpochState.cbor" (serialize' ls)
  where
    decodeExtLedgerState' :: forall s .Decoder s (ExtLedgerState (CardanoBlock StandardCrypto))
    decodeExtLedgerState' =
      let ccfg =
            Cardano.CardanoCodecConfig
              (ByronCodecConfig (EpochSlots 0))
              ShelleyCodecConfig
              ShelleyCodecConfig
              ShelleyCodecConfig
              ShelleyCodecConfig
              ShelleyCodecConfig
      in decodeExtLedgerState
           (decodeDisk @(CardanoBlock StandardCrypto) ccfg)
           (decodeDisk @(CardanoBlock StandardCrypto) ccfg)
           (decodeDisk @(CardanoBlock StandardCrypto) ccfg)
