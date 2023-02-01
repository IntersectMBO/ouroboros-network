{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tools.DBAnalyser.Block.Shelley (
    Args (..)
  , ShelleyBlockArgs
  ) where

import qualified Data.Aeson as Aeson
import           Data.Foldable (foldl', toList)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Maybe.Strict
import           Data.Sequence.Strict (StrictSeq)
import           Data.Word (Word64)
import           Lens.Micro ((^.))
import           Lens.Micro.Extras (view)
import           Text.Builder (decimal)

import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Era as CL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.RewardUpdate as SL

import           Cardano.Ledger.Allegra (AllegraEra)
import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import           Cardano.Ledger.Babbage (BabbageEra)
import           Cardano.Ledger.Conway (ConwayEra)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Shelley (ShelleyEra)

import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Node.ProtocolInfo

import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto,
                     StandardShelley)
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyCompatible,
                     shelleyLedgerState)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Node (Nonce (..),
                     ProtocolParamsShelley (..),
                     ProtocolParamsShelleyBased (..), ShelleyGenesis,
                     protocolInfoShelley)

import           Cardano.Tools.DBAnalyser.HasAnalysis

-- | Usable for each Shelley-based era
instance ( ShelleyCompatible proto era
         , PerEraAnalysis era
         ) => HasAnalysis (ShelleyBlock proto era) where

  countTxOutputs blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ body -> sum $ fmap countOutputs (CL.fromTxSeq @era body)
    where
      countOutputs :: Core.Tx era -> Int
      countOutputs tx = length $ tx ^. Core.bodyTxL . Core.outputsTxBodyL

  blockTxSizes blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ body ->
          toList
        $ fmap (fromIntegral . view Core.sizeTxF) (CL.fromTxSeq @era body)

  knownEBBs = const Map.empty

  emitTraces (WithLedgerState _blk lsb lsa) = catMaybes
    [
      let be = SL.nesEL . shelleyLedgerState $ lsb
          ae = SL.nesEL . shelleyLedgerState $ lsa
      in if be /= ae
        then
          Just $ "EPOCH_START_" <> show ae
        else Nothing
    , let brp = SL.nesRu . shelleyLedgerState $ lsb
          arp = SL.nesRu . shelleyLedgerState $ lsa
      in case (brp, arp) of
        (SNothing, SJust _) -> Just "RWDPULSER_START"
        (SJust (SL.Pulsing _ _), SJust (SL.Complete _)) -> Just "RWDPULSER_COMPLETE"
        (SJust _, SNothing) -> Just "RWDPULSER_RESET"
        (_, _) -> Nothing
    ]

  blockStats blk =
      [ decimal $ length $ blockTxSizes blk
      , decimal $ sum    $ blockTxSizes blk
      ]
      ++
      [ decimal $ foldl' (\acc tx -> acc + f tx) 0 txs
      | f <- maybeToList txExUnitsSteps
      ]
    where
      txs :: StrictSeq (Core.Tx era)
      txs = case Shelley.shelleyBlockRaw blk of
        SL.Block _ body -> CL.fromTxSeq @era body

class PerEraAnalysis era where
    txExUnitsSteps :: Maybe (Core.Tx era -> Word64)

instance PerEraAnalysis (ShelleyEra c) where txExUnitsSteps = Nothing
instance PerEraAnalysis (AllegraEra c) where txExUnitsSteps = Nothing
instance PerEraAnalysis (MaryEra    c) where txExUnitsSteps = Nothing

instance (Crypto c)
      => PerEraAnalysis (AlonzoEra c) where
    txExUnitsSteps = Just $ \tx ->
        let (Alonzo.ExUnits _mem steps) = Alonzo.totExUnits tx
        in toEnum $ fromEnum steps

instance (Crypto c)
      => PerEraAnalysis (BabbageEra c) where
    txExUnitsSteps = Just $ \tx ->
        let (Alonzo.ExUnits _mem steps) = Alonzo.totExUnits tx
        in toEnum $ fromEnum steps

instance (Crypto c)
      => PerEraAnalysis (ConwayEra c) where
    txExUnitsSteps = Just $ \tx ->
        let (Alonzo.ExUnits _mem steps) = Alonzo.totExUnits tx
        in toEnum $ fromEnum steps

-- | Shelley-era specific
instance HasProtocolInfo (ShelleyBlock (TPraos StandardCrypto) StandardShelley) where
  data Args (ShelleyBlock (TPraos StandardCrypto) StandardShelley) = ShelleyBlockArgs {
        configFileShelley :: FilePath
      , initialNonce      :: Nonce
      }
    deriving (Show)

  mkProtocolInfo ShelleyBlockArgs{configFileShelley, initialNonce} = do
    config <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileShelley
    return $ mkShelleyProtocolInfo config initialNonce

type ShelleyBlockArgs = Args (ShelleyBlock (TPraos StandardCrypto) StandardShelley)

mkShelleyProtocolInfo ::
     ShelleyGenesis StandardShelley
  -> Nonce
  -> ProtocolInfo IO (ShelleyBlock (TPraos StandardCrypto) StandardShelley)
mkShelleyProtocolInfo genesis initialNonce =
    protocolInfoShelley
      ProtocolParamsShelleyBased {
          shelleyBasedGenesis           = genesis
        , shelleyBasedInitialNonce      = initialNonce
        , shelleyBasedLeaderCredentials = []
        }
      ProtocolParamsShelley {
          shelleyProtVer                = SL.ProtVer 2 0
        , shelleyMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
