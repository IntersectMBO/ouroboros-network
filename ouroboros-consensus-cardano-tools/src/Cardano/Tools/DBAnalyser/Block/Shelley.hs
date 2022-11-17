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
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Maybe.Strict
import           Lens.Micro ((^.))
import           Lens.Micro.Extras (view)

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as CL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.RewardUpdate as SL

import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Node.ProtocolInfo

import           Ouroboros.Consensus.Ledger.SupportsProtocol
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
         , LedgerSupportsProtocol (ShelleyBlock proto era)
--         , HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era))
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
