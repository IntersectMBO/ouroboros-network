{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Block.Shelley (
    Args (..)
  , ShelleyBlockArgs
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Short as Short
import           Data.Foldable (asum, toList)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Maybe.Strict
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set
import           GHC.Records (HasField, getField)
import           Options.Applicative

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.TxIn as Core
import qualified Cardano.Ledger.Era as CL
import           Cardano.Ledger.SafeHash (extractHash, originalBytes)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.RewardUpdate as SL

import           Cardano.Ledger.Allegra (AllegraEra)
import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Ledger.Mary (MaryEra)

import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Node.ProtocolInfo

import           Ouroboros.Consensus.Shelley.Eras (ShelleyBasedEra,
                     StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger (shelleyLedgerState)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Node (Nonce (..),
                     ProtocolParamsShelley (..),
                     ProtocolParamsShelleyBased (..), ShelleyGenesis,
                     protocolInfoShelley)

import           HasAnalysis

{-------------------------------------------------------------------------------
  ActualTxOutputIdDelta
-------------------------------------------------------------------------------}

-- | As of Alonzo, a transaction's effect on the UTxO depends on more than just
-- the @"inputs"@ and @"outputs"@ field
class ActualTxOutputIdDelta era where
  -- | Which UTxO this transaction consumed
  consumedTxIn       :: Core.Tx era -> Set.Set (SL.TxIn (CL.Crypto era))
  -- | How many UTxO this transaction created
  createdTxOutputIds :: Core.Tx era -> Int

preAlonzo_consumedTxIn ::
  ( ShelleyBasedEra era
  , HasField "inputs" (Core.TxBody era) (Set.Set (SL.TxIn (CL.Crypto era)))
  ) => Core.Tx era -> Set.Set (SL.TxIn (CL.Crypto era))
preAlonzo_consumedTxIn = getField @"inputs" . getField @"body"

preAlonzo_createdTxOutputIds ::
     ShelleyBasedEra era
  => Core.Tx era -> Int
preAlonzo_createdTxOutputIds = length . getField @"outputs" . getField @"body"

instance SL.PraosCrypto c => ActualTxOutputIdDelta (ShelleyEra c) where
  consumedTxIn       = preAlonzo_consumedTxIn
  createdTxOutputIds = preAlonzo_createdTxOutputIds

instance SL.PraosCrypto c => ActualTxOutputIdDelta (AllegraEra c) where
  consumedTxIn       = preAlonzo_consumedTxIn
  createdTxOutputIds = preAlonzo_createdTxOutputIds

instance SL.PraosCrypto c => ActualTxOutputIdDelta (MaryEra c) where
  consumedTxIn       = preAlonzo_consumedTxIn
  createdTxOutputIds = preAlonzo_createdTxOutputIds

isValidTx :: Core.Tx (AlonzoEra c) -> Bool
isValidTx vtx = Alonzo.IsValid True == getField @"isValid" vtx

instance SL.PraosCrypto c => ActualTxOutputIdDelta (AlonzoEra c) where
  consumedTxIn vtx =
    if isValidTx vtx
    then preAlonzo_consumedTxIn vtx
    else getField @"collateral" (getField @"body" vtx)
  createdTxOutputIds vtx =
    if not (isValidTx vtx) then 0 else preAlonzo_createdTxOutputIds vtx

{-------------------------------------------------------------------------------
  HasAnalysis instance
-------------------------------------------------------------------------------}

-- | Usable for each Shelley-based era
instance ( ShelleyBasedEra era
         , ActualTxOutputIdDelta era
         , HasField "inputs"  (Core.TxBody era) (Set.Set (SL.TxIn (CL.Crypto era)))
         , HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era))
         ) => HasAnalysis (ShelleyBlock era) where

  countTxOutputs blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ body -> sum $ fmap countOutputs (CL.fromTxSeq @era body)
    where
      countOutputs :: Core.Tx era -> Int
      countOutputs = length . getField @"outputs" . getField @"body"

  extractTxOutputIdDelta blk =
      ( length txs
      , foldMap  inputs          txs
      , mapMaybe outputs (toList txs)
      )
    where
      SL.Block _ body = Shelley.shelleyBlockRaw blk

      txs = CL.fromTxSeq @era body

      asShort = Short.toShort . originalBytes . extractHash . Core._unTxId

      cnv :: SL.TxIn (CL.Crypto era) -> TxIn
      cnv (Core.TxIn txid nat) = TxIn (asShort txid) (fromInteger $ toInteger nat)

      inputs :: Core.Tx era -> [TxIn]
      inputs = map cnv . Set.toList . consumedTxIn

      outputs :: Core.Tx era -> Maybe TxOutputIds
      outputs tx =
          mkTxOutputIds (asShort txid) n
        where
          txid   = Core.txid $ getField @"body" tx
          n      = createdTxOutputIds tx

  -- TODO this is dead-code for a Cardano chain, but inaccurate for a pure
  -- Shelley chain
  genesisTxOutputIds _ = (0, [])

  blockTxSizes blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ body ->
          toList
        $ fmap (fromIntegral . (getField @"txsize")) (CL.fromTxSeq @era body)

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

{-------------------------------------------------------------------------------
  HasProtocolInfo instance
-------------------------------------------------------------------------------}

-- | Shelley-era specific
instance HasProtocolInfo (ShelleyBlock StandardShelley) where
  data Args (ShelleyBlock StandardShelley) = ShelleyBlockArgs {
        configFileShelley :: FilePath
      , initialNonce      :: Nonce
      }
    deriving (Show)

  argsParser _ = parseShelleyArgs
  mkProtocolInfo ShelleyBlockArgs {..}  = do
    config <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileShelley
    return $ mkShelleyProtocolInfo config initialNonce

type ShelleyBlockArgs = Args (ShelleyBlock StandardShelley)

mkShelleyProtocolInfo ::
     ShelleyGenesis StandardShelley
  -> Nonce
  -> ProtocolInfo IO (ShelleyBlock StandardShelley)
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

parseShelleyArgs :: Parser ShelleyBlockArgs
parseShelleyArgs = ShelleyBlockArgs
    <$> strOption (mconcat [
            long "configShelley"
          , help "Path to config file"
          , metavar "PATH"
          ])
    <*> asum [ Nonce  <$> parseNonce
             , pure NeutralNonce]
  where
    parseNonce = strOption (mconcat [
            long "nonce"
          , help "Initial nonce, i.e., hash of the genesis config file"
          , metavar "NONCE"
          ])
