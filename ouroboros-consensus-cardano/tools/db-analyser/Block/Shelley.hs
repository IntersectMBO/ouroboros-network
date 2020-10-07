{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Block.Shelley (
    Args (..)
  , ShelleyBlockArgs
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (asum, toList)
import qualified Data.Map.Strict as Map
import           Options.Applicative

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL (TxSeq (..))

import           Ouroboros.Consensus.Node.ProtocolInfo

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Node (Nonce (..),
                     ProtocolParamsShelley (..), ShelleyGenesis,
                     protocolInfoShelley)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto

import           HasAnalysis

instance HasAnalysis (ShelleyBlock StandardShelley) where
    data Args (ShelleyBlock StandardShelley) =
      ShelleyBlockArgs {
          configFileShelley :: FilePath
        , initialNonce      :: Nonce
        } deriving Show
    argsParser _ = parseShelleyArgs
    mkProtocolInfo ShelleyBlockArgs {..}  = do
      config <- either (error . show) return =<<
        Aeson.eitherDecodeFileStrict' configFileShelley
      return $ mkShelleyProtocolInfo config initialNonce
    countTxOutputs blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ (SL.TxSeq txs) -> sum $ fmap countOutputs txs
    blockTxSizes blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ (SL.TxSeq txs) ->
        toList $ fmap (fromIntegral . BL.length . SL.txFullBytes) txs
    knownEBBs = const Map.empty

type ShelleyBlockArgs = Args (ShelleyBlock StandardShelley)

mkShelleyProtocolInfo ::
     ShelleyGenesis StandardShelley
  -> Nonce
  -> ProtocolInfo IO (ShelleyBlock StandardShelley)
mkShelleyProtocolInfo genesis initialNonce =
    protocolInfoShelley $ ProtocolParamsShelley {
        shelleyGenesis           = genesis
      , shelleyInitialNonce      = initialNonce
      , shelleyProtVer           = SL.ProtVer 2 0
      , shelleyLeaderCredentials = []
      }

countOutputs :: TPraosCrypto era => SL.Tx era -> Int
countOutputs tx = length $ SL._outputs $ SL._body tx

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
