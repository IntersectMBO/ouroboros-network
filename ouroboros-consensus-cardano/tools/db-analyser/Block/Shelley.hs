{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
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

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Tx as SL

import           Ouroboros.Consensus.Node.ProtocolInfo

import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Node (Nonce (..), ShelleyGenesis,
                     protocolInfoShelley)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto

import           HasAnalysis

instance HasAnalysis (ShelleyBlock TPraosStandardCrypto) where
    data Args (ShelleyBlock TPraosStandardCrypto) =
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
    blockHeaderSize =
      fromIntegral . SL.bHeaderSize . SL.bheader . Shelley.shelleyBlockRaw
    blockTxSizes blk = case Shelley.shelleyBlockRaw blk of
      SL.Block _ (SL.TxSeq txs) ->
        toList $ fmap (fromIntegral . BL.length . SL.txFullBytes) txs
    knownEBBs = const Map.empty

type ShelleyBlockArgs = Args (ShelleyBlock TPraosStandardCrypto)

mkShelleyProtocolInfo :: forall c. TPraosCrypto c
                      => ShelleyGenesis c
                      -> Nonce
                      -> ProtocolInfo IO (ShelleyBlock c)
mkShelleyProtocolInfo genesis initialNonce =
    protocolInfoShelley
      genesis
      initialNonce
      2000
      (SL.ProtVer 0 0)
      Nothing

countOutputs :: Shelley.Crypto c => SL.Tx c -> Int
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
