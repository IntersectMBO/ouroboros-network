{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Block.Byron (
    Args (..)
  , ByronBlockArgs
  , openGenesisByron
  ) where

import           Control.Monad.Except
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (asum)
import           GHC.Natural (Natural)
import           Options.Applicative

import           Cardano.Binary (Raw, unAnnotated)
import qualified Cardano.Crypto as Crypto

import qualified Cardano.Chain.Block as Chain
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as Chain

import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation (SizeInBytes)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Node (PBftSignatureThreshold (..),
                     protocolInfoByron)

import           HasAnalysis

instance HasAnalysis ByronBlock where
    data Args ByronBlock =
      ByronBlockArgs {
          configFileByron      :: FilePath
        , requiresNetworkMagic :: Bool
        , genesisHash          :: Maybe (Crypto.Hash Raw)
        , threshold            :: Maybe PBftSignatureThreshold
        }
    argsParser _ = parseByronArgs
    mkProtocolInfo ByronBlockArgs {..} = do
      config <- openGenesisByron configFileByron genesisHash requiresNetworkMagic
      return $ mkByronProtocolInfo config threshold
    countTxOutputs = aBlockOrBoundary (const 0) countTxOutputsByron
    blockHeaderSize = fromIntegral .
      aBlockOrBoundary blockBoundaryHeaderSize blockHeaderSizeByron
    blockTxSizes = aBlockOrBoundary (const []) blockTxSizesByron
    knownEBBs = const Byron.knownEBBs

type ByronBlockArgs = Args ByronBlock

parseByronArgs :: Parser ByronBlockArgs
parseByronArgs = ByronBlockArgs
    <$> strOption (mconcat [
            long "configByron"
          , help "Path to config file"
          , metavar "PATH"
          ])
    <*> switch (mconcat [
            long "testnet"
          , help "The DB contains blocks from testnet rather than mainnet"
          ])
    <*> parseMaybe (option auto (mconcat [
            long "genesisHash"
          , help "Expected genesis hash"
          , metavar "HASH"
          ]))
    <*> parseMaybe (PBftSignatureThreshold <$> thresholdParser)
  where
    thresholdParser = option auto (mconcat [
            long "threshold"
          , help "PBftSignatureThreshold"
          , metavar "THRESHOLD"
          ])

parseMaybe ::  Parser a -> Parser (Maybe a)
parseMaybe parser = asum [Just <$> parser, pure Nothing]

-- | Equivalent of 'either' for 'ABlockOrBoundary'.
aBlockOrBoundary :: (Chain.ABoundaryBlock ByteString -> a)
                 -> (Chain.ABlock ByteString -> a)
                 -> ByronBlock -> a
aBlockOrBoundary fromBoundary fromRegular blk = case blk of
    Byron.ByronBlock (Chain.ABOBBoundary boundaryBlock) _ _
      -> fromBoundary boundaryBlock
    Byron.ByronBlock (Chain.ABOBBlock regularBlk) _ _
      -> fromRegular regularBlk

countTxOutputsByron :: Chain.ABlock ByteString -> Int
countTxOutputsByron Chain.ABlock{..} = countTxPayload bodyTxPayload
  where
    Chain.ABody { bodyTxPayload } = blockBody

    countTxPayload :: Chain.ATxPayload a -> Int
    countTxPayload = sum
                   . map (countTx . unAnnotated . Chain.aTaTx)
                   . Chain.aUnTxPayload

    countTx :: Chain.Tx -> Int
    countTx = length . Chain.txOutputs

blockBoundaryHeaderSize ::  Chain.ABoundaryBlock ByteString -> Natural
blockBoundaryHeaderSize =
    fromIntegral . BS.length . Chain.boundaryHeaderAnnotation . Chain.boundaryHeader

blockHeaderSizeByron ::  Chain.ABlock ByteString -> Natural
blockHeaderSizeByron = Chain.headerLength . Chain.blockHeader

blockTxSizesByron :: Chain.ABlock ByteString -> [SizeInBytes]
blockTxSizesByron block =
    map (fromIntegral . BL.length . BL.fromStrict . Chain.aTaAnnotation) blockTxAuxs
  where
    Chain.ABlock{ blockBody } = block
    Chain.ABody{ bodyTxPayload } = blockBody
    Chain.ATxPayload{ aUnTxPayload = blockTxAuxs } = bodyTxPayload

openGenesisByron :: FilePath -> Maybe (Crypto.Hash Raw) -> Bool -> IO Genesis.Config
openGenesisByron configFile mHash onMainNet = do
    genesisHash <- case mHash of
      Nothing -> either (error . show) return =<< runExceptT
        (Genesis.unGenesisHash . snd <$> Genesis.readGenesisData configFile)
      Just hash -> return hash
    genesisConfig <- either (error . show) return =<< runExceptT
      (Genesis.mkConfigFromFile
        (if onMainNet -- transactions on testnet include magic number
          then Crypto.RequiresNoMagic
          else Crypto.RequiresMagic)
        configFile
        genesisHash)
    return genesisConfig

mkByronProtocolInfo :: Genesis.Config
                    -> Maybe PBftSignatureThreshold
                    -> ProtocolInfo IO ByronBlock
mkByronProtocolInfo genesisConfig signatureThreshold =
    protocolInfoByron
      genesisConfig
      signatureThreshold
      (Update.ProtocolVersion 1 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "db-analyser") 2)
      Nothing
