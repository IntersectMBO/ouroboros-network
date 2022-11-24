{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}

module DBAnalyser.Parsers (parseCmdLine) where

#if __GLASGOW_HASKELL__ < 900
-- GHC 8.10 needs this, GHC 9.2 considers it a redundant import.
import           Data.Foldable (asum)
#endif
import           Options.Applicative

import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Ouroboros.Consensus.Block (SlotNo (..))
import           Ouroboros.Consensus.Byron.Node (PBftSignatureThreshold (..))
import           Ouroboros.Consensus.Shelley.Node (Nonce (..))
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (DiskSnapshot (..))

import           Cardano.Tools.DBAnalyser.Block.Byron
import           Cardano.Tools.DBAnalyser.Block.Cardano
import           Cardano.Tools.DBAnalyser.Block.Shelley
import           Cardano.Tools.DBAnalyser.Types


{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseCmdLine :: Parser DBAnalyserConfig
parseCmdLine = DBAnalyserConfig
    <$> strOption (mconcat [
            long "db"
          , help "Path to the Chain DB"
          , metavar "PATH"
          ])
    <*> switch (mconcat [
            long "verbose"
          , help "Enable verbose logging"
          ])
    <*> parseSelectDB
    <*> parseValidationPolicy
    <*> blockTypeParser
    <*> parseAnalysis
    <*> parseLimit

parseSelectDB :: Parser SelectDB
parseSelectDB = asum [
    SelectImmutableDB . snd <$> ((,) <$> onlyImmutableDB <*> analyseFrom)
  , pure SelectChainDB
  ]
  where
    onlyImmutableDB = flag' () (mconcat [
        long "only-immutable-db"
      , help "Validate only the Immutable DB (e.g. do not do ledger validation)"
      ])

    analyseFrom :: Parser (Maybe DiskSnapshot)
    analyseFrom = optional $ ((flip DiskSnapshot $ Just "db-analyser") . read) <$> strOption
      (  long "analyse-from"
      <> metavar "SLOT_NUMBER"
      <> help "Start analysis from ledger state stored at specific slot number" )


parseValidationPolicy :: Parser (Maybe ValidateBlocks)
parseValidationPolicy = parseMaybe $ asum [
      flag' ValidateAllBlocks $ mconcat [
          long "validate-all-blocks"
        , help "Validate all blocks of the Volatile and Immutable DB"
        ]
    , flag' MinimumBlockValidation $ mconcat [
          long "minimum-block-validation"
        , help "Validate a minimum part of the Volatile and Immutable DB"
        ]
    ]

parseAnalysis :: Parser AnalysisName
parseAnalysis = asum [
      flag' ShowSlotBlockNo $ mconcat [
          long "show-slot-block-no"
        , help "Show slot and block number of all blocks"
        ]
    , flag' CountTxOutputs $ mconcat [
          long "count-tx-outputs"
        , help "Show number of transaction outputs per block"
        ]
    , flag' ShowBlockHeaderSize $ mconcat [
          long "show-block-header-size"
        , help "Show the header sizes of all blocks"
        ]
    , flag' ShowBlockTxsSize $ mconcat [
          long "show-block-txs-size"
        , help "Show the total transaction sizes per block"
        ]
    , flag' ShowEBBs $ mconcat [
          long "show-ebbs"
        , help "Show all EBBs and their predecessors"
        ]
    , storeLedgerParser
    , flag' CountBlocks $ mconcat [
          long "count-blocks"
        , help "Count number of blocks processed"
        ]
    , checkNoThunksParser
    , flag' TraceLedgerProcessing $ mconcat [
          long "trace-ledger"
        , help $ "Maintain ledger state and trace ledger phases in the GHC event"
                <> " log. The db-analyser tool performs era-specific analysis"
                <> " of the ledger state and inserts markers for 'significant'"
                <> " events, such as for example epoch transitions."
        ]
    , fmap ReproMempoolAndForge $ option auto $ mconcat [
          long "repro-mempool-and-forge"
        , help $ "Maintain ledger state and mempool trafficking the"
              <> " transactions of each block. The integer is how many"
              <> "blocks to put in the mempool at once."
        , metavar "INT"
        ]
    , pure OnlyValidation
    ]

storeLedgerParser :: Parser AnalysisName
storeLedgerParser = (StoreLedgerStateAt . SlotNo) <$> option auto
  (  long "store-ledger"
  <> metavar "SLOT_NUMBER"
  <> help "Store ledger state at specific slot number" )

checkNoThunksParser :: Parser AnalysisName
checkNoThunksParser = CheckNoThunksEvery <$> option auto
  (  long "checkThunks"
  <> metavar "BLOCK_COUNT"
  <> help "Check the ledger state for thunks every n blocks" )

parseLimit :: Parser Limit
parseLimit = asum [
    Limit <$> option auto (mconcat [
        long "num-blocks-to-process"
      , help "Maximum number of blocks we want to process"
      , metavar "INT"
      ])
  , pure Unlimited
  ]

blockTypeParser :: Parser BlockType
blockTypeParser = subparser $ mconcat
  [ command "byron"
      (info (parseByronType   <**> helper) (progDesc "Analyse a Byron-only DB"))
  , command "shelley"
      (info (parseShelleyType <**> helper) (progDesc "Analyse a Shelley-only DB"))
  , command "cardano"
      (info (parseCardanoType <**> helper) (progDesc "Analyse a Cardano DB"))
  ]

parseByronType :: Parser BlockType
parseByronType = ByronBlock <$> parseByronArgs

parseShelleyType :: Parser BlockType
parseShelleyType = ShelleyBlock <$> parseShelleyArgs

parseCardanoType :: Parser BlockType
parseCardanoType = CardanoBlock <$> parseCardanoArgs

parseMaybe ::  Parser a -> Parser (Maybe a)
parseMaybe parser = asum [Just <$> parser, pure Nothing]


{-------------------------------------------------------------------------------
  Parse BlockType-specific arguments
-------------------------------------------------------------------------------}

parseCardanoArgs :: Parser CardanoBlockArgs
parseCardanoArgs = CardanoBlockArgs
    <$> parseConfigFile
    <*> parsePBftSignatureThreshold

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

parseConfigFile :: Parser FilePath
parseConfigFile = strOption $ mconcat [
      long "config"
    , help "Path to config file"
    , metavar "PATH"
    ]

parsePBftSignatureThreshold :: Parser (Maybe PBftSignatureThreshold)
parsePBftSignatureThreshold = optional $ fmap PBftSignatureThreshold $ option auto $ mconcat [
      long "threshold"
    , help "PBftSignatureThreshold"
    , metavar "THRESHOLD"
    ]

parseByronArgs :: Parser ByronBlockArgs
parseByronArgs = ByronBlockArgs
    <$> parseConfigFile
    <*> flag RequiresNoMagic RequiresMagic (mconcat [
            long "requires-magic"
          , help "The DB contains blocks from a testnet, requiring network magic, rather than mainnet"
          ])
    <*> optional (option auto (mconcat [
            long "genesisHash"
          , help "Expected genesis hash"
          , metavar "HASH"
          ]))
    <*> parsePBftSignatureThreshold
