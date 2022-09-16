{-# LANGUAGE ApplicativeDo #-}

module CardanoLedgerStateConverter.Parsers (parseCmdLine) where

import           Options.Applicative
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (DiskSnapshot (..))
import           Cardano.Tools.CardanoLedgerStateConverter.Types


{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseCmdLine :: Parser Config
parseCmdLine = Config
    <$> strOption (mconcat [
            long "db"
          , help "Path to the Chain DB"
          , metavar "PATH"
          ])
    <*> parseSelectDB

parseSelectDB :: Parser SelectDB
parseSelectDB = SelectImmutableDB . snd <$> ((,) <$> onlyImmutableDB <*> analyseFrom)
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
