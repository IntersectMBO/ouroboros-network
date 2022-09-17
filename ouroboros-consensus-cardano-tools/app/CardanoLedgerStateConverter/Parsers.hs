{-# LANGUAGE ApplicativeDo #-}

module CardanoLedgerStateConverter.Parsers (parseCmdLine) where

import           Options.Applicative
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (DiskSnapshot (..))
import           Cardano.Tools.CardanoLedgerStateConverter.Types


parseCmdLine :: Parser Config
parseCmdLine =
  Config
    <$> strOption ( long "db"
                    <> help "Path to the Chain DB"
                    <> metavar "PATH")
    <*> parseSnapshot

parseSnapshot :: Parser DiskSnapshot
parseSnapshot =
  flip DiskSnapshot (Just "db-analyser") . read <$> strOption
    (  long "slot"
    <> metavar "SLOT_NUMBER"
    <> help "slot number for snapshot" )
