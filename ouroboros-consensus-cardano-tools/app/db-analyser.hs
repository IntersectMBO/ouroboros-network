{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Database analysis tool.
--
-- Usage: db-analyser --db PATH [--verbose]
--                    [--only-immutable-db [--analyse-from SLOT_NUMBER]]
--                    [--validate-all-blocks | --minimum-block-validation] COMMAND
--                    [--show-slot-block-no | --count-tx-outputs |
--                      --show-block-header-size | --show-block-txs-size |
--                      --show-ebbs | --store-ledger SLOT_NUMBER | --count-blocks |
--                      --checkThunks BLOCK_COUNT | --trace-ledger |
--                      --repro-mempool-and-forge INT]
--                    [--num-blocks-to-process INT]
module Main (main) where

import           Cardano.Tools.DBAnalyser.Run
import           Cardano.Tools.DBAnalyser.Types
import           Control.Monad (void)
import           DBAnalyser.Parsers
import           Options.Applicative (execParser, fullDesc, helper, info,
                     progDesc, (<**>))


main :: IO ()
main = do
    cmdLine <- getCmdLine
    void $ case blockType cmdLine of
      ByronBlock   args -> analyse cmdLine args
      ShelleyBlock args -> analyse cmdLine args
      CardanoBlock args -> analyse cmdLine args

getCmdLine :: IO DBAnalyserConfig
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework used to analyse a Chain DB"
        ])
