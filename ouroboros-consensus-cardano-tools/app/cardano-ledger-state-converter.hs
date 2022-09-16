{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           CardanoLedgerStateConverter.Parsers

import           Options.Applicative (execParser, fullDesc, helper, info,
                     progDesc, (<**>))

import           Cardano.Tools.CardanoLedgerStateConverter.Run
import           Cardano.Tools.CardanoLedgerStateConverter.Types


main :: IO ()
main = getCmdLine >>= analyse

getCmdLine :: IO Config
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Convert a ledger snapshot to a CBOR encoded NewEpochState"
        ])
