{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Database analyse tool.
module Main (main) where

import           DBAnalyser.Parsers

import           Control.Monad (void)
import           Options.Applicative (execParser, fullDesc, helper, info,
                     progDesc, (<**>))

import           Cardano.Tools.DBAnalyser.Run
import           Cardano.Tools.DBAnalyser.Types


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
