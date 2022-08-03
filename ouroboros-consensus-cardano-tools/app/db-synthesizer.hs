-- | This tool aims to synthesize a valid ChainDB, replicating cardano-node's UX
module Main (main) where

import           System.Exit

import           Cardano.Tools.DBSynthesizer.Run

import           DBSynthesizer.Parsers


main :: IO ()
main = do
    (paths, creds, forgeOpts) <- parseCommandLine
    result <- initialize paths creds forgeOpts >>= either die (uncurry synthesize)
    putStrLn $ "--> done; result: " ++ show result
