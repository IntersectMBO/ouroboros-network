-- | This tool synthesizes a valid ChainDB, replicating cardano-node's UX
--
-- Usage: db-synthesizer --config FILE --db PATH
--                       [--shelley-operational-certificate FILE]
--                       [--shelley-vrf-key FILE] [--shelley-kes-key FILE]
--                       [--bulk-credentials-file FILE]
--                       ((-s|--slots NUMBER) | (-b|--blocks NUMBER) |
--                         (-e|--epochs NUMBER)) [-f | -a]
--
-- Available options:
--   --config FILE            Path to the node's config.json
--   --db PATH                Path to the Chain DB
--   --shelley-operational-certificate FILE
--                            Path to the delegation certificate
--   --shelley-vrf-key FILE   Path to the VRF signing key
--   --shelley-kes-key FILE   Path to the KES signing key
--   --bulk-credentials-file FILE
--                            Path to the bulk credentials file
--   -s,--slots NUMBER        Amount of slots to process
--   -b,--blocks NUMBER       Amount of blocks to forge
--   -e,--epochs NUMBER       Amount of epochs to process
--   -f                       Force overwrite an existing Chain DB
--   -a                       Append to an existing Chain DB
module Main (main) where

import           Cardano.Tools.DBSynthesizer.Run
import           DBSynthesizer.Parsers
import           System.Exit


main :: IO ()
main = do
    (paths, creds, forgeOpts) <- parseCommandLine
    result <- initialize paths creds forgeOpts >>= either die (uncurry synthesize)
    putStrLn $ "--> done; result: " ++ show result
