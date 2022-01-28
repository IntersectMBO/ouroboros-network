{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Block.Alonzo (
    AlonzoBlockArgs
  , Args (..)
  ) where

import           Control.Applicative
import           Options.Applicative
import           Prelude


import           HasAnalysis (HasProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardAlonzo)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)


instance HasProtocolInfo (ShelleyBlock StandardAlonzo) where
  data Args (ShelleyBlock StandardAlonzo) = AlonzoBlockArgs {
        configFileAlonzo :: FilePath
      }
    deriving (Show)

  argsParser _ = AlonzoBlockArgs
    <$> strOption (mconcat [
            long "configAlonzo"
          , help "Path to config file"
          , metavar "PATH"
          ])

  -- | This function would only be used if we run an
  -- Alonzo only chain. This should be dead code really.
  mkProtocolInfo _ =
    error $ "Not implemented because we don't "
         <> "anticipate running an 'Alonzo only' chain."

type AlonzoBlockArgs = Args (ShelleyBlock StandardAlonzo)
