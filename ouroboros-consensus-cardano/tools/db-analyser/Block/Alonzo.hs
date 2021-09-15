{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Block.Alonzo (
    AlonzoBlockArgs
  , Args (..)
  ) where

import           Control.Applicative
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Options.Applicative
import           Prelude

import           Data.Aeson (FromJSON (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (FromJSONKey (..))

import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import           HasAnalysis (HasProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardAlonzo)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

import           Plutus.V1.Ledger.Api (defaultCostModelParams)

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

  -- | Not implemented because we don't anticipate running
  -- an 'Alonzo only' chain.
  mkProtocolInfo _ = undefined

type AlonzoBlockArgs = Args (ShelleyBlock StandardAlonzo)

instance FromJSON Alonzo.AlonzoGenesis where
  parseJSON = Aeson.withObject "Alonzo Genesis" $ \o -> do
    coinsPerUTxOWord     <- o .:  "lovelacePerUTxOWord"
                        <|> o .:  "adaPerUTxOWord"
    cModels              <- o .:? "costModels"
    prices               <- o .:  "executionPrices"
    maxTxExUnits         <- o .:  "maxTxExUnits"
    maxBlockExUnits      <- o .:  "maxBlockExUnits"
    maxValSize           <- o .:  "maxValueSize"
    collateralPercentage <- o .:  "collateralPercentage"
    maxCollateralInputs  <- o .:  "maxCollateralInputs"
    case cModels of
      Nothing -> case Alonzo.CostModel <$> defaultCostModelParams of
        Just m -> return Alonzo.AlonzoGenesis
          { Alonzo.coinsPerUTxOWord
          , Alonzo.costmdls = Map.singleton Alonzo.PlutusV1 m
          , Alonzo.prices
          , Alonzo.maxTxExUnits
          , Alonzo.maxBlockExUnits
          , Alonzo.maxValSize
          , Alonzo.collateralPercentage
          , Alonzo.maxCollateralInputs
          }
        Nothing -> fail "Failed to extract the cost model params from defaultCostModel"
      Just costmdls -> return Alonzo.AlonzoGenesis
        { Alonzo.coinsPerUTxOWord
        , Alonzo.costmdls
        , Alonzo.prices
        , Alonzo.maxTxExUnits
        , Alonzo.maxBlockExUnits
        , Alonzo.maxValSize
        , Alonzo.collateralPercentage
        , Alonzo.maxCollateralInputs
        }

deriving instance FromJSON Alonzo.ExUnits

instance FromJSON Alonzo.Language where
  parseJSON = Aeson.withText "Language" languageFromText

instance FromJSONKey Alonzo.Language where
  fromJSONKey = Aeson.FromJSONKeyTextParser languageFromText

instance FromJSON Alonzo.Prices where
  parseJSON =
    Aeson.withObject "prices" $ \o -> do
      steps <- o .: "prSteps"
      mem   <- o .: "prMem"
      prSteps <- checkBoundedRational steps
      prMem   <- checkBoundedRational mem
      return Alonzo.Prices { Alonzo.prSteps, Alonzo.prMem }
    where
      -- We cannot round-trip via NonNegativeInterval, so we go via Rational
      checkBoundedRational r =
        case Ledger.boundRational r of
          Nothing -> fail ("too much precision for bounded rational: " ++ show r)
          Just s  -> return s

deriving newtype instance FromJSON Alonzo.CostModel

languageFromText :: MonadFail m => Text -> m Alonzo.Language
languageFromText "PlutusV1" = pure Alonzo.PlutusV1
languageFromText lang       = fail $ "Error decoding Language: " ++ show lang
