{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- DUPLICATE -- adapted from: cardano-node/src/Cardano/Node/Protocol/Alonzo.hs

module Cardano.Node.Protocol.Alonzo (
    AlonzoProtocolInstantiationError (..)
    -- * Reusable parts
  , readGenesis
  , validateGenesis
  ) where

import           Cardano.Api.Any
import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import           Cardano.Node.Protocol.Shelley (GenesisReadError,
                     readGenesisAny)
import           Cardano.Node.Types
import           Cardano.Prelude
import           Prelude (String)

--
-- Alonzo genesis
--

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Alonzo.AlonzoGenesis, GenesisHash)
readGenesis = readGenesisAny

validateGenesis :: Alonzo.AlonzoGenesis
                -> ExceptT AlonzoProtocolInstantiationError IO ()
validateGenesis _ = return () --TODO alonzo: do the validation

data AlonzoProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | AlonzoCostModelFileError !(FileError ())
  | AlonzoCostModelDecodeError !FilePath !String
  deriving Show

instance Error AlonzoProtocolInstantiationError where
  displayError (InvalidCostModelError fp) =
    "Invalid cost model: " <> show fp
  displayError (CostModelExtractionError fp) =
    "Error extracting the cost model at: " <> show fp
  displayError (AlonzoCostModelFileError err) =
    displayError err
  displayError (AlonzoCostModelDecodeError fp err) =
    "Error decoding cost model at: " <> show fp <> " Error: " <> err

