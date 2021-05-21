module Test.ThreadNet.Infra.Alonzo (degenerateAlonzoGenesis) where

import qualified Data.Map as Map

import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import           Cardano.Ledger.Alonzo.Scripts (Prices (..))
import           Shelley.Spec.Ledger.API (Coin (..))

degenerateAlonzoGenesis :: AlonzoGenesis
degenerateAlonzoGenesis = AlonzoGenesis {
     adaPerUTxOWord       = Coin 0
   , collateralPercentage = 0
   , costmdls             = Map.empty
   , maxBlockExUnits      = mempty
   , maxCollateralInputs  = 0
   , maxTxExUnits         = mempty
   , maxValSize           = 0
   , prices               = Prices (Coin 0) (Coin 0)
   }
