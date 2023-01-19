module Test.ThreadNet.Infra.Alonzo (degenerateAlonzoGenesis) where

import           Cardano.Ledger.Alonzo.Core (CoinPerWord (..))
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import           Cardano.Ledger.Alonzo.Scripts (Prices (..), emptyCostModels)
import           Cardano.Ledger.Shelley.API (Coin (..))

degenerateAlonzoGenesis :: AlonzoGenesis
degenerateAlonzoGenesis = AlonzoGenesis {
     agCoinsPerUTxOWord     = CoinPerWord (Coin 0)
   , agCollateralPercentage = 0
   , agCostModels           = emptyCostModels
   , agMaxBlockExUnits      = mempty
   , agMaxCollateralInputs  = 0
   , agMaxTxExUnits         = mempty
   , agMaxValSize           = 0
   , agPrices               = Prices minBound minBound
   }
