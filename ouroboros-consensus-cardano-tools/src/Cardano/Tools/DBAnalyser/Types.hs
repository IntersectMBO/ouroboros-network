
module Cardano.Tools.DBAnalyser.Types (
    module AnalysisTypes
  , module Cardano.Tools.DBAnalyser.Types
  ) where

import           Cardano.Tools.DBAnalyser.Analysis as AnalysisTypes
                     (AnalysisName (..), AnalysisResult (..), Limit (..))
import           Cardano.Tools.DBAnalyser.Block.Byron (ByronBlockArgs)
import           Cardano.Tools.DBAnalyser.Block.Cardano (CardanoBlockArgs)
import           Cardano.Tools.DBAnalyser.Block.Shelley (ShelleyBlockArgs)
import           Ouroboros.Consensus.Storage.LedgerDB (DiskSnapshot)


data SelectDB =
    SelectChainDB
  | SelectImmutableDB (Maybe DiskSnapshot)

data DBAnalyserConfig = DBAnalyserConfig {
    dbDir      :: FilePath
  , verbose    :: Bool
  , selectDB   :: SelectDB
  , validation :: Maybe ValidateBlocks
  , blockType  :: BlockType
  , analysis   :: AnalysisName
  , confLimit  :: Limit
  }

data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation

data BlockType =
    ByronBlock   ByronBlockArgs
  | ShelleyBlock ShelleyBlockArgs
  | CardanoBlock CardanoBlockArgs
