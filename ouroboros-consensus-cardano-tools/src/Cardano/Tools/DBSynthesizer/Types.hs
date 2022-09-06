
module Cardano.Tools.DBSynthesizer.Types (module Cardano.Tools.DBSynthesizer.Types) where

import           Data.Aeson as Aeson (Value)
import           Data.Word (Word64)

import           Ouroboros.Consensus.Block.Abstract (SlotNo)
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis)

import           Cardano.Node.Types (ProtocolFilepaths)


data NodeConfigStub = NodeConfigStub {
    ncsNodeConfig         :: !Aeson.Value
  , ncsAlonzoGenesisFile  :: !FilePath
  , ncsShelleyGenesisFile :: !FilePath
  , ncsByronGenesisFile   :: !FilePath
  , ncsConwayGenesisFile  :: !FilePath
  }
  deriving Show

data NodeFilePaths = NodeFilePaths {
    nfpConfig  :: !FilePath
  , nfpChainDB :: !FilePath
  }
  deriving Show

data NodeCredentials = NodeCredentials {
    credCertFile :: !(Maybe FilePath)
  , credVRFFile  :: !(Maybe FilePath)
  , credKESFile  :: !(Maybe FilePath)
  , credBulkFile :: !(Maybe FilePath)
  }
  deriving Show

data ForgeLimit =
    ForgeLimitBlock   !Word64
  | ForgeLimitSlot    !SlotNo
  | ForgeLimitEpoch   !Word64
  deriving (Eq, Show)

newtype ForgeResult = ForgeResult {resultForged :: Int}
  deriving (Eq, Show)

data DBSynthesizerOpenMode =
      OpenCreate
    | OpenCreateForce
    | OpenAppend
    deriving (Eq, Show)

data DBSynthesizerOptions = DBSynthesizerOptions {
    synthLimit    :: !ForgeLimit
  , synthOpenMode :: !DBSynthesizerOpenMode
  }
  deriving Show

data DBSynthesizerConfig = DBSynthesizerConfig {
    confConfigStub          :: NodeConfigStub
  , confOptions             :: DBSynthesizerOptions
  , confProtocolCredentials :: ProtocolFilepaths
  , confShelleyGenesis      :: ShelleyGenesis StandardShelley
  , confDbDir               :: FilePath
  }
  deriving Show
