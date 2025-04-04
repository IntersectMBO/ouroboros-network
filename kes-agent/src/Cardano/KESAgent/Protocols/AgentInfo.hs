module Cardano.KESAgent.Protocols.AgentInfo
where

import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.KES.Class (KESAlgorithm (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word

import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (
  KESPeriod (..),
  OCertSignable,
 )

data AgentInfo c
  = AgentInfo
  { agentInfoCurrentBundle :: !(Maybe (TaggedBundleInfo c))
  , agentInfoStagedKey :: !(Maybe (KeyInfo c))
  , agentInfoCurrentTime :: !UTCTime
  , agentInfoCurrentKESPeriod :: !KESPeriod
  , agentInfoBootstrapConnections :: ![BootstrapInfo]
  }

data BootstrapInfo
  = BootstrapInfo
  { bootstrapInfoAddress :: !Text
  , bootstrapInfoStatus :: !ConnectionStatus
  }
  deriving (Show, Eq)

data BundleInfo c
  = BundleInfo
  { bundleInfoEvolution :: !Word32
  , bundleInfoStartKESPeriod :: !KESPeriod
  , bundleInfoOCertN :: !Word64
  , bundleInfoVK :: !(VerKeyKES (KES c))
  , bundleInfoSigma :: !(DSIGN.SignedDSIGN (DSIGN c) (OCertSignable c))
  }

data TaggedBundleInfo c
  = TaggedBundleInfo
  { taggedBundleInfo :: !(Maybe (BundleInfo c))
  , taggedBundleInfoTimestamp :: !(Maybe UTCTime)
  }

newtype KeyInfo c
  = KeyInfo
  { keyInfoVK :: VerKeyKES (KES c)
  }

data ConnectionStatus
  = ConnectionUp
  | ConnectionConnecting
  | ConnectionDown
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
