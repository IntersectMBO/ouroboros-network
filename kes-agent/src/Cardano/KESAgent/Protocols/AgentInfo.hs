-- | Data types to represent agent state information to display to a user via
-- a control client.
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

-- | Agent state information.
data AgentInfo c
  = AgentInfo
  { agentInfoCurrentBundle :: !(Maybe (TaggedBundleInfo c))
  -- ^ The currently installed bundle (if any)
  , agentInfoStagedKey :: !(Maybe (KeyInfo c))
  -- ^ The key in the staging area (if any)
  , agentInfoCurrentTime :: !UTCTime
  -- ^ Current wall-clock time according to the KES agent. This is useful for
  -- debugging issues due to incorrectly set RTC.
  , agentInfoCurrentKESPeriod :: !KESPeriod
  -- ^ Current KES period according to the KES agent. Useful for verifying
  -- that the KES agent agrees with the live chain - KES agents do not
  -- connect to the chain themselves, so it is important to be able to verify
  -- that their KES period matches that of the chain.
  , agentInfoBootstrapConnections :: ![BootstrapInfo]
  -- Information about bootstrap connections, i.e., connections to other KES
  -- agents. These are only connections *to* other agents, that is,
  -- connections through which the local agent can receive KES keys; not
  -- connections *from* other agents through which the local agent can push
  -- keys out. For a two-way redundant setup, all agents must have
  -- boostrapping connections to the respective other agent(s).
  }

-- | Information about a bootstrapping connection.
data BootstrapInfo
  = BootstrapInfo
  { bootstrapInfoAddress :: !Text
  -- ^ Configured address
  , bootstrapInfoStatus :: !ConnectionStatus
  -- ^ Connection status
  }
  deriving (Show, Eq)

data BundleInfo c
  = BundleInfo
  { bundleInfoEvolution :: !Word32
  -- ^ Current evolution (0-based) of the KES key in the bundle
  , bundleInfoStartKESPeriod :: !KESPeriod
  -- ^ The KES period at which this key's evolution 0 is valid.
  , bundleInfoOCertN :: !Word64
  -- ^ OpCert serial number
  , bundleInfoVK :: !(VerKeyKES (KES c))
  -- ^ Verification key
  , bundleInfoSigma :: !(DSIGN.SignedDSIGN (DSIGN c) (OCertSignable c))
  -- ^ Signature
  }

-- | Information about a \"tagged bundle\". A tagged bundle is always tagged
-- with a timestamp, and may hold an active key bundle, or be empty. If it is
-- empty, then that indicates a key deletion.
data TaggedBundleInfo c
  = TaggedBundleInfo
  { taggedBundleInfo :: !(Maybe (BundleInfo c))
  , taggedBundleInfoTimestamp :: !(Maybe UTCTime)
  }

-- | Information about a KES signing key.
newtype KeyInfo c
  = KeyInfo
  { keyInfoVK :: VerKeyKES (KES c)
  }

-- | Status of a (bootstrapping) connection.
data ConnectionStatus
  = ConnectionUp
  | ConnectionConnecting
  | ConnectionDown
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
