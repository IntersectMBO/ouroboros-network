-- | The 'Agent' type, representing the state of a running KES agent process.
module Cardano.KESAgent.Processes.Agent.Type
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), VerKeyDSIGN)
import Cardano.Crypto.KES.Class (KESAlgorithm (..), SignKeyWithPeriodKES (..))
import Cardano.Crypto.Libsodium.MLockedSeed
import Control.Concurrent.Class.MonadSTM.TChan (TChan)
import Control.Concurrent.Class.MonadSTM.TMVar (TMVar)
import Control.Monad.Class.MonadTime (MonadTime, getCurrentTime)
import Control.Tracer (Tracer (..), nullTracer)
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket (Snocket (..))
import Text.Casing

import Cardano.KESAgent.KES.Bundle (TaggedBundle (..))
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.Evolution (
  EvolutionConfig (..),
  defEvolutionConfig,
 )
import Cardano.KESAgent.KES.OCert (KESPeriod (..))
import Cardano.KESAgent.Processes.ServiceClient (ServiceClientTrace (..))
import Cardano.KESAgent.Protocols.AgentInfo
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionHandshake.Driver (VersionHandshakeDriverTrace (..))
import Cardano.KESAgent.Util.Pretty (Pretty (..), strLength)
import Cardano.KESAgent.Util.RefCounting (
  CRef,
  CRefEvent (..),
 )

{-HLINT ignore "Use underscore" -}

-- | Trace messages emitted from a KES agent process.
data AgentTrace
  = AgentVersionHandshakeDriverTrace VersionHandshakeDriverTrace
  | AgentBootstrapTrace String ServiceClientTrace
  | AgentCheckEvolution KESPeriod
  | AgentControlClientConnected String String
  | AgentControlClientDisconnected String
  | AgentControlDriverTrace String ControlDriverTrace
  | AgentControlSocketClosed String
  | AgentControlSocketDisabled
  | AgentControlSocketError String
  | AgentControlVersionHandshakeFailed
  | AgentCRefEvent CRefEvent
  | AgentDroppingKey TaggedBundleTrace
  | AgentInstallingKeyDrop
  | AgentInstallingNewKey TaggedBundleTrace
  | AgentKeyEvolved KESPeriod KESPeriod
  | AgentKeyExpired KESPeriod KESPeriod
  | AgentKeyNotEvolved KESPeriod KESPeriod
  | AgentListeningOnControlSocket String
  | AgentListeningOnServiceSocket String
  | AgentLockAcquired String
  | AgentLockReleased String
  | AgentLockRequest String
  | AgentNoKeyToEvolve
  | AgentRejectingKey String
  | AgentReplacingPreviousKey TaggedBundleTrace TaggedBundleTrace
  | AgentServiceClientConnected String String
  | AgentServiceClientDisconnected String
  | AgentServiceDriverTrace String ServiceDriverTrace
  | AgentServiceSocketClosed String
  | AgentServiceSocketError String
  | AgentServiceVersionHandshakeFailed
  | AgentSkippingOldKey String String
  | AgentRequestingKeyUpdate TaggedBundleTrace
  | AgentPushingKeyUpdate TaggedBundleTrace String
  | AgentHandlingKeyUpdate
  | AgentUpdateKESPeriod KESPeriod KESPeriod
  | AgentGeneratedStagedKey String
  | AgentCouldNotGenerateStagedKey
  | AgentDroppedStagedKey String
  | AgentNoStagedKeyToDrop
  | AgentDebugTrace String
  deriving (Show)

instance Pretty AgentTrace where
  pretty (AgentVersionHandshakeDriverTrace d) = "Agent: version handshake driver: " ++ pretty d
  pretty (AgentBootstrapTrace b t) = "Agent: bootstrap " ++ b ++ ": " ++ drop (length "Service: ") (pretty t)
  pretty (AgentServiceDriverTrace addr d) = "Agent: service driver " ++ addr ++ ": " ++ pretty d
  pretty (AgentServiceSocketClosed a) = "Agent: service socket closed: " ++ a
  pretty (AgentServiceClientConnected a b) = "Agent: service client connected: " ++ a ++ " " ++ b
  pretty (AgentServiceClientDisconnected a) = "Agent: service client disconnected: " ++ a
  pretty (AgentServiceSocketError e) = "Agent: service socket error: " ++ e
  pretty (AgentControlDriverTrace addr d) = "Agent: control driver " ++ addr ++ ": " ++ pretty d
  pretty (AgentControlSocketClosed a) = "Agent: control socket closed: " ++ a
  pretty (AgentControlClientConnected a b) = "Agent: control client connected: " ++ a ++ " " ++ b
  pretty (AgentControlClientDisconnected a) = "Agent: control client disconnected: " ++ a
  pretty (AgentControlSocketError e) = "Agent: control socket error: " ++ e
  pretty (AgentRejectingKey msg) = "Agent: rejecting key: " ++ msg
  pretty (AgentPushingKeyUpdate tb socket) = "Agent: pushing key update: " ++ pretty tb ++ " to " ++ socket
  pretty (AgentRequestingKeyUpdate tb) = "Agent: requesting key update: " ++ pretty tb
  pretty (AgentReplacingPreviousKey old new) = "Agent: replacing previous key: " ++ pretty old ++ " -> " ++ pretty new
  pretty (AgentInstallingNewKey key) = "Agent: installing new key: " ++ pretty key
  pretty (AgentGeneratedStagedKey key) = "Agent: generated staged key: " ++ key
  pretty (AgentDroppedStagedKey key) = "Agent: dropped staged key: " ++ key
  pretty (AgentListeningOnControlSocket addr) = "Agent: listening on control socket: " ++ addr
  pretty (AgentListeningOnServiceSocket addr) = "Agent: listening on service socket: " ++ addr
  pretty x = "Agent: " ++ prettify (drop (strLength "Agent") (show x))
    where
      prettify str =
        case words str of
          [] -> ""
          x : xs -> unwords ((map toLower . toWords . fromAny) x : xs)

-- | Configuration options for creating a new KES agent process.
data AgentOptions m addr c
  = AgentOptions
  { agentTracer :: Tracer m AgentTrace
  , agentControlAddr :: Maybe addr
  -- ^ Socket on which the agent will be listening for control messages,
  -- i.e., KES keys being pushed from a control server.
  -- For \"relay\" agents that don't need to receive any control connections,
  -- this can be 'Nothing'.
  , agentServiceAddr :: addr
  -- ^ Socket on which the agent will send KES keys to any connected nodes.
  , agentBootstrapAddr :: [addr]
  -- ^ Sockets that the agent will use for bootstrapping from other agents.
  , agentEvolutionConfig :: EvolutionConfig
  -- ^ Evolution configuration: genesis, slot duration, slots per KES period
  , agentGetCurrentTime :: m UTCTime
  -- ^ Return the current time. Should normally be set to
  -- 'getPOSIXTime', but overriding this may be desirable for testing
  -- purposes.
  , agentColdVerKey :: VerKeyDSIGN (DSIGN c)
  -- ^ Cold verification key, used for verifying operational certificates. The
  -- KES agent will only accept operational certificates signed using the
  -- corresponding cold signing key.
  , agentGenSeed :: m (MLockedSeed (SeedSizeKES (KES c)))
  -- ^ Seed generator for generating KES signing keys. This is configurable for
  -- testing purposes.
  -- *It is vitally important for the security of generated keys that a
  -- cryptographically sound seed generator is used here. Production code
  -- should always use 'mlockedSeedNewRandom' from @cardano-crypto-class@
  -- here.*
  }

-- | Initialize agent options with reasonable defaults. The following fields
-- must be overridden before use, otherwise they will 'error':
-- - 'agentServiceAddr' (a KES agent that doesn't serve any keys is useless)
-- - 'agentColdVerKey' (a cold verification key is required to verify opcerts)
-- - 'agentGenSeed' (seed generator is required to generate keys)
defAgentOptions :: MonadTime m => AgentOptions m addr c
defAgentOptions =
  AgentOptions
    { agentControlAddr = Nothing
    , agentServiceAddr = error "missing service address"
    , agentBootstrapAddr = []
    , agentEvolutionConfig = defEvolutionConfig
    , agentGetCurrentTime = getCurrentTime
    , agentTracer = nullTracer
    , agentColdVerKey = error "missing cold verification key file"
    , agentGenSeed = error "missing seed generator"
    }

-- | KES agent state.
data Agent c m fd addr
  = Agent
  { agentSnocket :: Snocket m fd addr
  , agentMRB :: MakeRawBearer m fd
  , agentOptions :: AgentOptions m addr c
  , agentCurrentKeyVar :: TMVar m (Maybe (TaggedBundle m c))
  -- ^ Holds the currently active bundle, if any. By convention, the possible
  -- states and their meanings are:
  -- - Empty 'TMVar': a thread is actively accessing the key bundle store.
  --   Use 'takeTMVar' to block until it's your turn.
  -- - @Nothing@: no key bundle has been received, or the last active key
  --   bundle has reached the end of its available evolutions. We do not need
  --   to propagate this fact to service clients.
  -- - @Just TaggedBundle { taggedBundle = Nothing }@: a bundle has been
  --   deleted. We should propagate this to service clients.
  -- - @Just TaggedBundle { taggedBundle = bundle }@: we have an active
  --   bundle. We should propagate this to service clients.
  -- Further, we must hold a reference to any key stored here, and release
  -- that reference when we remove the key from the store.
  , agentStagedKeyVar :: TMVar m (Maybe (CRef m (SignKeyWithPeriodKES (KES c))))
  -- ^ Holds the staged KES signing key, if any. We must hold a reference to
  -- this key for the duration it is stored here. When moving a key from the
  -- staged area to the current slot ('agentCurrentKeyVar'), we may reuse the
  -- reference.
  , agentNextKeyChan :: TChan m (TaggedBundle m c)
  -- ^ Used for broadcasting new keys and forced key deletions as they are
  -- installed. Clients should obtain a reference to the contained keys before
  -- using them.
  , agentServiceFD :: fd
  -- ^ Snocket-specific file descriptor for the service socket.
  , agentControlFD :: Maybe fd
  -- ^ Snocket-specific file descriptor for the control socket, if any.
  , agentBootstrapConnections :: TMVar m (Map Text ConnectionStatus)
  -- ^ Boostrapping connections. Each of these acts as a service client to
  -- another KES agent, receiving keys and installing them locally. This is
  -- the core mechanism for achieving redundancy / self-healing.
  }
