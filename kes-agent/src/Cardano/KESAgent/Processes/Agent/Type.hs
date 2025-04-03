module Cardano.KESAgent.Processes.Agent.Type
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), VerKeyDSIGN)
import Cardano.Crypto.KES.Class (KESAlgorithm (..), SignKeyWithPeriodKES (..))
import Cardano.Crypto.Libsodium.MLockedSeed
import Control.Concurrent.Class.MonadSTM.TChan (TChan)
import Control.Concurrent.Class.MonadSTM.TMVar (TMVar)
import Control.Monad.Class.MonadTime (MonadTime, getCurrentTime)
import Control.Tracer (Tracer (..), nullTracer)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket (Snocket (..))

import Cardano.KESAgent.Protocols.AgentInfo
import Cardano.KESAgent.KES.Bundle (TaggedBundle (..))
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.Evolution (
  EvolutionConfig (..),
  defEvolutionConfig,
 )
import Cardano.KESAgent.KES.OCert (KESPeriod (..))
import Cardano.KESAgent.Processes.ServiceClient (ServiceClientTrace (..))
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionHandshake.Driver (VersionHandshakeDriverTrace (..))
import Cardano.KESAgent.Util.Pretty (Pretty (..), strLength)
import Cardano.KESAgent.Util.RefCounting (
  CRef,
  CRefEvent (..),
 )


{-HLINT ignore "Use underscore" -}

data AgentTrace
  = AgentVersionHandshakeDriverTrace VersionHandshakeDriverTrace
  | AgentBootstrapTrace ServiceClientTrace
  | AgentCheckEvolution KESPeriod
  | AgentControlClientConnected String String
  | AgentControlClientDisconnected String
  | AgentControlDriverTrace ControlDriverTrace
  | AgentControlSocketClosed String
  | AgentControlSocketDisabled
  | AgentControlSocketError String
  | AgentControlVersionHandshakeFailed
  | AgentCRefEvent CRefEvent
  | AgentDroppingKey String
  | AgentInstallingKeyDrop
  | AgentInstallingNewKey String
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
  | AgentReplacingPreviousKey String String
  | AgentServiceClientConnected String String
  | AgentServiceClientDisconnected String
  | AgentServiceDriverTrace ServiceDriverTrace
  | AgentServiceSocketClosed String
  | AgentServiceSocketError String
  | AgentServiceVersionHandshakeFailed
  | AgentSkippingOldKey String String
  | AgentPushingKeyUpdate
  | AgentHandlingKeyUpdate
  | AgentUpdateKESPeriod KESPeriod KESPeriod
  | AgentDebugTrace String
  deriving (Show)

instance Pretty AgentTrace where
  pretty (AgentServiceDriverTrace d) = "Agent: ServiceDriver: " ++ pretty d
  pretty (AgentServiceSocketClosed a) = "Agent: ServiceSocketClosed: " ++ a
  pretty (AgentServiceClientConnected a b) = "Agent: ServiceClientConnected: " ++ a ++ " " ++ b
  pretty (AgentServiceClientDisconnected a) = "Agent: ServiceClientDisconnected: " ++ a
  pretty (AgentServiceSocketError e) = "Agent: ServiceSocketError: " ++ e
  pretty (AgentControlDriverTrace d) = "Agent: ControlDriver: " ++ pretty d
  pretty (AgentControlSocketClosed a) = "Agent: ControlSocketClosed: " ++ a
  pretty (AgentControlClientConnected a b) = "Agent: ControlClientConnected: " ++ a ++ " " ++ b
  pretty (AgentControlClientDisconnected a) = "Agent: ControlClientDisconnected: " ++ a
  pretty (AgentControlSocketError e) = "Agent: ControlSocketError: " ++ e
  pretty x = "Agent: " ++ drop (strLength "Agent") (show x)

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
  , agentGenSeed :: m (MLockedSeed (SeedSizeKES (KES c)))
  }

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

-- The key update lock is required because we need to distinguish between two
-- different situations in which the currentKey TMVar may be empty:
-- - No key has been pushed yet (or the previous key has expired).
-- - The key is currently being updated.
-- In both cases, we want consumers to block until a key is present, but
-- producers need to distinguish these cases:
-- - If no key has been pushed yet, or the previous key has expired, the
--   key pusher is allowed to install a new key; the key evolver can't do
--   anything useful, so it will just sleep and try again later (it cannot
--   block, because that would make it impossible for the pusher to install
--   a key, resulting in a deadlock).
-- - If a key has been pushed, but it is currently being evolved, then the
--   pusher must wait until the evolution has finished, and then install the
--   new key.
-- - If the evolver is about to run, but the key is currently being
--   overwritten, then the evolver should wait for the overwriting to finish,
--   and then attempt to evolve the new key.
-- The keyLock setup achieves this, by allowing only one producer at a
-- time to manipulate the currentKeyVar, regardless of whether it is
-- currently empty or not, while consumers are free to perform blocking reads
-- on it without touching the keyLock.
--
-- Concretely, the rules for accessing currentKeyVar are:
--
-- - readTMVar is allowed, and should be done without acquiring the
--   keyLock.
-- - tryReadTMVar is always allowed, but may not be useful.
-- - takeTMVar is not allowed, since it would 1) block, and 2) require
--   acquisition of keyLock, resulting in a deadlock.
-- - tryTakeTMVar is allowed, but only while holding the keyLock.
data Agent c m fd addr
  = Agent
  { agentSnocket :: Snocket m fd addr
  , agentMRB :: MakeRawBearer m fd
  , agentOptions :: AgentOptions m addr c
  , agentCurrentKeyVar :: TMVar m (Maybe (TaggedBundle m c))
  , agentStagedKeyVar :: TMVar m (Maybe (CRef m (SignKeyWithPeriodKES (KES c))))
  , agentNextKeyChan :: TChan m (TaggedBundle m c)
  , agentServiceFD :: fd
  , agentControlFD :: Maybe fd
  , agentBootstrapConnections :: TMVar m (Map Text ConnectionStatus)
  }
