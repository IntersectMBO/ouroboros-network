module Cardano.KESAgent.Protocols.AgentInfo
where

import Cardano.KESAgent.KES.Bundle ( Bundle (..) )
import Cardano.KESAgent.KES.Crypto ( Crypto (..) )
import Cardano.KESAgent.KES.Evolution
  ( getCurrentKESPeriodWith
  , updateKESTo
  , EvolutionConfig (..)
  , defEvolutionConfig
  )
import Cardano.KESAgent.KES.OCert
  ( KESPeriod (..)
  , OCert (..)
  , OCertSignable
  , validateOCert
  )
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.StandardCrypto
import qualified Cardano.KESAgent.Protocols.Control.V0.Driver as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Peers as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Protocol as CP0
import qualified Cardano.KESAgent.Protocols.Control.V1.Driver as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Peers as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Protocol as CP1
import Cardano.KESAgent.Protocols.RecvResult ( RecvResult (..) )
import qualified Cardano.KESAgent.Protocols.Service.V0.Driver as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Peers as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Protocol as SP0
import qualified Cardano.KESAgent.Protocols.Service.V1.Driver as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Peers as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Protocol as SP1
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.VersionHandshake.Driver ( VersionHandshakeDriverTrace (..), versionHandshakeDriver )
import Cardano.KESAgent.Protocols.VersionHandshake.Peers ( versionHandshakeServer )
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol ( VersionHandshakeProtocol )
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Util.Pretty ( Pretty (..), strLength )
import Cardano.KESAgent.Util.RefCounting
  ( CRef
  , CRefEvent (..)
  , CRefID
  , acquireCRef
  , newCRef
  , newCRefWith
  , readCRef
  , releaseCRef
  , withCRefValue
  )

import Cardano.Crypto.DSIGN.Class ( DSIGNAlgorithm (..), VerKeyDSIGN )
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.DirectSerialise
  ( DirectDeserialise (..)
  , DirectSerialise (..)
  )
import Cardano.Crypto.KES.Class
  ( ContextKES
  , KESAlgorithm (..)
  , SignKeyWithPeriodKES (..)
  , forgetSignKeyKES
  , rawSerialiseSignKeyKES
  , genKeyKES
  , deriveVerKeyKES
  )
import Cardano.Crypto.Libsodium.MLockedSeed

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket ( Accept (..), Accepted (..), Snocket (..) )

import Data.Word
import Data.SerDoc.Info ( Description (..), aliasField, annField )
import Data.SerDoc.Class ( ViaEnum (..), Codec (..), HasInfo (..), Serializable (..), encodeEnum, decodeEnum, enumInfo )
import qualified Data.SerDoc.Info
import Data.SerDoc.TH (deriveSerDoc)
import Control.Concurrent.Class.MonadMVar
  ( MVar
  , MonadMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , tryTakeMVar
  , withMVar
  )
import Control.Concurrent.Class.MonadSTM ( MonadSTM, retry )
import Control.Concurrent.Class.MonadSTM.TChan
  ( TChan
  , dupTChan
  , newBroadcastTChan
  , readTChan
  , writeTChan
  )
import Control.Concurrent.Class.MonadSTM.TMVar
  ( TMVar
  , newEmptyTMVar
  , newTMVar
  , newTMVarIO
  , putTMVar
  , readTMVar
  , takeTMVar
  , tryReadTMVar
  , tryTakeTMVar
  )
import Control.Monad ( forever, void, when )
import Control.Monad.Class.MonadAsync
  ( MonadAsync
  , concurrently
  , concurrently_
  , mapConcurrently_
  )
import Control.Monad.Class.MonadFork ( labelThread, myThreadId )
import Control.Monad.Class.MonadST ( MonadST )
import Control.Monad.Class.MonadSTM ( atomically )
import Control.Monad.Class.MonadThrow
  ( MonadCatch
  , MonadThrow
  , SomeException
  , bracket
  , catch
  , finally
  , throwIO
  )
import Control.Monad.Class.MonadTime ( MonadTime (..) )
import Control.Monad.Class.MonadTimer ( threadDelay, MonadTimer )
import Control.Tracer ( Tracer (..), nullTracer, traceWith )
import Data.ByteString ( ByteString )
import Data.Coerce
import Data.Functor.Contravariant ( contramap, (>$<) )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy (..) )
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Time ( UTCTime )
import Data.Typeable ( Typeable )
import Text.Printf
import Data.Coerce


data AgentInfo c =
  AgentInfo
    { agentInfoCurrentBundle :: !(Maybe (BundleInfo c))
    , agentInfoStagedKey :: !(Maybe (KeyInfo c))
    , agentInfoCurrentTime :: !UTCTime
    , agentInfoCurrentKESPeriod :: !KESPeriod
    , agentInfoBootstrapConnections :: ![BootstrapInfo]
    }

data BootstrapInfo =
  BootstrapInfo
    { bootstrapInfoAddress :: !Text
    , bootstrapInfoStatus :: !ConnectionStatus
    }
    deriving (Show, Eq)

data BundleInfo c =
  BundleInfo
    { bundleInfoEvolution :: !Word32
    , bundleInfoStartKESPeriod :: !KESPeriod
    , bundleInfoOCertN :: !Word64
    , bundleInfoVK :: !(VerKeyKES (KES c))
    , bundleInfoSigma :: !(DSIGN.SignedDSIGN (DSIGN c) (OCertSignable c))
    }

newtype KeyInfo c =
  KeyInfo
    { keyInfoVK :: VerKeyKES (KES c)
    }

data ConnectionStatus
  = ConnectionUp
  | ConnectionConnecting
  | ConnectionDown
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


