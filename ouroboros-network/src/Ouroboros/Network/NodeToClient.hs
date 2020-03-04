{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This is the starting point for a module that will bring together the
-- overall node to client protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToClient (
    nodeToClientProtocols
  , NodeToClientVersion (..)
  , NodeToClientVersionData (..)
  , DictVersion (..)
  , nodeToClientCodecCBORTerm

  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers

  , NetworkServerTracers (..)
  , nullNetworkServerTracers
  , connectTo

  , withConnections

  , NetworkSubscriptionTracers (..)

  -- * Re-exported clients
  , chainSyncClientNull
  , localTxSubmissionClientNull

  -- * Re-exported network interface
  , AssociateWithIOCP
  , withIOManager
  , LocalSnocket
  , localSnocket
  , LocalAddress

  -- * Re-exports
  , ConnectionId (..)
  , LocalConnectionId
  , ErrorPolicies (..)
  , networkErrorPolicies
  , nullErrorPolicies
  , ErrorPolicy (..)
  , ErrorPolicyTrace (..)
  , WithAddr (..)
  , SuspendDecision (..)
  , TraceSendRecv (..)
  , DecoderFailureOrTooMuchInput
  , Handshake
  , LocalAddresses (..)
  , SubscriptionTrace (..)
  ) where

import           Control.Exception (IOException)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Typeable (Typeable)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..), DeserialiseFailure)

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Connections.Types (Connections)
import qualified Ouroboros.Network.Connections.Concurrent as Connection
import           Ouroboros.Network.Driver (TraceSendRecv(..))
import           Ouroboros.Network.Driver.ByteLimit (DecoderFailureOrTooMuchInput)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Magic
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Tracers
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientNull)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (localTxSubmissionClientNull)
import           Ouroboros.Network.Protocol.Handshake.Type

import           Ouroboros.Network.Subscription.Ip ( LocalAddresses (..)
                                                   , SubscriptionTrace (..)
                                                   )
import           Ouroboros.Network.Protocol.Handshake.Version hiding (Accept)
import qualified Ouroboros.Network.Protocol.Handshake.Version as V
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Socket hiding (withConnections)
import qualified Ouroboros.Network.Socket as Socket (withConnections)
import           Ouroboros.Network.IOManager

-- | Make an 'OuroborosApplication' for the bundle of mini-protocols that
-- make up the overall node-to-client protocol.
--
-- This function specifies the wire format protocol numbers.
--
-- They are chosen to not overlap with the node to node protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
nodeToClientProtocols
  :: RunMiniProtocol appType bytes m a b -- ^ localChainSync
  -> RunMiniProtocol appType bytes m a b -- ^ localTxSubmission
  -> OuroborosApplication appType bytes m a b
nodeToClientProtocols localChainSync localTxSubmission =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 5,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localChainSync
      }
    , MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 6,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localTxSubmission
      }
    ]

  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = 0xffffffff
    }

-- | Enumeration of node to client protocol versions.
--
data NodeToClientVersion = NodeToClientV_1
  deriving (Eq, Ord, Enum, Show, Typeable)

instance Serialise NodeToClientVersion where
    encode NodeToClientV_1 = CBOR.encodeWord 1
    decode = do
      tag <- CBOR.decodeWord
      case tag of
        1 -> return NodeToClientV_1
        _ -> fail "decode NodeToClientVersion: unknown tag"

-- | Version data for NodeToClient protocol v1
--
newtype NodeToClientVersionData = NodeToClientVersionData
  { networkMagic :: NetworkMagic }
  deriving (Eq, Show, Typeable)

instance Acceptable NodeToClientVersionData where
    acceptableVersion local remote | local == remote = V.Accept
                                   | otherwise =  Refuse $ T.pack $ "version data mismatch: " ++ show local
                                                    ++ " /= " ++ show remote

nodeToClientCodecCBORTerm :: CodecCBORTerm Text NodeToClientVersionData
nodeToClientCodecCBORTerm = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToClientVersionData -> CBOR.Term
      encodeTerm NodeToClientVersionData { networkMagic } =
        CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

      decodeTerm :: CBOR.Term -> Either Text NodeToClientVersionData
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (NodeToClientVersionData $ NetworkMagic $ fromIntegral x)
                               | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t

-- | A specialised version of 'Ouroboros.Network.Socket.connectToNode'.  It is
-- a general purpose function which can connect using any version of the
-- protocol.  This is mostly useful for future enhancements.
--
connectTo
  :: LocalSnocket
  -- ^ callback constructed by 'Ouroboros.Network.IOManager.withIOManager'
  -> NetworkConnectTracers LocalAddress NodeToClientVersion
  -> Versions NodeToClientVersion
              DictVersion
              (ConnectionId LocalAddress ->
                 OuroborosApplication InitiatorApp BL.ByteString IO a b)
  -- ^ A dictionary of protocol versions & applications to run on an established
  -- connection.  The application to run will be chosen by initial handshake
  -- protocol (the highest shared version will be chosen).
  -> FilePath
  -- ^ path of the unix socket or named pipe
  -> IO ()
connectTo snocket tracers versions path =
    connectToNode snocket
                  cborTermVersionDataCodec
                  tracers
                  versions
                  (localAddressFromPath "")
                  (localAddressFromPath path)

-- | `Ouroboros.Network.Socket.withConnections` but with the protocol types
-- specialized.
withConnections
  :: forall request fd addr t.
     ( Ord addr )
  => ErrorPolicies
  -> Snocket IO fd addr
  -> (forall provenance . request provenance -> SomeVersionedApplication NodeToClientVersion DictVersion addr provenance)
  -> (Connections (ConnectionId addr) fd request (Connection.Reject RejectConnection) (Connection.Accept (ConnectionHandle IO)) IO -> IO t)
  -> IO t
withConnections errorPolicies sn mkApp =
  Socket.withConnections sn mkConnectionData
  where
  -- Must give a type signature. Trying to do this in-line will confuse the
  -- type checker.
  mkConnectionData
    :: request provenance
    -> ConnectionData NodeToClientVersion provenance addr
  mkConnectionData request = case mkApp request of
    SomeVersionedResponderApp serverTracers versions -> ConnectionDataRemote
      serverTracers
      errorPolicies
      cborTermVersionDataCodec
      (\(DictVersion _) -> acceptableVersion)
      versions
    SomeVersionedInitiatorApp connectTracers versions -> ConnectionDataLocal
      connectTracers
      errorPolicies
      cborTermVersionDataCodec
      versions

-- | 'ErrorPolicies' for client application.  Additional rules can be added by
-- means of a 'Semigroup' instance of 'ErrorPolicies'.
--
-- This error policies will try to preserve `subscriptionWorker`, e.g. if the
-- connect function throws an `IOException` we will suspend it for
-- a 'shortDelay', and try to re-connect.
--
-- This allows to recover from a situation where a node temporarily shutsdown,
-- or running a client application which is subscribed two more than one node
-- (possibly over network).
--
-- If a trusted node sends us a wrong data or 
--
networkErrorPolicies :: ErrorPolicies
networkErrorPolicies = ErrorPolicies
    { epAppErrorPolicies = [
        -- Handshake client protocol error: we either did not recognise received
        -- version or we refused it.  This is only for outbound connections to
        -- a local node, thus we throw the exception.
        ErrorPolicy
          $ \(_ :: HandshakeClientProtocolError NodeToClientVersion)
                -> Just ourBug

        -- exception thrown by `runDecoderWithByteLimit`
        -- trusted node send too much input
      , ErrorPolicy
          $ \(_ :: DecoderFailureOrTooMuchInput DeserialiseFailure)
                -> Just ourBug

        -- deserialisation failure of a message from a trusted node
      , ErrorPolicy
          $ \(_ :: DeserialiseFailure)
                -> Just ourBug

      , ErrorPolicy
          $ \(e :: MuxError)
                -> case errorType e of
                      MuxUnknownMiniProtocol  -> Just ourBug
                      MuxDecodeError          -> Just ourBug
                      MuxIngressQueueOverRun  -> Just ourBug
                      MuxInitiatorOnly        -> Just ourBug

                      -- in case of bearer closed / or IOException we suspend
                      -- the peer for a short time
                      --
                      -- TODO: the same notes apply as to
                      -- 'NodeToNode.networkErrorPolicies'
                      MuxBearerClosed         -> Just (SuspendPeer shortDelay shortDelay)
                      MuxIOException{}        -> Just (SuspendPeer shortDelay shortDelay)
      ]
    , epConErrorPolicies = [
        -- If an 'IOException' is thrown by the 'connect' call we suspend the
        -- peer for 'shortDelay' and we will try to re-connect to it after that
        -- period.
        ErrorPolicy $ \(_ :: IOException) -> Just $
          SuspendPeer shortDelay shortDelay
      ]
    }
  where
    ourBug :: SuspendDecision DiffTime
    ourBug = Throw

    shortDelay :: DiffTime
    shortDelay = 20 -- seconds

type LocalConnectionId = ConnectionId LocalAddress
