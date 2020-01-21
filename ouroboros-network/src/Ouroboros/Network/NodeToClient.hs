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
    NodeToClientProtocols(..)
  , NodeToClientVersion (..)
  , NodeToClientVersionData (..)
  , DictVersion (..)
  , nodeToClientCodecCBORTerm

  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers

  , NetworkServerTracers (..)
  , nullNetworkServerTracers
  , NetworkMutableState (..)
  , newNetworkMutableState
  , newNetworkMutableStateSTM
  , cleanNetworkMutableState

  , withConnections

  , NetworkIPSubscriptionTracers (..)
  , IPSubscriptionParams
  , SubscriptionParams (..)
  , ncSubscriptionWorker
  , ncSubscriptionWorker_V1

  -- * Re-exported clients
  , chainSyncClientNull
  , localTxSubmissionClientNull

  -- * Re-exports
  , ConnectionId (..)
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
  , IPSubscriptionTarget (..)
  , SubscriptionTrace (..)
  , WithIPList (..)
  ) where

import           Control.Exception (IOException)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..), DeserialiseFailure)
import           Codec.SerialiseTerm
import qualified Network.Socket as Socket

import           Network.Mux.Types
import           Network.Mux.Interface
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput)
import           Network.TypedProtocol.Driver (TraceSendRecv (..))

import qualified Ouroboros.Network.Connections.Socket.Types as Connections (ConnectionId)
import           Ouroboros.Network.Connections.Types (Connections)
import qualified Ouroboros.Network.Connections.Concurrent as Connection
import           Ouroboros.Network.Magic
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Tracers
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientNull)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (localTxSubmissionClientNull)
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Socket hiding (withConnections)
import qualified Ouroboros.Network.Socket as Socket (withConnections)

import           Ouroboros.Network.Subscription.Ip (IPSubscriptionParams, SubscriptionParams (..))
import qualified Ouroboros.Network.Subscription.Ip as Subscription
import           Ouroboros.Network.Subscription.Ip ( IPSubscriptionTarget (..)
                                                   , WithIPList (..)
                                                   , SubscriptionTrace (..)
                                                   )
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..))

-- | An index type used with the mux to enumerate all the mini-protocols that
-- make up the overall node-to-client protocol.
--
data NodeToClientProtocols = ChainSyncWithBlocksPtcl
                           | LocalTxSubmissionPtcl
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | These are the actual wire format protocol numbers.
--
-- These are chosen to not overlap with the node to node protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
instance ProtocolEnum NodeToClientProtocols where

  fromProtocolEnum ChainSyncWithBlocksPtcl = 5
  fromProtocolEnum LocalTxSubmissionPtcl   = 6

  toProtocolEnum 5 = Just ChainSyncWithBlocksPtcl
  toProtocolEnum 6 = Just LocalTxSubmissionPtcl
  toProtocolEnum _ = Nothing

instance MiniProtocolLimits NodeToClientProtocols where
  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMessageSize  _ = 0xffffffff
  maximumIngressQueue _ = 0xffffffff

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

-- | `Ouroboros.Network.Socket.withConnections` but with the protocol types
-- specialized.
withConnections
  :: forall request t.
     NetworkMutableState
  -> ErrorPolicies Socket.SockAddr ()
  -> (forall provenance . request provenance -> SomeVersionedApplication NodeToClientProtocols NodeToClientVersion DictVersion provenance)
  -> (Connections Connections.ConnectionId Socket.Socket request (Connection.Reject RejectConnection) (Connection.Accept ()) IO -> IO t)
  -> IO t
withConnections mutableState errorPolicies mkApp =
  Socket.withConnections mkConnectionData
  where
  -- Must give a type signature. Trying to do this in-line will confuse the
  -- type checker.
  mkConnectionData
    :: request provenance
    -> ConnectionData NodeToClientProtocols NodeToClientVersion provenance
  mkConnectionData request = case mkApp request of
    SomeVersionedResponderApp serverTracers versions -> ConnectionDataRemote
      serverTracers
      mutableState
      errorPolicies
      cborTermVersionDataCodec
      (\(DictVersion _) -> acceptEq)
      versions
    SomeVersionedInitiatorApp connectTracers versions -> ConnectionDataLocal
      connectTracers
      mutableState
      cborTermVersionDataCodec
      versions

-- TODO use the Connections term to implement subscription workers.

-- | 'ncSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ncSubscriptionWorker
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => NetworkIPSubscriptionTracers NodeToClientProtocols NodeToClientVersion
    -> NetworkMutableState
    -> IPSubscriptionParams ()
    -> Versions
        NodeToClientVersion
        DictVersion
        (OuroborosApplication
          appType
          ConnectionId
          NodeToClientProtocols
          IO BL.ByteString x y)
    -> IO Void
ncSubscriptionWorker
  NetworkIPSubscriptionTracers
    { nistSubscriptionTracer
    , nistMuxTracer
    , nistHandshakeTracer
    , nistErrorPolicyTracer
    }
  networkState
  subscriptionParams
  versions
    = Subscription.ipSubscriptionWorker
        nistSubscriptionTracer
        nistErrorPolicyTracer
        networkState
        subscriptionParams
        (connectToNode'
          cborTermVersionDataCodec
          (NetworkConnectTracers nistMuxTracer nistHandshakeTracer)
          versions)


-- | Like 'ncSubscriptionWorker' but specific to 'NodeToClientV_1'.
--
ncSubscriptionWorker_V1
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => NetworkIPSubscriptionTracers NodeToClientProtocols NodeToClientVersion
    -> NetworkMutableState
    -> IPSubscriptionParams ()
    -> NodeToClientVersionData
    -> (OuroborosApplication
          appType
          ConnectionId
          NodeToClientProtocols
          IO BL.ByteString x y)
    -> IO Void
ncSubscriptionWorker_V1
  tracers
  networkState
  subscriptionParams
  versionData
  application
    = ncSubscriptionWorker
        tracers
        networkState
        subscriptionParams
        (simpleSingletonVersions
          NodeToClientV_1
          versionData
          (DictVersion nodeToClientCodecCBORTerm)
          application)

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
networkErrorPolicies :: ErrorPolicies Socket.SockAddr a
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
                      MuxControlProtocolError -> Just ourBug
                      MuxTooLargeMessage      -> Just ourBug

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
    , epReturnCallback = \_ _ _ -> ourBug
    }
  where
    ourBug :: SuspendDecision DiffTime
    ourBug = Throw

    shortDelay :: DiffTime
    shortDelay = 20 -- seconds
