{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

-- | This module provides simulation environment and a snocket implementation
-- suitable for 'IOSim'.
--
-- Though this module is designed for simulation \/ testing, it lives in the
-- library, since it is needed in `ouroboros-network-framework:test` and
-- `ouroboros-network:test' components.
--
-- TODO: Create a 'snocket' package, in order to avoid having to have
-- ouroboros-network-testing as a dependency for this cabal library.
module Simulation.Network.Snocket
  ( -- * Simulated Snocket
    withSnocket
  , ObservableNetworkState (..)
  , ResourceException (..)
  , SDUSize
  , Script (..)
  , Size
  , SnocketTrace (..)
  , SockType (..)
  , OpenType (..)
  , normaliseId
  , BearerInfo (..)
  , IOErrType (..)
  , SuccessOrFailure (..)
  , TimeoutDetail (..)
  , noAttenuation
  , FD
  , makeFDRawBearer
  , makeFDBearer
  , GlobalAddressScheme (..)
  , AddressType (..)
  , WithAddr (..)
  ) where

import Prelude hiding (read)

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (when)
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Tracer (Tracer, contramap, contramapM, traceWith)

import GHC.IO.Exception

import Data.Bifoldable (bitraverse_)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Typeable (Typeable)
import Foreign.Marshal (copyBytes)
import Foreign.Ptr (castPtr)
import Numeric.Natural (Natural)
import Text.Printf (printf)

import Data.Monoid.Synchronisation (FirstToFinish (..))
import Data.Wedge

import Network.Mux (SDUSize (..))
import Network.Mux.Bearer.AttenuatedChannel

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.Types (AddressType (..))
import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Test.Ouroboros.Network.Data.Script (Script (..), stepScriptSTM)

data Connection m addr = Connection
    { -- | Attenuated channels of a connection.
      --
      connChannelLocal  :: !(AttenuatedChannel m)
    , connChannelRemote ::   AttenuatedChannel m

      -- | SDU size of a connection.
      --
    , connSDUSize       :: !SDUSize

      -- | Opening state of a connection.  This is used to detect simultaneous
      -- open.
      --
    , connState         :: !ConnectionState

      -- | Provider of this Connection, so one can know its origin and decide
      -- accordingly when accepting/connecting a connection.
    , connProvider      :: !addr
    }


-- | Connection state as seen by the network environment.  We borrow TCP state
-- names, but be aware that these states, unlike in TCP, are not local to the
-- service point.
--
data ConnectionState
    -- | SYN_SENT connection state: after calling `connect` but before the
    -- other side accepted it: either as a simultaneous open or normal open.
    --
  = SYN_SENT

    -- | This corresponds to established state of a tcp connection.
    --
  | ESTABLISHED

    -- | Half opened connection.
    --
  | FIN
  deriving (Eq, Show)


dualConnection :: Connection m addr -> Connection m addr
dualConnection conn@Connection { connChannelLocal, connChannelRemote } =
    conn { connChannelLocal  = connChannelRemote
         , connChannelRemote = connChannelLocal
         }


mkConnection :: ( MonadDelay         m
                , MonadLabelledSTM   m
                , MonadTimer         m
                , MonadThrow         m
                , MonadThrow    (STM m)
                , Eq addr
                )
             => Tracer m (WithAddr (TestAddress addr)
                                   (SnocketTrace m (TestAddress addr)))
             -> BearerInfo
             -> ConnectionId (TestAddress addr)
             -> STM m (Connection m (TestAddress addr))

mkConnection tr bearerInfo connId@ConnectionId { localAddress, remoteAddress } | localAddress == remoteAddress = do
  -- we are connecting to onself.  On Linux this returns a connection which
  -- mirrors all sent data.
  qc <- echoQueueChannel
  channel <- newAttenuatedChannel
      ( ( WithAddr (Just localAddress) (Just remoteAddress)
        . STAttenuatedChannelTrace connId
        )
        `contramap` tr)
      Attenuation
        { aReadAttenuation  = biOutboundAttenuation  bearerInfo
        , aWriteAttenuation = biOutboundWriteFailure bearerInfo
        }
      qc
  return Connection {
    connChannelLocal  = channel,
    connChannelRemote = undefined,
    connSDUSize  = biSDUSize bearerInfo,
    connState    = SYN_SENT,
    connProvider = localAddress
  }

mkConnection tr bearerInfo connId@ConnectionId { localAddress, remoteAddress } =
    (\(connChannelLocal, connChannelRemote) ->
      Connection {
          connChannelLocal,
          connChannelRemote,
          connSDUSize  = biSDUSize bearerInfo,
          connState    = SYN_SENT,
          connProvider = localAddress
        })
  <$>
    newConnectedAttenuatedChannelPair
      ( ( WithAddr (Just localAddress) (Just remoteAddress)
        . STAttenuatedChannelTrace connId
        )
        `contramap` tr)
      ( ( WithAddr (Just remoteAddress) (Just localAddress)
        . STAttenuatedChannelTrace ConnectionId
            { localAddress  = remoteAddress
            , remoteAddress = localAddress
            }
        )
       `contramap` tr)
      Attenuation
        { aReadAttenuation  = biOutboundAttenuation  bearerInfo
        , aWriteAttenuation = biOutboundWriteFailure bearerInfo
        }
      Attenuation
        { aReadAttenuation  = biInboundAttenuation  bearerInfo
        , aWriteAttenuation = biInboundWriteFailure bearerInfo
        }


-- | Connection id independent of who provisioned the connection. 'NormalisedId'
-- satisfies the invariant that for @NormalisedId {nidLow, nidHight}@ we have
-- @nidLow <= nidHigh@.
--
data NormalisedId addr = UnsafeNormalisedId
    { nidLow  :: !addr
    , nidHigh :: !addr
    }
  deriving (Eq, Ord, Show)

-- | Safe constructor of 'NormalisedId'
--
normaliseId :: Ord addr
            => ConnectionId addr -> NormalisedId addr
normaliseId
  ConnectionId {localAddress, remoteAddress}
    | localAddress <= remoteAddress
    = UnsafeNormalisedId localAddress remoteAddress
    | otherwise
    = UnsafeNormalisedId remoteAddress localAddress


-- | Simulation network environment consumed by 'simSnocket'.
--
data NetworkState m addr = NetworkState {
      -- | All listening 'FD's.
      --
      nsListeningFDs      :: StrictTVar m (Map addr (FD m addr)),

      -- | Registry of active connections.
      --
      nsConnections       :: StrictTVar
                              m
                              (Map (NormalisedId addr) (Connection m addr)),

      -- | Get an unused ephemeral address.
      --
      nsNextEphemeralAddr :: AddressType -> STM m addr,

      nsDefaultBearerInfo :: BearerInfo,

      -- | Get the BearerInfo Script for a given connection.
      --
      nsAttenuationMap    :: Map (NormalisedId addr)
                                 (LazySTM.TVar m (Script BearerInfo))

    }

-- | Simulation accessible network environment consumed by 'simSnocket'.
--
newtype ObservableNetworkState addr = ObservableNetworkState {
      -- | Registry of active connections and respective provider
      --
      onsConnections :: Map (NormalisedId addr) addr
    }
    deriving Show


-- | Error types.
--
data IOErrType = IOErrConnectionAborted
               | IOErrResourceExhausted
  deriving (Eq, Show)


-- | Each bearer info describes outbound and inbound side of a point to
-- point bearer.
--
data BearerInfo = BearerInfo
    {
      -- | How long it take to create a connection
      biConnectionDelay      :: !DiffTime

      -- | attenuation of inbound side of the bearer, i.e. attenuation used by
      -- bearers that were 'accept'ed.
    , biInboundAttenuation   ::  Time -> Size -> ( DiffTime,
                                                   SuccessOrFailure )

      -- | attenuation of outbound side of the bearer, i.e. the attenuation used
      -- by bearers that were created with 'connect' call.
      --
    , biOutboundAttenuation  ::  Time -> Size -> ( DiffTime,
                                                   SuccessOrFailure )

      -- | Maximum number of successful writes for an inbound bearer.
    , biInboundWriteFailure  :: !(Maybe Int)

      -- | Maximum number of successful writes for an outbound bearer.
    , biOutboundWriteFailure :: !(Maybe Int)

      -- | Time after which  accept will throw an exception.
      --
      -- Currently it only supports two kinds of exceptions, ones that are
      -- caught and rethrown by the server (ECONNABORTED), and an exception
      -- which would be caught, and delivered to the application via
      -- 'AcceptFailure'.
      --
    , biAcceptFailures       :: !(Maybe (DiffTime, IOError))

      -- | SDU size of the bearer; it will be shared between outbound and inbound
      -- sides.
      --
      -- Note: shrinking 'SDUSize' means make it larger, as this allows to send
      -- fewer chunks through the bearer.
      --
    , biSDUSize              :: !SDUSize
    }

instance Show BearerInfo where
    show BearerInfo {biConnectionDelay, biInboundWriteFailure, biOutboundWriteFailure, biAcceptFailures, biSDUSize} =
      concat
        [ "BearerInfo "
        , show biConnectionDelay
        , " ("
        , show biInboundWriteFailure
        , ") ("
        , show biOutboundWriteFailure
        , ") ("
        , show biAcceptFailures
        , ") ("
        , show biSDUSize
        , ")"
        ]


-- | 'BearerInfo' without attenuation and instantaneous connect delay.  It also
-- using the production value of 'SDUSize'.
--
noAttenuation :: BearerInfo
noAttenuation = BearerInfo { biConnectionDelay      = 0
                           , biInboundAttenuation   = \_ _ -> (0, Success)
                           , biOutboundAttenuation  = \_ _ -> (0, Success)
                           , biInboundWriteFailure  = Nothing
                           , biOutboundWriteFailure = Nothing
                           , biAcceptFailures       = Nothing
                           , biSDUSize              = SDUSize 12228
                           }


-- | Create a new network snocket based on a 'BearerInfo' script.
--
newNetworkState
    :: forall m peerAddr.
       ( MonadLabelledSTM m
       , GlobalAddressScheme peerAddr
       )
    => BearerInfo
    -> Map (NormalisedId (TestAddress peerAddr))
           (Script BearerInfo)
    -- ^ the largest ephemeral address
    -> m (NetworkState m (TestAddress peerAddr))
newNetworkState defaultBearerInfo scriptMap = atomically $ do
  (v :: StrictTVar m Natural) <- newTVar 0
  let nextEphemeralAddr :: AddressType -> STM m (TestAddress peerAddr)
      nextEphemeralAddr addrType = do
        -- TODO: we should use `(\s -> (succ s, s)` but p2p-master does not
        -- include PR #3172.
         a <- stateTVar v (\s -> let s' = succ s in (s', s'))
         return (ephemeralAddress addrType a)

  scriptMapVars <- traverse LazySTM.newTVar scriptMap
  s <- NetworkState
    -- nsListeningFDs
    <$> newTVar Map.empty
    -- nsConnections
    <*> newTVar Map.empty
    -- nsNextEphemeralAddr
    <*> pure nextEphemeralAddr
    -- nsBearerInfo
    <*> pure defaultBearerInfo
    -- attenuationMap
    <*> pure scriptMapVars

  labelTVar (nsListeningFDs s)   "nsListeningFDs"
  labelTVar (nsConnections s)    "nsConnections"
  return s


data ResourceException
  = forall addr. (Typeable addr, Show addr)
              => NotReleasedListeningSockets [addr] (Maybe SomeException)
  | forall addr. (Typeable addr, Ord addr, Show addr)
              => NotReleasedConnections (Map (NormalisedId addr) ConnectionState)
                                        (Maybe SomeException)

deriving instance Show ResourceException
instance Exception ResourceException where


-- | A type class for global IP address scheme.  Every node in the simulation
-- has an ephemeral address.  Every node in the simulation has an implicit ipv4
-- and ipv6 address (if one is not bound by explicitly).
--
class GlobalAddressScheme addr where
    getAddressType   :: TestAddress addr -> AddressType
    ephemeralAddress :: AddressType -> Natural -> TestAddress addr



-- | All negative addresses are ephemeral.  Even address are IPv4, while odd
-- ones are IPv6.
--
instance GlobalAddressScheme Int where
    getAddressType (TestAddress n) = if n `mod` 2 == 0
                         then IPv4Address
                         else IPv6Address
    ephemeralAddress IPv4Address n = TestAddress $ (-2) * fromIntegral n
    ephemeralAddress IPv6Address n = TestAddress $ (-1) * fromIntegral n + 1


-- | A bracket which runs a network simulation.  When the simulation
-- terminates it verifies that all listening sockets and all connections are
-- closed.  It might throw 'ResourceException'.
--
withSnocket
    :: forall m peerAddr a.
       ( Alternative (STM m)
       , MonadDelay       m
       , MonadLabelledSTM m
       , MonadMask        m
       , MonadTimer       m
       , MonadThrow  (STM m)
       , GlobalAddressScheme peerAddr
       , Ord      peerAddr
       , Typeable peerAddr
       , Show     peerAddr
       )
    => Tracer m (WithAddr (TestAddress peerAddr)
                          (SnocketTrace m (TestAddress peerAddr)))
    -> BearerInfo
    -> Map (NormalisedId (TestAddress peerAddr))
           (Script BearerInfo)
    -> (Snocket m (FD m (TestAddress peerAddr)) (TestAddress peerAddr)
        -> m (ObservableNetworkState (TestAddress peerAddr))
        -> m a)
    -> m a
withSnocket tr defaultBearerInfo scriptMap k = do
    st <- newNetworkState defaultBearerInfo scriptMap
    a <- k (mkSnocket st tr) (toState st)
         `catch`
         \e -> do re <- checkResources st (Just e)
                  traverse_ throwIO re
                  throwIO e
    re <- checkResources st Nothing
    traverse_ throwIO re
    return a
  where
    -- verify that all sockets are closed
    checkResources :: NetworkState m (TestAddress peerAddr)
                   -> Maybe SomeException
                   -> m (Maybe ResourceException)
    checkResources NetworkState { nsListeningFDs, nsConnections } err = do
      (lstFDMap, connMap) <- atomically $ (,) <$> readTVar nsListeningFDs
                                              <*> readTVar nsConnections
      if |  not (Map.null lstFDMap)
         -> return $ Just (NotReleasedListeningSockets (Map.keys lstFDMap) err)

         |  not (Map.null connMap)
         -> return $ Just (NotReleasedConnections      ( fmap connState
                                                       $ connMap
                                                       ) err)

         |  otherwise
         -> return   Nothing

    toState :: NetworkState m (TestAddress peerAddr)
               -> m (ObservableNetworkState (TestAddress peerAddr))
    toState ns = atomically $ do
        onsConnections <- fmap connProvider <$> readTVar (nsConnections ns)
        return (ObservableNetworkState onsConnections)



-- | Channel together with information needed by the other end, e.g. address of
-- the connecting host, shared 'SDUSize'.
--
data ChannelWithInfo m addr = ChannelWithInfo {
    cwiAddress       :: !addr,
    cwiSDUSize       :: !SDUSize,
    cwiChannelLocal  :: !(AttenuatedChannel m),
    cwiChannelRemote :: !(AttenuatedChannel m)
  }


--
-- File descriptors
--

-- | Internal file descriptor type which tracks the file descriptor state
-- across 'Snocket' api calls.
--
data FD_ m addr
    -- | 'FD_' for uninitialised snockets (either not connected or not
    -- listening).
    --
    -- 'open' or 'openToConnect' creates an uninitialised file descriptor
    -- (which corresponds to 'socket' system call).
    -- 'bind' will update the address.
    = FDUninitialised
        !(Maybe addr)
        -- ^ address (initialised by a 'bind')

    -- | 'FD_' for snockets in listening state.
    --
    -- 'FDListening' is created by 'listen'
    --
    | FDListening
        !addr
        -- ^ listening address

        !(StrictTBQueue m (ChannelWithInfo m addr))
        -- ^ listening queue; when 'connect' is called; dual 'AttenuatedChannel'
        -- of 'FDConnected' file descriptor is passed through the listening
        -- queue.
        --
        -- 'connect' is the producer of this queue;
        -- 'accept' is the consumer.

    -- | 'FD_' was passed to 'connect' call, if needed an ephemeral address was
    -- assigned to it.  This corresponds to 'SYN_SENT' state.
    --
    | FDConnecting !(ConnectionId addr)
                   !(Connection m addr)

    -- | 'FD_' for snockets in connected state.
    --
    -- 'FDConnected' is created by either 'connect' or 'accept'.  It
    -- corresponds to 'ESTABLISHED' state.
    --
    | FDConnected
        !(ConnectionId addr)
        -- ^ local and remote addresses
        !(Connection m addr)
        -- ^ connection

    -- | 'FD_' of a closed file descriptor; we keep 'ConnectionId' just for
    -- tracing purposes.
    --
    | FDClosed
        !(Wedge (ConnectionId addr) addr)


instance Show addr => Show (FD_ m addr) where
    show (FDUninitialised mbAddr)   = "FDUninitialised " ++ show mbAddr
    show (FDListening addr _)       = "FDListening " ++ show addr
    show (FDConnecting connId conn) = concat
                                    [ "FDConnecting "
                                    , show connId
                                    , " "
                                    , show (connSDUSize conn)
                                    ]
    show (FDConnected connId conn)  = concat
                                    [ "FDConnected "
                                    , show connId
                                    , " "
                                    , show (connSDUSize conn)
                                    ]
    show (FDClosed mbConnId)        = "FDClosed " ++ show mbConnId


-- | File descriptor type.
--
newtype FD m peerAddr = FD { fdVar :: StrictTVar m (FD_ m peerAddr) }

data FDRawBearerSendTrace
  = SendingBytes Int
  | SentBytes Int
  deriving (Show, Eq)

data FDRawBearerRecvTrace
  = ReceivingBytes Int
  | ReceivedBytes Int
  | ReadingFromBuffer Int
  | ReadingFromSocket Int
  | CheckingBuffer
  | BufferSize Int
  | UpdateBuffer
      Int -- ^ take
      Int -- ^ keep
  | BufferUpdated
  | EndOfStream
  | Copying
  deriving (Show, Eq)

data FDRawBearerTrace
  = TraceSend FDRawBearerSendTrace
  | TraceRecv FDRawBearerRecvTrace
  deriving (Show, Eq)

-- | Make a 'RawBearer' from an 'FD'. Since this is only used for testing, we
-- can bypass the requirement of moving raw bytes directly between file
-- descriptors and provided memory buffers, and we can instead covertly use
-- plain old 'ByteString' under the hood. This allows us to use the
-- 'AttenuatedChannel' inside the `FD_`, even though its send and receive
-- methods do not have the right format.
makeFDRawBearer :: forall m addr.
                   ( MonadST m
                   , MonadThrow m
                   , MonadLabelledSTM m
                   , Show addr
                   )
                => Tracer m FDRawBearerTrace
                -> MakeRawBearer m (FD m (TestAddress addr))
makeFDRawBearer tracer = MakeRawBearer go
  where
    traceSend = traceWith tracer . TraceSend

    traceRecv = traceWith tracer . TraceRecv

    go (FD {fdVar}) = do
      (bufVar :: StrictTMVar m LBS.ByteString) <- newTMVarIO LBS.empty
      return RawBearer
                { send = \src srcSize -> do
                    labelTVarIO fdVar "sender"
                    traceSend $ SendingBytes srcSize
                    fd_ <- readTVarIO fdVar
                    case fd_ of
                      FDConnected _ conn -> do
                        bs <- stToIO . unsafeIOToST $ BS.packCStringLen (castPtr src, srcSize)
                        let bsl = LBS.fromStrict bs
                        acWrite (connChannelLocal conn) bsl
                        traceSend $ SentBytes srcSize
                        return srcSize
                      _ ->
                        throwIO (invalidError fd_)
                , recv = \dst size -> do
                    labelTVarIO fdVar "receiver"
                    let size64 = fromIntegral size
                    traceRecv $ ReceivingBytes size
                    fd_ <- readTVarIO fdVar
                    case fd_ of
                      FDConnected _ conn -> do
                        traceRecv CheckingBuffer
                        bytesFromBuffer <- atomically $ takeTMVar bufVar
                        traceRecv $ BufferSize (fromIntegral $ LBS.length bytesFromBuffer)
                        (lhs, rhs) <- if not (LBS.null bytesFromBuffer)
                          then do
                            traceRecv $ ReadingFromBuffer size
                            return (LBS.take size64 bytesFromBuffer, LBS.drop size64 bytesFromBuffer)
                          else do
                            traceRecv $ ReadingFromSocket size
                            bytesRead <- acRead (connChannelLocal conn)
                            traceRecv $ ReceivedBytes (fromIntegral . LBS.length $ LBS.take size64 bytesRead)
                            return (LBS.take size64 bytesRead, LBS.drop size64 bytesRead)
                        traceRecv $ UpdateBuffer (fromIntegral $ LBS.length lhs) (fromIntegral $ LBS.length rhs)
                        atomically $ putTMVar bufVar rhs
                        traceRecv $ BufferUpdated
                        if LBS.null lhs then do
                          traceRecv EndOfStream
                          return 0
                        else do
                          traceRecv Copying
                          let bs = LBS.toStrict lhs
                          stToIO . unsafeIOToST $ BS.useAsCStringLen bs $ \(src, srcSize) -> do
                              copyBytes dst (castPtr src) srcSize
                              return srcSize
                      _ ->
                        throwIO (invalidError fd_)
                }

    invalidError :: FD_ m (TestAddress addr) -> IOError
    invalidError fd_ = IOError
      { ioe_handle      = Nothing
      , ioe_type        = InvalidArgument
      , ioe_location    = "Ouroboros.Network.Snocket.Sim.toRawBearer"
      , ioe_description = printf "Invalid argument (%s)" (show fd_)
      , ioe_errno       = Nothing
      , ioe_filename    = Nothing
      }

makeFDBearer :: forall addr m.
                ( MonadMonotonicTime m
                , MonadSTM   m
                , MonadThrow m
                , Show addr
                )
             => MakeBearer m (FD m (TestAddress addr))
makeFDBearer = MakeBearer $ \sduTimeout FD { fdVar } _ -> do
        fd_ <- readTVarIO fdVar
        case fd_ of
          FDUninitialised {} ->
            throwIO (invalidError fd_)
          FDListening {} ->
            throwIO (invalidError fd_)
          FDConnecting _ _ -> do
            throwIO (invalidError fd_)
          FDConnected _ conn -> do
            return $ attenuationChannelAsBearer (connSDUSize conn)
                                                sduTimeout
                                                (connChannelLocal conn)
          FDClosed {} ->
            throwIO (invalidError fd_)
  where
    invalidError :: FD_ m (TestAddress addr) -> IOError
    invalidError fd_ = IOError
      { ioe_handle      = Nothing
      , ioe_type        = InvalidArgument
      , ioe_location    = "Ouroboros.Network.Snocket.Sim.toBearer"
      , ioe_description = printf "Invalid argument (%s)" (show fd_)
      , ioe_errno       = Nothing
      , ioe_filename    = Nothing
      }

--
-- Simulated snockets
--

-- TODO: use `Ouroboros.Network.ExitPolicy.WithAddr`
data WithAddr addr event =
    WithAddr { waLocalAddr  :: Maybe addr
             , waRemoteAddr :: Maybe addr
             , waEvent      :: event
             }
  deriving Show

data SockType = ListeningSock
              | ConnectionSock
              | UnknownType
  deriving Show

mkSockType :: FD_ m addr -> SockType
mkSockType FDUninitialised {} = UnknownType
mkSockType FDListening {}     = ListeningSock
mkSockType FDConnecting {}    = ConnectionSock
mkSockType FDConnected {}     = ConnectionSock
mkSockType FDClosed {}        = UnknownType

data TimeoutDetail
    = WaitingToConnect
    | WaitingToBeAccepted
  deriving Show

data SnocketTrace m addr
    = STConnecting   (FD_ m addr) addr
    | STConnected    (FD_ m addr) OpenType
    | STBearerInfo   BearerInfo
    | STConnectError (FD_ m addr) addr IOError
    | STConnectTimeout TimeoutDetail
    | STBindError    (FD_ m addr) addr IOError
    | STClosing      SockType (Wedge (ConnectionId addr) [addr])
    | STClosed       SockType (Maybe (Maybe ConnectionState))
    -- ^ TODO: Document meaning of 'Maybe (Maybe OpenState)'
    | STClosingQueue Bool
    | STClosedQueue  Bool
    | STAcceptFailure SockType SomeException
    | STAccepting
    | STAccepted      addr
    | STAttenuatedChannelTrace (ConnectionId addr) AttenuatedChannelTrace
  deriving Show

-- | Either simultaneous open or normal open.  Unlike in TCP, only one side will
-- will know that it is doing simultaneous open.
--
data OpenType =
    -- | Simultaneous open
      SimOpen

    -- | Normal open
    | NormalOpen
  deriving Show


connectTimeout :: DiffTime
connectTimeout = 120


-- | Simulated 'Snocket' running in 'NetworkState'.  A single 'NetworkState'
-- should be shared with all nodes in the same network.
--
mkSnocket :: forall m addr.
             ( Alternative   (STM m)
             , MonadDelay         m
             , MonadLabelledSTM   m
             , MonadThrow    (STM m)
             , MonadMask          m
             , MonadTimer         m
             , GlobalAddressScheme addr
             , Ord  addr
             , Show addr
             )
          => NetworkState m (TestAddress addr)
          -> Tracer m (WithAddr (TestAddress addr)
                                (SnocketTrace m (TestAddress addr)))
          -> Snocket m (FD m (TestAddress addr)) (TestAddress addr)
mkSnocket state tr = Snocket { getLocalAddr
                             , getRemoteAddr
                             , addrFamily
                             , open
                             , openToConnect
                             , connect
                             , bind
                             , listen
                             , accept
                             , close
                             }
  where
    getLocalAddrM :: FD m (TestAddress addr)
                  -> m (Either (FD_ m (TestAddress addr))
                               (TestAddress addr))
    getLocalAddrM FD { fdVar } = do
        fd_ <- atomically (readTVar fdVar)
        return $ case fd_ of
          FDUninitialised Nothing         -> Left fd_
          FDUninitialised (Just peerAddr) -> Right peerAddr
          FDListening peerAddr _          -> Right peerAddr
          FDConnecting ConnectionId { localAddress } _
                                          -> Right localAddress
          FDConnected  ConnectionId { localAddress } _
                                          -> Right localAddress
          FDClosed {}                     -> Left fd_

    getRemoteAddrM :: FD m (TestAddress addr)
                   -> m (Either (FD_ m (TestAddress addr))
                                (TestAddress addr))
    getRemoteAddrM FD { fdVar } = do
        fd_ <- atomically (readTVar fdVar)
        return $ case fd_ of
          FDUninitialised {}         -> Left fd_
          FDListening {}             -> Left fd_
          FDConnecting ConnectionId { remoteAddress } _
                                     -> Right remoteAddress
          FDConnected  ConnectionId { remoteAddress } _
                                     -> Right remoteAddress
          FDClosed {}                -> Left fd_

    traceWith' :: FD m (TestAddress addr)
               -> SnocketTrace m (TestAddress addr)
               -> m ()
    traceWith' fd =
      let tr' :: Tracer m (SnocketTrace m (TestAddress addr))
          tr' = (\ev -> (\a b -> WithAddr (hush a)
                                          (hush b) ev)
                    <$> getLocalAddrM  fd
                    <*> getRemoteAddrM fd)
                `contramapM` tr
      in traceWith tr'

    --
    -- Snocket api
    --

    getLocalAddr :: FD m (TestAddress addr) -> m (TestAddress addr)
    getLocalAddr fd = do
        maddr <- getLocalAddrM fd
        case maddr of
          Right addr -> return addr
          -- Socket would not error for an @FDUninitialised Nothing@; it would
          -- return '0.0.0.0:0'.
          Left fd_   -> throwIO (ioe fd_)
      where
        ioe :: FD_ m (TestAddress addr) -> IOError
        ioe fd_ = IOError
                { ioe_handle      = Nothing
                , ioe_type        = InvalidArgument
                , ioe_location    = "Ouroboros.Network.Snocket.Sim.getLocalAddr"
                , ioe_description = printf "Transport endpoint (%s) is not connected" (show fd_)
                , ioe_errno       = Nothing
                , ioe_filename    = Nothing
                }

    getRemoteAddr :: FD m (TestAddress addr) -> m (TestAddress addr)
    getRemoteAddr fd = do
      maddr <- getRemoteAddrM fd
      case maddr of
        Right addr -> return addr
        Left fd_   -> throwIO (ioe fd_)
      where
        ioe :: FD_ m (TestAddress addr) -> IOError
        ioe fd_ = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.getRemoteAddr"
          , ioe_description = printf "Transport endpoint is not connected" (show fd_)
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }


    addrFamily :: TestAddress addr -> AddressFamily (TestAddress addr)
    addrFamily _ = TestFamily


    open :: AddressFamily (TestAddress addr) -> m (FD m (TestAddress addr))
    open _ = atomically $ do
      fdVar <- newTVar (FDUninitialised Nothing)
      labelTVar fdVar "fd"
      return FD { fdVar }


    openToConnect :: TestAddress addr  -> m (FD m (TestAddress addr))
    openToConnect _ = open TestFamily


    connect :: FD m (TestAddress addr) -> TestAddress addr -> m ()
    connect fd@FD { fdVar = fdVarLocal } remoteAddress = do
        fd_ <- atomically (readTVar fdVarLocal)
        traceWith' fd (STConnecting fd_ remoteAddress)
        case fd_ of
          -- Mask asynchronous exceptions.  Only unmask when we really block
          -- with using a `threadDelay` or waiting for the connection to be
          -- accepted.
          FDUninitialised mbLocalAddr -> mask $ \unmask -> do
            (connId, bearerInfo, simOpen) <- atomically $ do
              localAddress <-
                case mbLocalAddr of
                  Just addr -> return addr
                  Nothing   -> nsNextEphemeralAddr state (getAddressType remoteAddress)

              let connId = ConnectionId { localAddress, remoteAddress }
                  normalisedId = normaliseId connId

              bearerInfo <- case Map.lookup normalisedId (nsAttenuationMap state) of
                Nothing     -> return (nsDefaultBearerInfo state)
                Just script -> stepScriptSTM script

              connMap <- readTVar (nsConnections state)
              case Map.lookup normalisedId connMap of
                Just Connection { connState = ESTABLISHED } ->
                  throwSTM (connectedIOError fd_)

                Just Connection { connState = SYN_SENT, connProvider }
                  | connProvider == localAddress ->
                  throwSTM (connectedIOError fd_)

                -- simultaneous open
                Just conn@Connection { connState = SYN_SENT } -> do
                  let conn' = conn { connState = ESTABLISHED }
                  writeTVar fdVarLocal (FDConnecting connId conn')
                  modifyTVar (nsConnections state)
                             (Map.adjust (const conn')
                                         (normaliseId connId))
                  return (connId, bearerInfo, SimOpen)

                Just Connection { connState = FIN } ->
                  throwSTM (connectedIOError fd_)

                Nothing -> do
                  conn <- mkConnection tr bearerInfo connId
                  writeTVar fdVarLocal (FDConnecting connId conn)
                  modifyTVar (nsConnections state)
                             (Map.insert (normaliseId connId) conn)
                  -- so far it looks like normal open, it still might turn up
                  -- a simultaneous open if the other side will open the
                  -- connection before it would be put on its accept loop
                  return (connId, bearerInfo, NormalOpen)

            traceWith tr (WithAddr (Just (localAddress connId))
                                   (Just remoteAddress)
                                   (STBearerInfo bearerInfo))
            -- connection delay
            --
            -- We need a way for a node to detect if the other end failed so
            -- we keep an eye on the network state while waiting the full amount
            -- of connection delay
            -- TODO: Improve this see #3628
            connDelayTimeoutVar <-
              registerDelay (biConnectionDelay bearerInfo `min` connectTimeout)
            unmask
              (atomically $ runFirstToFinish $
                  FirstToFinish
                    (LazySTM.readTVar connDelayTimeoutVar >>= check)
                  <>
                  FirstToFinish (do
                    b <- not . Map.member (normaliseId connId)
                      <$> readTVar (nsConnections state)
                    check b
                    throwSTM $ connectIOError connId
                             $ "unknown connection: "
                            ++ show (normaliseId connId))
              )
              `onException`
                -- In the SimOpen case, it can happen that 1 end of the
                -- connection receives an asynchronous exception and we don't
                -- want to remove the connection from the state in this case.
                -- This is because in the SimOpen case 1 of the connect calls
                -- would succeed, and right after that the other end (the one
                -- that got the async exception) would remove the connection
                -- wrongfully leading to an inconsistent state where there's an
                -- end of the connection that holds a successful connection that
                -- does not exist in the state.
                (case simOpen of
                  NormalOpen ->
                    atomically (modifyTVar (nsConnections state)
                                           (Map.delete (normaliseId connId)))
                  SimOpen -> pure ()
                )

            when (biConnectionDelay bearerInfo >= connectTimeout) $ do
              traceWith' fd (STConnectTimeout WaitingToConnect)
              atomically $ modifyTVar (nsConnections state)
                                      (Map.delete (normaliseId connId))
              throwIO (connectIOError connId "connect timeout: when connecting")

            efd <- atomically $ do
              lstMap <- readTVar (nsListeningFDs state)
              lstFd  <- traverse (readTVar . fdVar)
                                 (Map.lookup remoteAddress lstMap)
              mConn  <- Map.lookup (normaliseId connId)
                    <$> readTVar (nsConnections state)
              case lstFd of
                -- error cases
                Nothing ->
                  return (Left (connectIOError connId "no such listening socket"))
                (Just FDUninitialised {}) ->
                  return (Left (connectIOError connId "unitialised listening socket"))
                (Just FDConnecting {}) ->
                  return (Left (invalidError fd_))
                (Just FDConnected {}) ->
                  return (Left (connectIOError connId "not a listening socket"))
                (Just FDClosed {}) ->
                  return (Left notConnectedIOError)

                (Just (FDListening _ queue)) -> do
                  case mConn of
                    -- simultaneous open: this handles both cases: either we or
                    -- the remote side opened it late but before being able to
                    -- accept it.  In the later case we need to use
                    -- 'dualConnection'.
                    Just conn@Connection { connState = ESTABLISHED } -> do
                      let fd_' = FDConnected connId
                               $ case simOpen of
                                   SimOpen    -> dualConnection conn
                                   NormalOpen ->                conn
                      writeTVar fdVarLocal fd_'
                      return (Right (fd_', SimOpen))

                    -- normal open: at this stage the other side did not open
                    -- a connection, we add 'ChannelWithInfo' into accept loop.
                    Just conn@Connection { connState = SYN_SENT } -> do
                      let fd_' = FDConnected connId conn
                      writeTVar fdVarLocal fd_'
                      when (localAddress connId /= remoteAddress) $
                        -- We only write to the accept `queue` if we're not
                        -- connecting to ourselves.
                        writeTBQueue queue
                                     ChannelWithInfo
                                       { cwiAddress       = localAddress connId
                                       , cwiSDUSize       = biSDUSize bearerInfo
                                       , cwiChannelLocal  = connChannelRemote conn
                                       , cwiChannelRemote = connChannelLocal conn
                                       }
                      return (Right (fd_', NormalOpen))

                    Just Connection { connState = FIN } -> do
                      return (Left (connectIOError connId "connect error (FIN)"))

                    Nothing ->
                      return (Left (connectIOError connId "connect error"))

            case efd of
              Left e          -> do
                traceWith' fd (STConnectError fd_ remoteAddress e)
                atomically $ modifyTVar (nsConnections state)
                                        (Map.delete (normaliseId connId))
                throwIO e

              -- TODO: SimOpen and NormalOpen are irrelevant here
              -- If 'o' is SimOpen then 'connState' is already 'ESTABLISHED'
              Right (fd_', o) -> do
                -- successful open

                -- wait for a connection to be accepted; we can also be
                -- interrupted by an asynchronous exception in which case we
                -- just forget about the connection.
                timeoutVar <-
                  registerDelay (connectTimeout - biConnectionDelay bearerInfo)
                r <-
                  handleJust
                    (\e -> case fromException e of
                             Just SomeAsyncException {} -> Just e
                             Nothing                    -> Nothing)
                    (\e -> atomically $ modifyTVar (nsConnections state)
                                                   (Map.delete (normaliseId connId))
                        >> throwIO e)
                    $ unmask . atomically . runFirstToFinish $
                        FirstToFinish (do
                          LazySTM.readTVar timeoutVar >>= check
                          modifyTVar (nsConnections state)
                                     (Map.delete (normaliseId connId))
                          return Nothing
                        )
                        <>
                        FirstToFinish (do
                          mbConn <- Map.lookup (normaliseId connId)
                                <$> readTVar (nsConnections state)
                          case mbConn of
                            -- it could happen that the 'accept' removes the
                            -- connection from the state; we treat this as an io
                            -- exception.
                            Nothing -> do
                              modifyTVar (nsConnections state)
                                         (Map.delete (normaliseId connId))
                              throwSTM $ connectIOError connId
                                       $ "unknown connection: "
                                      ++ show (normaliseId connId)
                            Just Connection { connState } ->
                              Just <$> check (connState == ESTABLISHED))

                case r of
                  -- self connect
                  Nothing | localAddress connId == remoteAddress
                          -> traceWith' fd (STConnected fd_' o)

                  Nothing -> do
                    traceWith' fd (STConnectTimeout WaitingToBeAccepted)
                    throwIO (connectIOError connId "connect timeout: when waiting for being accepted")

                  Just _  -> traceWith' fd (STConnected fd_' o)

          FDConnecting {} ->
            throwIO (invalidError fd_)

          FDConnected {} ->
            throwIO (connectedIOError fd_)

          FDListening {} ->
            throwIO (connectedIOError fd_)

          FDClosed {} ->
            throwIO notConnectedIOError
      where
        notConnectedIOError = IOError
          { ioe_handle      = Nothing
          , ioe_type        = OtherError
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.connect"
          , ioe_description = "Transport endpoint is not connected"
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }

        connectIOError :: ConnectionId (TestAddress addr) -> String -> IOError
        connectIOError connId desc = IOError
          { ioe_handle      = Nothing
          , ioe_type        = OtherError
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.connect"
          , ioe_description = printf "connect failure (%s): (%s)" (show connId) desc
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }

        connectedIOError :: FD_ m (TestAddress addr) -> IOError
        connectedIOError fd_ = IOError
          { ioe_handle      = Nothing
          , ioe_type        = AlreadyExists
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.connect"
          , ioe_description = printf "Transport endpoint (%s) is already connected" (show fd_)
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }

        invalidError :: FD_ m (TestAddress addr) -> IOError
        invalidError fd_ = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.bind"
          , ioe_description = printf "Invalid argument (%s)" (show fd_)
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }


    bind :: FD m (TestAddress addr) -> TestAddress addr -> m ()
    bind fd@FD { fdVar } addr = do
        res <- atomically $ do
          fd_ <- readTVar fdVar
          case fd_ of
            FDUninitialised Nothing -> do
              writeTVar fdVar (FDUninitialised (Just addr))
              labelTVar fdVar ("fd-" ++ show addr)
              return Nothing
            _ ->
              return (Just (fd_, invalidError fd_))
        case res of
          Nothing       -> return ()
          Just (fd_, e) -> traceWith' fd (STBindError fd_ addr e)
                        >> throwIO e
      where
        invalidError fd_ = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.bind"
          , ioe_description = printf "Invalid argument (%s)" (show fd_)
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }


    listen :: FD m (TestAddress addr) -> m ()
    listen fd@FD { fdVar } = atomically $ do
        fd_ <- readTVar fdVar
        case fd_ of
          FDUninitialised Nothing ->
            -- Berkeley socket would not error; but then 'bind' would fail;
            throwSTM $ invalidError fd_

          FDUninitialised (Just addr) -> do
            queue <- newTBQueue bound
            labelTBQueue queue ("aq-" ++ show addr)
            writeTVar fdVar (FDListening addr queue)
            modifyTVar (nsListeningFDs state) (Map.insert addr fd)

          FDConnected {} ->
            throwSTM $ invalidError fd_
          FDConnecting {} ->
            throwSTM $ invalidError fd_
          FDListening {} ->
            return ()
          FDClosed {} ->
            throwSTM $ invalidError fd_
      where
        -- TODO: 'listen' should take this as an explicit argument
        bound :: Natural
        bound = 10

        invalidError :: FD_ m (TestAddress addr) -> IOError
        invalidError fd_ = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.listen"
          , ioe_description = printf "Invalid argument (%s)" (show fd_)
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }


    accept :: FD m (TestAddress addr)
           -> m (Accept m (FD m (TestAddress addr))
                                (TestAddress addr))
    accept FD { fdVar } = do time <- getMonotonicTime
                             let deltaAndIOErr =
                                   biAcceptFailures (nsDefaultBearerInfo state)
                             return $ accept_ time deltaAndIOErr
      where
        -- non-blocking; return 'True' if a connection is in 'SYN_SENT' state
        synSent :: TestAddress addr
                -> ChannelWithInfo m (TestAddress addr)
                -> STM m Bool
        synSent localAddress cwi = do
          connMap <- readTVar (nsConnections state)
          let connId = ConnectionId localAddress (cwiAddress cwi)

          case Map.lookup (normaliseId connId) connMap of
             Nothing                                     ->
               return False
             Just (Connection _ _ _ SYN_SENT provider) ->
               return ( provider /= localAddress
                     || localAddress == cwiAddress cwi
                      )
             _                                           ->
               return False

        accept_ :: Time
                -> Maybe (DiffTime, IOError)
                -> Accept m (FD m (TestAddress addr))
                                  (TestAddress addr)
        accept_ time deltaAndIOErr = Accept $ do
            ctime <- getMonotonicTime
            bracketOnError
              (atomically $ do
                fd <- readTVar fdVar
                case fd of
                  FDUninitialised mbAddr ->
                    -- 'berkeleyAccept' used by 'socketSnocket' will return
                    -- 'IOException's with 'AcceptFailure', we match this behaviour
                    -- here.
                    return $ Left ( toException $ invalidError fd
                                  , mbAddr
                                  , Nothing
                                  , mkSockType fd
                                  )
                  FDConnecting connId _ ->
                    return $ Left ( toException $ invalidError fd
                                  , Just (localAddress connId)
                                  , Nothing
                                  , mkSockType fd
                                  )
                  FDConnected connId _ ->
                    return $ Left ( toException $ invalidError fd
                                  , Just (localAddress connId)
                                  , Nothing
                                  , mkSockType fd
                                  )

                  FDListening localAddress queue -> do
                    -- We should not accept nor fail the 'accept' call in the
                    -- presence of a connection that is __not__ in SYN_SENT
                    -- state. So we take from the StrictTBQueue until we have found
                    -- one that is SYN_SENT state.
                    cwi <- readTBQueueUntil (synSent localAddress) queue
                    let connId = ConnectionId { localAddress,
                                                remoteAddress = cwiAddress cwi }

                    case deltaAndIOErr of
                      -- the `ctime` is the time when we issued 'accept' not
                      -- when read something from the queue.
                      Just (delta, ioErr) | delta `addTime` time >= ctime ->
                          return $ Left ( toException ioErr
                                        , Just localAddress
                                        , Just (connId, cwiChannelLocal cwi)
                                        , mkSockType fd
                                        )
                      _  -> return $ Right ( cwi
                                           , connId
                                           )

                  FDClosed {} ->
                    return $ Left ( toException $ invalidError fd
                                  , Nothing
                                  , Nothing
                                  , mkSockType fd
                                  )
              )
              ( \ result ->
                  case result of
                    Left {} -> return ()
                    Right (chann, connId) -> uninterruptibleMask_ $ do
                      acClose (cwiChannelLocal chann)
                      atomically $
                        modifyTVar (nsConnections state)
                                   (Map.update
                                     (\conn@Connection { connState } ->
                                       case connState of
                                         FIN ->
                                           Nothing
                                         _ ->
                                           Just conn { connState = FIN })
                                     (normaliseId connId))
              )
              $ \ result ->
                case result of
                  Left (err, mbLocalAddr, mbConnIdAndChann, fdType) -> do
                    uninterruptibleMask_ $
                      traverse_ (\(connId, chann) -> do
                                   acClose chann
                                   atomically $ modifyTVar
                                     (nsConnections state)
                                     (Map.update
                                       (\conn@Connection { connState } ->
                                         case connState of
                                           FIN -> Nothing
                                           _   -> Just conn { connState = FIN })
                                       (normaliseId connId))
                                )
                                mbConnIdAndChann
                    traceWith tr (WithAddr mbLocalAddr Nothing (STAcceptFailure fdType err))
                    return (AcceptFailure err, accept_ time deltaAndIOErr)

                  Right (chann, connId@ConnectionId { localAddress, remoteAddress }) -> do
                    traceWith tr (WithAddr (Just localAddress) (Just remoteAddress)
                                           STAccepting)
                    let ChannelWithInfo
                          { cwiSDUSize       = sduSize
                          , cwiChannelLocal  = channelLocal
                          , cwiChannelRemote = channelRemote
                          } = chann

                    fdRemote <- atomically $ do

                      modifyTVar (nsConnections state)
                                 (Map.adjust (\s -> s { connState = ESTABLISHED })
                                             (normaliseId connId))

                      FD <$> newTVar (FDConnected
                                          connId
                                          Connection
                                            { connChannelLocal  = channelLocal
                                            , connChannelRemote = channelRemote
                                            , connSDUSize       = sduSize
                                            , connState         = ESTABLISHED
                                            , connProvider      = remoteAddress
                                            })

                    traceWith tr (WithAddr (Just localAddress) Nothing
                                           (STAccepted remoteAddress))

                    return (Accepted fdRemote remoteAddress, accept_ time deltaAndIOErr)


        invalidError :: FD_ m (TestAddress addr) -> IOError
        invalidError fd = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.accept"
          , ioe_description = printf "Invalid argument (%s)" (show fd)
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }


    close :: FD m (TestAddress addr)
          -> m ()
    close FD { fdVar } =
      uninterruptibleMask_ $ do
        wChannel <- atomically $ do
          fd_ <- readTVar fdVar
          case fd_ of
            FDUninitialised Nothing
              -> writeTVar fdVar (FDClosed Nowhere)
              $> Nowhere
            FDUninitialised (Just addr)
              -> writeTVar fdVar (FDClosed (There addr))
              $> Nowhere
            FDConnecting connId conn
              -> writeTVar fdVar (FDClosed (Here connId))
              $> Here (connId, mkSockType fd_, connChannelLocal conn)
            FDConnected connId conn
              -> writeTVar fdVar (FDClosed (Here connId))
              $> Here (connId, mkSockType fd_, connChannelLocal conn)
            FDListening localAddress queue -> do
              writeTVar fdVar (FDClosed (There localAddress))
              (\as -> There ( localAddress
                            , mkSockType fd_
                            , map (\a -> ( cwiAddress a, cwiChannelLocal a)) as
                            )) <$> drainTBQueue queue
            FDClosed {} ->
              pure Nowhere

        -- trace 'STClosing'
        bitraverse_
          (\(connId, fdType, _) ->
              traceWith tr (WithAddr (Just (localAddress connId))
                                     (Just (remoteAddress connId))
                                     (STClosing fdType (Here connId))))
          (\(addr, fdType, as) ->
              traceWith tr (WithAddr (Just addr)
                                     Nothing
                                     (STClosing fdType (There (map fst as)))))
          wChannel

        -- close channels
        bitraverse_
          (\(_, _, chann)  -> acClose chann)
          (\(_, _, channs) -> traverse_ (acClose . snd) channs)
          wChannel

        -- update NetworkState
        atomically $ bitraverse_
          (\(connId, _, _) ->
             modifyTVar (nsConnections state)
                        (Map.update
                          (\conn@Connection { connState } ->
                            case connState of
                              FIN ->
                                Nothing
                              _ ->
                                Just conn { connState = FIN })
                          (normaliseId connId)))
          (\(addr,   _, _) ->
             modifyTVar (nsListeningFDs state)
                        (Map.delete addr))
          wChannel

        -- trace 'STClosed'
        bitraverse_
          (\(connId, fdType, _) -> do
            openState <- fmap connState . Map.lookup (normaliseId connId)
                     <$> atomically (readTVar (nsConnections state))
            traceWith tr (WithAddr (Just (localAddress connId))
                                   (Just (remoteAddress connId))
                                   (STClosed fdType (Just openState)))

          )
          (\(addr, fdType, _) ->
            traceWith tr (WithAddr (Just addr)
                                   Nothing
                                   (STClosed fdType Nothing))

          )
          wChannel


--
-- Utils
--

hush :: Either a b -> Maybe b
hush Left {}   = Nothing
hush (Right a) = Just a
{-# INLINE hush #-}

drainTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m [a]
drainTBQueue q = do
  ma <- tryReadTBQueue q
  case ma of
    Nothing -> return []
    Just a  -> (a :) <$> drainTBQueue q


-- | Return first element which satisfy the given predicate.
--
readTBQueueUntil :: MonadSTM m
                 => (a -> STM m Bool) -- ^ a monadic predicate
                 -> StrictTBQueue m a -- ^ queue
                 -> STM m a
readTBQueueUntil p q = do
  a <- readTBQueue q
  b <- p a
  if b
     then return a
     else readTBQueueUntil p q

