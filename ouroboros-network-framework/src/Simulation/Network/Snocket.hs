{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides simulation environment and a snocket implementation
-- suitable for 'IOSim'.
--
-- Though this module is designed for simulation \/ testing, it lives in the
-- library, since it is needed in `ouroboros-network-framework:test` and
-- `ouroboros-network:test' components.
--
module Simulation.Network.Snocket
  (
  -- * Simulated Snocket
    withSnocket
  , ResourceException (..)
  , SnocketTrace (..)
  , SockType (..)
  , OpenType (..)

  , BearerInfo (..)
  , SuccessOrFailure (..)
  , Size
  , noAttenuation
  , FD
  , Script (..)
  , singletonScript
  , SDUSize

  , GlobalAddressScheme (..)
  , AddressType (..)
  ) where

import           Prelude hiding (read)

import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer, contramap, contramapM, traceWith)

import           GHC.IO.Exception

import           Data.Bifoldable (bitraverse_)
import           Data.Foldable (traverse_)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Typeable (Typeable)
import           Numeric.Natural (Natural)
import           Text.Printf (printf)

import           Data.Wedge

import           Network.Mux.Bearer.AttenuatedChannel
import           Network.Mux.Types (MuxBearer, SDUSize (..))
import           Network.Mux.Trace (MuxTrace)

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionManager.Types (AddressType (..))
import           Ouroboros.Network.Snocket


newtype Script a = Script (NonEmpty a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

singletonScript :: a -> Script a
singletonScript x = (Script (x :| []))

initScriptSTM :: MonadSTMTx stm
              => Script a
              -> stm (TVar_ stm (Script a))
initScriptSTM = LazySTM.newTVar

stepScriptSTM :: MonadSTMTx stm
              => TVar_ stm (Script a) -> stm a
stepScriptSTM scriptVar = do
    Script (x :| xs) <- LazySTM.readTVar scriptVar
    case xs of
      []     -> return ()
      x':xs' -> LazySTM.writeTVar scriptVar (Script (x' :| xs'))
    return x


data Connection m = Connection
    { -- | Attenuated channels of a connection.
      --
      connChannelLocal  :: !(AttenuatedChannel m)
    , connChannelRemote :: !(AttenuatedChannel m)

      -- | SDU size of a connection.
      --
    , connSDUSize       :: !SDUSize

      -- | Opening state of a connection.  This is used to detect simultaneous
      -- open.
      --
    , connState         :: !OpenState
    }


data OpenState
    -- | Half opened connection is after calling `connect` but before the other
    -- side picked it. Either using simultaneous open or normal open.
  = HalfOpened

    -- | This corresponds to established state of a tcp connection.
  | Established


dualConnection :: Connection m -> Connection m
dualConnection conn@Connection { connChannelLocal, connChannelRemote } =
    conn { connChannelLocal  = connChannelRemote
         , connChannelRemote = connChannelLocal
         }


mkConnection :: ( MonadLabelledSTM   m
                , MonadTime          m
                , MonadTimer         m
                , MonadThrow         m
                , MonadThrow    (STM m)
                )
             => Tracer m (WithAddr (TestAddress addr)
                                   (SnocketTrace m (TestAddress addr)))
             -> BearerInfo
             -> ConnectionId (TestAddress addr)
             -> STM m (Connection m)
mkConnection tr bearerInfo connId@ConnectionId { localAddress, remoteAddress } = do
    (channelLocal, channelRemote)  <-
      newConnectedAttenuatedChannelPair
        ( ( WithAddr (Just localAddress) (Just remoteAddress)
          . STAttenuatedChannelTrace connId
          )
          `contramap` tr)
        ( ( WithAddr (Just localAddress) (Just remoteAddress)
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
    return $ Connection channelLocal
                        channelRemote
                        (biSDUSize bearerInfo)
                        HalfOpened


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
      nsConnections       :: StrictTVar m (Map (NormalisedId addr) (Connection m)),

      -- | Get an unused ephemeral address.
      --
      nsNextEphemeralAddr :: AddressType -> STM m addr,

      nsBearerInfo        :: LazySTM.TVar m (Script BearerInfo)

    }


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

      -- | SDU size of the bearer; it will be shared between outbound and inbound
      -- sides.
      --
      -- Note: shrinking 'SDUSize' means make it larger, as this allows to send
      -- fewer chunks through the bearer.
      --
    , biSDUSize              :: !SDUSize
    }

instance Show BearerInfo where
    show BearerInfo {biConnectionDelay, biInboundWriteFailure, biOutboundWriteFailure, biSDUSize} =
      concat
        [ "BearerInfo "
        , show biConnectionDelay
        , " ("
        , show biInboundWriteFailure
        , ") ("
        , show biOutboundWriteFailure
        , ") "
        , show biSDUSize
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
                           , biSDUSize              = SDUSize 12228
                           }


-- | Create a new network snocket based on a 'BearerInfo' script.
--
newNetworkState
    :: forall m peerAddr.
       ( MonadLabelledSTM m
       , GlobalAddressScheme peerAddr
       )
    => Script BearerInfo
    -- ^ the largest ephemeral address
    -> m (NetworkState m (TestAddress peerAddr))
newNetworkState bearerInfoScript = atomically $ do
  (v :: StrictTVar m Natural) <- newTVar 0
  let nextEphemeralAddr :: AddressType -> STM m (TestAddress peerAddr)
      nextEphemeralAddr addrType = do
        -- TODO: we should use `(\s -> (succ s, s)` but p2p-master does not
        -- include PR #3172.
         a <- stateTVar v (\s -> let s' = succ s in (s', s'))
         return (ephemeralAddress addrType a)
  s <- NetworkState
    -- nsListeningFDs
    <$> newTVar Map.empty
    -- nsConnections
    <*> newTVar Map.empty
    -- nsNextEphemeralAddr
    <*> pure nextEphemeralAddr
    -- nsBearerInfo
    <*> initScriptSTM bearerInfoScript
  labelTVar (nsListeningFDs s)   "nsListeningFDs"
  labelTVar (nsConnections s)    "nsConnections"
  return s


data ResourceException addr
  = NotReleasedListeningSockets [addr] (Maybe SomeException)
  | NotReleasedConnections      (Map (NormalisedId addr) OpenState)
                                (Maybe SomeException)
  deriving (Show, Typeable)

instance (Typeable addr, Show addr)
      => Exception (ResourceException addr)


-- | A type class for global IP address scheme.  Every node in the simulation
-- has an ephemeral address.  Every node in the simulation has an implicity ipv4
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
       ( MonadCatch       m
       , MonadLabelledSTM m
       , MonadMask        m
       , MonadTime        m
       , MonadTimer       m
       , MonadThrow  (STM m)
       , GlobalAddressScheme peerAddr
       , Ord      peerAddr
       , Typeable peerAddr
       , Show     peerAddr
       )
    => Tracer m (WithAddr (TestAddress peerAddr)
                          (SnocketTrace m (TestAddress peerAddr)))
    -> Script BearerInfo
    -> (Snocket m (FD m (TestAddress peerAddr)) (TestAddress peerAddr)
        -> m a)
    -> m a
withSnocket tr script k = do
    st <- newNetworkState script
    a <- k (mkSnocket st tr)
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
                   -> m (Maybe (ResourceException (TestAddress peerAddr)))
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
    | FDListening
        !addr
        -- ^ listening address

        !(TBQueue m (ChannelWithInfo m addr))
        -- ^ listening queue; when 'connect' is called; dual 'AttenuatedChannel'
        -- of 'FDConnected' file descriptor is passed through the listening
        -- queue.
        --
        -- 'connect' is the producer of this queue;
        -- 'accept' is the consumer.

    -- | 'FD_' was passed to 'connect' call, if needed an ephemeral address was
    -- assigned to it.
    --
    | FDConnecting !(ConnectionId addr)
                   !(Connection m)

    -- | 'FD_' for snockets in connected state.
    --
    -- 'FDConnected' is created by either 'connect' or 'accept'.
    | FDConnected
        !(ConnectionId addr)
        -- ^ local and remote addresses
        !(Connection m)
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
newtype FD m peerAddr = FD { fdVar :: (StrictTVar m (FD_ m peerAddr)) }


--
-- Simulated snockets
--

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

data SnocketTrace m addr
    = STConnect      (FD_ m addr) addr
    | STConnected    (FD_ m addr) OpenType
    | STBearerInfo   BearerInfo
    | STConnectError (FD_ m addr) addr IOError
    | STBindError    (FD_ m addr) addr IOError
    | STClosing      SockType Bool
    | STClosed       SockType
    | STClosingQueue Bool
    | STClosedQueue  Bool
    | STClosedWhenReading
    | STBearer       (FD_ m addr)
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

-- | Simulated 'Snocket' running in 'NetworkState'.  A single 'NetworkState'
-- should be shared with all nodes in the same network.
--
mkSnocket :: forall m addr.
             ( MonadLabelledSTM   m
             , MonadThrow    (STM m)
             , MonadMask          m
             , MonadTime          m
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
                             , toBearer
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
        traceWith' fd (STConnect fd_ remoteAddress)
        case fd_ of
          FDUninitialised mbLocalAddr -> do
            (efd, connId, bearerInfo) <- atomically $ do
              bearerInfo <- stepScriptSTM (nsBearerInfo state)
              localAddress <-
                case mbLocalAddr of
                  Just addr -> return addr
                  Nothing   -> nsNextEphemeralAddr state (getAddressType remoteAddress)
              let connId = ConnectionId { localAddress, remoteAddress }

              conMap <- readTVar (nsConnections state)
              case Map.lookup (normaliseId connId) conMap of
                Just      Connection { connState = Established } ->
                  throwSTM (connectedIOError fd_)
                Just conn@Connection { connState = HalfOpened } -> do
                  let conn' = conn { connState = Established }
                  writeTVar fdVarLocal (FDConnecting connId conn')
                  modifyTVar (nsConnections state)
                             (Map.adjust (const conn')
                                         (normaliseId connId))
                  return ( Left $ FDConnected connId conn'
                         , connId
                         , bearerInfo
                         )
                Nothing -> do
                  conn <- mkConnection tr bearerInfo connId
                  writeTVar fdVarLocal (FDConnecting connId conn)
                  modifyTVar (nsConnections state)
                             (Map.insert (normaliseId connId)
                                         (dualConnection conn))
                  return ( Right ( FDConnected connId conn
                                 , conn
                                 )
                         , connId
                         , bearerInfo
                         )

            traceWith tr (WithAddr (Just (localAddress connId))
                                   (Just remoteAddress)
                                   (STBearerInfo bearerInfo))
            -- connection delay
            threadDelay (biConnectionDelay bearerInfo)

            efd' <- atomically $ do
              lstMap <- readTVar (nsListeningFDs state)
              lstFd  <- traverse (readTVar . fdVar)
                                 (Map.lookup remoteAddress lstMap)
              case (efd, lstFd) of
                -- error cases
                (_, Nothing) ->
                  return (Left (connectIOError connId))
                (_, Just FDUninitialised {}) ->
                  return (Left (connectIOError connId))
                (_, Just FDConnecting {}) ->
                  return (Left (invalidError fd_))
                (_, Just FDConnected {}) ->
                  return (Left (connectIOError connId))
                (_, Just FDClosed {}) ->
                  return (Left notConnectedIOError)

                -- successful simultaneous open
                (Left  fd_', Just FDListening {}) -> do
                  writeTVar fdVarLocal fd_'
                  return (Right (fd_', SimOpen))

                (Right ( fd_'
                       , Connection { connChannelLocal, connChannelRemote })
                       , Just (FDListening _ queue)
                       ) -> do
                  writeTVar fdVarLocal fd_'
                  mConn <- Map.lookup (normaliseId connId) <$> readTVar (nsConnections state)
                  case mConn of
                    Just Connection { connState = Established } ->
                      -- successful simultaneous open
                      return ()
                    Just Connection { connState = HalfOpened } -> do
                      -- successful open
                      modifyTVar (nsConnections state)
                                 (Map.adjust (\s -> s { connState = Established })
                                             (normaliseId connId))
                      writeTBQueue queue
                                   ChannelWithInfo
                                     { cwiAddress       = localAddress connId
                                     , cwiSDUSize       = biSDUSize bearerInfo
                                     , cwiChannelLocal  = connChannelRemote
                                     , cwiChannelRemote = connChannelLocal
                                     }
                    Nothing ->
                      throwSTM (connectIOError connId)

                  return (Right (fd_', NormalOpen))

            case efd' of
              Left e          -> traceWith' fd (STConnectError fd_ remoteAddress e)
                              >> throwIO e
              Right (fd_', o) -> traceWith' fd (STConnected fd_' o)

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

        connectIOError :: ConnectionId (TestAddress addr) -> IOError
        connectIOError connId = IOError
          { ioe_handle      = Nothing
          , ioe_type        = OtherError
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.connect"
          , ioe_description = printf "connect failure (%s)" (show connId)
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
    accept FD { fdVar } = pure accept_
      where
        accept_ = Accept $ atomically $ do
          fd <- readTVar fdVar
          case fd of
            FDUninitialised {} ->
              -- 'berkeleyAccept' used by 'socketSnocket' will return
              -- 'IOException's with 'AcceptFailure', we match this behaviour
              -- here.
              return ( AcceptFailure (toException $ invalidError fd)
                     , accept_
                     )
            FDConnecting {} ->
              return ( AcceptFailure (toException $ invalidError fd)
                     , accept_
                     )
            FDConnected {} ->
              return ( AcceptFailure (toException $ invalidError fd)
                     , accept_
                     )
            FDListening localAddress queue -> do
              ChannelWithInfo
                { cwiAddress       = remoteAddress
                , cwiSDUSize       = sduSize
                , cwiChannelLocal  = channelLocal
                , cwiChannelRemote = channelRemote
                } <- readTBQueue queue
              fdRemote <- FD <$> newTVar (FDConnected
                                            ConnectionId
                                              { localAddress
                                              , remoteAddress
                                              }
                                            Connection
                                              { connChannelLocal  = channelLocal
                                              , connChannelRemote = channelRemote
                                              , connSDUSize       = sduSize
                                              , connState         = Established
                                              })
              return ( Accepted fdRemote remoteAddress
                     , accept_
                     )
            FDClosed {} ->
              return ( AcceptFailure (toException $ invalidError fd)
                     , accept_
                     )

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
        (wConnId, fdType, mq) <- atomically $ do
          fd_ <- readTVar fdVar
          let wConnId = case fd_ of
                     FDUninitialised {}    -> Nowhere
                     FDConnecting connId _ -> Here connId
                     FDConnected connId _  -> Here connId
                     FDListening addr _    -> There addr
                     FDClosed    wConnId_  -> wConnId_
              mq = case fd_ of
                     FDConnecting _ conn -> Just (connChannelLocal conn)
                     FDConnected  _ conn -> Just (connChannelLocal conn)
                     _                   -> Nothing

          writeTVar fdVar (FDClosed wConnId)
          -- TODO: We should move this removal after closing the attenuated channel!
          bitraverse_
            -- close a connected socket
            (\connId -> modifyTVar (nsConnections state)
                                   (Map.delete (normaliseId connId)))
            -- close a listening socket; For Berkeley sockets, accepted
            -- connections are not disturbed when one closes the listening
            -- socket.
            (\addr   -> modifyTVar (nsListeningFDs state)
                                   (Map.delete addr))
            wConnId
          return (wConnId, mkSockType fd_, mq)

        let mbLocalAddr = case wConnId of
              Here connId -> Just (localAddress connId)
              There addr  -> Just addr
              Nowhere     -> Nothing
            mbRemoteAddr = case wConnId of
              Here connId -> Just (remoteAddress connId)
              _           -> Nothing

        traceWith tr (WithAddr mbLocalAddr mbRemoteAddr
                               (STClosing fdType (isJust mq)))
        case mq of
          Nothing -> return ()
          Just q  -> acClose q
        traceWith tr (WithAddr mbLocalAddr mbRemoteAddr
                               (STClosed fdType))


    toBearer :: DiffTime
             -> Tracer m MuxTrace
             -> FD m (TestAddress addr)
             -> m (MuxBearer m)
    toBearer sduTimeout muxTracer fd@FD { fdVar } = do
        fd_ <- atomically (readTVar fdVar)
        case fd_ of
          FDUninitialised {} ->
            throwIO (invalidError fd_)
          FDListening {} ->
            throwIO (invalidError fd_)
          FDConnecting _ _ -> do
            throwIO (invalidError fd_)
          FDConnected _ conn -> do
            traceWith' fd (STBearer fd_)
            return $ attenuationChannelAsMuxBearer (connSDUSize conn)
                                                   sduTimeout muxTracer
                                                   (connChannelLocal conn)
          FDClosed {} ->
            throwIO (invalidError fd_)
      where
        -- io errors
        invalidError :: FD_ m (TestAddress addr) -> IOError
        invalidError fd_ = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.toBearer"
          , ioe_description = printf "Invalid argument (%s)" (show fd_)
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }


hush :: Either a b -> Maybe b
hush Left {}   = Nothing
hush (Right a) = Just a
{-# INLINE hush #-}
