{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
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

  , BearerInfo (..)
  , SuccessOrFailure (..)
  , Size
  , noAttenuation
  , FD
  , Script (..)
  , singletonScript
  , SDUSize
  ) where

import           Prelude hiding (read)

import           Control.Exception (assert)
import           Control.Monad (when)
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
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Numeric.Natural (Natural)

import           Data.Wedge

import           Network.Mux.Bearer.AttenuatedChannel
import           Network.Mux.Types (MuxBearer, SDUSize (..))
import           Network.Mux.Trace (MuxTrace)

import           Ouroboros.Network.ConnectionId
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

stepScript :: MonadSTM m => LazySTM.TVar m (Script a) -> m a
stepScript = atomically . stepScriptSTM


data Connection m = Connection
    { -- | Attenuated channels of a connection.
      --
      connChannelLocal  :: !(AttenuatedChannel m)
    , connChannelRemote :: !(AttenuatedChannel m)

      -- | SDU size of a connection.
      --
    , connSDUSize       :: !SDUSize
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
      nsConnections       :: StrictTVar m (Map (NormalisedId addr) (Connection m)),

      -- | Set of all used addresses.
      --
      nsBoundAddresses    :: StrictTVar m (Set addr),

      -- | Get an unused ephemeral address.
      --
      nsNextEphemeralAddr :: STM m addr,

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
       ( MonadSTM m
       , Enum     peerAddr
       )
    => Script BearerInfo
    -> peerAddr
    -- ^ the largest ephemeral address
    -> m (NetworkState m (TestAddress peerAddr))
newNetworkState bearerInfoScript peerAddr = atomically $
  NetworkState
    -- nsListeningFDs
    <$> newTVar Map.empty
    -- nsConnections
    <*> newTVar Map.empty
    -- nsBoundAddresses
    <*> newTVar Set.empty
    -- nsNextEphemeralAddr
    <*> do (v :: StrictTVar m peerAddr) <- newTVar peerAddr
           return $ do
             a <- readTVar v
             writeTVar v (pred a)
             return (TestAddress a)
    -- nsBearerInfo
    <*> initScriptSTM bearerInfoScript


data ResourceException addr
  = NotReleasedListeningSockets [addr]              (Maybe SomeException)
  | NotReleasedConnections      [NormalisedId addr] (Maybe SomeException)
  deriving (Show, Typeable)

instance (Typeable addr, Show addr)
      => Exception (ResourceException addr)


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
       , Enum     peerAddr
       , Ord      peerAddr
       , Typeable peerAddr
       , Show     peerAddr
       )
    => Tracer m (WithAddr (TestAddress peerAddr)
                          (SnocketTrace m (TestAddress peerAddr)))
    -> Script BearerInfo
    -> TestAddress peerAddr
    -- ^ the largest ephemeral address
    -> (Snocket m (FD m (TestAddress peerAddr)) (TestAddress peerAddr)
        -> m a)
    -> m a
withSnocket tr script (TestAddress peerAddr) k = do
    st <- newNetworkState script peerAddr
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
         -> return $ Just (NotReleasedConnections      (Map.keys connMap) err)

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
        !(Maybe (ConnectionId addr))


instance Show addr => Show (FD_ m addr) where
    show (FDUninitialised mbAddr)  = "FDUninitialised " ++ show mbAddr
    show (FDListening addr _)      = "FDListening " ++ show addr
    show (FDConnected connId conn) = concat
                                       [ "FDConnected "
                                       , show connId
                                       , " "
                                       , show (connSDUSize conn)
                                       ]
    show (FDClosed mbConnId)       = "FDClosed " ++ show mbConnId


-- | File descriptor type.
--
newtype FD m peerAddr = FD { fdVar :: (StrictTVar m (FD_ m peerAddr)) }


--
-- Simulated snockets
--

data WithAddr addr event =
    WithAddr { waLocalAddr :: Maybe addr
             , waEvent     :: event
             }
  deriving Show


data SnocketTrace m addr
    = STConnect      (FD_ m addr) addr
    | STConnected    (FD_ m addr)
    | STConnectError (FD_ m addr) addr IOError
    | STBindError    (FD_ m addr) addr IOError
    | STClosing      (FD_ m addr) Bool
    | STClosed       (FD_ m addr)
    | STClosingQueue Bool
    | STClosedQueue  Bool
    | STClosedWhenReading
    | STBearer       (FD_ m addr)
    | STAttenuatedChannelTrace (ConnectionId addr) AttenuatedChannelTrace
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
             , Ord addr
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
    getLocalAddrM :: FD m (TestAddress addr) -> m (Maybe (TestAddress addr))
    getLocalAddrM FD { fdVar } = do
        fd_ <- atomically (readTVar fdVar)
        return $ case fd_ of
          FDUninitialised Nothing         -> Nothing
          FDUninitialised (Just peerAddr) -> Just peerAddr
          FDListening peerAddr _          -> Just peerAddr
          FDConnected ConnectionId { localAddress } _
                                          -> Just localAddress
          FDClosed {}                     -> Nothing

    traceWith' :: FD m (TestAddress addr)
               -> SnocketTrace m (TestAddress addr)
               -> m ()
    traceWith' fd =
      let tr' :: Tracer m (SnocketTrace m (TestAddress addr))
          tr' = (\ev -> flip WithAddr ev <$> getLocalAddrM fd)
                `contramapM` tr
      in traceWith tr'

    getLocalAddr :: FD m (TestAddress addr) -> m (TestAddress addr)
    getLocalAddr fd = do
        maddr <- getLocalAddrM fd
        case maddr of
          Just addr -> return addr
          -- Socket would not error for an @FDUninitialised Nothing@; it would
          -- return '0.0.0.0:0'.
          Nothing   -> throwIO ioe
      where
        ioe = IOError
                { ioe_handle      = Nothing
                , ioe_type        = InvalidArgument
                , ioe_location    = "Ouroboros.Network.Snocket.Sim.getLocalAddr"
                , ioe_description = "Transport endpoint is not connected"
                , ioe_errno       = Nothing
                , ioe_filename    = Nothing
                }

    getRemoteAddr :: FD m (TestAddress addr) -> m (TestAddress addr)
    getRemoteAddr FD { fdVar } = do
        fd_ <- atomically (readTVar fdVar)
        case fd_ of
          FDUninitialised {}         -> throwIO ioe
          FDListening {}             -> throwIO ioe
          FDConnected ConnectionId { remoteAddress } _
                                     -> return remoteAddress
          FDClosed {}                -> throwIO ioe
      where
        ioe = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.getRemoteAddr"
          , ioe_description = "Transport endpoint is not connected"
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
            bearerInfo <- stepScript (nsBearerInfo state)
            threadDelay (biConnectionDelay bearerInfo)
            res <- atomically $ do
              localAddress <-
                case mbLocalAddr of
                  Just addr -> return addr
                  Nothing   -> nsNextEphemeralAddr state
              writeTVar fdVarLocal (FDUninitialised (Just localAddress))
              lstMap <- readTVar (nsListeningFDs state)
              conMap <- readTVar (nsConnections state)
              lstFd  <- traverse (readTVar . fdVar) (Map.lookup remoteAddress lstMap)
              let connId = ConnectionId {localAddress, remoteAddress}
              case (Map.lookup (normaliseId connId) conMap, lstFd) of

                (Just _, _) ->
                  -- connection exists
                  throwSTM connectedIOError

                (Nothing, Nothing) ->
                  throwSTM connectIOError
                (Nothing, Just FDUninitialised {}) ->
                  return (Left connectIOError)
                (Nothing, Just FDConnected {}) ->
                  return (Left connectIOError)
                (Nothing, Just (FDListening remoteAddress' queue)) ->
                  assert (remoteAddress' == remoteAddress) $ do
                  (channelLocal, channelRemote)  <-
                    newConnectedAttenuatedChannelPair
                      ( ( WithAddr (Just localAddress)
                        . STAttenuatedChannelTrace connId
                        )
                        `contramap` tr)
                      ( ( WithAddr (Just localAddress)
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
                  let conn = Connection channelLocal
                                        channelRemote
                                        (biSDUSize bearerInfo)
                  let fd' = FDConnected connId conn
                  writeTVar fdVarLocal fd'
                  modifyTVar (nsConnections state) (Map.insert (normaliseId connId) conn)
                  writeTBQueue queue
                               ChannelWithInfo
                                 { cwiAddress       = localAddress
                                 , cwiSDUSize       = biSDUSize bearerInfo
                                 , cwiChannelLocal  = channelRemote
                                 , cwiChannelRemote = channelLocal
                                 }
                  return (Right fd')
                (Nothing, Just FDClosed {}) ->
                  return (Left notConnectedIOError)
            case res of
              Left e    -> traceWith' fd (STConnectError fd_ remoteAddress e)
                        >> throwIO e
              Right fd' -> traceWith' fd (STConnected fd') 

          FDConnected {} ->
            throwIO connectedIOError

          FDListening {} ->
            throwIO connectedIOError

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

        connectIOError = IOError
          { ioe_handle      = Nothing
          , ioe_type        = OtherError
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.connect"
          , ioe_description = "connect failure"
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }

        connectedIOError = IOError
          { ioe_handle      = Nothing
          , ioe_type        = AlreadyExists
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.connect"
          , ioe_description = "Transport endpoint is already connected"
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }


    bind :: FD m (TestAddress addr) -> TestAddress addr -> m ()
    bind fd@FD { fdVar } addr = do
        res <- atomically $ do
          boundSet <- readTVar (nsBoundAddresses state)
          when (addr `Set.member` boundSet)
               (throwSTM addressInUseError)
          fd_ <- readTVar fdVar
          case fd_ of
            FDUninitialised Nothing -> do
              writeTVar fdVar (FDUninitialised (Just addr))
              return Nothing
            _ ->
              return (Just (fd_, invalidError))
        case res of
          Nothing       -> return ()
          Just (fd_, e) -> traceWith' fd (STBindError fd_ addr e)
                        >> throwIO e
      where
        invalidError = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.bind"
          , ioe_description = "Invalid argument"
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }

        addressInUseError = IOError
          { ioe_handle      = Nothing
          , ioe_type        = ResourceBusy
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.bind"
          , ioe_description = "Address already in use"
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }


    listen :: FD m (TestAddress addr) -> m ()
    listen fd@FD { fdVar } = atomically $ do
        fd_ <- readTVar fdVar
        case fd_ of
          FDUninitialised Nothing ->
            -- Berkeley socket would not error; but then 'bind' would fail;
            throwSTM invalidError

          FDUninitialised (Just addr) -> do
            queue <- newTBQueue bound
            writeTVar fdVar (FDListening addr queue)
            modifyTVar (nsListeningFDs state) (Map.insert addr fd)
            
          FDConnected {} ->
            throwSTM invalidError
          FDListening {} ->
            return ()
          FDClosed {} ->
            throwSTM invalidError
      where
        -- TODO: 'listen' should take this as an explicit argument
        bound :: Natural
        bound = 10

        invalidError = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.listen"
          , ioe_description = "Invalid argument"
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
              return ( AcceptFailure (toException invalidError)
                     , accept_
                     )
            FDConnected {} ->
              return ( AcceptFailure (toException invalidError)
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
                                              })
              return ( Accepted fdRemote remoteAddress
                     , accept_
                     )
            FDClosed {} ->
              return ( AcceptFailure (toException invalidError)
                     , accept_
                     )

        invalidError = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument

          , ioe_location    = "Ouroboros.Network.Snocket.Sim.accept"
          , ioe_description = "Invalid argument"
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }


    close :: FD m (TestAddress addr)
          -> m ()
    close fd@FD { fdVar } =
      uninterruptibleMask_ $ do
        (fd', mq) <- atomically $ do
          fd_ <- readTVar fdVar
          let (wConnId, fd') = case fd_ of
                     FDUninitialised {}   -> (Nowhere,     FDClosed Nothing)
                     FDConnected connId _ -> (Here connId, FDClosed (Just connId))
                     FDListening addr _   -> (There addr,  FDClosed Nothing)
                     FDClosed    connId   -> (Nowhere,     FDClosed connId)
              mq = case fd_ of
                     FDConnected _ conn -> Just (connChannelLocal conn)
                     _                  -> Nothing

          writeTVar fdVar fd'
          bitraverse_
            -- close a connection
            (\connId -> modifyTVar (nsConnections state)
                                   (Map.delete (normaliseId connId)))

            -- close listening connection; For berkely sockets, accepted
            -- connections are not disturbed when one closes the listening
            -- socket, but we exlude that through an assertion.
            (\addr   -> modifyTVar (nsListeningFDs state)
                                   (Map.delete addr))
            wConnId

          return (fd', mq)
        traceWith' fd (STClosing fd' (isJust mq))
        case mq of
          Nothing -> return ()
          Just q  -> acClose q
        traceWith' fd (STClosed fd')


    toBearer :: DiffTime
             -> Tracer m MuxTrace
             -> FD m (TestAddress addr)
             -> m (MuxBearer m)
    toBearer sduTimeout muxTracer fd@FD { fdVar } = do
        fd_ <- atomically (readTVar fdVar)
        case fd_ of
          FDUninitialised {} ->
            throwIO (invalidError "uninitialised file descriptor")
          FDListening {} ->
            throwIO (invalidError "listening snocket")
          FDConnected _ conn -> do
            traceWith' fd (STBearer fd_)
            return $ attenuationChannelAsMuxBearer (connSDUSize conn)
                                                   sduTimeout muxTracer
                                                   (connChannelLocal conn)
          FDClosed {} ->
            throwIO (invalidError "closed snocket")
      where
        -- io errors
        invalidError desc = IOError
          { ioe_handle      = Nothing
          , ioe_type        = InvalidArgument
          , ioe_location    = "Ouroboros.Network.Snocket.Sim.toBearer"
          , ioe_description = "Invalid argument: " ++ desc
          , ioe_errno       = Nothing
          , ioe_filename    = Nothing
          }
