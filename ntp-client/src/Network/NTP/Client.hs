{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements functionality of NTP client.

module Network.NTP.Client
    ( NtpClientSettings (..)
    , NtpClient (..)
    , withNtpClient
    ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, concurrently_, race, race_)
import           Control.Concurrent.STM
import           Control.Exception (Exception, IOException, bracket, catch, handle)
import           Control.Monad (forever, when)
import           Control.Tracer
import           Data.Binary (decodeOrFail)
import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (catMaybes)
import           Data.Semigroup (Last (..))
import           Data.These (These (..))
import           Data.Time.Units (Microsecond)
import           Data.Typeable (Typeable)
import qualified Network.Socket as Socket
import           Network.Socket.ByteString (recvFrom)

import           Network.NTP.Packet (NtpOffset (..) , NtpPacket (..), clockOffset,
                     mkNtpPacket, ntpPacketSize)
import           Network.NTP.Trace (NtpTrace (..))
import           Network.NTP.Util (AddrFamily (..), Addresses, Sockets,
                     WithAddrFamily (..), createAndBindSock,
                     resolveNtpHost,
                     runWithAddrFamily, sendPacket, udpLocalAddresses)

data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ list of servers addresses
    , ntpResponseTimeout :: Microsecond
      -- ^ delay between making requests and response collection
    , ntpPollDelay       :: Microsecond
      -- ^ how long to wait between to send requests to the servers
    , ntpSelection       :: NonEmpty NtpOffset -> NtpOffset
      -- ^ way to sumarize results received from different servers.
      -- this may accept list of lesser size than @length ntpServers@ in case
      -- some servers failed to respond in time, but never an empty list
    }

data NtpClient = NtpClient
    { -- | Query the current NTP status.
      ntpGetStatus        :: STM NtpStatus

      -- | Bypass all internal threadDelays and trigger a new NTP query.
    , ntpTriggerUpdate    :: IO ()
    }

data NtpStatus =
      -- | The difference between NTP time and local system time
      NtpDrift NtpOffset
      -- | NTP client has send requests to the servers
    | NtpSyncPending
      -- | NTP is not available: the client has not received any respond within
      -- `ntpResponseTimeout` or NTP was not configured.
    | NtpSyncUnavailable deriving (Eq, Show)

-- | Setup a NtpClient and run a computation that uses that client.
-- Todo : proper bracket-style tear-down of the NTP client.
withNtpClient :: Tracer IO NtpTrace -> NtpClientSettings -> (NtpClient -> IO a) -> IO a
withNtpClient tracer ntpSettings action
   = forkNtpClient tracer ntpSettings >>= action

-- This function should be called once, it will run an NTP client in a new
-- thread until the program terminates.
forkNtpClient :: Tracer IO NtpTrace -> NtpClientSettings -> IO NtpClient
forkNtpClient tracer ntpSettings = do
    traceWith tracer NtpTraceStartNtpClient
    ncStatus <- newTVarIO NtpSyncPending
    -- using async so the NTP thread will be left running even if the parent
    -- thread finished.
    _ <- async (spawnNtpClient tracer ntpSettings ncStatus)
    return $ NtpClient
        { ntpGetStatus = readTVar ncStatus
        , ntpTriggerUpdate = do
            traceWith tracer NtpTraceClientActNow
            atomically $ writeTVar ncStatus NtpSyncPending
        }

data NtpState = NtpState
    { ncSockets  :: TVar Sockets
      -- ^ Ntp client sockets: ipv4 / ipv6 / both.
    , ncState    :: TVar [NtpOffset]
      -- ^ List of ntp offsets and origin times (i.e. time when a request was
      -- send) received from ntp servers since last polling interval.
    , ncStatus   :: TVar NtpStatus
      -- ^ Ntp status: holds `NtpOffset` or a status of ntp client:
      -- `NtpSyncPending`, `NtpSyncUnavailable`.  It is computed from `ncState`
      -- once all responses arrived.
    , ncSettings :: NtpClientSettings
      -- ^ Ntp client configuration.
    }

mkNtpClient :: NtpClientSettings -> TVar NtpStatus -> Sockets -> IO NtpState
mkNtpClient ncSettings ncStatus sock = do
    ncSockets <- newTVarIO sock
    ncState   <- newTVarIO []
    return NtpState{..}

data NoHostResolved = NoHostResolved
    deriving (Show, Typeable)

instance Exception NoHostResolved

updateStatus :: Tracer IO NtpTrace -> NtpState -> IO ()
updateStatus tracer cli = do
    offsets <- readTVarIO (ncState cli)
    status <- case offsets of
        [] -> do
           traceWith tracer NtpTraceUpdateStatusNoResponses
           return NtpSyncUnavailable
        l -> do
           let o = ntpSelection (ncSettings cli) $ NE.fromList l
           traceWith tracer $ NtpTraceUpdateStatusClockOffset $ getNtpOffset o
           return $ NtpDrift o
    atomically $ writeTVar (ncStatus cli) status

-- Every `ntpPollDelay` we send a request to the list of `ntpServers`.  Before
-- sending a request, we put `NtpSyncPending` to `ncState`.  After sending
-- all requests we wait until either all servers responded or
-- `ntpResponseTimeout` passesed.  If at least one server responded
-- `handleCollectedResponses` will update `ncStatus` in `NtpClient` with a new
-- drift.
sendLoop :: Tracer IO NtpTrace -> NtpState -> [Addresses] -> IO ()
sendLoop tracer cli addrs = forever $ do
    let respTimeout = ntpResponseTimeout (ncSettings cli)
    let poll        = ntpPollDelay (ncSettings cli)

    -- send packets and wait until end of poll delay
    sock <- readTVarIO $ ncSockets cli
    pack <- mkNtpPacket
    sendPacket tracer sock pack addrs

    _ <- timeout respTimeout waitForResponses
    updateStatus tracer cli
    -- after @'updateStatus'@ @'ntpStatus'@ is guaranteed to be
    -- different from @'NtpSyncPending'@, now we can wait until it was
    -- changed back to @'NtpSyncPending'@ to force a request.
    _ <- timeout poll waitForRequest

    -- reset state & status before next loop
    atomically $ writeTVar (ncState cli) []
    atomically $ writeTVar (ncStatus cli) NtpSyncPending

    where
        waitForResponses = do
            atomically $ do
                resps <- readTVar $ ncState cli
                let svs = length $ ntpServers $ ncSettings cli
                when (length resps < svs)
                    retry
            traceWith tracer NtpTraceSendLoopCollectedAllResponses

        -- Wait for a request to force an ntp check.
        waitForRequest =
            atomically $ do
                status <- readTVar $ ncStatus cli
                check (status == NtpSyncPending)
                return ()

        timeout :: Microsecond -> IO a -> IO (Either () a)
        timeout t io = race (threadDelay (fromIntegral t)) io

-- |
-- Listen for responses on the socket @'ncSockets'@
receiveLoop :: Tracer IO NtpTrace -> NtpState -> IO ()
receiveLoop tracer cli =
    readTVarIO (ncSockets cli) >>= \case
        These (Last (WithIPv6 sock_ipv6)) (Last (WithIPv4 sock_ipv4)) ->
            loop IPv6 sock_ipv6
            `concurrently_`
            loop IPv4 sock_ipv4
        This (Last (WithIPv6 sock_ipv6)) ->
            loop IPv6 sock_ipv6
        That (Last (WithIPv4 sock_ipv4)) ->
            loop IPv4 sock_ipv4
    where
    -- Receive responses from the network and update NTP client state.
    loop :: AddrFamily -> Socket.Socket -> IO ()
    loop addressFamily sock
        = handle (handleIOException addressFamily) $ forever $ do
            (bs, _) <- recvFrom sock ntpPacketSize
            case decodeOrFail $ LBS.fromStrict bs of
                Left  (_, _, err)    ->
                    traceWith tracer $ NtpTraceReceiveLoopDecodeError err
                Right (_, _, packet) ->
                    handleNtpPacket packet

    -- Restart the @loop@ in case of errors; wait 5s before recreating the
    -- socket.
    handleIOException
        :: AddrFamily
        -> IOException
        -> IO ()
    handleIOException addressFamily e = do
        traceWith tracer $ NtpTraceReceiveLoopHandleIOException e
        threadDelay 5000000
        udpLocalAddresses >>= createAndBindSock tracer addressFamily >>= \case
            Nothing   -> do
                traceWith tracer NtpTraceReceiveLoopException
--                logWarning "recreating of sockets failed (retrying)"
                handleIOException addressFamily e
            Just sock -> do
                atomically $ modifyTVar' (ncSockets cli) (\s -> s <> sock)
                case sock of
                    This (Last sock_)
                        -> loop addressFamily $ runWithAddrFamily sock_
                    That (Last sock_)
                        -> loop addressFamily $ runWithAddrFamily sock_
                    These _ _
                        -> error "NtpClient: startReceive: impossible"

    -- Compute the clock offset based on current time and record it in the NTP
    -- client state.   A packet will be disgarded if it came after
    -- @'ntpResponseTimeout'@.
    handleNtpPacket
        :: NtpPacket
        -> IO ()
    handleNtpPacket packet = do
        traceWith tracer NtpTraceReceiveLoopPacketReceived -- packet
        clockOffset (ntpResponseTimeout $ ncSettings cli) packet >>= \case
            Nothing -> traceWith tracer NtpTraceReceiveLoopLatePacket
            Just offset -> do
                traceWith tracer $ NtpTraceReceiveLoopPacketDeltaTime $ getNtpOffset offset
                atomically $ modifyTVar' (ncState cli) ( offset : )

-- |
-- Spawn NTP client which will send request to NTP servers every @'ntpPollDelay'@
-- and will listen for responses.  The @'ncStatus'@ will be updated every
-- @'ntpPollDelay'@ with the most recent value.  It should be run in a separate
-- thread, since it will block infinitely.
spawnNtpClient :: Tracer IO NtpTrace -> NtpClientSettings -> TVar NtpStatus -> IO ()
spawnNtpClient tracer settings ncStatus = do
    traceWith tracer NtpTraceSpawnNtpClientStarting
    bracket (mkSockets tracer settings) closeSockets $ \sock -> do
        cli <- mkNtpClient settings ncStatus sock

        addrs <- resolve
        -- TODO
        -- we should start listening for requests when we send something, since
        -- we're not expecting anything to come unless we send something.  This
        -- way we could simplify the client and remove `ncState` mutable cell.
        receiveLoop tracer cli
            `concurrently_` sendLoop tracer cli addrs
            `concurrently_` traceWith tracer NtpTraceSpawnNtpClientStarted

    where
    closeSockets :: Sockets -> IO ()
    closeSockets sockets = do
        case sockets of
            This a -> fn a
            That a -> fn a
            These a b -> fn a >> fn b
        traceWith tracer NtpTraceSpawnNtpClientSocketsClosed

    fn :: Last (WithAddrFamily t Socket.Socket) -> IO ()
    fn (Last sock) = Socket.close $ runWithAddrFamily sock

    -- Try to resolve addresses, on failure wait 30s and start again.
    resolve = do
        traceWith tracer NtpTraceSpawnNtpClientResolveDNS
        addrs <- catMaybes <$> traverse (resolveNtpHost tracer) (ntpServers settings)
        if null addrs
          then do
            atomically $ writeTVar ncStatus NtpSyncUnavailable
            -- either wait 30s or wait for `NtpSyncPending` which might be set
            -- by a client (e.g. wallet), in which case try to resolve the dns.
            race_
              (threadDelay 30000000)
              (do
                atomically $ do
                  s <- readTVar ncStatus
                  case s of
                    NtpSyncPending -> return ()
                    _              -> retry
                traceWith tracer NtpTraceSpawnNtpClientResolvePending
              )
            resolve
          else return addrs

-- Try to create IPv4 and IPv6 socket.
mkSockets :: Tracer IO NtpTrace -> NtpClientSettings -> IO Sockets
mkSockets tracer settings =
    doMkSockets `catch` handleIOException >>= \case
        Just socks -> pure socks
        Nothing     -> do
            traceWith tracer NtpTraceMkSocketsNoSockets
--            logWarning "Couldn't create either IPv4 or IPv6 socket, retrying in 5 sec..."
            threadDelay 5000000
            mkSockets tracer settings
  where
    doMkSockets :: IO (Maybe Sockets)
    doMkSockets = do
        addrs <- udpLocalAddresses
        (<>) <$> (createAndBindSock tracer IPv4 addrs)
             <*> (createAndBindSock tracer IPv6 addrs)

    handleIOException :: IOException -> IO (Maybe Sockets)
    handleIOException e = do
        traceWith tracer $ NtpTraceMkSocketsIOExecption e
        threadDelay 5000000
        doMkSockets
