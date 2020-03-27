-- TODO: provide a corrss platform  network bindings using `network` or
-- `Win32-network`, to get rid of CPP.
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.NTP.Client.Query (
    NtpSettings(..)
  , NtpStatus(..)
  , ntpQuery
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (IOException, bracket, catch)
import           System.IO.Error (userError, ioError)
import           Control.Monad (forM, forM_, replicateM_)
import           Control.Tracer
import           Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void)
import           Data.List (find)
import           Data.Maybe
import           Network.Socket (Socket, SockAddr (..), AddrInfo (..))
import qualified Network.Socket as Socket
#if !defined(mingw32_HOST_OS)
import qualified Network.Socket.ByteString as Socket.ByteString (recvFrom, sendManyTo)
#else
import qualified System.Win32.Async.Socket.ByteString as Win32.Async
#endif
import           System.IOManager
import           Network.NTP.Client.Packet ( mkNtpPacket
                                    , ntpPacketSize
                                    , Microsecond
                                    , NtpOffset (..)
                                    , getCurrentTime
                                    , clockOffsetPure
                                    , mkResultOrFailure
                                    , IPVersion (..)
                                    )
import           Network.NTP.Client.Trace (NtpTrace (..))

data NtpSettings = NtpSettings
    { ntpServers         :: [String]
      -- ^ List of server addresses. At least three servers are needed.
    , ntpResponseTimeout :: Microsecond
      -- ^ Timeout between sending NTP requests and response collection.
    , ntpPollDelay       :: Microsecond
      -- ^ How long to wait between two rounds of requests.
    }

data NtpStatus =
      -- | The difference between NTP time and local system time
      NtpDrift !NtpOffset
      -- | NTP client has send requests to the servers
    | NtpSyncPending
      -- | NTP is not available: the client has not received any respond within
      -- `ntpResponseTimeout` or NTP was not configured.
    | NtpSyncUnavailable deriving (Eq, Show)


-- | Wait for at least three replies and report the minimum of the reported
-- offsets.
--
minimumOfThree :: [NtpOffset] -> Maybe NtpOffset
minimumOfThree l
    = if length l >= 3
        then Just $ minimum l
        else Nothing


-- | Get a list local udp addresses.
--
udpLocalAddresses :: IO [AddrInfo]
udpLocalAddresses = Socket.getAddrInfo (Just hints) Nothing (Just $ show port)
  where
    hints = Socket.defaultHints
          { addrFlags = [Socket.AI_PASSIVE]
          , addrSocketType = Socket.Datagram
          }
    port = Socket.defaultPort

-- | Resolve hostname into 'AddrInfo'.  We use 'Socket.AI_ADDRCONFIG' so we get IPv4/6
-- address only if the local.  We don't need 'AI_V4MAPPED' which would be set
-- by default.
--
resolveHost :: String -> IO [AddrInfo]
resolveHost host = Socket.getAddrInfo (Just hints) (Just host) Nothing
  where
  -- The library uses 'Socket.AI_ADDRCONFIG' as simple test if IPv4 or IPv6 are configured.
  -- According to the documentation, 'Socket.AI_ADDRCONFIG' is not available on all platforms,
  -- but it is expected to work on win32, Mac OS X and Linux.
  -- TODO: use addrInfoFlagImplemented :: AddrInfoFlag -> Bool to test if the flag is available.
    hints = Socket.defaultHints
            { addrSocketType = Socket.Datagram
            , addrFlags = [Socket.AI_ADDRCONFIG]
            }

firstAddr :: Socket.Family -> [AddrInfo] -> Maybe AddrInfo
firstAddr family l = find ((==) family . addrFamily ) l

setNtpPort :: SockAddr ->  SockAddr
setNtpPort addr = case addr of
    (SockAddrInet  _ host)            -> SockAddrInet  ntpPort host
    (SockAddrInet6 _ flow host scope) -> SockAddrInet6 ntpPort flow host scope
    sockAddr                          -> sockAddr
  where
    ntpPort :: Socket.PortNumber
    ntpPort = 123


-- | Resolve dns names
--
lookupServers :: Tracer IO NtpTrace -> [String] -> IO ([AddrInfo], [AddrInfo])
lookupServers tracer names = do
    dests <- forM names $ \server -> do
        addr <- resolveHost server
        case (firstAddr Socket.AF_INET addr, firstAddr Socket.AF_INET6 addr) of
            (Nothing, Nothing) -> do
                traceWith tracer $ NtpTraceLookupServerFailed server
                ioError $ userError $ "lookup NTP server failed " ++ server
            l -> return l
    return (mapMaybe fst dests, mapMaybe snd dests)

-- | Perform a series of NTP queries: one for each dns name.  Resolve each dns
-- name, get local addresses: both IPv4 and IPv6 and engage in ntp protocol
-- towards one ip address per address family per dns name, but only for address
-- families for which we have a local address.  This is to avoid trying to send
-- IPv4/6 requests if IPv4/6 gateway is not configured.
--
-- It may throw an `IOException`:
--
-- * if neither IPv4 nor IPv6 address is configured
-- * if network I/O errors 
--
ntpQuery
    :: IOManager
    -> Tracer IO NtpTrace
    -> NtpSettings
    -> IO NtpStatus
ntpQuery ioManager tracer ntpSettings = do
    traceWith tracer NtpTraceClientStartQuery
    (v4Servers,   v6Servers)   <- lookupServers tracer $ ntpServers ntpSettings
    localAddrs <- udpLocalAddresses
    (v4LocalAddr, v6LocalAddr) <- case (firstAddr Socket.AF_INET localAddrs, firstAddr Socket.AF_INET6 localAddrs) of
        (Nothing, Nothing) -> do
            traceWith tracer NtpTraceNoLocalAddr
            ioError $ userError "no local address IPv4 and IPv6"
        l -> return l
    withAsync (runProtocol IPv4 v4LocalAddr v4Servers) $ \ipv4Async ->
      withAsync (runProtocol IPv6 v6LocalAddr v6Servers) $ \ipv6Async -> do
        results <- mkResultOrFailure
                    <$> waitCatch ipv4Async
                    <*> waitCatch ipv6Async
        traceWith tracer (NtpTraceRunProtocolResults results)
        handleResults (foldMap id results)
  where
    runProtocol :: IPVersion -> Maybe AddrInfo -> [AddrInfo] -> IO [NtpOffset]
    -- no addresses to sent to
    runProtocol _protocol _localAddr  []      = return []
    -- local address is not configured, e.g. no IPv6 or IPv6 gateway.
    runProtocol _protocol Nothing     _       = return []
    -- local address is configured, remote address list is non empty
    runProtocol protocol  (Just addr) servers = do
       runNtpQueries ioManager tracer protocol ntpSettings addr servers

    handleResults :: [NtpOffset] -> IO NtpStatus
    handleResults [] = pure NtpSyncUnavailable
    handleResults results = case minimumOfThree results of
      Nothing -> do
          traceWith tracer NtpTraceReportPolicyQueryFailed
          return NtpSyncUnavailable
      Just offset -> do
          traceWith tracer $ NtpTraceQueryResult $ getNtpOffset offset
          return $ NtpDrift offset


-- | Run an ntp query towards each address
--
runNtpQueries
    :: IOManager
    -> Tracer IO NtpTrace
    -> IPVersion   -- ^ address family, it must afree with local and remote
                   -- addresses
    -> NtpSettings
    -> AddrInfo    -- ^ local address
    -> [AddrInfo]  -- ^ remote addresses, they are assumed to have the same
                   -- family as the local address
    -> IO [NtpOffset]
runNtpQueries ioManager tracer protocol netSettings localAddr destAddrs
    = bracket acquire release action
  where
    acquire :: IO Socket
    acquire = Socket.socket (addrFamily localAddr) Socket.Datagram Socket.defaultProtocol

    release :: Socket -> IO ()
    release s = do
        Socket.close s
        traceWith tracer $ NtpTraceSocketClosed protocol

    action :: Socket -> IO [NtpOffset]
    action socket = do
        associateWithIOManager ioManager (Right socket)
        traceWith tracer $ NtpTraceSocketOpen protocol
        Socket.setSocketOption socket Socket.ReuseAddr 1
        Socket.bind socket (Socket.addrAddress localAddr)
        inQueue <- atomically $ newTVar []
        withAsync timeout $ \timeoutAsync ->
          withAsync (receiver socket inQueue) $ \receiverAsync -> do
            forM_ destAddrs $ \addr ->
              sendNtpPacket socket addr
              `catch`
              -- catch 'IOException's so we don't bring the loop down;
              \(e :: IOException) -> traceWith tracer (NtpTracePacketSendError (Socket.addrAddress addr) e)
            void $ waitAny [timeoutAsync, receiverAsync]
        atomically $ readTVar inQueue

    --
    -- sending thread; send a series of requests: one towards each address
    --
    sendNtpPacket :: Socket -> AddrInfo -> IO ()
    sendNtpPacket sock addr = do
        p <- mkNtpPacket
#if !defined(mingw32_HOST_OS)
        _ <- Socket.ByteString.sendManyTo sock
                  (LBS.toChunks $ encode p)
                  (setNtpPort $ Socket.addrAddress addr)
#else
        -- TODO: add `sendManyTo` to `Win32-network`
        _ <- Win32.Async.sendAllTo sock
                  (LBS.toStrict $ encode p)
                  (setNtpPort $ Socket.addrAddress addr)
#endif
        -- delay 100ms between sending requests, this avoids dealing with ntp
        -- results at the same time from various ntp servers, and thus we
        -- should get better results.
        threadDelay 100_000

    --
    -- timeout thread
    --
    timeout = do
        threadDelay
          $ (fromIntegral $ ntpResponseTimeout netSettings)
            + 100_000 * length destAddrs
        traceWith tracer $ NtpTraceWaitingForRepliesTimeout protocol

    --
    -- receiving thread
    --
    receiver :: Socket -> TVar [NtpOffset] -> IO ()
    receiver socket inQueue = replicateM_ (length destAddrs) $ do
        -- We don't catch exception here, we let them propagate.  This will
        -- reach top level handler in 'Network.NTP.Client.ntpClientThread' (see
        -- 'queryLoop' therein), which will be able to decide for how long to
        -- pause the the ntp-client.
#if !defined(mingw32_HOST_OS)
        (bs, _) <- Socket.ByteString.recvFrom socket ntpPacketSize
#else
        (bs, _) <- Win32.Async.recvFrom socket ntpPacketSize
#endif
        t <- getCurrentTime
        case decodeOrFail $ LBS.fromStrict bs of
            Left  (_, _, err) -> traceWith tracer $ NtpTracePacketDecodeError protocol err
            -- TODO : filter bad packets, i.e. late packets and spoofed packets
            Right (_, _, packet) -> do
                traceWith tracer $ NtpTracePacketReceived protocol
                let offset = (clockOffsetPure packet t)
                atomically $ modifyTVar' inQueue ((:) offset)
