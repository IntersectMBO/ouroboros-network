{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
module Network.NTP.Client.Query (
    NtpSettings(..)
  , NtpStatus(..)
  , ntpQuery
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (bracket, mask, throwIO)
import           System.IO.Error (tryIOError, userError, ioError)
import           Control.Monad (forM, forM_, replicateM_)
import           Control.Tracer
import           Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void)
import           Data.List (find)
import           Data.Maybe
import           Network.Socket ( AddrInfo
                                , AddrInfoFlag (AI_ADDRCONFIG, AI_PASSIVE)
                                , Family (AF_INET, AF_INET6)
                                , PortNumber
                                , Socket
                                , SockAddr (..)
                                , SocketOption (ReuseAddr)
                                , SocketType (Datagram)
                                , addrFamily
                                , addrFlags
                                , addrSocketType)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString (recvFrom, sendManyTo)
import           Network.NTP.Client.Packet ( mkNtpPacket
                                    , ntpPacketSize
                                    , Microsecond
                                    , NtpOffset (..)
                                    , getCurrentTime
                                    , clockOffsetPure)
import           Network.NTP.Client.Trace (NtpTrace (..), IPVersion (..))

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
          { addrFlags = [AI_PASSIVE]
          , addrSocketType = Datagram
          }
    port = Socket.defaultPort

-- | Resolve hostname into 'AddrInfo'.  We use 'AI_ADDRCONFIG' so we get IPv4/6
-- address only if the local.  We don't need 'AI_V4MAPPED' which would be set
-- by default.
--
resolveHost :: String -> IO [AddrInfo]
resolveHost host = Socket.getAddrInfo (Just hints) (Just host) Nothing
  where
  -- The library uses AI_ADDRCONFIG as simple test if IPv4 or IPv6 are configured.
  -- According to the documentation, AI_ADDRCONFIG is not available on all platforms,
  -- but it is expected to work on win32, Mac OS X and Linux.
  -- TODO: use addrInfoFlagImplemented :: AddrInfoFlag -> Bool to test if the flag is available.
    hints = Socket.defaultHints
            { addrSocketType = Datagram
            , addrFlags = [AI_ADDRCONFIG]
            }

firstAddr :: Family -> [AddrInfo] -> Maybe AddrInfo
firstAddr family l = find ((==) family . addrFamily ) l

setNtpPort :: SockAddr ->  SockAddr
setNtpPort addr = case addr of
    (SockAddrInet  _ host)            -> SockAddrInet  ntpPort host
    (SockAddrInet6 _ flow host scope) -> SockAddrInet6 ntpPort flow host scope
    sockAddr                          -> sockAddr
  where
    ntpPort :: PortNumber
    ntpPort = 123


-- | Resolve dns names
--
lookupServers :: Tracer IO NtpTrace -> [String] -> IO ([AddrInfo], [AddrInfo])
lookupServers tracer names = do
    dests <- forM names $ \server -> do
        addr <- resolveHost server
        case (firstAddr AF_INET addr, firstAddr AF_INET6 addr) of
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
ntpQuery ::
       Tracer IO NtpTrace
    -> NtpSettings
    -> IO NtpStatus
ntpQuery tracer ntpSettings = do
    traceWith tracer NtpTraceClientStartQuery
    (v4Servers,   v6Servers)   <- lookupServers tracer $ ntpServers ntpSettings
    localAddrs <- udpLocalAddresses
    (v4LocalAddr, v6LocalAddr) <- case (firstAddr AF_INET localAddrs, firstAddr AF_INET6 localAddrs) of
        (Nothing, Nothing) -> do
            traceWith tracer NtpTraceNoLocalAddr
            ioError $ userError "no local address IPv4 and IPv6"
        l -> return l
    (v4Replies, v6Replies) <- concurrently (runProtocol IPv4 v4LocalAddr v4Servers)
                                           (runProtocol IPv6 v6LocalAddr v6Servers)
    case v4Replies ++ v6Replies of
        [] -> do
            traceWith tracer NtpTraceIPv4IPv6NoReplies
            return NtpSyncUnavailable
        l -> case minimumOfThree l of
            Nothing -> do
                traceWith tracer NtpTraceReportPolicyQueryFailed
                return NtpSyncUnavailable
            Just offset -> do
                traceWith tracer $ NtpTraceQueryResult $ getNtpOffset offset
                return $ NtpDrift offset
    where
        runProtocol :: IPVersion -> Maybe AddrInfo -> [AddrInfo] -> IO [NtpOffset]
        -- no addresses to sent to
        runProtocol _protocol _localAddr  []      = return []
        -- local address is not configured, e.g. no IPv6   or IPv6 gateway.       
        runProtocol _protocol Nothing     _       = return []
        -- local address is configured, remote address list is non empty
        runProtocol protocol  (Just addr) servers = do
           runNtpQueries tracer protocol ntpSettings addr servers >>= \case
              Left err -> do
                  traceWith tracer $ NtpTraceRunProtocolError protocol err
                  return []
              Right [] -> do
                  traceWith tracer $ NtpTraceRunProtocolNoResult protocol
                  return []
              Right r -> do
                  traceWith tracer $ NtpTraceRunProtocolSuccess protocol
                  return r


-- | Run an ntp query towards each address
--
runNtpQueries
    :: Tracer IO NtpTrace
    -> IPVersion   -- ^ address family, it must afree with local and remote
                   -- addresses
    -> NtpSettings
    -> AddrInfo    -- ^ local address
    -> [AddrInfo]  -- ^ remote addresses, they are assumed to have the same
                   -- family as the local address
    -> IO (Either IOError [NtpOffset])
runNtpQueries tracer protocol netSettings localAddr destAddrs
    = tryIOError $ bracket acquire release action
  where
    acquire :: IO Socket
    acquire = Socket.socket (addrFamily localAddr) Datagram Socket.defaultProtocol

    release :: Socket -> IO ()
    release s = do
        Socket.close s
        traceWith tracer $ NtpTraceSocketClosed protocol

    action :: Socket -> IO [NtpOffset]
    action socket = do
        traceWith tracer $ NtpTraceSocketOpen protocol
        Socket.setSocketOption socket ReuseAddr 1
        inQueue <- atomically $ newTVar []
        withAsync timeout $ \timeoutT ->
          withAsync (receiver socket inQueue ) $ \receiverT ->
            -- mask async exceptions to guarantee that the other threads are
            -- cancelled correctly.  'timeoutT' and 'receiverT' threads were
            -- started using 'withAsync', so they will be terminated when the
            -- callbak either returns or errors.
            mask $ \unmask ->
              async (unmask $ send socket) >>= \senderT -> unmask $
                waitCatch senderT >>= \case
                  Left e  -> throwIO e
                  Right _ -> void $ waitAny [timeoutT, receiverT]
        atomically $ readTVar inQueue

    --
    -- sending thread; send a series of requests: one towards each address
    --
    send :: Socket -> IO ()
    send sock = forM_ destAddrs $ \addr -> do
        p <- mkNtpPacket
        err <- tryIOError
                $ Socket.ByteString.sendManyTo sock
                  (LBS.toChunks $ encode p)
                  (setNtpPort $ Socket.addrAddress addr)
        case err of
            Right _ -> traceWith tracer $ NtpTracePacketSent protocol
            Left e  -> do
                traceWith tracer $ NtpTracePacketSentError protocol e
                ioError e
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
        (bs, _) <- Socket.ByteString.recvFrom socket ntpPacketSize
        t <- getCurrentTime
        case decodeOrFail $ LBS.fromStrict bs of
            Left  (_, _, err) -> traceWith tracer $ NtpTracePacketDecodeError protocol err
            -- TODO : filter bad packets, i.e. late packets and spoofed packets
            Right (_, _, packet) -> do
                traceWith tracer $ NtpTracePacketReceived protocol
                let offset = (clockOffsetPure packet t)
                atomically $ modifyTVar' inQueue ((:) offset)
