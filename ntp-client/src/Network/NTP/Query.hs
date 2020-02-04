{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
module Network.NTP.Query (
    NtpSettings(..)
  , NtpStatus(..)
  , ntpQuery
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (bracket)
import           System.IO.Error (tryIOError, userError, ioError)
import           Control.Monad (forever, forM, forM_, replicateM_)
import           Control.Tracer
import           Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
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
import           Network.NTP.Packet ( mkNtpPacket
                                    , ntpPacketSize
                                    , Microsecond
                                    , NtpOffset (..)
                                    , getCurrentTime
                                    , clockOffsetPure)
import           Network.NTP.Trace (NtpTrace (..), IPVersion (..))

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

-- | Wait for at least three replies and report the minimum of the reported offsets.
minimumOfThree :: [NtpOffset] -> Maybe NtpOffset
minimumOfThree l
    = if length l >= 3 then Just $ minimum l
         else Nothing

udpLocalAddresses :: IO [AddrInfo]
--                                      Hints        Host    Service
udpLocalAddresses = Socket.getAddrInfo (Just hints) Nothing (Just $ show port)
  where
    hints = Socket.defaultHints
          { addrFlags = [AI_PASSIVE]
          , addrSocketType = Datagram
          }
    port = Socket.defaultPort

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

-- | Perform a single NTP query and return the result.
--   This function my throw an IO exception.
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
        runProtocol _proto _localAddr [] = return [] -- No servers found for that protocol.
        runProtocol _proto Nothing    _  = return [] -- No local interface for that protocol.
        runProtocol protocol (Just addr) servers = do
             runNtpQueries tracer protocol ntpSettings addr servers >>= \case
                Left err -> do
                    traceWith tracer $ NtpTraceRunProtocolError protocol err
                    return []
                Right [] -> do
                    traceWith tracer $ NtpTraceRunProtocolNoResult protocol
                    return []
                Right r@(_:_) -> do
                    traceWith tracer $ NtpTraceRunProtocolSuccess protocol
                    return r

runNtpQueries ::
       Tracer IO NtpTrace
    -> IPVersion
    -> NtpSettings
    -> AddrInfo
    -> [AddrInfo]
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
        _err <- withAsync (send socket  >> loopForever)  $ \sender ->
                withAsync timeout                        $ \delay ->
                withAsync (receiver socket inQueue )     $ \revc ->
                    waitAnyCancel [sender, delay, revc]        
        atomically $ readTVar inQueue

    send :: Socket -> IO ()
    send sock = forM_ destAddrs $ \addr -> do
        p <- mkNtpPacket
        err <- tryIOError $ Socket.ByteString.sendManyTo sock
                          (LBS.toChunks $ encode p) (setNtpPort $ Socket.addrAddress addr)
        case err of
            Right _ -> traceWith tracer $ NtpTracePacketSent protocol
            Left e  -> do
                traceWith tracer $ NtpTracePacketSentError protocol e
                ioError e
        threadDelay 100_000

    loopForever = forever $ threadDelay maxBound

    timeout = do
        threadDelay $ (fromIntegral $ ntpResponseTimeout netSettings) + 100_000 * length destAddrs
        traceWith tracer $ NtpTraceWaitingForRepliesTimeout protocol

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
