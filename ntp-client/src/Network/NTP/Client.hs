{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}

module Network.NTP.Client
where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM (STM, atomically, check)
import           Control.Concurrent.STM.TVar
import           Control.Exception (bracket)
import           System.IO.Error (catchIOError, tryIOError, userError, ioError)
import           Control.Monad (forever, forM, forM_, when)
import           Control.Tracer
import           Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (find)
import           Data.Maybe
import           Network.Socket ( AddrInfo ()
                                , AddrInfoFlag (AI_ADDRCONFIG, AI_PASSIVE)
                                , Family (AF_INET, AF_INET6)
                                , PortNumber
                                , Socket
                                , SockAddr (..)
                                , SocketOption (ReuseAddr)
                                , SocketType (Datagram)
                                , addrAddress
                                , addrFamily
                                , addrFlags
                                , addrSocketType)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString (recvFrom, sendManyTo)
import           Network.NTP.Packet ( NtpPacket
                                    , mkNtpPacket
                                    , ntpPacketSize
                                    , Microsecond
                                    , NtpOffset (..)
                                    , getCurrentTime
                                    , clockOffsetPure)
import           Network.NTP.Trace (NtpTrace (..), IPVersion (..))

main :: IO ()
main = testClient

data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ List of servers addresses.
    , ntpResponseTimeout :: Microsecond
      -- ^ Timeout between sending NTP requests and response collection.
    , ntpPollDelay       :: Microsecond
      -- ^ How long to wait between two rounds of requests.
    , ntpReportPolicy    :: [ReceivedPacket] -> Maybe NtpOffset
    }

data NtpClient = NtpClient
    { -- | Query the current NTP status.
      ntpGetStatus        :: STM NtpStatus
      -- | Bypass all internal threadDelays and trigger a new NTP query.
    , ntpTriggerUpdate    :: IO ()
    , ntpThread           :: Async ()
    }

data NtpStatus =
      -- | The difference between NTP time and local system time
      NtpDrift NtpOffset
      -- | NTP client has send requests to the servers
    | NtpSyncPending
      -- | NTP is not available: the client has not received any respond within
      -- `ntpResponseTimeout` or NTP was not configured.
    | NtpSyncUnavailable deriving (Eq, Show)

data ReceivedPacket = ReceivedPacket
    { receivedPacket    :: !NtpPacket
    , receivedLocalTime :: !Microsecond
    , receivedOffset    :: !NtpOffset
    } deriving (Eq, Show)

-- | Wait for at least three replies and report the minimum of the reported offsets.
minimumOfThree :: [ReceivedPacket] -> Maybe NtpOffset
minimumOfThree l
    = if length l >= 3 then Just $ minimum $ map receivedOffset l
         else Nothing

-- | Setup a NtpClient and run a application that uses that client.
-- The NtpClient is terminated when the application returns.
-- And also the application is terminated when the NtpClient crashes.
withNtpClient :: Tracer IO NtpTrace -> NtpClientSettings -> (NtpClient -> IO a) -> IO a
withNtpClient tracer ntpSettings action = do
    traceWith tracer NtpTraceStartNtpClient
    ntpStatus <- newTVarIO NtpSyncPending
    withAsync (ntpClientThread tracer (ntpSettings, ntpStatus)) $ \tid -> do
        let client = NtpClient
              { ntpGetStatus = readTVar ntpStatus
              , ntpTriggerUpdate = do
                   traceWith tracer NtpTraceClientActNow
                   atomically $ writeTVar ntpStatus NtpSyncPending
              , ntpThread = tid
              }
        link tid         -- an error in the ntp-client kills the appliction !
        action client

udpLocalAddresses :: IO [AddrInfo]
udpLocalAddresses = do
    let hints = Socket.defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Datagram }
        port = Socket.defaultPort
    --                 Hints        Host    Service
    Socket.getAddrInfo (Just hints) Nothing (Just $ show port)

resolveHost :: String -> IO [AddrInfo]
resolveHost host = Socket.getAddrInfo (Just hints) (Just host) Nothing
  where
    hints = Socket.defaultHints
            { addrSocketType = Datagram
            , addrFlags = [AI_ADDRCONFIG]  -- since we use @AF_INET@ family
            }

firstAddr :: String -> [AddrInfo] -> IO (Maybe AddrInfo, Maybe AddrInfo)
firstAddr name l = case (find isV4Addr l, find isV6Addr l) of
    (Nothing, Nothing) -> ioError $ userError $ "lookup host failed :" ++ name
    p -> return p
    where
        isV4Addr :: AddrInfo -> Bool
        isV4Addr addr = addrFamily addr == AF_INET

        isV6Addr :: AddrInfo -> Bool
        isV6Addr addr = addrFamily addr == AF_INET6


setNtpPort :: SockAddr ->  SockAddr
setNtpPort addr = case addr of
    (SockAddrInet  _ host)            -> SockAddrInet  ntpPort host
    (SockAddrInet6 _ flow host scope) -> SockAddrInet6 ntpPort flow host scope
    sockAddr                   -> sockAddr
  where
    ntpPort :: PortNumber
    ntpPort = 123

threadDelayInterruptible :: TVar NtpStatus -> Int -> IO ()
threadDelayInterruptible tvar t
    = race_
       ( threadDelay t )
       ( atomically $ do
           s <- readTVar tvar
           check $ s == NtpSyncPending
       )

-- TODO: maybe reset the delaytime if the oneshotClient did one sucessful query
ntpClientThread ::
       Tracer IO NtpTrace
    -> (NtpClientSettings, TVar NtpStatus)
    -> IO ()
ntpClientThread tracer args@(_, ntpStatus) = forM_ restartDelay $ \t -> do
    traceWith tracer $ NtpTraceRestartDelay t
    threadDelayInterruptible ntpStatus $ t * 1_000_000
    traceWith tracer NtpTraceRestartingClient
    (forever $ oneshotClient tracer args) `catchIOError`
        \err -> traceWith tracer $ NtpTraceIOError err
    atomically $ writeTVar ntpStatus NtpSyncUnavailable
    where
        restartDelay :: [Int]
        restartDelay = [0, 5, 10, 20, 60, 180, 600] ++ repeat 600

-- | Setup and run the NTP client.
-- In case of an IOError (for example when network interface goes down) cleanup and return.

oneshotClient ::
       Tracer IO NtpTrace
    -> (NtpClientSettings, TVar NtpStatus)
    -> IO ()
oneshotClient tracer (ntpSettings, ntpStatus) = do
    traceWith tracer NtpTraceClientStartQuery
    (v4Servers,   v6Servers)   <- lookupServers $ ntpServers ntpSettings
    (v4LocalAddr, v6LocalAddr) <- udpLocalAddresses >>= firstAddr "localhost"
    (v4Replies, v6Replies) <- concurrently (runProtocol IPv4 v4LocalAddr v4Servers)
                                           (runProtocol IPv6 v6LocalAddr v6Servers)
    when (null v4Replies && null v6Replies) $ do
        traceWith tracer NtpTraceIPv4IPv6BothFailed
        atomically $ writeTVar ntpStatus NtpSyncUnavailable        
        ioError $ userError "IPv4 and IPv6 failed"
    case (ntpReportPolicy ntpSettings) (v4Replies ++ v6Replies) of
        Nothing -> do
            traceWith tracer NtpTraceUpdateStatusQueryFailed
            atomically $ writeTVar ntpStatus NtpSyncUnavailable
        Just offset -> do
            traceWith tracer $ NtpTraceUpdateStatusClockOffset $ getNtpOffset offset
            atomically $ writeTVar ntpStatus $ NtpDrift offset
    traceWith tracer NtpTraceClientSleeping
    threadDelayInterruptible ntpStatus $ fromIntegral $ ntpPollDelay ntpSettings
    where
        runProtocol :: IPVersion -> Maybe AddrInfo -> [AddrInfo] -> IO [ReceivedPacket]
        runProtocol _version _localAddr [] = return []
        runProtocol _version Nothing    _  = return []
        runProtocol version (Just addr) servers = do
            queryServers tracer ntpSettings addr servers >>= \case
                Left err -> do
                    traceWith tracer $ NtpTraceRunProtocolError version err
                    return []
                Right [] -> do
                    traceWith tracer $ NtpTraceRunProtocolNoResult version
                    return []
                Right r@(_:_) -> do
                    traceWith tracer $ NtpTraceRunProtocolSuccess version
                    return r

queryServers ::
       Tracer IO NtpTrace
    -> NtpClientSettings
    -> AddrInfo
    -> [AddrInfo]
    -> IO (Either IOError [ReceivedPacket])
queryServers tracer netSettings localAddr destAddrs
    = tryIOError $ bracket acquire release action
  where
    acquire :: IO Socket
    acquire = do
        s <- Socket.socket (addrFamily localAddr) Datagram Socket.defaultProtocol
        traceWith tracer NtpTraceSocketOpen
        return s

    release :: Socket -> IO ()
    release s = do
        Socket.close s
        traceWith tracer NtpTraceSocketClosed

    action :: Socket -> IO [ReceivedPacket]
    action socket = do
        Socket.setSocketOption socket ReuseAddr 1
        inQueue <- atomically $ newTVar []
        _err <- withAsync (send socket  >> loopForever)  $ \sender ->
                withAsync timeout                        $ \delay ->
                withAsync (reader socket inQueue )       $ \revc ->
                    waitAnyCancel [sender, delay, revc]        
        atomically $ readTVar inQueue

    send :: Socket -> IO ()
    send sock = forM_ destAddrs $ \addr -> do
        p <- mkNtpPacket
        err <- tryIOError $ Socket.ByteString.sendManyTo sock
                          (LBS.toChunks $ encode p) (setNtpPort $ Socket.addrAddress addr)
        case err of
            Right _ -> traceWith tracer NtpTracePacketSent
            Left e  -> do
                traceWith tracer $ NtpTracePacketSentError e
                ioError e
        threadDelay 100_000

    loopForever = forever $ threadDelay maxBound

    timeout = do
        threadDelay $ (fromIntegral $ ntpResponseTimeout netSettings) + 100_000 * length destAddrs
        traceWith tracer NtpTraceClientWaitingForRepliesTimeout

    reader :: Socket -> TVar [ReceivedPacket] -> IO ()
    reader socket inQueue = forever $ do
        (bs, _) <- Socket.ByteString.recvFrom socket ntpPacketSize
        t <- getCurrentTime
        case decodeOrFail $ LBS.fromStrict bs of
            Left  (_, _, err) -> traceWith tracer $ NtpTraceSocketReaderDecodeError err
            Right (_, _, packet) -> do
            -- todo : filter bad packets, i.e. late packets and spoofed packets
                traceWith tracer NtpTraceReceiveLoopPacketReceived
                let received = ReceivedPacket packet t (clockOffsetPure packet t)
                atomically $ modifyTVar' inQueue ((:) received)

lookupServers :: [String] -> IO ([AddrInfo], [AddrInfo])
lookupServers names = do
   dests <- forM names $ \server -> resolveHost server >>= firstAddr server
   return (mapMaybe fst dests, mapMaybe snd dests)

testClient :: IO ()
testClient = withNtpClient (contramapM (return . show) stdoutTracer) settings runApplication
  where
    runApplication ntpClient = race_ getLine $ forever $ do
        status <- atomically $ ntpGetStatus ntpClient
        traceWith stdoutTracer $ show ("main"::String, status)
        threadDelay 10_000_000
        ntpTriggerUpdate ntpClient

    settings :: NtpClientSettings
    settings = NtpClientSettings
        { ntpServers = ["0.de.pool.ntp.org", "0.europe.pool.ntp.org", "0.pool.ntp.org"
                       , "1.pool.ntp.org", "2.pool.ntp.org", "3.pool.ntp.org"]
        , ntpResponseTimeout = fromInteger 1_000_000
        , ntpPollDelay       = fromInteger 300_000_000
        , ntpReportPolicy    = minimumOfThree
        }
