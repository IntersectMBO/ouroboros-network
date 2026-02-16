{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Demo application using mux over unix sockets or named pipes on Windows.
--
module Main (main) where

import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.IP (IP)
import Data.IP qualified as IP
import Data.TDigest (TDigest, insert, maximumValue, mean, minimumValue, quantile, stddev, tdigest)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import Control.Exception
import Control.Monad
import Control.Monad.Class.MonadTime.SI (getCurrentTime)
import Control.Tracer

import System.Environment qualified as SysEnv
import System.Exit
import System.IO

import System.IOManager
import Network.Socket (PortNumber)
import Network.Socket qualified as Socket

import Network.Mux as Mx
import Network.Mux.Bearer qualified as Mx

import Test.Mux.ReqResp


data ClientType = Sequential | Bursty

unusedValue :: a
unusedValue = error "unused"

data LatencyStats = LatencyStats {
  lsCount  :: !Int,
  lsDigest :: !(TDigest 5)
  }

main :: IO ()
main = do
  args <- SysEnv.getArgs
  case args of
    ["server", ip, port, len1, len2]
      | Just ip'   <- readMaybe ip
      , Just port' <- readMaybe port
      , Just len1' <- readMaybe len1
      , Just len2' <- readMaybe len2
      -> server Sequential ip' port' unusedValue len1' len2'

    ["client", ip, port, len, n1, n2]
      | Just ip'   <- readMaybe ip
      , Just port' <- readMaybe port
      , Just len'  <- readMaybe len
      , Just n1'   <- readMaybe n1
      , Just n2'   <- readMaybe n2
      -> client Sequential ip' port' len' n1' n2'

    ["server-burst", ip, port, n1, len1, n2, len2]
      | Just ip'   <- readMaybe ip
      , Just port' <- readMaybe port
      , Just n1'   <- readMaybe n1
      , Just len1' <- readMaybe len1
      , Just n2'   <- readMaybe n2
      , Just len2' <- readMaybe len2
      -> server Bursty ip' port' (n1', n2') len1' len2'

    ["client-burst", ip, port]
      | Just ip'   <- readMaybe ip
      , Just port' <- readMaybe port
      -> client Bursty ip' port' unusedValue unusedValue unusedValue
    _ -> usage


usage :: IO ()
usage = do
  hPutStrLn stderr $ "usage: mux-demo server       addr port <resp-length> <resp-length>\n"
                  ++ "       mux-demo client       addr port <req-length> <num-requests> <num-requests>\n"
                  ++ "       mux-demo server-burst addr port <num-responses> <resp-length> <num-responses> <resp-length>\n"
                  ++ "       mux-demo client-burst addr port"
  exitFailure


putStrLn_ :: String -> IO ()
putStrLn_ = mempty
-- putStrLn_ = BSC.putStrLn . BSC.pack

debugPutStrLn_ :: String -> IO ()
debugPutStrLn_ = BSC.putStrLn . BSC.pack

reqrespTracer
  :: String
  -- ^ tag
  -> Tracer IO (TraceSendRecv (MsgReqResp ByteString ByteString))
reqrespTracer tag = Tracer $ \case
  TraceSend (MsgReq a)  -> putStrLn_ $ tag ++ " Send MsgReq " ++ show (BSC.length a)
  TraceSend (MsgResp a) -> putStrLn_ $ tag ++ " Send MsgResp " ++ show (BSC.length a)
  TraceSend MsgDone     -> putStrLn_ $ tag ++ " Send MsgDone"
  TraceRecv (MsgReq a)  -> putStrLn_ $ tag ++ " Recv MsgReq "  ++ show (BSC.length a)
  TraceRecv (MsgResp a) -> putStrLn_ $ tag ++ " Recv MsgResp " ++ show (BSC.length a)
  TraceRecv MsgDone     -> putStrLn_ $ tag ++ " Recv MsgDone"
  TraceEarlyExit        -> putStrLn_ $ tag ++ " EarlyExit"
  TraceFailure err      -> debugPutStrLn_ $ tag ++ " Failure: " ++ show err

--
-- Protocols
--

defaultProtocolLimits :: MiniProtocolLimits
defaultProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = 10_000_000
    }


protocols :: MiniProtocolDirection mode -> [MiniProtocolInfo mode]
protocols miniProtocolDir =
  [ MiniProtocolInfo {
      miniProtocolNum        = MiniProtocolNum 2,
      miniProtocolDir,
      miniProtocolLimits     = defaultProtocolLimits,
      miniProtocolCapability = Nothing
    }
  , MiniProtocolInfo {
      miniProtocolNum        = MiniProtocolNum 3,
      miniProtocolDir,
      miniProtocolLimits     = defaultProtocolLimits,
      miniProtocolCapability = Nothing
    }
  ]

--
-- server: accept loop, server loop
--

-- | Server accept loop.
--
server :: ClientType -> IP.IP -> PortNumber -> (Int, Int) -> Int -> Int -> IO ()
server ct ip port num len1 len2 =
  withIOManager $ \ioManager -> do
    let hints = Socket.defaultHints
                  { Socket.addrFlags = [Socket.AI_ADDRCONFIG ]
                  , Socket.addrFamily = Socket.AF_INET
                  , Socket.addrSocketType = Socket.Stream
                  }
    addr:_ <- Socket.getAddrInfo (Just hints) (Just $ show ip) (Just $ show port)
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    associateWithIOManager ioManager (Right sock)
    debugPutStrLn_ $ "server: " ++ show addr
    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bind sock (Socket.addrAddress addr)
    Socket.listen sock 10
    forever $ do
      (sock', _addr) <- Socket.accept sock
      void $ forkIO $ do
        bearer <- getBearer Mx.makeSocketBearer 1.0 sock' Nothing
        case ct of
          Sequential -> 
            serverWorkerSequential bearer len1 len2
              `finally` Socket.close sock'
          Bursty ->
            serverWorkerBursty bearer num len1 len2
              `finally` Socket.close sock'

  `catch` \e -> do
    case fromException e of
      Just SomeAsyncException{} -> return ()
      Nothing -> hPutStrLn stderr $ "server: exception: " ++ show e
    throwIO e


serverWorkerSequential :: Bearer IO -> Int -> Int -> IO ()
serverWorkerSequential bearer len1 len2 = do
    debugPutStrLn_ $ "server: " ++ show (len1, len2)
    mux <- Mx.new Mx.nullTracers (protocols ResponderDirectionOnly)
    void $ forkIO $
      do awaitResult1 <-
          runMiniProtocol
            mux
            (MiniProtocolNum 2)
            ResponderDirectionOnly
            StartOnDemand
            (\chan -> runServerBin (reqrespTracer "server:praos") chan (serverReqResp len1))
         awaitResult2 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 3)
             ResponderDirectionOnly
             StartOnDemand
             (\chan -> runServerBin (reqrespTracer "server:leios") chan (serverReqResp len2))
         -- wait for both mini-protocols to finish
         results <- atomically $ (,) <$> awaitResult1
                                     <*> awaitResult2
         debugPutStrLn_ $ "server results: " ++ show results
      `finally`
        Mx.stop mux
    Mx.run mux bearer
  where
    serverReqResp
      :: Int
      -> ReqRespServer ByteString ByteString IO Int
    serverReqResp n = go minBound
      where
        go :: Char -> ReqRespServer ByteString ByteString IO Int
        go c =
          ReqRespServer {
            recvMsgReq  = \(!_) -> do
              msg <- makeTimestampedPayload n c
              pure (msg, go (succ c)),
            recvMsgDone = pure n
          }


serverWorkerBursty :: Bearer IO -> (Int, Int) -> Int -> Int -> IO ()
serverWorkerBursty bearer (n1, n2) len1 len2 = do
    debugPutStrLn_ $ "server: " ++ show (len1, len2)
    mux <- Mx.new Mx.nullTracers (protocols ResponderDirectionOnly)
    void $ forkIO $
      do awaitResult1 <-
          runMiniProtocol
            mux
            (MiniProtocolNum 2)
            ResponderDirectionOnly
            StartOnDemand
            (\chan -> runServerBurstBin (reqrespTracer "server:praos") chan (serverReqResp n1 len1))
         awaitResult2 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 3)
             ResponderDirectionOnly
             StartOnDemand
             (\chan -> runServerBurstBin (reqrespTracer "server:leios") chan (serverReqResp n2 len2))
         -- wait for both mini-protocols to finish
         results <- atomically $ (,) <$> awaitResult1
                                     <*> awaitResult2
         debugPutStrLn_ $ "server results: " ++ show results
      `finally`
        Mx.stop mux
    Mx.run mux bearer
  where
    serverReqResp
      :: Int
      -> Int
      -> ReqRespServerBurst ByteString ByteString IO Int
    serverReqResp n len = ReqRespServerBurst $ \_ -> go n minBound
      where
        go :: Int -> Char -> IO (ReqRespServerLoop ByteString IO Int)
        go m c | m > 0 = do
          msg <- makeTimestampedPayload len c
          return $ SendMsgResp msg (go (m - 1) (succ c))
               | otherwise =
          return $ SendMsgDoneServer (pure n)


--
-- client
--


client :: ClientType -> IP -> PortNumber -> Int -> Int -> Int -> IO ()
client ct ip port len n1 n2 =
  withIOManager $ \ioManager -> do
    addr:_ <- Socket.getAddrInfo Nothing (Just $ show ip) (Just $ show port)
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    associateWithIOManager ioManager (Right sock)
    Socket.connect sock (Socket.addrAddress addr)
    bearer <- getBearer Mx.makeSocketBearer 1.0 sock Nothing
    case ct of
      Sequential ->
        clientWorkerSequential bearer len n1 n2
          `catch` \e -> do
            case fromException e of
              Just SomeAsyncException{} -> return ()
              Nothing -> hPutStrLn stderr $ "client: exception: " ++ show e
            throwIO e
      Bursty ->
        clientWorkerBursty bearer
          `catch` \e -> do
            case fromException e of
              Just SomeAsyncException{} -> return ()
              Nothing -> hPutStrLn stderr $ "client: exception: " ++ show e
            throwIO e


clientWorkerSequential
  :: Mx.Bearer IO
  -> Int
  -- ^ length of request message
  -> Int
  -- ^ number of requests to send over `MiniProtocolNum 2`
  -> Int
  -- ^ number of requests to send over `MiniProtocolNum 3`
  -> IO ()
clientWorkerSequential bearer len n1 n2 = do
    praosStats <- newTVarIO newLatencyStats
    praosMissing <- newTVarIO 0
    leiosStats <- newTVarIO newLatencyStats
    leiosMissing <- newTVarIO 0
    mux <- Mx.new Mx.nullTracers (protocols InitiatorDirectionOnly)
    void $ forkIO $
      do awaitResult1 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 2)
             InitiatorDirectionOnly
             StartEagerly
             (\chan -> runClientBin (reqrespTracer "client:praos") chan
                                    (clientReqResp praosStats praosMissing '0' n1))
         awaitResult2 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 3)
             InitiatorDirectionOnly
             StartEagerly
             (\chan -> runClientBin (reqrespTracer "client:leios") chan
                                    (clientReqResp leiosStats leiosMissing '1' n2))
         -- wait for both mini-protocols to finish
         results <- atomically $ (,) <$> awaitResult1
                                     <*> awaitResult2
         debugPutStrLn_ $ "client results: " ++ show results
         reportLatencyStats "praos" praosStats praosMissing
         reportLatencyStats "leios" leiosStats leiosMissing
      `finally`
        Mx.stop mux
    Mx.run mux bearer
  where
    clientReqResp
      :: TVar LatencyStats
      -> TVar Int
      -> Char
      -> Int
      -> ReqRespClient ByteString ByteString IO Int
    clientReqResp statsVar missingVar c n = go n
      where
        !msg = BSC.replicate len c

        go :: Int -> ReqRespClient ByteString ByteString IO Int
        go m | m <= 0
             = SendMsgDone (pure n)
        go m =
          SendMsgReq msg (\rsp -> do
            recordLatency statsVar missingVar rsp
            pure $ go (m-1))


clientWorkerBursty :: Mx.Bearer IO -> IO ()
clientWorkerBursty bearer = do
    praosStats <- newTVarIO newLatencyStats
    praosMissing <- newTVarIO 0
    leiosStats <- newTVarIO newLatencyStats
    leiosMissing <- newTVarIO 0
    mux <- Mx.new Mx.nullTracers (protocols InitiatorDirectionOnly)
    void $ forkIO $
      do awaitResult1 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 2)
             InitiatorDirectionOnly
             StartEagerly
             (\chan -> runClientBurstBin (reqrespTracer "client:praos") chan
                                         (clientReqResp praosStats praosMissing))
         awaitResult2 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 3)
             InitiatorDirectionOnly
             StartEagerly
             (\chan -> runClientBurstBin (reqrespTracer "client:leios") chan
                                         (clientReqResp leiosStats leiosMissing))
         -- wait for both mini-protocols to finish
         results <- atomically $ (,) <$> awaitResult1
                                     <*> awaitResult2
         debugPutStrLn_ $ "client results: " ++ show results
         reportLatencyStats "praos" praosStats praosMissing
         reportLatencyStats "leios" leiosStats leiosMissing
      `finally`
        Mx.stop mux
    Mx.run mux bearer
  where
    clientReqResp
      :: TVar LatencyStats
      -> TVar Int
      -> ReqRespClientBurst ByteString ByteString IO Int
    clientReqResp statsVar missingVar = SendMsgReqBurst (BSC.replicate 10 '\NUL') (go 0)
      where
        go :: Int -> ReqRespClientLoop ByteString IO Int
        go !count =
          AwaitResp { handleMsgDone = pure count
                    , handleMsgResp = \(!rsp) -> do
                      recordLatency statsVar missingVar rsp
                      return $ go (count + 1)
                    }

newLatencyStats :: LatencyStats
newLatencyStats =
  LatencyStats {
      lsCount = 0
    , lsDigest = tdigest []
    }

updateLatencyStats :: Word64 -> LatencyStats -> LatencyStats
updateLatencyStats us LatencyStats{lsCount, lsDigest} =
  let !lsCount' = lsCount + 1
      !lsDigest' = insert (fromIntegral us :: Double) lsDigest in
  LatencyStats {
      lsCount = lsCount'
    , lsDigest = lsDigest'
    }

timestampSize :: Int
timestampSize = 8

getWallClockTimeUs :: IO Word64
getWallClockTimeUs = do
  now <- getCurrentTime
  let ns = floor (utcTimeToPOSIXSeconds now * 1_000_000) :: Integer
  return $ fromInteger ns

encodeTimeStamp :: Word64 -> ByteString
encodeTimeStamp ts =
  BL.toStrict (Bin.runPut (Bin.putWord64be ts))

extractTimestamp :: ByteString -> Maybe Word64
extractTimestamp bs
  | BS.length bs < timestampSize = Nothing
  | otherwise =
      Just $ Bin.runGet Bin.getWord64be
                        (BL.fromStrict (BS.take timestampSize bs))

makeTimestampedPayload :: Int -> Char -> IO ByteString
makeTimestampedPayload len c
  | len < timestampSize = pure (BSC.replicate len c)
  | otherwise = do
      ts <- getWallClockTimeUs
      let tsBytes = encodeTimeStamp ts
      return $ tsBytes <> BSC.replicate (len - timestampSize) c

recordLatency :: TVar LatencyStats -> TVar Int -> ByteString -> IO ()
recordLatency statsVar missingVar rsp =
  case extractTimestamp rsp of
       Nothing -> atomically $ modifyTVar' missingVar succ
       Just ts -> do
         now <- getWallClockTimeUs
         let latency = max 0 $ now - ts
         atomically $ modifyTVar' statsVar (updateLatencyStats latency)

reportLatencyStats :: String -> TVar LatencyStats -> TVar Int -> IO ()
reportLatencyStats label statsVar missingVar = do
  (stats, missing) <- atomically $ (,) <$> readTVar statsVar <*> readTVar missingVar
  let count = lsCount stats 
  if count <= 0
     then printf "%s latency: count=0 missing=%d\n" label missing
     else do
       let td       = lsDigest stats
           minMs    = minimumValue td / 1000
           maxMs    = maximumValue td / 1000
           stddevMs = case stddev td of
                           Nothing -> 0
                           Just sd -> sd / 1000
           meanMs   = case mean td of
                           Nothing -> 0
                           Just m  -> m / 1000
           medianMs = quantile' 0.5 td
           p90Ms    = quantile' 0.9 td
           p95Ms    = quantile' 0.95 td
           p99Ms    = quantile' 0.99 td
       printf "%s latency: count=%d min=%.3fms mean=%.3fms median=%.3fms p90=%.3fms p95=%.3fms p99=%.3fms max=%.3fms stddev=%.3fms missing=%d\n"
         label count minMs meanMs medianMs p90Ms p95Ms p99Ms maxMs stddevMs missing
  where
    quantile' q td = case quantile q td of
                       Nothing -> 0
                       Just w  -> w / 1000

