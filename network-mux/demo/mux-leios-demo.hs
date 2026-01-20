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

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity
import Data.IP (IP)
import Data.IP qualified as IP
import Text.Read (readMaybe)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Exception
import Control.Monad
import Control.Monad.Class.MonadTime.SI
import Control.Tracer

import System.Environment qualified as SysEnv
import System.Exit
import System.IO

import System.IOManager
#if defined(mingw32_HOST_OS)
import System.Win32 qualified Win32
import System.Win32.Async qualified as Win32.Async
#endif
import Network.Socket (PortNumber)
import Network.Socket qualified as Socket

import Network.Mux as Mx
import Network.Mux.Bearer qualified as Mx
import Network.Mux.Time
import Network.Mux.Types (ProtocolBurst(..))

import Test.Mux.ReqResp

data ClientType = Sequential | Bursty

unusedValue :: a
unusedValue = error "unused"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
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
  `catch` (\(SomeException e) -> hPutStrLn stderr $ "Error: " <> displayException e)


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

timeTrace :: Show a => Tracer IO a
timeTrace = f `contramapM` stdoutTracer
  where
    f a = do
      ts <- getMonotonicTime
      let ts32 = timestampMicrosecondsLow32Bits ts
      return $ show ts32 <> " " <> show a

--
-- Protocols
--

defaultProtocolLimits :: MiniProtocolLimits
defaultProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = 10_000_000,
      burst = Nothing
    }


protocols :: MiniProtocolDirection mode -> [MiniProtocolInfo mode]
protocols miniProtocolDir =
  [ MiniProtocolInfo {
      miniProtocolNum        = MiniProtocolNum 2,
      miniProtocolDir,
      miniProtocolLimits     = defaultProtocolLimits { burst = Just (ProtocolBurst 10)},
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
server ct ip port num len1 len2 = do
  withIOManager $ \ioManager -> do
    let hints = Socket.defaultHints
                  { Socket.addrFlags = [Socket.AI_ADDRCONFIG ]
                  , Socket.addrFamily = Socket.AF_INET
                  , Socket.addrSocketType = Socket.Stream
                  }
    addr:_ <- Socket.getAddrInfo (Just hints) (Just $ show ip) (Just $ show port)
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    associateWithIOManager ioManager sock
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
            recvMsgReq  = \(!_) ->
              let msg = BSC.replicate n c in
              pure (msg, go (succ c)),
            recvMsgDone = pure n
          }


serverWorkerBursty :: Bearer IO -> (Int, Int) -> Int -> Int -> IO ()
serverWorkerBursty bearer (n1, n2) len1 len2 = do
    debugPutStrLn_ $ "server: " ++ show (len1, len2)
    mux <- Mx.new Mx.nullTracers { channelTracer = runIdentity >$< timeTrace } (protocols ResponderDirectionOnly)
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
    serverReqResp n len = ReqRespServerBurst $ \_ -> pure (go n minBound)
      where
        go :: Int -> Char -> ReqRespServerLoop ByteString IO Int
        go m c | m > 0 =
          SendMsgResp (BSC.replicate len c) (return $ go (m-1) (succ c))
               | otherwise =
          SendMsgDoneServer (pure n)


--
-- client
--


client :: ClientType -> IP -> PortNumber -> Int -> Int -> Int -> IO ()
client ct ip port len n1 n2 =
  withIOManager $ \ioManager -> do
    addr:_ <- Socket.getAddrInfo Nothing (Just $ show ip) (Just $ show port)
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    associateWithIOManager ioManager sock
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
    mux <- Mx.new Mx.nullTracers (protocols InitiatorDirectionOnly)
    void $ forkIO $
      do awaitResult1 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 2)
             InitiatorDirectionOnly
             StartEagerly
             (\chan -> runClientBin (reqrespTracer "client:praos") chan (clientReqResp '0' n1))
         awaitResult2 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 3)
             InitiatorDirectionOnly
             StartEagerly
             (\chan -> runClientBin (reqrespTracer "client:leios") chan (clientReqResp '1' n2))
         -- wait for both mini-protocols to finish
         results <- atomically $ (,) <$> awaitResult1
                                     <*> awaitResult2
         debugPutStrLn_ $ "client results: " ++ show results
      `finally`
        Mx.stop mux
    Mx.run mux bearer
  where
    clientReqResp
      :: Char
      -> Int
      -> ReqRespClient ByteString ByteString IO Int
    clientReqResp c n = go n
      where
        !msg = BSC.replicate len c

        go :: Int -> ReqRespClient ByteString ByteString IO Int
        go m | m <= 0
             = SendMsgDone (pure n)
        go m = SendMsgReq msg (\_ -> pure $ go (m-1))


clientWorkerBursty :: Mx.Bearer IO -> IO ()
clientWorkerBursty bearer = do
    mux <- Mx.new Mx.nullTracers (protocols InitiatorDirectionOnly)
    void $ forkIO $
      do awaitResult1 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 2)
             InitiatorDirectionOnly
             StartEagerly
             (\chan -> runClientBurstBin (reqrespTracer "client:praos") chan clientReqResp)
         awaitResult2 <-
           runMiniProtocol
             mux
             (MiniProtocolNum 3)
             InitiatorDirectionOnly
             StartEagerly
             (\chan -> runClientBurstBin (reqrespTracer "client:leios") chan clientReqResp)
         -- wait for both mini-protocols to finish
         results <- atomically $ (,) <$> awaitResult1
                                     <*> awaitResult2
         debugPutStrLn_ $ "client results: " ++ show results
      `finally`
        Mx.stop mux
    Mx.run mux bearer
  where
    clientReqResp
      :: ReqRespClientBurst ByteString ByteString IO Int
    clientReqResp = SendMsgReqBurst (BSC.replicate 10 '\NUL') (go 0)
      where
        go :: Int -> ReqRespClientLoop ByteString IO Int
        go !count =
          AwaitResp { handleMsgDone = pure count
                    , handleMsgResp = \(!_) -> pure (go (count + 1))
                    }
