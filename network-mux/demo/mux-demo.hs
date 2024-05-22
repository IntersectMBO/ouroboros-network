{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

#if defined(mingw32_HOST_OS)
{-# LANGUAGE PackageImports     #-}
#endif

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Demo application using mux over unix sockets or named pipes on Windows.
--
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Exception (finally)
import Control.Monad
import Control.Tracer (Tracer (..), nullTracer, showTracing)

import System.Environment qualified as SysEnv
import System.Exit
import System.IO

#if defined(mingw32_HOST_OS)
import Data.Bits
import System.IOManager
import System.Win32
import System.Win32.Async qualified as Win32.Async
import "Win32-network" System.Win32.NamedPipes
#else
import Network.Socket (Family (AF_UNIX), SockAddr (..))
import Network.Socket qualified as Socket
import System.Directory
#endif

import Network.Mux
import Network.Mux.Bearer

import Test.Mux.ReqResp


main :: IO ()
main = do
    args <- SysEnv.getArgs
    case args of
      ["server"]         -> server
      ["client", n, msg] -> client (read n) msg
      _                  -> usage

usage :: IO ()
usage = do
  hPutStrLn stderr $ "usage: mux-demo server\n"
                  ++ "       mux-demo client (n :: Int) (msg :: String)"
  exitFailure

pipeName :: String
#if defined(mingw32_HOST_OS)
pipeName = "\\\\.\\pipe\\mux-demo"
#else
pipeName = "./mux-demo.sock"
#endif

putStrLn_ :: String -> IO ()
putStrLn_ = BSC.putStrLn . BSC.pack

debugTracer :: Show a => Tracer IO a
debugTracer = showTracing (Tracer putStrLn_)

--
-- Protocols
--

defaultProtocolLimits :: MiniProtocolLimits
defaultProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = 64_000
    }

--
-- server: accept loop, server loop
--


-- | Server accept loop.
--
server :: IO ()
#if defined(mingw32_HOST_OS)
server =
  withIOManager $ \ioManager ->
  forever $ do
    hpipe <- createNamedPipe pipeName
                             (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             4096 -- pipe buffer size
                             4096 -- pipe buffer size
                             0
                             Nothing
    associateWithIOManager ioManager (Left hpipe)
    Win32.Async.connectNamedPipe hpipe
    void $ forkIO $ do
      bearer <- getBearer makeNamedPipeBearer (-1) nullTracer hpipe
      serverWorker bearer
        `finally` closeHandle hpipe
#else
server = do
    sock <- Socket.socket AF_UNIX Socket.Stream Socket.defaultProtocol
    removeFile pipeName
    Socket.bind sock (SockAddrUnix pipeName)
    Socket.listen sock 1
    forever $ do
      (sock', _addr) <- Socket.accept sock
      void $ forkIO $ do
        bearer <- getBearer makeSocketBearer 1.0 nullTracer sock'
        serverWorker bearer
          `finally` Socket.close sock'
#endif


serverWorker :: MuxBearer IO -> IO ()
serverWorker bearer = do
    mux <- newMux ptcls

    void $ forkIO $ do
      awaitResult <-
        runMiniProtocol mux (MiniProtocolNum 2)
                             ResponderDirectionOnly
                             StartOnDemand $ \channel ->
          runServer debugTracer channel (echoServer 0)
      result <- atomically awaitResult
      putStrLn $ "Result: " ++ show result
      stopMux mux

    runMux nullTracer mux bearer
  where
    ptcls ::  MiniProtocolBundle ResponderMode
    ptcls = MiniProtocolBundle
            [ MiniProtocolInfo {
                miniProtocolNum    = MiniProtocolNum 2,
                miniProtocolDir    = ResponderDirectionOnly,
                miniProtocolLimits = defaultProtocolLimits
              }
            ]

echoServer :: Int -> ReqRespServer ByteString ByteString IO Int
echoServer !n = ReqRespServer {
    recvMsgReq  = \req -> pure (req, echoServer (n+1)),
    recvMsgDone = pure n
  }


--
-- client
--


client :: Int -> String -> IO ()
#if defined(mingw32_HOST_OS)
client n msg =
    withIOManager $ \ioManager -> do
    hpipe <- createFile pipeName
                        (gENERIC_READ .|. gENERIC_WRITE)
                        fILE_SHARE_NONE
                        Nothing
                        oPEN_EXISTING
                        fILE_FLAG_OVERLAPPED
                        Nothing
    associateWithIOManager ioManager (Left hpipe)
    bearer <- getBearer makeNamedPipeBearer (-1) nullTracer hpipe
    clientWorker bearer n msg
#else
client n msg = do
    sock <- Socket.socket AF_UNIX Socket.Stream Socket.defaultProtocol
    Socket.connect sock (SockAddrUnix pipeName)
    bearer <- getBearer makeSocketBearer 1.0 nullTracer sock
    clientWorker bearer n msg
#endif


clientWorker :: MuxBearer IO -> Int -> String -> IO ()
clientWorker bearer n msg = do
    mux <- newMux ptcls

    void $ forkIO $ do
      awaitResult <-
        runMiniProtocol mux (MiniProtocolNum 2)
                             InitiatorDirectionOnly
                             StartEagerly $ \channel ->
          runClient debugTracer channel (echoClient 0 n (BSC.pack msg))
      result <- atomically awaitResult
      putStrLn $ "Result: " ++ show result
      stopMux mux

    runMux nullTracer mux bearer
  where
    ptcls :: MiniProtocolBundle InitiatorMode
    ptcls = MiniProtocolBundle
            [ MiniProtocolInfo {
                miniProtocolNum    = MiniProtocolNum 2,
                miniProtocolDir    = InitiatorDirectionOnly,
                miniProtocolLimits = defaultProtocolLimits
              }
            ]

echoClient :: Int -> Int -> ByteString
           -> ReqRespClient ByteString ByteString IO Int
echoClient !n 0 _      = SendMsgDone (pure n)
echoClient !n m rawmsg = SendMsgReq rawmsg (pure . echoClient (n+1) (m-1))

