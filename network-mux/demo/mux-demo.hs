{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Demo application which for now is only using mux over named pipes on
-- Windows.
--
-- TODO: extend it to use unix sockets.
--
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Control.Tracer (Tracer (..), nullTracer, showTracing)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Void

import qualified Network.Mux as Mx
import qualified Network.Mux.Bearer.Pipe as Mx

import Test.Mux.ReqResp

import System.Win32
import System.Win32.NamedPipes
import qualified System.Win32.Async as Win32.Async
import System.IOManager

import System.IO
import System.Exit
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["server"]         -> echoServer
      ["client", n, msg] -> client (read n) msg
      _                  -> usage

usage :: IO ()
usage = do
  hPutStr stderr $ "usage: mux-demo server\n"
                 ++"       mux-demo client (n :: Int) (msg :: String)"
  exitFailure

pipeName :: String
pipeName = "\\\\.\\pipe\\mux-demo"

putStrLn_ :: String -> IO ()
putStrLn_ = BSC.putStrLn . BSC.pack

debugTracer :: Show a => Tracer IO a
debugTracer = showTracing (Tracer putStrLn_)

--
-- Protocols
--

defaultProtocolLimits :: Mx.MiniProtocolLimits
defaultProtocolLimits =
    Mx.MiniProtocolLimits {
      Mx.maximumIngressQueue = 3_000_000
    }

--
-- server: accept loop, server loop
--


-- | Server accept loop.
--
echoServer :: IO ()
echoServer = withIOManager $ \ioManager -> do
    hpipe <- createNamedPipe pipeName
                             (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             1024
                             1024
                             0
                             Nothing
    associateWithIOManager ioManager (Left hpipe)
    Win32.Async.connectNamedPipe hpipe
    _ <- forkIO $ do
           serverLoop hpipe
             `finally` closeHandle hpipe
    threadDelay 1
    echoServer


serverLoop :: HANDLE
           -> IO ()
serverLoop h = do
    let pipeChannel = Mx.pipeChannelFromNamedPipe h
        bearer = Mx.pipeAsMuxBearer nullTracer pipeChannel
    Mx.muxStart
        nullTracer
        app
        bearer
  where
    app :: Mx.MuxApplication 'Mx.ResponderApp IO Void ()
    app = Mx.MuxApplication
      [ Mx.MuxMiniProtocol {
          Mx.miniProtocolNum    = Mx.MiniProtocolNum 2,
          Mx.miniProtocolLimits = defaultProtocolLimits,
          Mx.miniProtocolRun    = Mx.ResponderProtocolOnly
            $ \channel -> runServer debugTracer channel serverApp
        }
      ]

    serverApp :: ReqRespServer ByteString ByteString IO ()
    serverApp = ReqRespServer {
        recvMsgReq  = \req -> pure (req, serverApp),
        recvMsgDone = pure ()
      }


--
-- client
--
    

client :: Int -> String -> IO ()
client n msg = withIOManager $ \ioManager -> do
    hpipe <- createFile pipeName
                        (gENERIC_READ .|. gENERIC_WRITE)
                        fILE_SHARE_NONE
                        Nothing
                        oPEN_EXISTING
                        fILE_FLAG_OVERLAPPED
                        Nothing
    associateWithIOManager ioManager (Left hpipe)
    let pipeChannel = Mx.pipeChannelFromNamedPipe hpipe
        bearer = Mx.pipeAsMuxBearer nullTracer pipeChannel
    Mx.muxStart
        nullTracer
        app
        bearer
  where
    app :: Mx.MuxApplication 'Mx.InitiatorApp IO () Void
    app = Mx.MuxApplication
      [ Mx.MuxMiniProtocol {
          Mx.miniProtocolNum    = Mx.MiniProtocolNum 2,
          Mx.miniProtocolLimits = defaultProtocolLimits,
          Mx.miniProtocolRun    = Mx.InitiatorProtocolOnly
            $ \channel -> runClient debugTracer channel (clientApp n (BSC.pack msg))
        }
      ]

    clientApp :: Int -> ByteString -> ReqRespClient ByteString ByteString IO ()
    clientApp 0 _      = SendMsgDone (pure ())
    clientApp m rawmsg = SendMsgReq rawmsg
                                    (pure . clientApp (pred m)) -- send back request
                             
