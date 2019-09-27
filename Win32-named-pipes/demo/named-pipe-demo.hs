{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import System.IO
import System.Exit
import System.Win32
import System.Win32.NamedPipes
import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["server"] -> server
    ["client"] -> client
    _          -> usage

usage :: IO ()
usage = do
  hPutStr stderr "usage: named-pipe-demo [server|client]"
  exitFailure

pipeName :: String
pipeName = "\\\\.\\pipe\\named-pipe-demo"

putStrLn_ :: String -> IO ()
putStrLn_ = BSC.putStrLn . BSC.pack

server :: IO ()
server = do

  putStrLn_ "creating pipe..."
  hpipe <- createNamedPipe pipeName
                           pIPE_ACCESS_DUPLEX
                           (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                           pIPE_UNLIMITED_INSTANCES
                           512
                           512
                           0
                           Nothing
  putStrLn_ "created pipe, waiting for client"
  connectNamedPipe hpipe Nothing
  putStrLn_ "client connected"
  _ <- forkIO $ do
         putStrLn_ "starting client conversation"
         serverLoop hpipe
           `finally` (do putStrLn "client disconnected"
                         closePipe hpipe)
  threadDelay 1
  server


encodeMsg :: String -> ByteString
encodeMsg msg = BSC.pack (msg ++ "\n")

serverLoop :: HANDLE -> IO ()
serverLoop hpipe = do
  _ <- writePipe hpipe (encodeMsg "Hi! >")
  putStrLn "Sent prompt, awaiting reply"
  resp <- pGetLine hpipe
  putStrLn $ "received: " ++ show resp
  let reply = "reversed: " ++ show (reverse resp)
  putStrLn $ "replying: " ++ show reply
  _ <- writePipe hpipe (encodeMsg reply)
  serverLoop hpipe

client :: IO ()
client = do
  hpipe <- createFile pipeName
                      (gENERIC_READ .|. gENERIC_WRITE)
                      fILE_SHARE_NONE
                      Nothing
                      oPEN_EXISTING
                      fILE_ATTRIBUTE_NORMAL
                      Nothing
  putStrLn "opened pipe"
  clientLoop hpipe
    `finally` closePipe hpipe

clientLoop :: HANDLE -> IO ()
clientLoop hpipe = do
  putStrLn "awaiting prompt..."
  prompt <- pGetLine hpipe
  putStrLn prompt
  reply <- getLine
  case reply of
    "quit" -> return ()
    _      -> do putStrLn $ "sending reply: " ++ show reply
                 _ <- writePipe hpipe (encodeMsg reply)
                 putStrLn "reply sent"
                 putStrLn "reply flushed"
                 resp <- pGetLine hpipe
                 putStrLn $ "response: " ++ resp
                 clientLoop hpipe

