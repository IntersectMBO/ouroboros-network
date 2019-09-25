{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bits
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
  hpipe' <- pipeToHandle hpipe pipeName ReadWriteMode
  _ <- forkIO $ do
         putStrLn_ "starting client conversation"
         serverLoop hpipe hpipe'
           `finally` (do putStrLn "client disconnected"
                         hClose hpipe')
  threadDelay 1
  server

serverLoop :: HANDLE -> Handle -> IO ()
serverLoop hPIPE hpipe = do
  hPutStrLn hpipe "Hi! >"
  putStrLn "Sent prompt, awaiting reply"
  resp <- pGetLine hPIPE
  putStrLn $ "received: " ++ show resp
  let reply = "reversed: " ++ show (reverse resp)
  putStrLn $ "replying: " ++ show reply
  hPutStrLn hpipe reply
  serverLoop hPIPE hpipe

client :: IO ()
client = do
  hPIPE <- createFile pipeName
                      (gENERIC_READ .|. gENERIC_WRITE)
                      fILE_SHARE_NONE
                      Nothing
                      oPEN_EXISTING
                      fILE_ATTRIBUTE_NORMAL
                      Nothing
  putStrLn "opened pipe"
  hpipe <- pipeToHandle hPIPE pipeName ReadWriteMode
  clientLoop hPIPE hpipe
    `finally` hClose hpipe

clientLoop :: HANDLE -> Handle -> IO ()
clientLoop hPIPE hpipe = do
  putStrLn "awaiting prompt..."
  prompt <- pGetLine hPIPE
  putStrLn prompt
  reply <- getLine
  case reply of
    "quit" -> return ()
    _      -> do putStrLn $ "sending reply: " ++ show reply
                 hPutStrLn hpipe reply
                 putStrLn "reply sent"
                 putStrLn "reply flushed"
                 resp <- pGetLine hPIPE
                 putStrLn $ "response: " ++ resp
                 clientLoop hPIPE hpipe

