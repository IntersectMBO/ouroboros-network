{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

#if defined(mingw32_HOST_OS)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import System.IO hiding (hGetLine)
import System.Exit
import           System.Win32 (HANDLE)
import qualified System.Win32.NamedPipes as Win32
import qualified System.Win32 as Win32
import qualified System.Win32.Async as Win32
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
server = Win32.withIOManager loop
  where
    loop iocp = do
      putStrLn_ "creating pipe..."
      hpipe <- Win32.createNamedPipe pipeName
                                     (Win32.pIPE_ACCESS_DUPLEX .|. Win32.fILE_FLAG_OVERLAPPED)
                                     (Win32.pIPE_TYPE_BYTE .|. Win32.pIPE_READMODE_BYTE)
                                     Win32.pIPE_UNLIMITED_INSTANCES
                                     512
                                     512
                                     0
                                     Nothing
      Win32.associateWithIOCompletionPort hpipe iocp
      putStrLn_ "created pipe, waiting for client"
      Win32.connectNamedPipe hpipe
      putStrLn_ "client connected"
      _ <- forkIO $ do
             putStrLn_ "starting client conversation"
             serverLoop hpipe
               `finally` (do putStrLn "client disconnected"
                             Win32.closeHandle hpipe)
      threadDelay 1
      loop iocp


encodeMsg :: String -> ByteString
encodeMsg msg = BSC.pack (msg ++ "\n")

hGetLine :: HANDLE
         -> IO String
hGetLine h = go ""
    where
      go :: String -> IO String
      go !s = do
        [x] <- BSC.unpack <$> Win32.readHandle h 1
        if x == '\n'
          then pure (reverse s)
          else go (x : s)

serverLoop :: HANDLE -> IO ()
serverLoop hpipe = do
  _ <- Win32.writeHandle hpipe (encodeMsg "Hi! >")
  putStrLn "Sent prompt, awaiting reply"
  resp <- hGetLine hpipe
  putStrLn $ "received: " ++ show resp
  let reply = "reversed: " ++ show (reverse resp)
  putStrLn $ "replying: " ++ show reply
  _ <- Win32.writeHandle hpipe (encodeMsg reply)
  serverLoop hpipe

client :: IO ()
client = Win32.withIOManager $ \iocp -> do
    hpipe <- Win32.createFile pipeName
                              (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE)
                              Win32.fILE_SHARE_NONE
                              Nothing
                              Win32.oPEN_EXISTING
                              Win32.fILE_FLAG_OVERLAPPED
                              Nothing
    Win32.associateWithIOCompletionPort hpipe iocp
    putStrLn "opened pipe"
    clientLoop hpipe
      `finally` Win32.closeHandle hpipe

clientLoop :: HANDLE -> IO ()
clientLoop hpipe = do
  putStrLn "awaiting prompt..."
  prompt <- hGetLine hpipe
  putStrLn prompt
  reply <- getLine
  case reply of
    "quit" -> return ()
    _      -> do putStrLn $ "sending reply: " ++ show reply
                 _ <- Win32.writeHandle hpipe (encodeMsg reply)
                 putStrLn "reply sent"
                 putStrLn "reply flushed"
                 resp <- hGetLine hpipe
                 putStrLn $ "response: " ++ resp
                 clientLoop hpipe
#else
main :: IO ()
main = putStrLn "named-pipe-demo is only supported on Windows"
#endif
