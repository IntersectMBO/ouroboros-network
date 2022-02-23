module Ouroboros.Network.Out
  ( putStrLn
  , bracket
  ) where

import qualified System.Environment as IO
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

import Prelude hiding (putStrLn)

hOut :: IO.Handle
hOut = IO.unsafePerformIO $ do
  Just home <- IO.lookupEnv "HOME"
  let logFile = home <> "/output.ouroboros.log"
  IO.openFile logFile IO.WriteMode
{-# NOINLINE hOut #-}

putStrLn :: String -> IO ()
putStrLn s = do
  IO.hPutStrLn hOut s
  IO.hFlush hOut

bracket :: String -> IO () -> IO ()
bracket s f = do
  putStrLn $ "BEGIN " <> s
  f
  putStrLn $ "END " <> s
