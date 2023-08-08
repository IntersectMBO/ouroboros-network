{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.KESAgent.POC
  where

import Cardano.KESAgent.API
import Cardano.KESAgent.DirectReadWrite

import Control.Concurrent
import Control.Concurrent.Async
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Random.Stateful
import System.Socket
import System.Socket.Family.Unix
import System.Socket.Protocol.Default
import System.Socket.Type.Stream

data Message =
  Message
    { messageValue :: Word32
    }
    deriving (Show)

instance DirectWrite Message where
  directWrite fetchBytes =
    alloca (\ptr -> do
      fetchBytes ptr (fromIntegral $ sizeOf (undefined :: Word32))
      Message <$> peek (castPtr ptr)
    )

instance DirectRead Message where
  directRead putBytes msg =
    alloca (\ptr -> do
      poke (castPtr ptr) (messageValue msg)
      putBytes ptr (fromIntegral $ sizeOf (undefined :: Word32))
    )

runPOC :: IO ()
runPOC =
  concurrently_
    pocServer
    pocClient

pocServer :: IO ()
pocServer = do
  gen <- newIOGenM =<< getStdGen

  let getCurrent = Message <$> uniformWord32 gen
      getNext = threadDelay 1000000 >> Message <$> uniformWord32 gen

  s <- socket @Unix @Stream @Default
  runServer s
    (maybe (error "NOPE") id $ socketAddressUnixAbstract "KESAgent/POC")
    getCurrent
    getNext

pocClient :: IO ()
pocClient = do
  threadDelay 1000000
  s <- socket @Unix @Stream @Default
  runClient s
    (maybe (error "NOPE") id $ socketAddressUnixAbstract "KESAgent/POC")
    (print . messageValue)
