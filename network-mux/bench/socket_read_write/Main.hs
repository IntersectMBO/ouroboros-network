{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}


import Control.Exception (bracket)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forever, replicateM_)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer
import Data.Int
import qualified Network.Socket as Socket
import Network.Socket (Socket)
import qualified Data.ByteString.Lazy as BL
import Test.Tasty.Bench

import Network.Mux.Bearer
import Network.Mux
import Network.Mux.Types

sduTimeout :: DiffTime
sduTimeout = 10

numberOfPackets :: Int64
numberOfPackets = 100000

totalPayloadLen :: Int64 -> Int64
totalPayloadLen sndSize = sndSize * numberOfPackets

readBenchmark :: StrictTMVar IO Int64 -> Int64 -> Socket.SockAddr -> IO ()
readBenchmark sndSizeV sndSize addr = do
  bracket
    (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
    Socket.close
    (\sd -> do
      atomically $ putTMVar sndSizeV sndSize
      Socket.connect sd addr
      bearer <- getBearer makeSocketBearer sduTimeout nullTracer sd

      let chan = bearerAsChannel bearer (MiniProtocolNum 42) InitiatorDir
      doRead (totalPayloadLen sndSize) chan 0
    )
 where
   doRead :: Int64 -> ByteChannel IO -> Int64 -> IO ()
   doRead maxData _ cnt | cnt >= maxData = return ()
   doRead maxData chan !cnt = do
     msg_m <- recv chan
     case msg_m of
          Just msg -> do
             doRead maxData chan (cnt + BL.length msg)
          Nothing -> error "doRead: nullread"


-- Start the server in a separate thread
startServer :: StrictTMVar IO Int64 -> Socket -> IO ()
startServer sndSizeV ad = forever $ do

    (sd, _) <- Socket.accept ad
    bearer <- getBearer makeSocketBearer sduTimeout nullTracer sd
    sndSize <- atomically $ takeTMVar sndSizeV

    let chan = bearerAsChannel bearer (MiniProtocolNum 42) ResponderDir
        payload = BL.replicate sndSize 0xa5
        -- maxData = totalPayloadLen bearer
        maxData = totalPayloadLen sndSize
        numberOfSdus = fromIntegral $ maxData `div` sndSize
    replicateM_ numberOfSdus $ do
      send chan payload

-- Main function to run the benchmarks
main :: IO ()
main = do
    -- Start the server in a separate thread

    bracket
      (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
      Socket.close
      (\ad -> do
        sndSizeV <- newEmptyTMVarIO
        muxAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
        Socket.setSocketOption ad Socket.ReuseAddr 1
        Socket.bind ad (Socket.addrAddress muxAddress)
        addr <- Socket.getSocketName ad
        Socket.listen ad 3

        withAsync (startServer sndSizeV ad) $ \said -> do

          defaultMain [
              -- Suggested Max SDU size for Socket bearer
              bench "Read/Write Benchmark 12288 byte SDUs"  $ nfIO $ readBenchmark sndSizeV 12288 addr
              -- Payload size for ChainSync's RequestNext
            , bench "Read/Write Benchmark 914 byte SDUs"  $ nfIO $ readBenchmark sndSizeV 914 addr
              -- Payload size for ChainSync's RequestNext
            , bench "Read/Write Benchmark 10 byte SDUs"  $ nfIO $ readBenchmark sndSizeV 10 addr
            ]
          cancel said
      )
