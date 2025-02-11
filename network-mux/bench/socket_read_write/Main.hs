{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}


import Control.Exception (bracket)
import Control.Concurrent.Class.MonadSTM.Strict
import Data.Functor (void)
import Control.Monad (forever, replicateM_)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer
import Data.Int
import Network.Socket qualified as Socket
import Network.Socket (Socket)
import Data.ByteString.Lazy qualified as BL
import Test.Tasty.Bench

import Network.Mux.Bearer
import Network.Mux
import Network.Mux.Types
import Network.Mux.Channel

import Network.Mux.Timeout (withTimeoutSerial)

sduTimeout :: DiffTime
sduTimeout = 10

numberOfPackets :: Int64
numberOfPackets = 100000

totalPayloadLen :: Int64 -> Int64
totalPayloadLen sndSize = sndSize * numberOfPackets

-- | Run a client that connects to the specified addr.
-- Signals the message sndSize to the server by writing it
-- in the provided TMVar.
readBenchmark :: StrictTMVar IO Int64 -> Int64 -> Socket.SockAddr -> IO ()
readBenchmark sndSizeV sndSize addr = do
  bracket
    (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
    Socket.close
    (\sd -> do
      atomically $ putTMVar sndSizeV sndSize
      Socket.connect sd addr
      bearer <- getBearer makeSocketBearer sduTimeout nullTracer sd

      let chan = muxBearerAsChannel bearer (MiniProtocolNum 42) InitiatorDir
      doRead (totalPayloadLen sndSize) chan 0
    )
 where
   doRead :: Int64 -> Channel IO -> Int64 -> IO ()
   doRead maxData _ cnt | cnt >= maxData = return ()
   doRead maxData chan !cnt = do
     msg_m <- recv chan
     case msg_m of
          Just msg -> do
             doRead maxData chan (cnt + BL.length msg)
          Nothing -> error "doRead: nullread"

-- | Run a server that accept connections on `ad`.
startServer :: StrictTMVar IO Int64 -> Socket -> IO ()
startServer sndSizeV ad = forever $ do
    (sd, _) <- Socket.accept ad
    bearer <- getBearer makeSocketBearer sduTimeout nullTracer sd
    sndSize <- atomically $ takeTMVar sndSizeV

    let chan = muxBearerAsChannel bearer (MiniProtocolNum 42) ResponderDir
        payload = BL.replicate sndSize 0xa5
        maxData = totalPayloadLen sndSize
        numberOfSdus = fromIntegral $ maxData `div` sndSize
    replicateM_ numberOfSdus $ do
      send chan payload

-- | Like startServer but it uses the `writeMany` function
-- for vector IO.
startServerMany :: StrictTMVar IO Int64 -> Socket -> IO ()
startServerMany sndSizeV ad = forever $ do
    (sd, _) <- Socket.accept ad
    bearer <- getBearer makeSocketBearer sduTimeout nullTracer sd
    sndSize <- atomically $ takeTMVar sndSizeV

    let maxData = totalPayloadLen sndSize
        numberOfSdus = fromIntegral $ maxData `div` sndSize
        numberOfCalls = numberOfSdus `div` 10
        runtSdus = numberOfSdus `mod` 10

    withTimeoutSerial $ \timeoutFn -> do
      replicateM_ numberOfCalls $ do
        let sdus = replicate 10 $ wrap $ BL.replicate sndSize 0xa5
        void $ writeMany bearer timeoutFn sdus
      if runtSdus > 0
         then do
           let sdus = replicate runtSdus $ wrap $ BL.replicate sndSize 0xa5
           void $ writeMany bearer timeoutFn sdus
         else return ()

 where
  -- wrap a 'ByteString' as 'MuxSDU'
  wrap :: BL.ByteString -> MuxSDU
  wrap blob = MuxSDU {
        -- it will be filled when the 'MuxSDU' is send by the 'bearer'
       msHeader = MuxSDUHeader {
           mhTimestamp = RemoteClockModel 0,
           mhNum       = MiniProtocolNum 42,
           mhDir       = ResponderDir,
           mhLength    = fromIntegral $ BL.length blob
          },
        msBlob = blob
     }

setupServer :: Socket -> IO Socket.SockAddr
setupServer ad = do
  muxAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
  Socket.setSocketOption ad Socket.ReuseAddr 1
  Socket.bind ad (Socket.addrAddress muxAddress)
  addr <- Socket.getSocketName ad
  Socket.listen ad 3

  return addr

-- Main function to run the benchmarks
main :: IO ()
main = do
    bracket
      (do
        ad1 <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        ad2 <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol

        return (ad1, ad2)
      )
      (\(ad1, ad2) -> do
        Socket.close ad1
        Socket.close ad2
      )
      (\(ad1, ad2) -> do
        sndSizeV <- newEmptyTMVarIO
        sndSizeMV <- newEmptyTMVarIO
        addr <- setupServer ad1
        addrM <- setupServer ad2

        withAsync (startServer sndSizeV ad1) $ \said -> do
          withAsync (startServerMany sndSizeMV ad2) $ \saidM -> do

            defaultMain [
                -- Suggested Max SDU size for Socket bearer
                bench "Read/Write Benchmark 12288 byte SDUs"  $ nfIO $ readBenchmark sndSizeV 12288 addr
                -- Payload size for ChainSync's RequestNext
              , bench "Read/Write Benchmark 914 byte SDUs"  $ nfIO $ readBenchmark sndSizeV 914 addr
              -- Payload size for ChainSync's RequestNext
              , bench "Read/Write Benchmark 10 byte SDUs"  $ nfIO $ readBenchmark sndSizeV 10 addr

                -- Send batches of SDUs at the same time
              , bench "Read/Write-Many Benchmark 12288 byte SDUs"  $ nfIO $ readBenchmark sndSizeMV 12288 addrM
              , bench "Read/Write-Many Benchmark 914 byte SDUs"  $ nfIO $ readBenchmark sndSizeMV 914 addrM
              , bench "Read/Write-Many Benchmark 10 byte SDUs"  $ nfIO $ readBenchmark sndSizeMV 10 addrM
              ]
            cancel said
            cancel saidM
      )
