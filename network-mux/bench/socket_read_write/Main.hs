{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (bracket)
import Control.Monad (forever, replicateM_, unless, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Functor (void)
import Data.Int
import Data.Strict.Tuple as Strict (Pair ((:!:)))
import Data.Word
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString.Lazy qualified as Socket (recv)
import Test.Tasty.Bench

import Network.Mux
import Network.Mux.Bearer
import Network.Mux.Egress
import Network.Mux.Ingress
import Network.Mux.Timeout (withTimeoutSerial)
import Network.Mux.Types

activeTracer :: Tracer IO a
activeTracer = nullTracer
--activeTracer = showTracing stdoutTracer

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
      withReadBufferIO (\buffer -> do
        bearer <- getBearer makeSocketBearer sduTimeout activeTracer sd buffer

        let chan = bearerAsChannel bearer (MiniProtocolNum 42) InitiatorDir
        doRead chan 0
       )
    )
 where
   doRead :: ByteChannel IO -> Int64 -> IO ()
   doRead _ cnt | cnt >= totalPayloadLen sndSize = return ()
   doRead  chan !cnt = do
     msg_m <- recv chan
     case msg_m of
          Just msg -> doRead chan (cnt + BL.length msg)
          Nothing  -> error "doRead: nullread"

-- | Like readDemuxerBenchmark but it doesn't empty the ingress queue until
-- all data has been sent.
readDemuxerQueueBenchmark :: StrictTMVar IO Int64 -> Int64 -> Socket.SockAddr -> IO ()
readDemuxerQueueBenchmark sndSizeV sndSize addr = do
  bracket
    (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
    Socket.close
    (\sd -> do
      atomically $ putTMVar sndSizeV sndSize

      Socket.connect sd addr
      withReadBufferIO (\buffer -> do
        bearer <- getBearer makeSocketBearer sduTimeout activeTracer sd buffer
        ms42 <- mkMiniProtocolState 42
        withAsync (demuxer [ms42] bearer) $ \aid -> do
          doRead 0xa5 (totalPayloadLen sndSize) (miniProtocolIngressQueue ms42)
          cancel aid
       )
    )
 where
   doRead :: Word8 -> Int64 -> StrictTVar IO (Strict.Pair Int64 Builder) -> IO ()
   doRead tag maxData queue = do
     msg <- atomically $ do
       l :!: b <- readTVar queue
       if l == maxData
          then
            return (toLazyByteString b)
          else
            retry
     if BL.all ( == tag) msg
        then return ()
        else error "corrupt stream"

-- Like readBenchmark but uses a demuxer thread
readDemuxerBenchmark :: StrictTMVar IO Int64 -> Int64 -> Socket.SockAddr -> IO ()
readDemuxerBenchmark sndSizeV sndSize addr = do
  bracket
    (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
    Socket.close
    (\sd -> do
      atomically $ putTMVar sndSizeV sndSize

      Socket.connect sd addr
      withReadBufferIO (\buffer -> do
        bearer <- getBearer makeSocketBearer sduTimeout activeTracer sd buffer
        ms42 <- mkMiniProtocolState 42
        ms41 <- mkMiniProtocolState 41
        withAsync (demuxer [ms41, ms42] bearer) $ \aid -> do
          withAsync (doRead 42 (totalPayloadLen sndSize) (miniProtocolIngressQueue ms42) 0) $ \aid42 -> do
            withAsync (doRead 41 (totalPayloadLen 10) (miniProtocolIngressQueue ms41) 0) $ \aid41 -> do
              _ <- waitBoth aid42 aid41
              cancel aid
              return ()
       )
    )
 where
   doRead :: Word8 -> Int64 -> StrictTVar IO (Strict.Pair Int64 Builder) -> Int64 -> IO ()
   doRead _ maxData _ cnt | cnt >= maxData = return ()
   doRead tag maxData queue !cnt = do
     msg <- atomically $ do
       l :!: b <- readTVar queue
       if l == 0
          then retry
          else do
            writeTVar queue $ 0 :!: mempty
            return (toLazyByteString b)
     if BL.all ( == tag) msg
        then doRead tag maxData queue (cnt + BL.length msg)
        else error "corrupt stream"

mkMiniProtocolState :: MonadSTM m => Word16 -> m (MiniProtocolState 'InitiatorMode m)
mkMiniProtocolState num = do
  mpq <- newTVarIO $ 0 :!: mempty
  mpv    <- newTVarIO StatusRunning

  let mpi = MiniProtocolInfo (MiniProtocolNum num) InitiatorDirectionOnly
                             (MiniProtocolLimits maxBound) Nothing
  return $ MiniProtocolState mpi mpq mpv

-- | Run a server that accept connections on `ad`.
startServer :: StrictTMVar IO Int64 -> Socket -> IO ()
startServer sndSizeV ad = forever $ do
    (sd, _) <- Socket.accept ad
    withReadBufferIO (\buffer -> do
      bearer <- getBearer makeSocketBearer sduTimeout activeTracer sd buffer
      sndSize <- atomically $ takeTMVar sndSizeV

      let chan = bearerAsChannel bearer (MiniProtocolNum 42) ResponderDir
          payload = BL.replicate sndSize 0xa5
          maxData = totalPayloadLen sndSize
          numberOfSdus = fromIntegral $ maxData `div` sndSize
      replicateM_ numberOfSdus $ do
        send chan payload
     )
-- | Like startServer but it uses the `writeMany` function
-- for vector IO.
startServerMany :: StrictTMVar IO Int64 -> Socket -> IO ()
startServerMany sndSizeV ad = forever $ do
    (sd, _) <- Socket.accept ad
    withReadBufferIO (\buffer -> do
      bearer <- getBearer makeSocketBearer sduTimeout activeTracer sd buffer
      sndSize <- atomically $ takeTMVar sndSizeV

      let maxData = totalPayloadLen sndSize
          numberOfSdus = fromIntegral $ maxData `div` sndSize
          numberOfCalls = numberOfSdus `div` 10
          runtSdus = numberOfSdus `mod` 10

      withTimeoutSerial $ \timeoutFn -> do
        replicateM_ numberOfCalls $ do
          let sdus = replicate 10 $ wrap $ BL.replicate sndSize 0xa5
          void $ writeMany bearer timeoutFn sdus
        when (runtSdus > 0) $ do
          let sdus = replicate runtSdus $ wrap $ BL.replicate sndSize 0xa5
          void $ writeMany bearer timeoutFn sdus
     )
 where
  -- wrap a 'ByteString' as 'SDU'
  wrap :: BL.ByteString -> SDU
  wrap blob = SDU {
        -- it will be filled when the 'SDU' is send by the 'bearer'
       msHeader = SDUHeader {
           mhTimestamp = RemoteClockModel 0,
           mhNum       = MiniProtocolNum 42,
           mhDir       = ResponderDir,
           mhLength    = fromIntegral $ BL.length blob
          },
        msBlob = blob
     }

-- | Run a server that accept connections on `ad`.
-- It will send streams of data over the 41 and 42 miniprotocol.
-- Multiplexing is done with a separate thread running
-- the Egress.muxer function.
startServerEgresss :: DiffTime -> StrictTMVar IO Int64 -> Socket -> IO ()
startServerEgresss pollInterval sndSizeV ad = forever $ do
    (sd, _) <- Socket.accept ad
    withReadBufferIO (\buffer -> do
      bearer <- getBearer (makeSocketBearer' pollInterval) sduTimeout activeTracer sd buffer
      sndSize <- atomically $ takeTMVar sndSizeV
      eq <- atomically $ newTBQueue 100
      w42 <- newTVarIO BL.empty
      w41 <- newTVarIO BL.empty

      let maxData = totalPayloadLen sndSize
          numberOfSdus = fromIntegral $ maxData `div` sndSize
          numberOfCalls = numberOfSdus `div` 10 :: Int
          runtSdus = numberOfSdus `mod` 10 :: Int

      withAsync (muxer eq bearer) $ \aid -> do

        replicateM_ numberOfCalls $ do
          let payload42s = replicate 10 $ BL.replicate sndSize 42
          let payload41s = replicate 10 $ BL.replicate 10 41
          mapM_ (sendToMux w42 eq (MiniProtocolNum 42) ResponderDir) payload42s
          mapM_ (sendToMux w41 eq (MiniProtocolNum 41) ResponderDir) payload41s
        when (runtSdus > 0) $ do
          let payload42s = replicate runtSdus $ BL.replicate sndSize 42
          let payload41s = replicate runtSdus $ BL.replicate 10 41
          mapM_ (sendToMux w42 eq (MiniProtocolNum 42) ResponderDir) payload42s
          mapM_ (sendToMux w41 eq (MiniProtocolNum 41) ResponderDir) payload41s

        -- Wait for the egress queue to empty
        atomically $ do
          r42 <- readTVar w42
          r41 <- readTVar w42
          unless (BL.null r42 || BL.null r41) retry

        -- when the client is done they will close the socket
        -- and we will read zero bytes.
        _ <- Socket.recv sd 128

        cancel aid
     )
  where
    sendToMux :: StrictTVar IO BL.ByteString -> EgressQueue IO -> MiniProtocolNum -> MiniProtocolDir
              -> BL.ByteString -> IO ()
    sendToMux w eq mc md msg = do
     atomically $ do
      buf <- readTVar w
      if BL.length buf < 0x3ffff
         then do
           let wasEmpty = BL.null buf
           writeTVar w (BL.append buf msg)
           when wasEmpty $
             writeTBQueue eq (TLSRDemand mc md $ Wanton w)
         else retry

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
        ad3 <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        ad4 <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol

        return (ad1, ad2, ad3, ad4)
      )
      (\(ad1, ad2, ad3, ad4) -> do
        Socket.close ad1
        Socket.close ad2
        Socket.close ad3
        Socket.close ad4
      )
      (\(ad1, ad2, ad3, ad4) -> do
        sndSizeV <- newEmptyTMVarIO
        sndSizeMV <- newEmptyTMVarIO
        sndSizeEV <- newEmptyTMVarIO
        addr <- setupServer ad1
        addrM <- setupServer ad2
        addrE <- setupServer ad3
        addrF <- setupServer ad4

        withAsync (startServer sndSizeV ad1) $ \said -> do
          withAsync (startServerMany sndSizeMV ad2) $ \saidM -> do
            withAsync (startServerEgresss 0.001 sndSizeEV ad3) $ \saidE -> withAsync (startServerEgresss 0 sndSizeEV ad4) $ \saidF -> do
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

                  -- Use standard muxer and demuxer, 1ms poll
                , bench "Read/Write Mux Benchmark 800+10 byte SDUs, 1ms Poll"  $ nfIO $ readDemuxerBenchmark sndSizeEV 800 addrE
                , bench "Read/Write Mux Benchmark 12288+10 byte SDUs, 1ms Poll"  $ nfIO $ readDemuxerBenchmark sndSizeEV 12288 addrE

                  -- Use standard muxer and demuxer, 0ms poll
                , bench "Read/Write Mux Benchmark 800+10 byte SDUs, 0ms Poll"  $ nfIO $ readDemuxerBenchmark sndSizeEV 800 addrF
                , bench "Read/Write Mux Benchmark 12288+10 byte SDUs, 0ms Poll"  $ nfIO $ readDemuxerBenchmark sndSizeEV 12288 addrF

                  -- Use standard demuxer
                , bench "Read/Write Demuxer Queuing Benchmark 10 byte SDUs"  $ nfIO $ readDemuxerQueueBenchmark sndSizeV 10 addr
                , bench "Read/Write Demuxer Queuing Benchmark 256 byte SDUs"  $ nfIO $ readDemuxerQueueBenchmark sndSizeV 256 addr
                ]
              cancel said
              cancel saidM
              cancel saidE
              cancel saidF
      )
