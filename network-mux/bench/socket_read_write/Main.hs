{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (bracket)
import Control.Monad (forM, forM_, forever, replicateM, replicateM_, unless, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Functor (void)
import Data.Int
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Strict.Tuple as Strict (Pair ((:!:)))
import Data.Word
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString.Lazy qualified as Socket (recv)
import Test.Tasty.Bench

import Network.Mux
import Network.Mux.Bearer
import Network.Mux.Egress
import Network.Mux.Egress.Bucket (awaitGrant, registerBearer, setRankSource)
import Network.Mux.Ingress
import Network.Mux.Timeout (withTimeoutSerial)
import Network.Mux.Types

activeTracer :: Tracer IO a
activeTracer = nullTracer
--activeTracer = show >$< stdoutTracer

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
        bearer <- getBearer makeSocketBearer sduTimeout sd buffer

        let chan = bearerAsChannel activeTracer bearer (MiniProtocolNum 42) InitiatorDir
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
        bearer <- getBearer makeSocketBearer sduTimeout sd buffer
        ms42 <- mkMiniProtocolState 42
        withAsync (demuxer [ms42] activeTracer bearer) $ \aid -> do
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
        bearer <- getBearer makeSocketBearer sduTimeout sd buffer
        ms42 <- mkMiniProtocolState 42
        ms41 <- mkMiniProtocolState 41
        withAsync (demuxer [ms41, ms42] activeTracer bearer) $ \aid -> do
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
      bearer <- getBearer makeSocketBearer sduTimeout sd buffer
      sndSize <- atomically $ takeTMVar sndSizeV

      let chan = bearerAsChannel activeTracer bearer (MiniProtocolNum 42) ResponderDir
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
      bearer <- getBearer makeSocketBearer sduTimeout sd buffer
      sndSize <- atomically $ takeTMVar sndSizeV

      let maxData = totalPayloadLen sndSize
          numberOfSdus = fromIntegral $ maxData `div` sndSize
          numberOfCalls = numberOfSdus `div` 10
          runtSdus = numberOfSdus `mod` 10

      withTimeoutSerial $ \timeoutFn -> do
        replicateM_ numberOfCalls $ do
          let sdus = replicate 10 $ wrap $ BL.replicate sndSize 0xa5
          void $ writeMany bearer activeTracer timeoutFn sdus
        when (runtSdus > 0) $ do
          let sdus = replicate runtSdus $ wrap $ BL.replicate sndSize 0xa5
          void $ writeMany bearer activeTracer timeoutFn sdus
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
startServerEgresss :: DiffTime -> IO (LaneEgress IO) -> StrictTMVar IO Int64 -> Socket -> IO ()
startServerEgresss pollInterval mkLane sndSizeV ad = forever $ do
    (sd, _) <- Socket.accept ad
    withReadBufferIO (\buffer -> do
      bearer <- getBearer (makeSocketBearer' pollInterval) sduTimeout sd buffer
      sndSize <- atomically $ takeTMVar sndSizeV
      eq <- atomically $ newTBQueue 100
      w42 <- newTVarIO BL.empty
      w41 <- newTVarIO BL.empty

      let maxData = totalPayloadLen sndSize
          numberOfSdus = fromIntegral $ maxData `div` sndSize
          numberOfCalls = numberOfSdus `div` 10 :: Int
          runtSdus = numberOfSdus `mod` 10 :: Int

      laneEgress <- mkLane
      withAsync (muxer eq activeTracer Nothing laneEgress bearer) $ \aid -> do

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

-- | A scheduled lane on its own unlimited budget: every batch takes the fast
-- path, so the difference to 'Unscheduled' is the scheduler's per-batch cost.
scheduledLane :: IO (LaneEgress IO)
scheduledLane = do
    bucket <- newBucket 1e15 (1024 * 1024) Nothing
    -- nothing charges what the benchmark sends
    sink <- newTVarIO (\_ _ _ -> return ())
    Scheduled_ <$> registerBearer bucket <*> pure sink

-- | The tier of a peer as a rule over the local root groups, evaluated in the
-- transaction that queues the bearer, against a stored rank.
rankRule :: StrictTVar IO [(Int, Int, Map Socket.SockAddr ())] -> Socket.SockAddr -> IO Rank
rankRule groupsVar addr = atomically (rankRuleSTM groupsVar addr)

rankRuleSTM :: StrictTVar IO [(Int, Int, Map Socket.SockAddr ())] -> Socket.SockAddr -> STM IO Rank
rankRuleSTM groupsVar addr = do
    groups <- readTVar groupsVar
    return $ if any (\(_, _, m) -> Map.member addr m) groups
                then Rank 0
                else Rank 1

-- | @g@ groups of @n@ distinct addresses each.
localRootGroups :: Int -> Int -> [(Int, Int, Map Socket.SockAddr ())]
localRootGroups g n =
    [ (i, i, Map.fromList [ (rootAddr i j, ()) | j <- [1 .. n] ]) | i <- [1 .. g] ]

rootAddr :: Int -> Int -> Socket.SockAddr
rootAddr i j = Socket.SockAddrInet (fromIntegral (3000 + i)) (fromIntegral (i * 1000 + j))

-- | @bearers@ contending for one budget, each taking @grants `div` bearers@
-- batches of 128 KiB: at 10 GB/s every take joins the queue and sleeps for
-- its tokens; unlimited, the queue only forms under contention.
bucketContention :: Int -> Double -> Maybe Rotation
                 -> Maybe (StrictTVar IO [(Int, Int, Map Socket.SockAddr ())])
                 -- ^ rank the bearers by this rule, half of them members
                 -> Int -> IO ()
bucketContention bearers rate rotation rule grants = do
    bucket <- newBucket rate (2 * batch) rotation
    hs <- replicateM bearers (registerBearer bucket)
    case rule of
         Just groupsVar ->
           forM_ (zip [0 :: Int ..] hs) $ \(i, h) ->
             atomically $ setRankSource h $ \_ -> rankRuleSTM groupsVar $
               if even i then rootAddr 3 (i `mod` 8 + 1) else rootAddr 0 i
         Nothing -> return ()
    as <- forM hs $ \h -> async (replicateM_ (grants `div` bearers) (awaitGrant h batch))
    mapM_ wait as
  where
    batch = 131072

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
        ad5 <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol

        return (ad1, ad2, ad3, ad4, ad5)
      )
      (\(ad1, ad2, ad3, ad4, ad5) -> do
        Socket.close ad1
        Socket.close ad2
        Socket.close ad3
        Socket.close ad4
        Socket.close ad5
      )
      (\(ad1, ad2, ad3, ad4, ad5) -> do
        sndSizeV <- newEmptyTMVarIO
        sndSizeMV <- newEmptyTMVarIO
        sndSizeEV <- newEmptyTMVarIO
        addr <- setupServer ad1
        addrM <- setupServer ad2
        addrE <- setupServer ad3
        addrF <- setupServer ad4
        addrS <- setupServer ad5
        -- the tier decision, stored and as a rule over generated local roots
        rankVar <- newTVarIO (Rank 1)
        ruleVar <- newTVarIO (localRootGroups 3 8)
        rankBenches <- forM [(1, 8), (3, 8), (3, 64), (10, 64)] $ \(g, n) -> do
          groupsVar <- newTVarIO (localRootGroups g n)
          let label = show g ++ " groups of " ++ show n
          return [ bench (label ++ ", member")   $ whnfIO (rankRule groupsVar (rootAddr g n))
                 , bench (label ++ ", stranger") $ whnfIO (rankRule groupsVar (rootAddr 0 0)) ]

        withAsync (startServer sndSizeV ad1) $ \said -> do
          withAsync (startServerMany sndSizeMV ad2) $ \saidM -> do
            withAsync (startServerEgresss 0.001 (pure Unscheduled) sndSizeEV ad3) $ \saidE ->
             withAsync (startServerEgresss 0 (pure Unscheduled) sndSizeEV ad4) $ \saidF ->
             withAsync (startServerEgresss 0 scheduledLane sndSizeEV ad5) $ \saidS -> do
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
                , bgroup "Egress" [
                    -- a bearer's tier as it joins the queue: today's stored rank
                    -- against a rule over the local root groups
                    bgroup "rank" $
                      bench "stored" (whnfIO (atomically (readTVar rankVar)))
                      : concat rankBenches
                    -- the slow path: bearers queue for one budget
                  , bgroup "contention" [
                      bench "8 bearers, 10 GB/s"               $ nfIO $ bucketContention 8  10e9 Nothing Nothing 4096
                    , bench "64 bearers, 10 GB/s"              $ nfIO $ bucketContention 64 10e9 Nothing Nothing 4096
                    , bench "64 bearers, 10 GB/s, rank rule"   $ nfIO $ bucketContention 64 10e9 Nothing (Just ruleVar) 4096
                    , bench "8 bearers, unlimited"             $ nfIO $ bucketContention 8  1e15 Nothing Nothing 4096
                    , bench "64 bearers, unlimited"            $ nfIO $ bucketContention 64 1e15 Nothing Nothing 4096
                    , bench "64 bearers, unlimited, rotation"  $ nfIO $ bucketContention 64 1e15 (Just (Rotation 42 599)) Nothing 4096
                    , bench "64 bearers, unlimited, rank rule" $ nfIO $ bucketContention 64 1e15 Nothing (Just ruleVar) 4096
                    ]
                    -- the fast path over a socket: the unscheduled twins are the
                    -- 0ms Poll Mux Benchmarks above
                  , bench "Read/Write Mux Benchmark 800+10 byte SDUs, 0ms Poll, scheduled"   $ nfIO $ readDemuxerBenchmark sndSizeEV 800 addrS
                  , bench "Read/Write Mux Benchmark 12288+10 byte SDUs, 0ms Poll, scheduled" $ nfIO $ readDemuxerBenchmark sndSizeEV 12288 addrS
                  ]
                ]
              cancel said
              cancel saidM
              cancel saidE
              cancel saidF
              cancel saidS
      )
