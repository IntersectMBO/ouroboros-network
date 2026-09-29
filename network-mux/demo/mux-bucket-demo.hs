{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Egress-bucket demo, shaped like a leios sim expriment: X
-- downstream peers each fetch one 12 MB EB from a server at the same instant;
-- the server's egress is either unscheduled (today's mux) or scheduled by one
-- token bucket at a declared budget, gated on socket writability, optionally
-- with TCP_NOTSENT_LOWAT.
--
-- > mux-bucket-demo server --port 6000 --peers 200 --eb-mb 12 --chunk-kb 197
-- >                        --budget-mbps 950 --lowat 131072 --order fifo|arrival
-- > mux-bucket-demo client --host 127.0.0.1 --port 6000 --peers 200
-- >                        --stall 0 --stall-read-bps 0
-- > mux-bucket-demo --help
--
module Main (main) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer

import Data.Binary.Put qualified as Bin
import Data.Word (Word64)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Identity (runIdentity)
import Data.List (sort)
import Options.Applicative
import Text.Printf (printf)

import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import Network.Socket.ByteString.Lazy qualified as SocketBL
import System.IO
import System.IOManager

import Network.Mux as Mx
import Network.Mux.Bearer (makeSocketBearer, makeSocketBearerWith,
  tcpNotSentLowWat)
import Network.Mux.Codec (encodeSDU)
import Network.Mux.Types (MiniProtocolDir (..), RemoteClockModel (..), SDU (..),
  SDUHeader (..))

import Test.Mux.ReqResp

--
-- Options
--

data Order = Fifo | Arrival | Random

orderName :: Order -> String
orderName Fifo    = "fifo"
orderName Arrival = "arrival"
orderName Random  = "random"

data ServerOpts = ServerOpts {
  soPort       :: Int,
  soPeers      :: Int,
  soEbMb       :: Double,
  soChunkKb    :: Int,
  soBudgetMbps :: Double,
  soLowat      :: Int,
  soOrder      :: Order,
  soSeed       :: Word64,
  soPeriod     :: Double,
  soLanes      :: Bool,
  soTxMbps     :: Double,
  soSlicePct   :: Int,
  soSduTimeout :: Double,
  soHorizon    :: Double
  }

data ClientOpts = ClientOpts {
  coHost         :: String,
  coPort         :: Int,
  coPeers        :: Int,
  coStall        :: Int,
  coStallReadBps :: Int,
  coHorizon      :: Double,
  coLinger       :: Double
  }

data Command = ServerCmd ServerOpts | ClientCmd ClientOpts

serverOptsParser :: Parser ServerOpts
serverOptsParser = ServerOpts
  <$> option auto
    (  long "port"
    <> help "TCP port to listen on"
    <> metavar "PORT"
    <> value 6000
    <> showDefault )
  <*> option auto
    (  long "peers"
    <> help "Number of downstream peers to accept and serve"
    <> metavar "N"
    <> value 1
    <> showDefault )
  <*> option auto
    (  long "eb-mb"
    <> help "EB size served to every peer, in MB"
    <> metavar "MB"
    <> value 12
    <> showDefault )
  <*> option auto
    (  long "chunk-kb"
    <> help "Chunk size in kB; the EB is served as one message per chunk (0: a single message)"
    <> metavar "KB"
    <> value 197
    <> showDefault )
  <*> option auto
    (  long "budget-mbps"
    <> help "Egress budget of the shared token bucket in Mb/s (0: no bucket, today's mux)"
    <> metavar "MBPS"
    <> value 0
    <> showDefault )
  <*> option auto
    (  long "lowat"
    <> help "TCP_NOTSENT_LOWAT set on every connection, in bytes (0: leave the socket alone)"
    <> metavar "BYTES"
    <> value 0
    <> showDefault )
  <*> option order
    (  long "order"
    <> help ("Service order among connections: fifo (equal per-batch share), arrival "
             ++ "(strict priority by connection order) or random (strict priority in a "
             ++ "seeded order, re-dealt every period)")
    <> metavar "ORDER"
    <> value Fifo
    <> showDefaultWith orderName )
  <*> option auto
    (  long "seed"
    <> help "Seed of the random order"
    <> metavar "N"
    <> value 1
    <> showDefault )
  <*> option auto
    (  long "period"
    <> help "Period after which the random order is re-dealt, in seconds"
    <> metavar "SECONDS"
    <> value 599
    <> showDefault )
  <*> option onOff
    (  long "lanes"
    <> help ("Egress lanes: on (the server's own requests go direct, the tx stream on its "
             ++ "slice) or off (everything scheduled)")
    <> metavar "on|off"
    <> value True
    <> showDefaultWith (\b -> if b then "on" else "off") )
  <*> option auto
    (  long "tx-mbps"
    <> help "A tx-like stream the server pushes to all peers, total Mb/s (0: none)"
    <> metavar "MBPS"
    <> value 0
    <> showDefault )
  <*> option auto
    (  long "slice-pct"
    <> help "Share of the budget reserved for the tx stream"
    <> metavar "PERCENT"
    <> value 15
    <> showDefault )
  <*> option auto
    (  long "sdu-timeout"
    <> help "Bearer SDU write timeout, in seconds"
    <> metavar "SECONDS"
    <> value 30
    <> showDefault )
  <*> option auto
    (  long "horizon"
    <> help "Give up on a connection after this many seconds"
    <> metavar "SECONDS"
    <> value 120
    <> showDefault )
  where
    order = eitherReader $ \case
      "fifo"    -> Right Fifo
      "arrival" -> Right Arrival
      "random"  -> Right Random
      _         -> Left "expected fifo, arrival or random"
    onOff = eitherReader $ \case
      "on"  -> Right True
      "off" -> Right False
      _     -> Left "expected on or off"

clientOptsParser :: Parser ClientOpts
clientOptsParser = ClientOpts
  <$> option str
    (  long "host"
    <> help "Server address"
    <> metavar "HOST"
    <> value "127.0.0.1"
    <> showDefault )
  <*> option auto
    (  long "port"
    <> help "Server port"
    <> metavar "PORT"
    <> value 6000
    <> showDefault )
  <*> option auto
    (  long "peers"
    <> help "Number of connections; all request the EB at the same instant"
    <> metavar "N"
    <> value 1
    <> showDefault )
  <*> option auto
    (  long "stall"
    <> help "The first K connections are raw sockets that request the EB and then stop reading"
    <> metavar "K"
    <> value 0
    <> showDefault )
  <*> option auto
    (  long "stall-read-bps"
    <> help "Read rate of a stalled connection in bytes/s (0: never read)"
    <> metavar "BPS"
    <> value 0
    <> showDefault )
  <*> option auto
    (  long "horizon"
    <> help "Give up on a connection after this many seconds"
    <> metavar "SECONDS"
    <> value 120
    <> showDefault )
  <*> option auto
    (  long "linger"
    <> help ("Keep the connections up this many seconds after every EB is done, so the "
             ++ "server's tx stream runs on an idle link")
    <> metavar "SECONDS"
    <> value 0
    <> showDefault )

commandParser :: Parser Command
commandParser = hsubparser
  (  command "server"
       (info (ServerCmd <$> serverOptsParser)
             (progDesc "Serve one EB to every peer that asks, with or without the egress bucket"))
  <> command "client"
       (info (ClientCmd <$> clientOptsParser)
             (progDesc "Open N connections and request the EB on all of them at once"))
  )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  cmd <- execParser $ info (commandParser <**> helper)
    (  fullDesc
    <> header "mux-bucket-demo - the mux egress token bucket on real TCP"
    <> progDesc "X peers each fetch one EB from a server at the same instant; the server's \
                \egress is unscheduled or shaped by one token bucket gated on socket \
                \writability, optionally with TCP_NOTSENT_LOWAT." )
  case cmd of
       ServerCmd o -> server o
       ClientCmd o -> client o

--
-- Shared
--

-- | The mini-protocols, from the server's side: it serves the EB (2) and
-- initiates the probe (3) and the tx-like stream (4). The client mirrors it.
serverProtocols, clientProtocols :: [MiniProtocolInfo InitiatorResponderMode]
serverProtocols = [ protocol ebNum ResponderDirection, protocol probeNum InitiatorDirection
                  , protocol txNum InitiatorDirection ]
clientProtocols = [ protocol ebNum InitiatorDirection, protocol probeNum ResponderDirection
                  , protocol txNum ResponderDirection ]

ebNum, probeNum, txNum :: MiniProtocolNum
ebNum    = MiniProtocolNum 2
probeNum = MiniProtocolNum 3
txNum    = MiniProtocolNum 4

protocol :: MiniProtocolNum -> MiniProtocolDirection InitiatorResponderMode
         -> MiniProtocolInfo InitiatorResponderMode
protocol miniProtocolNum miniProtocolDir = MiniProtocolInfo {
  miniProtocolNum,
  miniProtocolDir,
  miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 64_000_000 },
  miniProtocolCapability = Nothing
  }

-- | Which lane each protocol travels in on the server: the EB it serves is
-- scheduled, the probe -- standing in for the node's own requests -- goes
-- direct, the tx-like stream takes the slice.
laneRule :: MiniProtocolNum -> MiniProtocolDir -> Mx.Lane
laneRule num InitiatorDir | num == txNum = Mx.Slice
laneRule num dir = Mx.directionSplit num dir

-- | Read until @n@ bytes have arrived.
drainBytes :: ByteChannel IO -> Int -> IO ()
drainBytes _    n | n <= 0 = return ()
drainBytes chan n = do
  mbs <- recv chan
  case mbs of
       Nothing -> return ()
       Just bs -> drainBytes chan (n - fromIntegral (BL.length bs))

batchBytes :: Int
batchBytes = 131_072

secs :: Time -> Time -> Double
secs t0 t1 = realToFrac (t1 `diffTime` t0)

closeQuietly :: Socket.Socket -> IO ()
closeQuietly sock = Socket.close sock `catch` \(_ :: SomeException) -> return ()

-- | The EB as it is served: its size, and the chunk sizes it is sent in.
data EbShape = EbShape {
  ebBytes  :: !Int,
  ebChunks :: [Int]      -- ^ one response message per element
  }

ebShape :: ServerOpts -> EbShape
ebShape opts = EbShape { ebBytes = total, ebChunks = chunks }
  where
    total      = round (soEbMb opts * 1_000_000)
    chunkBytes = soChunkKb opts * 1_000
    chunks | chunkBytes <= 0 = [total]
           | otherwise       = go 0
      where
        go off | off >= total = []
               | otherwise    = min chunkBytes (total - off) : go (off + chunkBytes)

-- | Serve the EB as one response message per chunk.
ebBurst :: EbShape -> ReqRespServerBurst BS.ByteString BS.ByteString IO ()
ebBurst shape = ReqRespServerBurst $ \_ -> return (go (ebChunks shape))
  where
    go []       = SendMsgDoneServer (return ())
    go (c : cs) = SendMsgResp (BSC.replicate c 'x') (return (go cs))

--
-- Server
--

-- | Per-connection egress statistics, from 'TraceEgressGrant'.
data Stats = Stats {
  stGrants   :: !Int,
  stBytes    :: !Int,
  stSumWrit  :: !Double, stMaxWrit  :: !Double,  -- seconds waited for writability
  stSumTok   :: !Double, stMaxTok   :: !Double,  -- seconds waited for tokens
  stProbes   :: !Int,
  stSumProbe :: !Double, stMaxProbe :: !Double   -- probe round trips, seconds
  }

emptyStats :: Stats
emptyStats = Stats 0 0 0 0 0 0 0 0 0

addProbe :: Double -> Stats -> Stats
addProbe rtt st = st { stProbes = stProbes st + 1, stSumProbe = stSumProbe st + rtt
                     , stMaxProbe = max (stMaxProbe st) rtt }

addGrant :: Int -> DiffTime -> DiffTime -> Stats -> Stats
addGrant len waitedWritable waitedTokens st =
  st { stGrants  = stGrants st + 1
     , stBytes   = stBytes st + len
     , stSumWrit = stSumWrit st + w, stMaxWrit = max (stMaxWrit st) w
     , stSumTok  = stSumTok st + t,  stMaxTok  = max (stMaxTok st) t
     }
  where
    w = realToFrac waitedWritable
    t = realToFrac waitedTokens

-- | What serving one connection produced.
data ConnResult = ConnResult {
  crPeer    :: !Int,
  crOutcome :: Either String Double,   -- ^ why it did not finish, or seconds taken
  crLife    :: !Double,                -- ^ seconds from accept to close
  crStats   :: !Stats
  }

server :: ServerOpts -> IO ()
server opts = withIOManager $ \ioManager -> do
  let shape = ebShape opts

  buckets_m <- newBucketsFor opts
  printServerBanner opts shape

  withListenSocket ioManager (soPort opts) $ \lsock -> do
    conns <- forM [0 .. soPeers opts - 1] $ \i -> do
      (sock, _) <- Socket.accept lsock
      associateWithIOManager ioManager (Right sock)
      async (serveConn opts shape buckets_m i sock)
    results <- mapM wait conns
    printServerSummary opts results

-- | The budget's bucket and, when a tx stream is offered, the slice's.
newBucketsFor :: ServerOpts -> IO (Maybe (Mx.Bucket IO, Maybe (Mx.Bucket IO)))
newBucketsFor opts
  | soBudgetMbps opts > 0 = do
      budget <- Mx.newBucket rate (2 * batchBytes) rotation
      slice  <- if soTxMbps opts > 0
                   then Just <$> Mx.newBucket (rate * fromIntegral (soSlicePct opts) / 100)
                                              (2 * batchBytes) Nothing
                   else return Nothing
      return (Just (budget, slice))
  | otherwise = return Nothing
  where
    rate = soBudgetMbps opts * 1e6 / 8
    rotation = case soOrder opts of
                    Random -> Just Mx.Rotation { Mx.roSeed   = soSeed opts
                                               , Mx.roPeriod = realToFrac (soPeriod opts) }
                    _      -> Nothing

printServerBanner :: ServerOpts -> EbShape -> IO ()
printServerBanner opts shape =
  printf ("==== mux-bucket-demo server: %d peers x %.1f MB (%d chunks), "
          ++ "budget %s, lowat %d, order %s%s, lanes %s%s ====\n")
    (soPeers opts) (fromIntegral (ebBytes shape) / 1e6 :: Double)
    (length (ebChunks shape)) budgetStr (soLowat opts) (orderName (soOrder opts)) orderDetail
    (if soLanes opts then "on" else "off" :: String) txDetail
  where
    txDetail | soTxMbps opts > 0 = printf ", tx stream %.1f Mb/s on a %d%% slice"
                                          (soTxMbps opts) (soSlicePct opts)
             | otherwise         = "" :: String
    budgetStr | soBudgetMbps opts > 0 = printf "%.0f Mb/s" (soBudgetMbps opts)
              | otherwise             = "off" :: String
    orderDetail = case soOrder opts of
                       Random -> printf " (seed %d, period %.0f s)" (soSeed opts) (soPeriod opts)
                       _      -> "" :: String

withListenSocket :: IOManager -> Int -> (Socket.Socket -> IO a) -> IO a
withListenSocket ioManager port k = do
  let hints = Socket.defaultHints { Socket.addrFlags      = [Socket.AI_PASSIVE]
                                  , Socket.addrSocketType = Socket.Stream }

  addr:_ <- Socket.getAddrInfo (Just hints) Nothing (Just (show port))

  let open = Socket.socket (Socket.addrFamily addr) Socket.Stream Socket.defaultProtocol

  bracket open Socket.close $ \lsock -> do
    associateWithIOManager ioManager (Right lsock)
    Socket.setSocketOption lsock Socket.ReuseAddr 1
    Socket.bind lsock (Socket.addrAddress addr)
    Socket.listen lsock 512
    k lsock

-- | A mux tracer that accumulates this connection's egress grants.
newStatsTracer :: IO (StrictTVar IO Stats, Mx.Tracers IO)
newStatsTracer = do
  statsVar <- newTVarIO emptyStats
  let grantTracer :: Tracer IO BearerTrace
      grantTracer = Tracer $ emit $ \case
        TraceEgressGrant len tw tt -> atomically $ modifyTVar statsVar (addGrant len tw tt)
        _                          -> return ()
  return ( statsVar
         , Mx.nullTracers { Mx.bearerTracer = contramap runIdentity grantTracer } )

-- | Report whether the platform honoured @TCP_NOTSENT_LOWAT@.
reportLowat :: Int -> Socket.Socket -> IO ()
reportLowat lowat sock =
  case tcpNotSentLowWat of
       Just o  -> do
         v <- Socket.getSocketOption sock o
         printf "TCP_NOTSENT_LOWAT applied: %d (requested %d)\n" v lowat
       Nothing ->
         putStrLn ("TCP_NOTSENT_LOWAT: unsupported on this platform, "
                   ++ "gate is send-buffer space")

serveConn :: ServerOpts -> EbShape -> Maybe (Mx.Bucket IO, Maybe (Mx.Bucket IO)) -> Int
  -> Socket.Socket -> IO ConnResult
serveConn opts shape buckets_m i sock = do
  tStart <- getMonotonicTime
  (statsVar, tracers) <- newStatsTracer
  let lowat     = soLowat opts
      horizonDt = realToFrac (soHorizon opts) :: DiffTime
      mkBearer  = makeSocketBearerWith 0 (if lowat > 0 then Just lowat else Nothing)

  bearer <- getBearer mkBearer (realToFrac (soSduTimeout opts)) sock Nothing
  when (i == 0 && lowat > 0) $
    reportLowat lowat sock
  let policy (budget, slice) = Mx.EgressPolicy {
          Mx.egressBudget = budget,
          Mx.egressSlice  = slice,
          Mx.egressLaneOf = if soLanes opts then laneRule else \_ _ -> Mx.Scheduled }
  mux <- case buckets_m of
              Just bs -> Mx.newWithEgress (policy bs) tracers serverProtocols
              Nothing -> Mx.new tracers serverProtocols

  case soOrder opts of
       Arrival -> atomically $ Mx.setEgressRank mux (Mx.Rank (fromIntegral (min 255 i)))
       Fifo    -> return ()
       Random  -> return ()          -- the bucket's rotation orders tier 0

  let serveEb chan = do
        t0 <- getMonotonicTime
        (_, trailing) <- runServerBurstBin nullTracer chan (ebBurst shape)
        t1 <- getMonotonicTime
        return (secs t0 t1, trailing)

  r <- withAsync (Mx.run mux bearer) $ \muxA -> do
    await <- Mx.runMiniProtocol mux ebNum ResponderDirection StartOnDemand serveEb
    _ <- Mx.runMiniProtocol mux probeNum InitiatorDirection StartEagerly (probe statsVar)
    when (soTxMbps opts > 0) $
      void $ Mx.runMiniProtocol mux txNum InitiatorDirection StartEagerly
               (txStream (soTxMbps opts * 1e6 / 8 / fromIntegral (soPeers opts)))

    res <- timeout horizonDt (atomically await)
    -- the protocol completing means its bytes left the wanton, not that they
    -- were written: the last batch may still be waiting for a grant.  Let the
    -- client close the connection (its mux stops after MsgDone), which ends
    -- 'Mx.run' here, before stopping.
    _ <- timeout horizonDt (waitCatch muxA)
    Mx.stop mux
    return res

  closeQuietly sock
  st <- readTVarIO statsVar
  tEnd <- getMonotonicTime
  let life = secs tStart tEnd
  case r of
       Just (Right dt) -> return $ ConnResult i (Right dt) life st
       Just (Left e)   -> return $ ConnResult i (Left (show e)) life st
       Nothing         -> return $ ConnResult i (Left "horizon") life st

-- | Every 250 ms: 32 bytes to the peer and its echo back. The round trip is
-- what the node's own requests would see on this connection.
probe :: StrictTVar IO Stats -> ByteChannel IO -> IO ((), Maybe BL.ByteString)
probe statsVar chan = forever $ do
  t0 <- getMonotonicTime
  send chan (BL.replicate 32 0x70)
  drainBytes chan 32
  t1 <- getMonotonicTime
  atomically $ modifyTVar statsVar (addProbe (secs t0 t1))
  threadDelay 0.25

-- | A tx-like stream: 4 kB messages offered at @bps@ at most; the lane decides
-- how fast they actually go, and a message the lane held back is not made up
-- for afterwards.
txStream :: Double -> ByteChannel IO -> IO ((), Maybe BL.ByteString)
txStream bps chan = forever $ do
  t0 <- getMonotonicTime
  send chan (BL.replicate 4096 0x74)
  t1 <- getMonotonicTime
  threadDelay (realToFrac (4096 / bps) - (t1 `diffTime` t0))

printServerSummary :: ServerOpts -> [ConnResult] -> IO ()
printServerSummary opts results = do
  printf "served: %d completed, %d not completed within %.0f s (stalled or failed)\n"
    (length results - length failed) (length failed) (soHorizon opts)
  when (soBudgetMbps opts > 0) $ do
    printf ("grants: %d (%.1f MB); waited writable mean %.2f ms max %.2f ms; "
            ++ "waited tokens mean %.2f ms max %.2f ms\n")
      totG (fromIntegral (tot stBytes) / 1e6 :: Double)
      (1e3 * mean (tot stSumWrit)) (1e3 * maxOf stMaxWrit)
      (1e3 * mean (tot stSumTok))  (1e3 * maxOf stMaxTok)
    forM_ failed $ \(i, why, life, st) ->
      printf "  conn %3d (%s): %d grants, %.2f MB, waited writable max %.1f ms, closed after %.1f s\n"
        i why (stGrants st) (fromIntegral (stBytes st) / 1e6 :: Double)
        (1e3 * stMaxWrit st) life
  printf "probes: %d; round trip mean %.2f ms max %.2f ms\n"
    totP (1e3 * tot stSumProbe / fromIntegral (max 1 totP)) (1e3 * maxOf stMaxProbe)
  where
    totP    = tot stProbes
    failed  = [ (crPeer r, why, crLife r, crStats r) | r <- results, Left why <- [crOutcome r] ]
    tot f   = sum [ f (crStats r) | r <- results ]
    totG    = tot stGrants
    mean s  = if totG == 0
                 then 0
                 else s / fromIntegral totG
    maxOf f = maximum (0 : [ f (crStats r) | r <- results ]) :: Double

--
-- Client
--

-- | Start barrier: every peer reports ready, then all are released together.
data Barrier = Barrier {
  barrierReady :: IO (),
  barrierWait  :: IO ()
  }

-- | A barrier for @n@ peers, and the action that waits for all of them and
-- releases them at once, returning the instant they were released.
newBarrier :: Int -> IO (Barrier, IO Time)
newBarrier n = do
  readyVar <- newTVarIO (0 :: Int)
  goVar    <- newTVarIO False
  let release = do
        atomically $ readTVar readyVar >>= check . (>= n)
        t <- getMonotonicTime
        atomically $ writeTVar goVar True
        return t
  return ( Barrier { barrierReady = atomically $ modifyTVar readyVar (+ 1)
                   , barrierWait  = atomically $ readTVar goVar >>= check }
         , release )

client :: ClientOpts -> IO ()
client opts = withIOManager $ \ioManager -> do
  (barrier, release) <- newBarrier (coPeers opts)
  addr:_ <- Socket.getAddrInfo Nothing (Just (coHost opts)) (Just (show (coPort opts)))
  txVar <- newTVarIO (0 :: Int)
  -- a connection stays up until every peer has its EB, so the probes and the
  -- tx stream are measured over the whole run on every connection
  (done, releaseDone) <- newBarrier (coPeers opts - coStall opts)
  doneA <- async $ do
    t <- releaseDone
    b <- readTVarIO txVar
    return (t, b)
  let peer i | i < coStall opts = stalledPeer opts barrier
             | otherwise        = muxPeer opts barrier done txVar
  peersA <- forM [0 .. coPeers opts - 1] $ \i ->
    async $ bracket (connectTo ioManager addr) closeQuietly (peer i)
  t0 <- release
  printf ("==== mux-bucket-demo client: %d peers (%d stalled), "
          ++ "all requests fired at once ====\n") (coPeers opts) (coStall opts)
  results <- mapM wait peersA
  txBytes <- readTVarIO txVar
  -- the tx stream after the last EB, on an otherwise idle link
  atDone_m <- timeout (realToFrac (coHorizon opts) :: DiffTime) (wait doneA)
  let linger = [ (coLinger opts, txBytes - txAtDone)
               | coLinger opts > 0, Just (_, txAtDone) <- [atDone_m] ]
  printClientSummary opts t0 results txBytes linger

connectTo :: IOManager -> Socket.AddrInfo -> IO Socket.Socket
connectTo ioManager addr = do
  sock <- Socket.socket (Socket.addrFamily addr) Socket.Stream Socket.defaultProtocol
  associateWithIOManager ioManager (Right sock)
  Socket.connect sock (Socket.addrAddress addr)
  return sock

-- | A normal peer: request the EB at the barrier, count the response bytes;
-- meanwhile echo the server's probes and sink its tx stream.
muxPeer :: ClientOpts -> Barrier -> Barrier -> StrictTVar IO Int -> Socket.Socket
        -> IO (Maybe (Time, Int))
muxPeer opts barrier done txVar sock = do
  let horizon = realToFrac (coHorizon opts) :: DiffTime
  bearer <- getBearer makeSocketBearer 30 sock Nothing
  mux <- Mx.new Mx.nullTracers clientProtocols
  let fetchEb chan = do
        barrierReady barrier
        barrierWait barrier
        (bytes, trailing) <- runClientBurstBin nullTracer chan
          (SendMsgReqBurst (BSC.replicate 32 'r') (countBytes 0))
        t <- getMonotonicTime
        return ((t, bytes), trailing)

  r <- withAsync (Mx.run mux bearer) $ \_ -> do
    _ <- Mx.runMiniProtocol mux probeNum ResponderDirection StartOnDemand echo
    _ <- Mx.runMiniProtocol mux txNum ResponderDirection StartOnDemand sink
    await <- Mx.runMiniProtocol mux ebNum InitiatorDirection StartEagerly fetchEb
    res <- timeout horizon (atomically await)
    barrierReady done
    _ <- timeout horizon (barrierWait done)
    threadDelay (realToFrac (coLinger opts))
    Mx.stop mux
    return res
  case r of
       Just (Right x) -> return $ Just x
       _              -> return Nothing
  where
    echo chan = forever $ recv chan >>= maybe (return ()) (send chan)
    sink chan = forever $ recv chan >>= \mbs ->
      case mbs of
           Just bs -> atomically $ modifyTVar txVar (+ fromIntegral (BL.length bs))
           Nothing -> return ()

    countBytes :: Int -> ReqRespClientLoop BS.ByteString IO Int
    countBytes !acc = AwaitResp {
      handleMsgDone = return acc,
      handleMsgResp = \resp -> return (countBytes (acc + BS.length resp))
      }

-- | A stalled peer: raw socket, hand-encoded request SDU, then reads at
-- @--stall-read-bps@ (0: never).  A mux client cannot stall - its demuxer
-- treats a non-consuming protocol as an ingress overrun.
stalledPeer :: ClientOpts -> Barrier -> Socket.Socket -> IO (Maybe (Time, Int))
stalledPeer opts barrier sock = do
  barrierReady barrier
  barrierWait barrier
  SocketBL.sendAll sock (requestSdu (BSC.replicate 32 'r'))
  let readBps = coStallReadBps opts
      drain   = forever $ do
        threadDelay 0.1
        when (readBps > 0) $ void $ SocketBS.recv sock (max 1 (readBps `div` 10))
  _ <- timeout (realToFrac (coHorizon opts) :: DiffTime) drain
  return Nothing

-- | A @MsgReq@ on mini-protocol 2, encoded as the mux would send it.
requestSdu :: BS.ByteString -> BL.ByteString
requestSdu req = encodeSDU SDU {
  msHeader = SDUHeader { mhTimestamp = RemoteClockModel 0
                       , mhNum       = MiniProtocolNum 2
                       , mhDir       = InitiatorDir
                       , mhLength    = fromIntegral (BL.length payload) },
  msBlob   = payload
  }
  where
    payload = Bin.runPut $ do
      Bin.putWord8 0
      Bin.putWord32be (fromIntegral (BS.length req))
      Bin.putByteString req

printClientSummary :: ClientOpts -> Time -> [Maybe (Time, Int)] -> Int -> [(Double, Int)]
                   -> IO ()
printClientSummary opts t0 results txBytes linger = do
  forM_ [0.25, 0.5, 0.75, 0.95 :: Double] $ \q -> do
    let k = ceiling (q * fromIntegral (coPeers opts)) :: Int
    if k <= n
       then printf "  %2.0f%% of peers complete at %7.2f s\n" (100 * q) (done !! (k - 1))
       else printf "  %2.0f%% of peers never complete within %.0f s\n" (100 * q) (coHorizon opts)
  printf "  inside 7 s: %d / %d\n" (length (takeWhile (<= 7) done)) (coPeers opts)
  when (n > 0) $
    printf "  aggregate: %.1f MB in %.2f s = %.0f Mb/s\n"
      (fromIntegral bytes / 1e6 :: Double) (last done)
      (fromIntegral bytes * 8 / 1e6 / last done)
  when (n > 0 && txBytes > 0) $
    printf "  tx stream: %.1f MB in %.2f s = %.1f Mb/s\n"
      (fromIntegral txDuring / 1e6 :: Double) (last done)
      (fromIntegral txDuring * 8 / 1e6 / last done)
  forM_ linger $ \(secs', bytes') ->
    printf "  tx stream on the idle link: %.1f MB in %.1f s = %.1f Mb/s\n"
      (fromIntegral bytes' / 1e6 :: Double) secs' (fromIntegral bytes' * 8 / 1e6 / secs')
  where
    done     = sort [ secs t0 t | Just (t, _) <- results ]
    bytes    = sum [ b | Just (_, b) <- results ]
    n        = length done
    txDuring = txBytes - sum (map snd linger)
