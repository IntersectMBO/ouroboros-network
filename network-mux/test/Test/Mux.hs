{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
#if defined(mingw32_HOST_OS)
{-# LANGUAGE PackageImports             #-}
#endif
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans            #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial          #-}
#endif

module Test.Mux (tests) where

import Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (..), serialise)
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Exception (ErrorCall (..))
import Control.Monad
import Data.Bifunctor qualified as Bifunctor
import Data.Binary.Put qualified as Bin
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8 (pack)
import Data.Functor.Identity (Identity (..))
import Data.List (dropWhileEnd, nub)
import Data.List qualified as List
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Data.Word
import Foreign.C.Error (eCONNRESET, eNETDOWN, ePIPE, errnoToIOError)
import Formatting (formatToString, (%), (%+))
import Formatting qualified as F
import GHC.IO.Exception (IOException)
import NoThunks.Class (unsafeNoThunks)
import System.Random.SplitMix qualified as SM
import Test.Cardano.Base.QuickCheck qualified as BaseQC
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer

#if defined(mingw32_HOST_OS)
import System.Win32.Async qualified as Win32.Async
import System.Win32.File qualified as Win32.File
#if MIN_VERSION_Win32_network(0,2,0)
import "Win32" System.Win32.NamedPipes qualified as Win32.NamedPipes
#else
import "Win32-network" System.Win32.NamedPipes qualified as Win32.NamedPipes
#endif
#else
import System.IO (hClose)
import System.Process (createPipe)
#endif
import System.IOManager

import Test.Mux.FloorBucket ()
import Test.Mux.ReqResp

import Network.Mux (Mux)
import Network.Mux qualified as Mx
import Network.Mux.Bearer as Mx
import Network.Mux.Bearer.AttenuatedChannel as AttenuatedChannel
import Network.Mux.Bearer.Pipe qualified as Mx
import Network.Mux.Bearer.Queues as Mx
import Network.Mux.Codec qualified as Mx
import Network.Mux.Counters qualified as Counters
import Network.Mux.Egress (egressSoftBufferLimit)
import Network.Mux.Egress qualified as Egress
import Network.Mux.Egress.Bucket qualified as Bucket
import Network.Mux.Egress.Counters qualified as Counters
import Network.Mux.Egress.Floor (floorClass)
import Network.Mux.Timeout (TimeoutFn)
import Network.Mux.Types (MiniProtocolInfo (..), MiniProtocolLimits (..))
import Network.Mux.Types qualified as Mx
import Network.Socket qualified as Socket
import Text.Show.Functions ()
-- import qualified Debug.Trace as Debug

tests :: TestTree
tests =
  testGroup "Mux"
  [ testProperty "mux send receive"             prop_mux_snd_recv
  , testProperty "mux send receive bidir"       prop_mux_snd_recv_bi
  , testProperty "mux send receive compat"      prop_mux_snd_recv_compat
  , testProperty "1 miniprot Queue"             (BaseQC.withNumTests 50 prop_mux_1_mini_Queue)
  , testProperty "2 miniprots Queue"            (BaseQC.withNumTests 50 prop_mux_2_minis_Queue)
  , testProperty "1 miniprot Pipe"              (BaseQC.withNumTests 50 prop_mux_1_mini_Pipe)
  , testProperty "2 miniprots Pipe"             (BaseQC.withNumTests 50 prop_mux_2_minis_Pipe)
  , testProperty "1 miniprot Socket"            (BaseQC.withNumTests 50 prop_mux_1_mini_Socket)
  , testProperty "1 miniprot Socket, buffered"  (BaseQC.withNumTests 50 prop_mux_1_mini_Socket_buf)
  , testProperty "2 miniprot Socket"            (BaseQC.withNumTests 50 prop_mux_2_minis_Socket)
  , testProperty "2 miniprots Socket, buffered" (BaseQC.withNumTests 50 prop_mux_2_minis_Socket_buf)
  , testProperty "starvation"                   prop_mux_starvation
  , testProperty "demuxing (Sim)"               prop_demux_sdu_sim
  , testProperty "demuxing (IO)"                prop_demux_sdu_io
  , testProperty "mux start and stop"           prop_mux_start
  , testProperty "mux restart"                  prop_mux_restart
  , testProperty "mux close (Sim)"              prop_mux_close_sim
  , testProperty "mux close (IO)"               (BaseQC.withNumTests 50 prop_mux_close_io)
  , testProperty "trailing bytes (Sim)"         prop_mux_trailing_bytes_iosim
  , testProperty "trailing bytes (IO)"          prop_mux_trailing_bytes_io
  , testProperty "pure exception (Sim)"         prop_mux_pure_exception_iosim
  , testProperty "pure exception (IO)"          prop_mux_pure_exception_io
  , testProperty "sdu codec"                    prop_sdu_codec
  , testProperty "send forces a prefix (IO)"    prop_mux_send_forces_prefix_only_io
  , testProperty "send forces a prefix (Sim)"   prop_mux_send_forces_prefix_only_sim
  , testProperty "send traces the length (Sim)" prop_mux_send_end_length
  , testGroup "Counters"
    [ testGroup "each failure is counted once, where it belongs"
        [ testProperty (show failure) (once (prop_mux_counters_failure failure))
        | failure <- [minBound .. maxBound] ]
    , testProperty "withCounters makes the sets asked for and traces them every interval"
                   prop_mux_counters_loop
    , testProperty "a close after every mini-protocol finished counts nothing"
                   prop_mux_close_after_exit
    , testProperty "a snapshot reads a counter per transaction, a step whole or not at all"
                   prop_mux_counters_snapshot_split
    , testProperty "bytes the bearer took are counted, the last batch's once the write returns"
                   prop_mux_written_is_counted
    , testProperty "bytes the bearer delivered are counted, the last SDU's once the read returns"
                   prop_mux_read_is_counted
    , testProperty "a write blocked part way takes the cancellation, nothing counted"
                   prop_mux_partial_write_cancelled
    , testProperty "a read landing on its deadline is read, the mux reading on, or timed out"
                   prop_mux_read_on_the_deadline
    ]
  , testGroup "Egress bucket"
    [ testProperty "grantAt: earliest instant"    prop_grantAt_ready
    , testProperty "grantAt: not late"            prop_grantAt_tight
    , testProperty "grantAt: takes what it grants" prop_grantAt_take
    , testProperty "grantAt: rate zero disables"  prop_grantAt_disabled
    , testProperty "wakeAt: within a microsecond" prop_wakeAt
    , testProperty "atRate: keeps the level"      prop_atRate
    , testProperty "rotatedRank: re-dealt each period" prop_rotatedRank
    , testProperty "queueRank: zero period is none"   prop_queueRank_zeroPeriod
    , testProperty "served: each bearer once per tier, within the window" prop_served_window
    , testProperty "served: the counters drop the stale bearers" prop_served_counters
    , testProperty "served: the record prunes itself, bounded" prop_served_bounded
    , testProperty "served: an unqueued grant asks its tier at the grant" prop_served_fast_tier
    , testProperty "slowed: a standing is a decayed ratio" prop_slowdown_laws
    , testProperty "slowed: counted per tier"              prop_slowed_by_tier
    , testProperty "slowed: the counters count the slowed bearers of each tier" prop_slowed_counters
    , testProperty "queueRank: turns from the threshold" prop_queueRank_turns
    , testProperty "chargeAt: takes without waiting"  prop_chargeAt
    , testProperty "schedule matches the replay"  prop_bucket_schedule
    , testProperty "the counters hold no thunks"  prop_bucket_stats_no_thunks
    , testProperty "schedule matches the replay, idle floor attached" prop_bucket_schedule_idle_floor
    , testProperty "cancellation is safe"         prop_bucket_cancel
    , testProperty "cancellation is safe with a floor" prop_bucket_cancel_floor
    , testProperty "disabled bucket never waits"  prop_bucket_disabled
    , testProperty "disabled bucket never waits, floor attached" prop_bucket_disabled_floor
    , testProperty "rate change takes effect"     prop_bucket_rate_change
    , testProperty "tiers: one number each, one name each, the floor on the credited"
                   prop_tiers
    ]
  , testGroup "Egress lanes"
    [ testProperty "own requests never wait, the slice gets its share"
                   (BaseQC.withNumTests 30 prop_mux_lanes)
    , testProperty "a torn-down head hands the queue on"
                   prop_mux_egress_head_torn_down
    , testProperty "the muxer reports the gate and the write to the budget"
                   prop_mux_scheduled_own_time
    , testProperty "a final message is written before completion is reported"
                   prop_mux_final_message_before_release
    , testProperty "own requests wait at the gate, charged once written"
                   (BaseQC.withNumTests 30 prop_mux_direct_gate)
    , testProperty "counters are snapshotted every interval"
                   prop_egress_counters_loop
    ]
  , testGroup "Generators"
    [ testProperty "genByteString"              prop_arbitrary_genByteString
    , testProperty "genLargeByteString"         prop_arbitrary_genLargeByteString
    ]
  ]

defaultMiniProtocolLimits :: MiniProtocolLimits
defaultMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = defaultMiniProtocolLimit
    }

defaultMiniProtocolLimit :: Int
defaultMiniProtocolLimit = 3000000

smallMiniProtocolLimits :: MiniProtocolLimits
smallMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = smallMiniProtocolLimit
    }

smallMiniProtocolLimit :: Int
smallMiniProtocolLimit = 16*1024

activeTracer :: forall m a. MonadSay m => Tracer m a
activeTracer = nullTracer
-- activeTracer = sayTracer

--
-- Generators
--

newtype DummyPayload = DummyPayload {
      unDummyPayload :: BL.ByteString
    } deriving Eq

instance Show DummyPayload where
    show d = formatToString ("DummyPayload" %+ F.int % "\n") (BL.length $ unDummyPayload d)

-- | Generate a byte string of a given size.
--
genByteString :: Int -> Gen BL.ByteString
genByteString size = do
    g0 <- return . SM.mkSMGen =<< chooseAny
    return $ BL.unfoldr gen (size, g0)
  where
    gen :: (Int, SM.SMGen) -> Maybe (Word8, (Int, SM.SMGen))
    gen (!i, !g)
        | i <= 0    = Nothing
        | otherwise = Just (fromIntegral w64, (i - 1, g'))
      where
        !(w64, g') = SM.nextWord64 g

prop_arbitrary_genByteString :: NonNegative (Small Int) -> Property
prop_arbitrary_genByteString (NonNegative (Small size)) = ioProperty $ do
  bs <- generate $ genByteString size
  return $ fromIntegral size == BL.length bs

genLargeByteString :: Int -> Int -> Gen BL.ByteString
genLargeByteString chunkSize size | chunkSize < size = do
  chunk <- genByteString chunkSize
  return $ BL.concat $
        replicate (size `div` chunkSize) chunk
      ++
        [BL.take (fromIntegral $ size `mod` chunkSize) chunk]
genLargeByteString _chunkSize size = genByteString size

-- | Large Int values, but not too large, up to @1024*1024@.
--
newtype LargeInt = LargeInt Int
  deriving (Eq, Ord, Num, Show)

instance Arbitrary LargeInt where
    arbitrary = LargeInt <$> choose (1, 1024*1024)
    shrink (LargeInt n) = map LargeInt $ shrink n

prop_arbitrary_genLargeByteString :: NonNegative LargeInt -> Property
prop_arbitrary_genLargeByteString (NonNegative (LargeInt size)) = ioProperty $ do
  bs <- generate $ genLargeByteString 1024 size
  return $ fromIntegral size == BL.length bs

instance Arbitrary DummyPayload where
    arbitrary = do
        n <- choose (1, 128)
        len <- oneof [ return n
                     , return $ n * 8
                     , return $ defaultMiniProtocolLimit - n - cborOverhead
                     , choose (1, defaultMiniProtocolLimit - cborOverhead)
                     ]
        -- Generating a completly arbitrary bytestring is too costly so it is only
        -- done for short bytestrings.
        DummyPayload <$> genLargeByteString 1024 len
      where
        cborOverhead = 7 -- XXX Bytes needed by CBOR to encode the dummy payload

instance Serialise DummyPayload where
    encode a = CBOR.encodeBytes (BL.toStrict $ unDummyPayload a)
    decode = DummyPayload . BL.fromStrict <$> CBOR.decodeBytes

-- | A sequences of dummy requests and responses for test with the ReqResp protocol.
newtype DummyTrace = DummyTrace {unDummyTrace :: [(DummyPayload, DummyPayload)]}
    deriving (Show)

instance Arbitrary DummyTrace where
    arbitrary = do
        len <- choose (1, 20)
        DummyTrace <$> vector len

    shrink (DummyTrace a) =
        let a' = shrink a in
        map DummyTrace $ filter (not . null) a'

-- | A sequence of DymmyTraces
newtype DummyRun = DummyRun [DummyTrace] deriving Show

instance Arbitrary DummyRun where
    arbitrary = do
        len <- choose (1, 4)
        DummyRun <$> vector len
    shrink (DummyRun a) =
        let a' = shrink a in
        map DummyRun $ filter (not . null) a'

data InvalidSDU = InvalidSDU {
      isTimestamp  :: !Mx.RemoteClockModel
    , isIdAndMode  :: !Word16
    , isLength     :: !Word16
    , isRealLength :: !Int
    , isPattern    :: !Word8
    }

instance Show InvalidSDU where
    show a = formatToString
              ("InvalidSDU" %+ F.hexPrefix 8 %+ F.hexPrefix 4 %+ F.hexPrefix 4 %+ F.hexPrefix 4 %+ F.hexPrefix 2 F.% "\n")
              (Mx.unRemoteClockModel $ isTimestamp a)
              (isIdAndMode a)
              (isLength a)
              (isRealLength a)
              (isPattern a)

data ArbitrarySDU = ArbitraryInvalidSDU InvalidSDU Mx.Error
                  | ArbitraryValidSDU DummyPayload (Maybe Mx.Error)
                  deriving Show

instance Arbitrary ArbitrarySDU where
    arbitrary = oneof [ unknownMiniProtocol
                      , invalidLenght
                      , validSdu
                      , tooLargeSdu
                      ]
      where
        validSdu = do
            b <- arbitrary

            return $ ArbitraryValidSDU b Nothing

        tooLargeSdu = do
            l <- choose (1 + smallMiniProtocolLimit , 2 * smallMiniProtocolLimit)
            pl <- BL8.pack <$> replicateM l arbitrary

            -- This SDU is still considered valid, since the header itself will
            -- not cause a trouble, the error will be triggered by the fact that
            -- it is sent as a single message.
            return $ ArbitraryValidSDU (DummyPayload pl) (Just (Mx.IngressQueueOverRun (Mx.MiniProtocolNum 0) Mx.InitiatorDir))

        unknownMiniProtocol = do
            ts  <- arbitrary
            mid <- choose (6, 0x7fff) -- ClientChainSynWithBlocks with 5 is the highest valid mid
            mode <- oneof [return 0x0, return 0x8000]
            len <- choose (1, 0xffff)
            p <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) (mid .|. mode) len
                                          (8 + fromIntegral len) p)
                                         (Mx.UnknownMiniProtocol (Mx.MiniProtocolNum 0))
        invalidLenght = do
            ts  <- arbitrary
            mid <- arbitrary
            realLen <- choose (0, Mx.msHeaderLength)
            -- An SDU with a payload length of 0 is also invalid.
            -- When sending a full header and setting the length to 0
            -- we can verify that they throw an exception.
            len <- if realLen == Mx.msHeaderLength then return 0
                                                   else arbitrary
            p <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) mid len (fromIntegral realLen) p)
                                         (Mx.SDUDecodeError "")

instance Arbitrary Mx.State where
     arbitrary = elements [Mx.Mature, Mx.Dead]

newtype DummyCapability = DummyCapability {
    unDummyCapability :: Maybe Int
  } deriving (Eq, Show)

instance Arbitrary DummyCapability where
    arbitrary =
      frequency [ (1, return $ DummyCapability Nothing)
                , (8, DummyCapability . Just <$> choose (0, 7))
                , (1, DummyCapability . Just <$> arbitrary)
                ]


-- | A pair of two bytestrings which lengths are unevenly distributed
--
data Uneven = Uneven DummyPayload DummyPayload
  deriving (Eq, Show)

instance Arbitrary Uneven where
    arbitrary = do
      n <- choose (1, 128)
      b <- arbitrary
      (l, k) <- (if b then swap else id) <$>
                oneof [ (n,) <$> choose (2 * n, defaultMiniProtocolLimit - cborOverhead)
                      , let k = defaultMiniProtocolLimit - n - cborOverhead
                        in (k,) <$> choose (1, k `div` 2)
                      , do
                          k <- choose (1, defaultMiniProtocolLimit - cborOverhead)
                          return (k, k `div` 2)
                      ]
      Uneven <$> (DummyPayload <$> genLargeByteString 1024 l)
             <*> (DummyPayload <$> genLargeByteString 1024 k)
      where
        cborOverhead = 7 -- XXX Bytes needed by CBOR to encode the dummy payload


--
-- QuickChekc Properties
--


-- | Verify that an initiator and a responder can send and receive messages
-- from each other.  Large DummyPayloads will be split into sduLen sized
-- messages and the testcases will verify that they are correctly reassembled
-- into the original message.
--
prop_mux_snd_recv :: DummyRun
                  -> Property
prop_mux_snd_recv (DummyRun messages) = ioProperty $ do
    let sduLen = Mx.SDUSize 1260

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10

    let server_w = client_r
        server_r = client_w

        clientBearer = queueChannelAsBearer
                         sduLen
                         QueueChannel { writeQueue = client_w, readQueue = client_r }
        serverBearer = queueChannelAsBearer
                         sduLen
                         QueueChannel { writeQueue = server_w, readQueue = server_r }

        clientTracer' = contramap (Mx.WithBearer ("client" :: String)) activeTracer
        serverTracer' = contramap (Mx.WithBearer ("server" :: String)) activeTracer
        clientTracer = Mx.TracersI clientTracer' clientTracer' clientTracer'
        serverTracer = Mx.TracersI serverTracer' serverTracer' serverTracer'

        clientApp = MiniProtocolInfo {
                       miniProtocolNum = Mx.MiniProtocolNum 2,
                       miniProtocolDir = Mx.InitiatorDirectionOnly,
                       miniProtocolLimits = defaultMiniProtocolLimits,
                       miniProtocolCapability = Nothing
                     }

        serverApp = MiniProtocolInfo {
                       miniProtocolNum = Mx.MiniProtocolNum 2,
                       miniProtocolDir = Mx.ResponderDirectionOnly,
                       miniProtocolLimits = defaultMiniProtocolLimits,
                       miniProtocolCapability = Nothing
                     }

    clientMux <- Mx.new clientTracer Nothing [clientApp]

    serverMux <- Mx.new serverTracer Nothing [serverApp]

    withAsync (Mx.run clientMux clientBearer) $ \clientAsync ->
      withAsync (Mx.run serverMux serverBearer) $ \serverAsync -> do

        r <- step clientMux clientApp serverMux serverApp messages
        Mx.stop serverMux
        Mx.stop clientMux
        wait serverAsync
        wait clientAsync
        return r

  where
    step _ _ _ _ [] = return $ property True
    step clientMux clientApp serverMux serverApp (msgs:msgss) = do
        (client_mp, server_mp) <- setupMiniReqRsp (return ()) msgs

        clientRes <- Mx.runMiniProtocol clientMux (Mx.miniProtocolNum clientApp) (Mx.miniProtocolDir clientApp)
                   Mx.StartEagerly client_mp
        serverRes <- Mx.runMiniProtocol serverMux (Mx.miniProtocolNum serverApp) (Mx.miniProtocolDir serverApp)
                   Mx.StartEagerly server_mp
        rs_e <- atomically serverRes
        rc_e <- atomically clientRes
        case (rs_e, rc_e) of
         (Right True, Right True) -> step clientMux clientApp serverMux serverApp msgss
         (_, _)                   -> return $ property False



-- | Like prop_mux_snd_recv but using a bidirectional mux with client and server
-- on both endpoints.
prop_mux_snd_recv_bi :: DummyRun
                     -> DummyCapability
                     -> DummyCapability
                     -> Property
prop_mux_snd_recv_bi (DummyRun messages) (DummyCapability clientCap) (DummyCapability serverCap) = ioProperty $ do
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10

    let server_w = client_r
        server_r = client_w

        clientTracer' = contramap (Mx.WithBearer ("client" :: String)) activeTracer
        serverTracer' = contramap (Mx.WithBearer ("server" :: String)) activeTracer
        clientTracer  = Mx.TracersI clientTracer' clientTracer' clientTracer'
        serverTracer  = Mx.TracersI serverTracer' serverTracer' serverTracer'

    clientBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      QueueChannel { writeQueue = client_w, readQueue = client_r }
                      Nothing
    serverBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      QueueChannel { writeQueue = server_w, readQueue = server_r }
                      Nothing

    let clientApps :: [MiniProtocolInfo Mx.InitiatorResponderMode]
        clientApps = [ MiniProtocolInfo {
                        miniProtocolNum = Mx.MiniProtocolNum 2,
                        miniProtocolDir = Mx.InitiatorDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits,
                        miniProtocolCapability = Nothing
                       }
                     , MiniProtocolInfo {
                        miniProtocolNum = Mx.MiniProtocolNum 2,
                        miniProtocolDir = Mx.ResponderDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits,
                        miniProtocolCapability = clientCap
                      }
                     ]

        serverApps :: [MiniProtocolInfo Mx.InitiatorResponderMode]
        serverApps = [ MiniProtocolInfo {
                        miniProtocolNum = Mx.MiniProtocolNum 2,
                        miniProtocolDir = Mx.ResponderDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits,
                        miniProtocolCapability = serverCap
                       }
                     , MiniProtocolInfo {
                        miniProtocolNum = Mx.MiniProtocolNum 2,
                        miniProtocolDir = Mx.InitiatorDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits,
                        miniProtocolCapability = Nothing
                       }
                     ]


    clientCounters <- Counters.newCounters
    serverCounters <- Counters.newCounters
    clientMux <- Mx.new clientTracer (Just clientCounters) clientApps
    clientAsync <- async $ Mx.run clientMux clientBearer

    serverMux <- Mx.new serverTracer (Just serverCounters) serverApps
    serverAsync <- async $ Mx.run serverMux serverBearer

    r <- step clientMux clientApps serverMux serverApps messages
    -- Each side's counters, read while both muxes are live and again once
    -- both have ended and their counts are retired into the totals: no
    -- failures, and the bytes of its messages. A last message is counted
    -- after its peer could read it, so the live read waits for the counts
    -- to settle rather than sleep.
    let expected :: Mx.Counts
        expected = expectedCounts (Mx.sduSize clientBearer)

        snapshot :: STM IO (Mx.Counts, Mx.Counts)
        snapshot = (,) <$> Counters.readCounters clientCounters
                       <*> Counters.readCounters serverCounters
    _ <- timeout 10 $ atomically $ snapshot >>= check . (== (expected, expected))
    live <- atomically snapshot
    Mx.stop clientMux
    Mx.stop serverMux
    wait serverAsync
    wait clientAsync
    ended <- atomically snapshot
    return $ r .&&. counterexample ("live " ++ show live) (live === (expected, expected))
               .&&. counterexample ("ended " ++ show ended) (ended === (expected, expected))

  where
    -- What each side counts: both run the same exchanges in both directions,
    -- so the requests and the done are sent by the initiator and read by the
    -- responder, the responses the other way round, each message alone in
    -- its SDUs with a header per SDU.
    expectedCounts :: Mx.SDUSize -> Mx.Counts
    expectedCounts sduSize = Mx.Counts Mx.noFailures sent received
      where
        initiator, responder :: (Mx.MiniProtocolNum, Mx.MiniProtocolDir)
        initiator = (Mx.MiniProtocolNum 2, Mx.InitiatorDir)
        responder = (Mx.MiniProtocolNum 2, Mx.ResponderDir)

        sent, received :: Mx.ByteCounts
        sent     = M.fromList [ (initiator, asked), (responder, answered) ]
        received = M.fromList [ (initiator, answered), (responder, asked) ]

        asked, answered :: Word64
        asked    = sum [ onWire (MsgReq req)
                       | DummyTrace msgs <- messages
                       , (req, _) <- msgs
                       ]
                 + fromIntegral (length messages) * onWire MsgDone
        answered = sum [ onWire (MsgResp resp)
                       | DummyTrace msgs <- messages
                       , (_, resp) <- msgs
                       ]

        -- The message's encoding, and a header per SDU it is cut into.
        onWire :: MsgReqResp DummyPayload DummyPayload -> Word64
        onWire msg = len + fromIntegral Mx.msHeaderLength * ((len + sdu - 1) `div` sdu)
          where
            len, sdu :: Word64
            len = fromIntegral (BL.length (serialise msg))
            sdu = fromIntegral (Mx.getSDUSize sduSize)

    step _ _ _ _ [] = return $ property True
    step clientMux clientApps serverMux serverApps (msgs:msgss) = do
        (client_mp, server_mp) <- setupMiniReqRsp (return ()) msgs
        clientRes <- sequence
          [ Mx.runMiniProtocol
            clientMux
            miniProtocolNum
            miniProtocolDir
            strat
            chan
          | MiniProtocolInfo {miniProtocolNum, miniProtocolDir} <- clientApps
          , (strat, chan) <- case miniProtocolDir of
                              Mx.InitiatorDirection -> [(Mx.StartEagerly, client_mp)]
                              _                     -> [(Mx.StartOnDemand, server_mp)]
          ]
        serverRes <- sequence
          [ Mx.runMiniProtocol
            serverMux
            miniProtocolNum
            miniProtocolDir
            strat
            chan
          | MiniProtocolInfo {miniProtocolNum, miniProtocolDir} <- serverApps
          , (strat, chan) <- case miniProtocolDir of
                              Mx.InitiatorDirection -> [(Mx.StartEagerly, client_mp)]
                              _                     -> [(Mx.StartOnDemand, server_mp)]
          ]

        rs <- mapM getResult serverRes
        rc <- mapM getResult clientRes
        if and $ rs ++ rc
           then step clientMux clientApps serverMux serverApps msgss
           else return $ property False


    getResult :: STM IO (Either SomeException Bool) -> IO Bool
    getResult get = do
        r <- atomically get
        case r of
             (Left _)  -> return False
             (Right b) -> return b


-- | Like prop_mux_snd_recv but using the Compat interface.
prop_mux_snd_recv_compat :: DummyTrace
                         -> Property
prop_mux_snd_recv_compat messages = ioProperty $ do
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    endMpsVar <- newTVarIO 2

    let server_w = client_r
        server_r = client_w

        clientTracer' = contramap (Mx.WithBearer ("client" :: String)) activeTracer
        serverTracer' = contramap (Mx.WithBearer ("server" :: String)) activeTracer
        clientTracer  = Mx.TracersI clientTracer' clientTracer' clientTracer'
        serverTracer  = Mx.TracersI serverTracer' serverTracer' serverTracer'


    clientBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      QueueChannel { writeQueue = client_w, readQueue = client_r }
                     Nothing
    serverBearer <- getBearer makeQueueChannelBearer
                     (-1)
                     QueueChannel { writeQueue = server_w, readQueue = server_r }
                     Nothing
    (verify, client_mp, server_mp) <- setupMiniReqRspCompat
                                        (return ()) endMpsVar messages

    let clientBundle = [ MiniProtocolInfo {
                           miniProtocolNum        = Mx.MiniProtocolNum 2,
                           miniProtocolLimits     = defaultMiniProtocolLimits,
                           miniProtocolDir        = Mx.InitiatorDirectionOnly,
                           miniProtocolCapability = Nothing }
                       ]

        serverBundle = [ MiniProtocolInfo {
                           miniProtocolNum        = Mx.MiniProtocolNum 2,
                           miniProtocolLimits     = defaultMiniProtocolLimits,
                           miniProtocolDir        = Mx.ResponderDirectionOnly,
                           miniProtocolCapability = Nothing }
                       ]

    clientAsync <- async $ do
      clientMux <- Mx.new clientTracer Nothing clientBundle
      res <- Mx.runMiniProtocol
        clientMux
        (Mx.MiniProtocolNum 2)
        Mx.InitiatorDirectionOnly
        Mx.StartEagerly
        (\chann -> do
          r <- client_mp chann
          return (r, Nothing)
        )

      -- Wait for the first MuxApplication to finish, then stop the mux.
      withAsync (Mx.run clientMux clientBearer) $ \aid -> do
        _ <- atomically res
        Mx.stop clientMux
        wait aid

    serverAsync <- async $ do
      serverMux <- Mx.new serverTracer Nothing serverBundle
      res <- Mx.runMiniProtocol
        serverMux
        (Mx.MiniProtocolNum 2)
        Mx.ResponderDirectionOnly
        Mx.StartEagerly
        (\chann -> do
          r <- server_mp chann
          return (r, Nothing)
        )

      -- Wait for the first MuxApplication to finish, then stop the mux.
      withAsync (Mx.run serverMux serverBearer) $ \aid -> do
        _ <- atomically res
        Mx.stop serverMux
        wait aid

    _ <- waitBoth clientAsync serverAsync
    property <$> verify

-- | Create a verification function, a MiniProtocolDescription for the client
-- side and a MiniProtocolDescription for the server side for a RequestResponce
-- protocol.
--
setupMiniReqRspCompat :: IO ()
                      -- ^ Action performed by responder before processing the response
                      -> StrictTVar IO Int
                      -- ^ Total number of miniprotocols.
                      -> DummyTrace
                      -- ^ Trace of messages
                      -> IO ( IO Bool
                            , Mx.ByteChannel IO -> IO ((), Maybe BL.ByteString)
                            , Mx.ByteChannel IO -> IO ((), Maybe BL.ByteString)
                            )
setupMiniReqRspCompat serverAction mpsEndVar (DummyTrace msgs) = do
    serverResultVar <- newEmptyTMVarIO
    clientResultVar <- newEmptyTMVarIO

    return ( verifyCallback serverResultVar clientResultVar
           , clientApp clientResultVar
           , serverApp serverResultVar
           )
  where
    requests  = map fst msgs
    responses = map snd msgs

    verifyCallback serverResultVar clientResultVar =
        atomically $ (&&) <$> takeTMVar serverResultVar <*> takeTMVar clientResultVar

    reqRespServer :: [DummyPayload]
                  -> ReqRespServer DummyPayload DummyPayload IO Bool
    reqRespServer = go []
      where
        go reqs (resp:resps) = ReqRespServer {
            recvMsgReq  = \req -> serverAction >> return (resp, go (req:reqs) resps),
            recvMsgDone = pure $ reverse reqs == requests
          }
        go reqs [] = ReqRespServer {
            recvMsgReq  = error "server out of replies",
            recvMsgDone = pure $ reverse reqs == requests
          }


    reqRespClient :: [DummyPayload]
                  -> ReqRespClient DummyPayload DummyPayload IO Bool
    reqRespClient = go []
      where
        go resps []         = SendMsgDone (pure $ reverse resps == responses)
        go resps (req:reqs) = SendMsgReq req $ \resp -> return (go (resp:resps) reqs)

    clientApp :: StrictTMVar IO Bool
              -> Mx.ByteChannel IO
              -> IO ((), Maybe BL.ByteString)
    clientApp clientResultVar clientChan = do
        (result, trailing) <- runClientCBOR nullTracer clientChan (reqRespClient requests)
        atomically (putTMVar clientResultVar result)
        (,trailing) <$> end

    serverApp :: StrictTMVar IO Bool
              -> Mx.ByteChannel IO
              -> IO ((), Maybe BL.ByteString)
    serverApp serverResultVar serverChan = do
        (result, trailing) <- runServerCBOR nullTracer serverChan (reqRespServer responses)
        atomically (putTMVar serverResultVar result)
        (,trailing) <$> end

    -- Wait on all miniprotocol jobs before letting a miniprotocol thread exit.
    end = do
        atomically $ modifyTVar mpsEndVar (\a -> a - 1)
        atomically $ do
            c <- readTVar mpsEndVar
            unless (c == 0) retry

waitOnAllClients :: StrictTVar IO Int
                 -> Int
                 -> IO ()
waitOnAllClients clientVar clientTot = do
        atomically $ modifyTVar clientVar (+ 1)
        atomically $ do
            c <- readTVar clientVar
            unless (c == clientTot) retry

setupMiniReqRsp :: IO ()
                -- ^ Action performed by responder before processing the response
                -> DummyTrace
                -- ^ Trace of messages
                -> IO ( Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)
                      , Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)
                      )
setupMiniReqRsp serverAction (DummyTrace msgs) = do

    return ( clientApp
           , serverApp
           )
  where
    requests  = map fst msgs
    responses = map snd msgs

    reqRespServer :: [DummyPayload]
                  -> ReqRespServer DummyPayload DummyPayload IO Bool
    reqRespServer = go []
      where
        go reqs (resp:resps) = ReqRespServer {
            recvMsgReq  = \req -> serverAction >> return (resp, go (req:reqs) resps),
            recvMsgDone = pure $ reverse reqs == requests
          }
        go reqs [] = ReqRespServer {
            recvMsgReq  = error "server out of replies",
            recvMsgDone = pure $ reverse reqs == requests
          }

    reqRespClient :: [DummyPayload]
                  -> ReqRespClient DummyPayload DummyPayload IO Bool
    reqRespClient = go []
      where
        go resps []         = SendMsgDone (pure $ reverse resps == responses)
        go resps (req:reqs) = SendMsgReq req $ \resp -> return (go (resp:resps) reqs)

    clientApp :: Mx.ByteChannel IO
              -> IO (Bool, Maybe BL.ByteString)
    clientApp clientChan = runClientCBOR nullTracer clientChan (reqRespClient requests)

    serverApp :: Mx.ByteChannel IO
              -> IO (Bool, Maybe BL.ByteString)
    serverApp serverChan = runServerCBOR nullTracer serverChan (reqRespServer responses)

--
-- Running with queues and pipes
--

-- Run applications continuation
type RunMuxApplications
    =  [Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)]
    -> [Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)]
    -> IO Bool


runMuxApplication :: DummyCapability
                  -> [Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)]
                  -> Mx.Bearer IO
                  -> [Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)]
                  -> Mx.Bearer IO
                  -> IO Bool
runMuxApplication (DummyCapability rspCap) initApps initBearer respApps respBearer = do
    let clientTracer' = contramap (Mx.WithBearer ("client" :: String)) activeTracer
        serverTracer' = contramap (Mx.WithBearer ("server" :: String)) activeTracer
        clientTracer  = Mx.TracersI clientTracer' clientTracer' clientTracer'
        serverTracer  = Mx.TracersI serverTracer' serverTracer' serverTracer'
        protNum = [1..]
        respApps' = zip protNum respApps
        initApps' = zip protNum initApps

    respMux <- Mx.new serverTracer Nothing $ map (\(pn,_) ->
          MiniProtocolInfo {
            miniProtocolNum        = Mx.MiniProtocolNum pn,
            miniProtocolDir        = Mx.ResponderDirectionOnly,
            miniProtocolLimits     = defaultMiniProtocolLimits,
            miniProtocolCapability = rspCap
          }
        )
        respApps'
    respAsync <- async $ Mx.run respMux respBearer
    getRespRes <- sequence [ Mx.runMiniProtocol
                              respMux
                              (Mx.MiniProtocolNum pn)
                              Mx.ResponderDirectionOnly
                              Mx.StartOnDemand
                              app
                           | (pn, app) <- respApps'
                           ]

    initMux <- Mx.new clientTracer Nothing $ map (\(pn,_) ->
          MiniProtocolInfo {
            miniProtocolNum        = Mx.MiniProtocolNum pn,
            miniProtocolDir        = Mx.InitiatorDirectionOnly,
            miniProtocolLimits     = defaultMiniProtocolLimits,
            miniProtocolCapability = Nothing
          }
        )
        initApps'
    initAsync <- async $ Mx.run initMux initBearer
    getInitRes <- sequence [ Mx.runMiniProtocol
                              initMux
                              (Mx.MiniProtocolNum pn)
                              Mx.InitiatorDirectionOnly
                              Mx.StartEagerly
                              app
                           | (pn, app) <- initApps'
                           ]

    initRes <- mapM getResult getInitRes
    respRes <- mapM getResult getRespRes
    Mx.stop initMux
    Mx.stop respMux
    void $ waitBoth initAsync respAsync

    return $ and $ initRes ++ respRes

  where
    getResult :: STM IO (Either SomeException Bool) -> IO Bool
    getResult get = do
        r <- atomically get
        case r of
             (Left _)  -> return False
             (Right b) -> return b

runWithQueues :: DummyCapability
              -> RunMuxApplications
runWithQueues cap initApps respApps = do
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    let server_w = client_r
        server_r = client_w

    clientBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      QueueChannel { writeQueue = client_w, readQueue = client_r }
                      Nothing
    serverBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      QueueChannel { writeQueue = server_w, readQueue = server_r }
                      Nothing
    runMuxApplication cap initApps clientBearer respApps serverBearer

runWithPipe :: DummyCapability
            -> RunMuxApplications
runWithPipe cap initApps respApps =
#if defined(mingw32_HOST_OS)
    withIOManager $ \ioManager -> do
      let pipeName = "\\\\.\\pipe\\mux-test-pipe"
      bracket
        (Win32.NamedPipes.createNamedPipe
          pipeName
          (Win32.NamedPipes.pIPE_ACCESS_DUPLEX .|. Win32.File.fILE_FLAG_OVERLAPPED)
          (Win32.NamedPipes.pIPE_TYPE_BYTE .|. Win32.NamedPipes.pIPE_READMODE_BYTE)
          Win32.NamedPipes.pIPE_UNLIMITED_INSTANCES
          512
          512
          0
          Nothing)
       Win32.File.closeHandle
       $ \hSrv -> do
         bracket (Win32.File.createFile
                   pipeName
                   (Win32.File.gENERIC_READ .|. Win32.File.gENERIC_WRITE)
                   Win32.File.fILE_SHARE_NONE
                   Nothing
                   Win32.File.oPEN_EXISTING
                   Win32.File.fILE_FLAG_OVERLAPPED
                   Nothing)
                Win32.File.closeHandle
          $ \hCli -> do
             associateWithIOManager ioManager (Left hSrv)
             associateWithIOManager ioManager (Left hCli)

             let clientChannel = Mx.pipeChannelFromNamedPipe hCli
                 serverChannel = Mx.pipeChannelFromNamedPipe hSrv

             clientBearer <- getBearer makePipeChannelBearer (-1) clientChannel Nothing
             serverBearer <- getBearer makePipeChannelBearer (-1) serverChannel Nothing

             Win32.Async.connectNamedPipe hSrv
             runMuxApplication cap initApps clientBearer respApps serverBearer
#else
    bracket
      ((,) <$> createPipe <*> createPipe)
      (\((rCli, wCli), (rSrv, wSrv)) -> do
        hClose rCli
        hClose wCli
        hClose rSrv
        hClose wSrv)
      $ \ ((rCli, wCli), (rSrv, wSrv)) -> do
        let clientChannel = Mx.pipeChannelFromHandles rCli wSrv
            serverChannel = Mx.pipeChannelFromHandles rSrv wCli

        clientBearer <- getBearer makePipeChannelBearer (-1) clientChannel Nothing
        serverBearer <- getBearer makePipeChannelBearer (-1) serverChannel Nothing
        runMuxApplication cap initApps clientBearer respApps serverBearer
#endif

runWithSocket :: DummyCapability
              -> Maybe (Mx.ReadBuffer IO)
              -> Maybe (Mx.ReadBuffer IO)
              -> RunMuxApplications
runWithSocket cap clientBuf_m serverBuf_m initApps respApps = withIOManager (\iocp -> do
    bracket
      (do
        sd <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        associateWithIOManager iocp (Right sd)
        ephemAddr :_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
        Socket.bind sd (Socket.addrAddress ephemAddr)
        Socket.listen sd 1
        servAddr <- Socket.getSocketName sd
        cd <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        associateWithIOManager iocp (Right cd)
        Socket.connect cd servAddr
        (sd', _) <- Socket.accept sd
        associateWithIOManager iocp (Right sd')
        Socket.close sd
        return (cd, sd')
      )
      (\(cd, sd) -> do
        Socket.close cd
        Socket.close sd
      )
      (\(cd, sd) -> do
        clientB <- mkBearer clientBuf_m cd
        serverB <- mkBearer serverBuf_m sd

        runMuxApplication cap initApps clientB respApps serverB
      )
   )
  where
    mkBearer buf_m sock = getBearer (makeSocketBearer' 0.001) (-1) sock buf_m

-- | Verify that it is possible to run two miniprotocols over the same bearer.
-- Makes sure that messages are delivered to the correct miniprotocol in order.
--
test_mux_1_mini :: RunMuxApplications
                -> DummyTrace
                -> IO Bool
test_mux_1_mini run msgTrace = do
    (clientApp, serverApp) <- setupMiniReqRsp (return ()) msgTrace
    run [clientApp] [serverApp]


prop_mux_1_mini_Queue :: DummyCapability -> DummyTrace -> Property
prop_mux_1_mini_Queue cap = ioProperty . test_mux_1_mini (runWithQueues cap)

prop_mux_1_mini_Pipe :: DummyCapability -> DummyTrace -> Property
prop_mux_1_mini_Pipe cap = ioProperty . test_mux_1_mini (runWithPipe cap)

prop_mux_1_mini_Socket :: DummyCapability -> DummyTrace -> Property
prop_mux_1_mini_Socket cap = ioProperty . test_mux_1_mini (runWithSocket cap Nothing Nothing)

prop_mux_1_mini_Socket_buf :: DummyCapability -> DummyTrace -> Property
prop_mux_1_mini_Socket_buf cap dt = ioProperty $ withReadBufferIO (\buf_a -> withReadBufferIO (\buf_b ->
    test_mux_1_mini (runWithSocket cap buf_a buf_b) dt))

-- | Verify that it is possible to run two miniprotocols over the same bearer.
-- Makes sure that messages are delivered to the correct miniprotocol in order.
--
test_mux_2_minis
    :: RunMuxApplications
    -> DummyTrace
    -> DummyTrace
    -> IO Bool
test_mux_2_minis  run msgTrace0 msgTrace1 = do
    (clientApp0, serverApp0) <-
        setupMiniReqRsp (return ()) msgTrace0
    (clientApp1, serverApp1) <-
        setupMiniReqRsp (return ()) msgTrace1
    run [clientApp0, clientApp1] [serverApp0, serverApp1]


prop_mux_2_minis_Queue :: DummyCapability
                       -> DummyTrace
                       -> DummyTrace
                       -> Property
prop_mux_2_minis_Queue cap a b = ioProperty $ test_mux_2_minis (runWithQueues cap) a b

prop_mux_2_minis_Pipe :: DummyCapability
                      -> DummyTrace
                      -> DummyTrace
                      -> Property
prop_mux_2_minis_Pipe cap a b = ioProperty $ test_mux_2_minis (runWithPipe cap) a b

prop_mux_2_minis_Socket :: DummyCapability
                        -> DummyTrace
                        -> DummyTrace
                        -> Property
prop_mux_2_minis_Socket cap a b = ioProperty $ test_mux_2_minis (runWithSocket cap Nothing Nothing) a b

prop_mux_2_minis_Socket_buf :: DummyCapability
                            -> DummyTrace
                            -> DummyTrace
                            -> Property
prop_mux_2_minis_Socket_buf cap a b = ioProperty $
  withReadBufferIO (\buf_a -> withReadBufferIO (\buf_b ->
      test_mux_2_minis (runWithSocket cap buf_a buf_b) a b))

-- | Attempt to verify that capacity is diveded fairly between two active
-- miniprotocols.  Two initiators send a request over two different
-- miniprotocols and the corresponding responders each send a large reply back.
-- The Mux bearer should alternate between sending data for the two responders.
--
prop_mux_starvation :: Uneven
                    -> Property
prop_mux_starvation (Uneven response0 response1) =
    let sduLen = Mx.SDUSize 1280 in
    (BL.length (unDummyPayload response0) > 2 * fromIntegral (Mx.getSDUSize sduLen)) &&
    (BL.length (unDummyPayload response1) > 2 * fromIntegral (Mx.getSDUSize sduLen)) ==>
    ioProperty $ do
    let request       = DummyPayload $ BL.replicate 4 0xa

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    activeMpsVar <- newTVarIO 0
    traceHeaderVar <- newTVarIO []
    let headerTracer =
          mkTracer $ \e -> case e of
            Mx.TraceRecvHeaderEnd header
              -> atomically (modifyTVar traceHeaderVar (header:))
            _ -> return ()

    let server_w = client_r
        server_r = client_w

        clientTracer' = contramap (Mx.WithBearer ("client" :: String)) activeTracer
        serverTracer' = contramap (Mx.WithBearer ("server" :: String)) activeTracer
        clientTracer  = Mx.TracersI clientTracer' clientTracer' (clientTracer' <> headerTracer)
        serverTracer  = Mx.TracersI serverTracer' serverTracer' serverTracer'

    clientBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      QueueChannel { writeQueue = client_w, readQueue = client_r }
                      Nothing
    serverBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      QueueChannel { writeQueue = server_w, readQueue = server_r }
                      Nothing
    (client_short, server_short) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                         $ DummyTrace [(request, response1)]
    (client_long, server_long) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                         $ DummyTrace [(request, response1)]


    let clientApp2, clientApp3 :: MiniProtocolInfo Mx.InitiatorMode
        clientApp2 = MiniProtocolInfo {
                         miniProtocolNum = Mx.MiniProtocolNum 2,
                         miniProtocolDir = Mx.InitiatorDirectionOnly,
                         miniProtocolLimits = defaultMiniProtocolLimits,
                         miniProtocolCapability = Nothing
                       }
        clientApp3 = MiniProtocolInfo {
                         miniProtocolNum = Mx.MiniProtocolNum 3,
                         miniProtocolDir = Mx.InitiatorDirectionOnly,
                         miniProtocolLimits = defaultMiniProtocolLimits,
                         miniProtocolCapability = Nothing
                       }

        serverApp2, serverApp3 :: MiniProtocolInfo Mx.ResponderMode
        serverApp2 = MiniProtocolInfo {
                         miniProtocolNum = Mx.MiniProtocolNum 2,
                         miniProtocolDir = Mx.ResponderDirectionOnly,
                         miniProtocolLimits = defaultMiniProtocolLimits,
                         miniProtocolCapability = Nothing
                       }
        serverApp3 = MiniProtocolInfo {
                         miniProtocolNum = Mx.MiniProtocolNum 3,
                         miniProtocolDir = Mx.ResponderDirectionOnly,
                         miniProtocolLimits = defaultMiniProtocolLimits,
                         miniProtocolCapability = Nothing
                       }

    serverMux <- Mx.new serverTracer Nothing [serverApp2, serverApp3]
    serverMux_aid <- async $ Mx.run serverMux serverBearer
    serverRes2 <- Mx.runMiniProtocol serverMux (miniProtocolNum serverApp2) (miniProtocolDir serverApp2)
                   Mx.StartOnDemand server_short
    serverRes3 <- Mx.runMiniProtocol serverMux (miniProtocolNum serverApp3) (miniProtocolDir serverApp3)
                   Mx.StartOnDemand server_long

    clientMux <- Mx.new clientTracer Nothing [clientApp2, clientApp3]
    clientMux_aid <- async $ Mx.run clientMux clientBearer
    clientRes2 <- Mx.runMiniProtocol clientMux (miniProtocolNum clientApp2) (miniProtocolDir clientApp2)
                   Mx.StartEagerly client_short
    clientRes3 <- Mx.runMiniProtocol clientMux (miniProtocolNum clientApp3) (miniProtocolDir clientApp3)
                   Mx.StartEagerly client_long


    -- Fetch results
    srvRes2 :: Either SomeException Bool <- atomically serverRes2
    srvRes3 :: Either SomeException Bool <- atomically serverRes3
    cliRes2 :: Either SomeException Bool <- atomically clientRes2
    cliRes3 :: Either SomeException Bool <- atomically clientRes3

    -- First verify that all messages where received correctly
    let res_short = case (srvRes2, cliRes2) of
                         (Left _, _)        -> False
                         (_, Left _)        -> False
                         (Right a, Right b) -> a && b
    let res_long  = case (srvRes3, cliRes3) of
                         (Left _, _)        -> False
                         (_, Left _)        -> False
                         (Right a, Right b) -> a && b

    Mx.stop serverMux
    Mx.stop clientMux
    _ <- waitBoth serverMux_aid clientMux_aid

    -- Then look at the message trace to check for starvation.
    trace <- atomically $ readTVar traceHeaderVar
    let es = map Mx.mhNum (take 100 (reverse trace))
        ls = dropWhile (\e -> e == head es) es
        fair = verifyStarvation ls
    return $ res_short .&&. res_long .&&. fair
  where
   -- We can't make 100% sure that both servers start responding at the same
   -- time but once they are both up and running messages should alternate
   -- between ReqResp2 and ReqResp3
    verifyStarvation :: Eq a => [a] -> Property
    verifyStarvation [] = property True
    verifyStarvation ms =
      let ms' = dropWhileEnd (\e -> e == last ms)
                  (head ms : dropWhile (\e -> e == head ms) ms)
                ++ [last ms]
      in
        label ("length " ++ labelPr_ ((length ms' * 100) `div` length ms) ++ "%")
        $ label ("length " ++ label_ (length ms')) $ alternates ms'

      where
        alternates []           = True
        alternates (_:[])       = True
        alternates (a : b : as) = a /= b && alternates (b : as)

    label_ :: Int -> String
    label_ n = mconcat
      [ show $ n `div` 10 * 10
      , "-"
      , show $ n `div` 10 * 10 + 10 - 1
      ]

    labelPr_ :: Int -> String
    labelPr_ n | n >= 100  = "100"
               | otherwise = label_ n


encodeInvalidMuxSDU :: InvalidSDU -> BL.ByteString
encodeInvalidMuxSDU sdu =
    let header = Bin.runPut enc in
    BL.append header $ BL.replicate (fromIntegral $ isLength sdu) (isPattern sdu)
  where
    enc = do
        Bin.putWord32be $ Mx.unRemoteClockModel $ isTimestamp sdu
        Bin.putWord16be $ isIdAndMode sdu
        Bin.putWord16be $ isLength sdu

-- | An 'Mx.SDU' header for the codec round-trip test: covers both
-- directions as well as boundary timestamps, mini-protocol numbers and
-- payload lengths.
--
data SDUCodecCase = SDUCodecCase Word32 Word16 Mx.MiniProtocolDir Word16
  deriving Show

instance Arbitrary SDUCodecCase where
    arbitrary =
        SDUCodecCase
          <$> boundary [0, maxBound]                  -- timestamp
          <*> ((.&. 0x7fff) <$> boundary [0, 0x7fff]) -- mini-protocol number
          <*> elements [Mx.InitiatorDir, Mx.ResponderDir]
          <*> boundary [0, 1, maxBound]               -- payload length
      where
        boundary :: Arbitrary a => [a] -> Gen a
        boundary xs = frequency ([(1, pure x) | x <- xs] ++ [(3, arbitrary)])

-- | Wire format and round-trip check of 'Mx.encodeSDU' / 'Mx.decodeSDU'.
--
-- The encoding must be byte identical to a reference 'Bin.runPut' based
-- encoder (the previous implementation of 'Mx.encodeSDU') and
-- 'Mx.decodeSDU' must recover the header exactly.  Note that
-- 'Mx.decodeSDU' rejects zero length SDUs and truncated headers.
--
prop_sdu_codec :: SDUCodecCase -> Property
prop_sdu_codec (SDUCodecCase ts num dir len) =
         counterexample "wire format differs from reference encoder"
           (encoded === reference)
    .&&. forAll (choose (0, Mx.msHeaderLength - 1)) (\k ->
           counterexample ("truncated header of length " ++ show k ++ " not rejected") $
             case Mx.decodeSDU (BL.take k encoded) of
               Left (Mx.SDUDecodeError _) -> True
               _                          -> False)
    .&&. case Mx.decodeSDU encoded of
           Left (Mx.SDUDecodeError e) ->
             counterexample ("unexpected decode error: " ++ e) (len === 0)
           Left e ->
             counterexample ("unexpected error: " ++ show e) False
           Right sdu' ->
                  len =/= 0
             .&&. Mx.unRemoteClockModel (Mx.msTimestamp sdu') === ts
             .&&. Mx.msNum sdu'    === Mx.MiniProtocolNum num
             .&&. Mx.msDir sdu'    === dir
             .&&. Mx.msLength sdu' === len
  where
    blob = BL.replicate (fromIntegral len) 0xa5
    sdu  = Mx.SDU {
        Mx.msHeader = Mx.SDUHeader {
            Mx.mhTimestamp = Mx.RemoteClockModel ts,
            Mx.mhNum       = Mx.MiniProtocolNum num,
            Mx.mhDir       = dir,
            Mx.mhLength    = len
          },
        Mx.msBlob = blob
      }
    encoded   = Mx.encodeSDU sdu
    reference = Bin.runPut refPut `BL.append` blob
    refPut = do
        Bin.putWord32be ts
        Bin.putWord16be $ case dir of
                            Mx.InitiatorDir -> num
                            Mx.ResponderDir -> num .|. 0x8000
        Bin.putWord16be len

-- | Verify ingress processing of valid and invalid SDUs.
--
prop_demux_sdu :: forall m.
                    ( Alternative (STM m)
                    , MonadAsync m
                    , MonadDelay m
                    , MonadEvaluate m
                    , MonadFork m
                    , MonadLabelledSTM m
                    , MonadMask m
                    , MonadSay m
                    , MonadThrow (STM m)
                    , MonadTimer m
                    )
                 => ArbitrarySDU
                 -> m Property
prop_demux_sdu a = do
    r <- run a
    return $ tabulate "SDU type" [stateLabel a] $
             tabulate "SDU Violation " [violationLabel a] r
  where
    run (ArbitraryValidSDU sdu (Just Mx.IngressQueueOverRun {})) = do
        stopVar <- newEmptyTMVarIO

        -- To trigger MuxIngressQueueOverRun we use a special test protocol
        -- with an ingress queue which is less than 0xffff so that it can be
        -- triggered by a single segment.
        let server_mps = MiniProtocolInfo {
                           miniProtocolNum = Mx.MiniProtocolNum 2,
                           miniProtocolDir = Mx.ResponderDirectionOnly,
                           miniProtocolLimits = smallMiniProtocolLimits,
                           miniProtocolCapability = Nothing
                         }

        (client_w, said, waitServerRes, mux, counters) <- plainServer server_mps (serverRsp stopVar)

        writeSdu client_w $! unDummyPayload sdu

        atomically $! putTMVar stopVar $ unDummyPayload sdu

        _ <- atomically waitServerRes
        Mx.stop mux
        res <- waitCatch said
        counts <- atomically (Counters.readCounters counters)
        case res of
            Left e  ->
                case fromException e of
                    Just me ->
                      return $ case me of
                        -- The overrun is counted; the SDU that overran is not,
                        -- the write that would have counted it aborted.
                        Mx.IngressQueueOverRun {} ->
                          counted counts (countsOf Mx.noFailures { Mx.mfOverruns = 1 } [])
                        _ -> counterexample (show me) False
                    Nothing -> return $ counterexample (show e) False
            Right _ -> return $ counterexample "expected an exception" False

    run (ArbitraryValidSDU sdu err_m) = do
        stopVar <- newEmptyTMVarIO

        let server_mps = MiniProtocolInfo {
                            miniProtocolNum = Mx.MiniProtocolNum 2,
                            miniProtocolDir = Mx.ResponderDirectionOnly,
                            miniProtocolLimits = defaultMiniProtocolLimits,
                            miniProtocolCapability = Nothing
                          }

        (client_w, said, waitServerRes, mux, counters) <- plainServer server_mps (serverRsp stopVar)

        atomically $! putTMVar stopVar $! unDummyPayload sdu
        writeSdu client_w $ unDummyPayload sdu

        _ <- atomically waitServerRes
        Mx.stop mux
        res <- waitCatch said
        counts <- atomically (Counters.readCounters counters)
        case res of
            Left e  ->
                case fromException e of
                    Just me -> case err_m of
                                    Just err -> return $ counterexample (show me)
                                                       $ me `compareErrors` err
                                    Nothing  -> return $ counterexample (show me) False
                    Nothing -> return $ counterexample (show e) False
            -- Every SDU read is counted, header included, under our responder.
            Right _ -> return $ counterexample "expected an exception" (isNothing err_m)
                          .&&. counted counts
                                 (countsOf Mx.noFailures (fragments (unDummyPayload sdu)))

    run (ArbitraryInvalidSDU badSdu err) = do
        stopVar <- newEmptyTMVarIO

        let server_mps = MiniProtocolInfo {
                            miniProtocolNum = Mx.MiniProtocolNum 2,
                            miniProtocolDir = Mx.ResponderDirectionOnly,
                            miniProtocolLimits = defaultMiniProtocolLimits,
                            miniProtocolCapability = Nothing
                          }

        (client_w, said, waitServerRes, mux, counters) <- plainServer server_mps (serverRsp stopVar)

        atomically $ writeTBQueue client_w $
                       BL.take (fromIntegral (isRealLength badSdu))
                               (encodeInvalidMuxSDU badSdu)
        -- Incase this is an SDU with a payload of 0 byte, we still ask the responder to wait for
        -- one byte so that we fail with an exception while parsing the header instead of risk
        -- having the responder succed after reading 0 bytes.
        atomically $ putTMVar stopVar $ BL.replicate (max (fromIntegral $ isLength badSdu) 1) 0xa

        _ <- atomically waitServerRes
        Mx.stop mux
        res <- waitCatch said
        counts <- atomically (Counters.readCounters counters)
        case res of
            Left e  ->
                case fromException e of
                    -- A protocol error, and nothing read: an unknown protocol
                    -- has no slot, and an undecodable SDU never reaches one.
                    Just me -> return $ counterexample (show me) (me `compareErrors` err)
                                   .&&. counted counts
                                          (countsOf Mx.noFailures { Mx.mfProtocolErrors = 1 } [])
                    Nothing -> return $ counterexample (show e) False
            Right _ -> return $ counterexample "expected an exception" False

    plainServer :: MiniProtocolInfo  mode
                -> (Mx.ByteChannel m -> m (a, Maybe BL.ByteString))
                -> m ( StrictTBQueue m BL.ByteString
                     , Async m ()
                     , STM m (Either SomeException a), Mx.Mux mode m
                     , Mx.Counters m
                     )
    plainServer serverApp server_mp = do
        server_w <- atomically $ newTBQueue 10
        server_r <- atomically $ newTBQueue 10

        let serverTracer' = contramap (Mx.WithBearer ("server" :: String)) activeTracer
            serverTracer  = Mx.TracersI serverTracer' serverTracer' serverTracer'

        serverBearer <- getBearer makeQueueChannelBearer
                          (-1)
                          QueueChannel { writeQueue = server_w,
                                         readQueue  = server_r
                                       }
                          Nothing

        counters  <- Counters.newCounters
        serverMux <- Mx.new serverTracer (Just counters) [serverApp]
        serverRes <- Mx.runMiniProtocol serverMux
                                        (Mx.miniProtocolNum serverApp)
                                        (Mx.miniProtocolDir serverApp)
                                        Mx.StartEagerly
                                        server_mp

        said <- async $ Mx.run serverMux serverBearer
        return (server_r, said, serverRes, serverMux, counters)

    -- Server that expects to receive a specific ByteString.
    -- Doesn't send a reply.
    serverRsp :: StrictTMVar m BL.ByteString
              -> Mx.ByteChannel m
              -> m ((), Maybe BL.ByteString)
    serverRsp stopVar chan =
        atomically (takeTMVar stopVar) >>= loop
      where
        loop e | BL.null e = return ((), Nothing)
        loop e = do
            msg_m <- Mx.recv chan
            case msg_m of
                 Just msg ->
                     case BL.stripPrefix msg e of
                          Just e' -> loop e'
                          Nothing -> error "recv corruption"
                 Nothing -> error "eof corruption"


    writeSdu :: StrictTBQueue m BL.ByteString
             -> BL.ByteString
             -> m ()
    writeSdu queue payload =
      forM_ (fragments payload) $ \frag -> do
        let sdu' = Mx.SDU
                    (Mx.SDUHeader
                      (Mx.RemoteClockModel 0)
                      (Mx.MiniProtocolNum 2)
                      Mx.InitiatorDir
                      (fromIntegral $ BL.length frag))
                    frag
            !pkt = Mx.encodeSDU (sdu' :: Mx.SDU)
        atomically $ writeTBQueue queue pkt

    -- The payload cut into the SDUs 'writeSdu' sends, of at most 0xffff bytes.
    fragments :: BL.ByteString -> [BL.ByteString]
    fragments payload
      | BL.null payload = []
      | otherwise       = frag : fragments rest
      where
        (frag, rest) = BL.splitAt 0xffff payload

    -- The counters once the mux has ended: the failures, and the SDUs the
    -- demuxer read, header included, under our responder.
    countsOf :: Mx.Failures -> [BL.ByteString] -> Mx.Counts
    countsOf failures frags = Mx.noCounts { Mx.mcFailures = failures, Mx.mcReceived = received }
      where
        received :: Mx.ByteCounts
        received = M.fromList [ ((Mx.MiniProtocolNum 2, Mx.ResponderDir), bytes)
                              | not (null frags)
                              ]

        bytes :: Word64
        bytes = sum [ fromIntegral Mx.msHeaderLength + fromIntegral (BL.length frag)
                    | frag <- frags
                    ]

    counted :: Mx.Counts -> Mx.Counts -> Property
    counted counts expected = counterexample (show counts) (counts === expected)

    stateLabel (ArbitraryInvalidSDU _ _) = "Invalid"
    stateLabel (ArbitraryValidSDU _ _)   = "Valid"

    violationLabel (ArbitraryValidSDU _ err_m) = sduViolation err_m
    violationLabel (ArbitraryInvalidSDU _ err) = sduViolation $ Just err

    sduViolation (Just Mx.UnknownMiniProtocol {}) = "unknown miniprotocol"
    sduViolation (Just Mx.SDUDecodeError {})      = "decode error"
    sduViolation (Just Mx.IngressQueueOverRun {}) = "ingress queue overrun"
    sduViolation (Just _)                         = "unknown violation"
    sduViolation Nothing                          = "none"

prop_demux_sdu_sim :: ArbitrarySDU
                   -> Property
prop_demux_sdu_sim badSdu =
    let r_e =  runSimStrictShutdown $ prop_demux_sdu badSdu in
    case r_e of
         Left  e -> counterexample (show e) False
         Right r -> r

prop_demux_sdu_io :: ArbitrarySDU
                  -> Property
prop_demux_sdu_io badSdu = ioProperty $ prop_demux_sdu badSdu

instance Arbitrary Mx.MiniProtocolNum where
    arbitrary = do
        n <- arbitrary
        return $ Mx.MiniProtocolNum $ 0x7fff .&. n

    shrink (Mx.MiniProtocolNum n) = Mx.MiniProtocolNum <$> shrink n

instance Arbitrary Mx.Mode where
    arbitrary = elements [Mx.InitiatorMode, Mx.ResponderMode, Mx.InitiatorResponderMode]

    shrink Mx.InitiatorResponderMode = [Mx.InitiatorMode, Mx.ResponderMode]
    shrink _                         = []

data DummyAppResult = DummyAppSucceed | DummyAppFail deriving (Eq, Show)

instance Arbitrary DummyAppResult where
    arbitrary = elements [DummyAppSucceed, DummyAppFail]

instance Arbitrary DiffTime where
    arbitrary = fromIntegral <$> choose (0, 100::Word16)

    shrink = map (fromRational . getNonNegative)
           . shrink
           . NonNegative
           . toRational

-- | An arbitrary instance for `StartOnDemand` & `StartOnDemandAny`.
--
newtype DummyStart = DummyStart {
    unDummyStart :: Mx.StartOnDemandOrEagerly
  } deriving (Eq, Show)

instance Arbitrary DummyStart where
  -- Only used for responder side so we don't generate StartEagerly
  arbitrary = fmap DummyStart (elements [Mx.StartOnDemand, Mx.StartOnDemandAny])

  shrink (DummyStart Mx.StartOnDemandAny) = [DummyStart Mx.StartOnDemand]
  shrink _                                = []

data DummyApp = DummyApp {
      daNum        :: !Mx.MiniProtocolNum
    , daAction     :: !DummyAppResult
    , daStart      :: !DummyStart
    , daRunTime    :: !DiffTime
    , daStartAfter :: !DiffTime
    } deriving (Eq, Show)

instance Arbitrary DummyApp where
    arbitrary = DummyApp <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data DummyApps =
    DummyResponderApps [DummyApp]
  | DummyResponderAppsKillMux [DummyApp]
  | DummyInitiatorApps [DummyApp]
  | DummyInitiatorResponderApps [DummyApp]
  deriving Show

instance Arbitrary DummyApps where
    arbitrary = do
        nums <- listOf1 arbitrary
        apps <- mapM genApp $ nub nums
        mode <- arbitrary
        case mode of
             Mx.InitiatorMode          -> return $ DummyInitiatorApps $
                                            map (\a -> a { daStart = DummyStart Mx.StartEagerly }) apps
             Mx.ResponderMode          -> frequency [ (3, return $ DummyResponderApps apps)
                                                    , (1, return $ DummyResponderAppsKillMux apps)
                                                    ]
             Mx.InitiatorResponderMode -> return $ DummyInitiatorResponderApps apps

      where
        genApp num = DummyApp num <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

    shrink (DummyResponderApps apps) = [ DummyResponderApps apps'
                                       | apps' <- filter (not . null) $ shrinkList (const []) apps
                                       ]
    shrink (DummyResponderAppsKillMux apps)
                                     = [ DummyResponderAppsKillMux apps'
                                       | apps' <- filter (not . null) $ shrinkList (const []) apps
                                       ]
    shrink (DummyInitiatorApps apps) = [ DummyResponderApps apps'
                                       | apps' <- filter (not . null) $ shrinkList (const []) apps
                                       ]
    shrink (DummyInitiatorResponderApps apps) = [ DummyResponderApps apps'
                                       | apps' <- filter (not . null) $ shrinkList (const []) apps
                                       ]

dummyAppToChannel :: forall m.
                     ( MonadAsync m
                     , MonadDelay m
                     , MonadCatch m
                     )
                  => DummyApp
                  -> (Mx.ByteChannel m -> m ((), Maybe BL.ByteString))
dummyAppToChannel DummyApp {daAction, daRunTime} = \_ -> do
    threadDelay daRunTime
    case daAction of
         DummyAppSucceed -> return ((), Nothing)
         DummyAppFail    -> throwIO $ Mx.Shutdown Nothing Mx.Ready

data DummyRestartingApps =
    DummyRestartingResponderApps [(DummyApp, Int)]
  | DummyRestartingInitiatorApps [(DummyApp, Int)]
  | DummyRestartingInitiatorResponderApps [(DummyApp, Int)]
  deriving Show

instance Arbitrary DummyRestartingApps where
    arbitrary = do
        nums <- listOf1 arbitrary
        apps <- mapM genApp $ nub nums
        mode <- arbitrary
        case mode of
             Mx.InitiatorMode          -> return $ DummyRestartingInitiatorApps apps
             Mx.ResponderMode          -> return $ DummyRestartingResponderApps apps
             Mx.InitiatorResponderMode -> return $ DummyRestartingInitiatorResponderApps apps
      where
        genApp num = do
            app <- DummyApp num DummyAppSucceed <$> arbitrary <*> arbitrary <*> arbitrary
            restarts <- choose (0, 5)
            return (app, restarts)


dummyRestartingAppToChannel :: forall a m.
                     ( MonadAsync m
                     , MonadCatch m
                     , MonadDelay m
                     )
                  => (DummyApp, a)
                  -> (Mx.ByteChannel m -> m ((DummyApp, a), Maybe BL.ByteString))
dummyRestartingAppToChannel (app, r) = \_ -> do
    threadDelay $ daRunTime app
    case daAction app of
         DummyAppSucceed -> return ((app, r), Nothing)
         DummyAppFail    -> throwIO $ Mx.Shutdown Nothing Mx.Ready


appToInfo :: Mx.MiniProtocolDirection mode -> DummyApp -> MiniProtocolInfo mode
appToInfo d da = MiniProtocolInfo (daNum da) d defaultMiniProtocolLimits Nothing

triggerApp :: forall m.
              ( MonadAsync m
              , MonadDelay m
              , MonadSay m
              )
            => Mx.Bearer m
            -> DummyApp
            -> m ()
triggerApp bearer app = do
    let chan = Mx.bearerAsChannel nullTracer bearer (daNum app) Mx.InitiatorDir
    traceWith verboseTracer $ "app waiting " ++ (show $ daNum app)
    threadDelay (daStartAfter app)
    traceWith verboseTracer $ "app starting " ++ (show $ daNum app)
    Mx.send chan $ BL.singleton 0xa5
    return ()

prop_mux_start_mX :: forall m.
                       ( Alternative (STM m)
                       , MonadAsync m
                       , MonadDelay m
                       , MonadEvaluate m
                       , MonadFork m
                       , MonadLabelledSTM m
                       , MonadMask m
                       , MonadSay m
                       , MonadThrow (STM m)
                       , MonadTimer m
                       )
                    => DummyApps
                    -> DiffTime
                    -> m Property
prop_mux_start_mX apps runTime = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    bearer <-
      getBearer makeQueueChannelBearer
        (-1)
        QueueChannel { writeQueue = mux_w, readQueue = mux_r }
        Nothing
    peerBearer <-
      getBearer makeQueueChannelBearer
        (-1)
        QueueChannel { writeQueue = mux_r, readQueue = mux_w }
        Nothing
    prop_mux_start_m bearer (triggerApp peerBearer) checkRes apps runTime anyStartAfter

  where
    anyStartAfter :: DiffTime
    anyStartAfter =
      case apps of
           DummyResponderApps as          -> minimum (map daStartAfter as)
           DummyResponderAppsKillMux as   -> minimum (map daStartAfter as)
           DummyInitiatorApps as          -> minimum (map daStartAfter as)
           DummyInitiatorResponderApps as -> minimum (map daStartAfter as)

    checkRes :: DiffTime
             -> ((STM m (Either SomeException ())), DummyApp)
             -> m (Property, Either SomeException ())
    checkRes minRunTime (get,da) = do
        let totTime = case unDummyStart (daStart da) of
                           Mx.StartOnDemand    -> daRunTime da + daStartAfter da
                           Mx.StartOnDemandAny -> daRunTime da + anyStartAfter
                           Mx.StartEagerly     -> daRunTime da
        r <- atomically get
        case daAction da of
             DummyAppSucceed ->
                 case r of
                      Left _  -> return (counterexample
                                          (formatToString (F.shown %+ "≰" %+ F.shown) minRunTime totTime)
                                          (minRunTime <= totTime)
                                        , r)
                      Right _ -> return (counterexample
                                          (formatToString (F.shown %+ "≱" %+ F.shown) minRunTime totTime)
                                          (minRunTime >= totTime)
                                        , r)
             DummyAppFail ->
                 case r of
                      Left _  -> return (property True, r)
                      Right _ -> return (counterexample "not-failed" False, r)

prop_mux_restart_m :: forall m.
                       ( Alternative (STM m)
                       , MonadAsync m
                       , MonadDelay m
                       , MonadEvaluate m
                       , MonadFork m
                       , MonadLabelledSTM m
                       , MonadMask m
                       , MonadSay m
                       , MonadThrow (STM m)
                       , MonadTimer m
                       )
                    => DummyRestartingApps
                    -> m Property
prop_mux_restart_m (DummyRestartingInitiatorApps apps) = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    bearer <- getBearer Mx.makeQueueChannelBearer
                (-1)
                QueueChannel { writeQueue = mux_w, readQueue = mux_r }
                Nothing
    let minis = map (appToInfo Mx.InitiatorDirectionOnly . fst) apps

    mux <- Mx.new Mx.nullTracers Nothing minis
    mux_aid <- async $ Mx.run mux bearer
    getRes <- sequence [ Mx.runMiniProtocol
                           mux
                          (daNum $ fst app)
                          Mx.InitiatorDirectionOnly
                          Mx.StartEagerly
                          (dummyRestartingAppToChannel app)
                       | app <- apps
                       ]
    r <- runRestartingApps mux $ M.fromList $ zip (map (daNum . fst) apps) getRes
    Mx.stop mux
    void $ waitCatch mux_aid
    return $ property r

  where
    runRestartingApps :: Mux Mx.InitiatorMode m
                      -> M.Map Mx.MiniProtocolNum (STM m (Either SomeException (DummyApp, Int)))
                      -> m Bool
    runRestartingApps _ ops | M.null ops = return True
    runRestartingApps mux ops = do
        appResult <- atomically $ foldr (<|>) retry $ M.elems ops
        case appResult of
             Left _ -> return False
             Right (app, 0) -> do
                 runRestartingApps mux $ M.delete (daNum app) ops
             Right (app, restarts) -> do
                 op <- Mx.runMiniProtocol mux (daNum app) Mx.InitiatorDirectionOnly Mx.StartEagerly
                         (dummyRestartingAppToChannel (app, restarts - 1))
                 runRestartingApps mux $ M.insert (daNum app) op ops

prop_mux_restart_m (DummyRestartingResponderApps rapps) = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    bearer <-
      getBearer makeQueueChannelBearer
        (-1)
        QueueChannel { writeQueue = mux_w, readQueue = mux_r }
        Nothing
    peerBearer <-
      getBearer makeQueueChannelBearer
        (-1)
        QueueChannel { writeQueue = mux_r, readQueue = mux_w }
        Nothing
    let apps = map fst rapps
        minis = map (appToInfo Mx.ResponderDirectionOnly) apps

    mux <- Mx.new Mx.nullTracers Nothing minis
    mux_aid <- async $ Mx.run mux bearer
    getRes <- sequence [ Mx.runMiniProtocol
                           mux
                          (daNum $ fst app)
                          Mx.ResponderDirectionOnly
                          Mx.StartEagerly
                          (dummyRestartingAppToChannel app)
                       | app <- rapps
                       ]
    triggers <- mapM (async . (triggerApp peerBearer)) apps
    r <- runRestartingApps mux $ M.fromList $ zip (map daNum apps) getRes
    Mx.stop mux
    void $ waitCatch mux_aid
    mapM_ cancel triggers
    return $ property r
  where
    runRestartingApps :: Mux Mx.ResponderMode m
                      -> M.Map Mx.MiniProtocolNum (STM m (Either SomeException (DummyApp, Int)))
                      -> m Bool
    runRestartingApps _ ops | M.null ops = return True
    runRestartingApps mux ops = do
        appResult <- atomically $ foldr (<|>) retry $ M.elems ops
        case appResult of
             Left _ -> return False
             Right (app, 0) -> do
                 runRestartingApps mux $ M.delete (daNum app) ops
             Right (app, restarts) -> do
                 op <- Mx.runMiniProtocol mux (daNum app) Mx.ResponderDirectionOnly
                           (unDummyStart $ daStart app)
                           (dummyRestartingAppToChannel (app, restarts - 1))
                 runRestartingApps mux $ M.insert (daNum app) op ops

prop_mux_restart_m (DummyRestartingInitiatorResponderApps rapps) = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    bearer <-
      getBearer makeQueueChannelBearer
        (-1)
        QueueChannel { writeQueue = mux_w, readQueue = mux_r }
        Nothing
    peerBearer <-
      getBearer makeQueueChannelBearer
        (-1)
        QueueChannel { writeQueue = mux_r, readQueue = mux_w }
        Nothing
    let apps = map fst rapps
        initMinis = map (appToInfo Mx.InitiatorDirection) apps
        respMinis = map (appToInfo Mx.ResponderDirection) apps

    mux <- Mx.new Mx.nullTracers Nothing $ initMinis ++ respMinis
    mux_aid <- async $ Mx.run mux bearer
    getInitRes <- sequence [ Mx.runMiniProtocol
                               mux
                               (daNum $ fst app)
                               Mx.InitiatorDirection
                               Mx.StartEagerly
                               (dummyRestartingAppToChannel (fst app, (Mx.InitiatorDirection, snd app)))
                           | app <- rapps
                           ]
    getRespRes <- sequence [ Mx.runMiniProtocol
                               mux
                               (daNum $ fst app)
                               Mx.ResponderDirection
                               (unDummyStart $ daStart $ fst app)
                               (dummyRestartingAppToChannel (fst app, (Mx.ResponderDirection, snd app)))
                           | app <- rapps
                           ]

    triggers <- mapM (async . triggerApp peerBearer) apps
    let gi = M.fromList $ map (\(n, g) -> ((Mx.InitiatorDirection, n), g)) $ zip (map daNum apps) getInitRes
        gr = M.fromList $ map (\(n, g) -> ((Mx.ResponderDirection, n), g)) $ zip (map daNum apps) getRespRes
    r <- runRestartingApps mux $ gi <> gr
    Mx.stop mux
    void $ waitCatch mux_aid
    mapM_ cancel triggers
    return $ property r
  where
    runRestartingApps :: Mx.Mux Mx.InitiatorResponderMode m
                      -> M.Map (Mx.MiniProtocolDirection Mx.InitiatorResponderMode, Mx.MiniProtocolNum)
                               (STM m (Either SomeException (DummyApp, (Mx.MiniProtocolDirection Mx.InitiatorResponderMode, Int))))
                      -> m Bool
    runRestartingApps _ ops | M.null ops = return True
    runRestartingApps mux ops = do
        appResult <- atomically $ foldr (<|>) retry $ M.elems ops
        case appResult of
             Left _ -> return False
             Right (app, (dir, 0)) ->
                 let opKey = (dir, daNum app) in
                 runRestartingApps mux $ M.delete opKey ops
             Right (app, (dir, restarts)) -> do
                 let opKey = (dir, daNum app)
                     strat = case dir of
                                  Mx.InitiatorDirection -> Mx.StartEagerly
                                  Mx.ResponderDirection -> unDummyStart $ daStart app
                 op <- Mx.runMiniProtocol mux (daNum app) dir strat (dummyRestartingAppToChannel (app, (dir, restarts - 1)))
                 runRestartingApps mux $ M.insert opKey op ops



-- | Verifying starting and stopping of miniprotocols. Both normal exits and by exception.
prop_mux_start_m :: forall m.
                       ( Alternative (STM m)
                       , MonadAsync m
                       , MonadDelay m
                       , MonadEvaluate m
                       , MonadFork  m
                       , MonadLabelledSTM m
                       , MonadMask m
                       , MonadSay m
                       , MonadThrow (STM m)
                       , MonadTimer m
                       )
                    => Mx.Bearer m
                    -- ^ Mux bearer
                    -> (DummyApp -> m ())
                    -- ^ trigger action that starts the app
                    -> (    DiffTime
                         -- ^ How long did the test run.
                         -> ((STM m (Either SomeException ())), DummyApp)
                         -- ^ Result for running the app, along with the app
                         -> m (Property, Either SomeException ())
                       )
                    -- ^ Verify that the app succeded/failed as expected when
                    -- the test stopped
                    -> DummyApps
                    -- ^ List of apps to test
                    -> DiffTime
                    -- ^ Maximum run time
                    -> DiffTime
                    -- ^ Time at which StartOnDemandAny should run
                    -> m Property
prop_mux_start_m bearer _ checkRes (DummyInitiatorApps apps) runTime _ = do
    let minis = map (appToInfo Mx.InitiatorDirectionOnly) apps
        minRunTime = minimum $ runTime : (map daRunTime $ filter (\app -> daAction app == DummyAppFail) apps)

    mux <- Mx.new Mx.nullTracers Nothing minis
    mux_aid <- async $ Mx.run mux bearer
    killer <- async $ (threadDelay runTime) >> Mx.stop mux
    getRes <- sequence [ Mx.runMiniProtocol
                           mux
                          (daNum app)
                          Mx.InitiatorDirectionOnly
                          Mx.StartEagerly
                          (dummyAppToChannel app)
                       | app <- apps
                       ]
    rc <- mapM (checkRes minRunTime) $ zip getRes apps
    wait killer
    void $ waitCatch mux_aid

    return (conjoin $ map fst rc)

prop_mux_start_m bearer trigger checkRes (DummyResponderApps apps) runTime anyStartAfter = do
    let minis = map (appToInfo Mx.ResponderDirectionOnly) apps
        minRunTime = minimum $ runTime : (map (\a -> case unDummyStart (daStart a) of
                                                          Mx.StartOnDemandAny -> daRunTime a + anyStartAfter
                                                          _                   -> daRunTime a + daStartAfter a
                                              ) $ filter (\app -> daAction app == DummyAppFail) apps)

    mux <- Mx.new muxVerboseTracer Nothing minis
    mux_aid <- async $ Mx.run mux bearer
    getRes <- sequence [ Mx.runMiniProtocol
                           mux
                          (daNum app)
                          Mx.ResponderDirectionOnly
                          (unDummyStart $ daStart app)
                          (dummyAppToChannel app)
                       | app <- apps
                       ]

    triggers <- mapM (async . trigger) $
                  filter (\app -> case unDummyStart (daStart app) of
                                       Mx.StartOnDemandAny -> anyStartAfter <= minRunTime
                                       _                   -> daStartAfter app <= minRunTime
                         ) apps
    killer <- async $ (threadDelay runTime) >> Mx.stop mux
    rc <- mapM (checkRes minRunTime) $ zip getRes apps
    wait killer
    mapM_ cancel triggers
    void $ waitCatch mux_aid

    return (conjoin $ map fst rc)

prop_mux_start_m bearer _trigger _checkRes (DummyResponderAppsKillMux apps) runTime _ = do
    -- Start a mini-protocol on demand, but kill mux before the application is
    -- triggered.  This test assures that mini-protocol completion action does
    -- not deadlocks.
    let minis = map (appToInfo Mx.ResponderDirectionOnly) apps

    mux <- Mx.new muxVerboseTracer Nothing minis
    mux_aid <- async $ Mx.run mux bearer
    getRes <- sequence [ Mx.runMiniProtocol
                           mux
                          (daNum app)
                          Mx.ResponderDirectionOnly
                          (unDummyStart $ daStart app)
                          (dummyAppToChannel app)
                       | app <- apps
                       ]

    killer <- async $ threadDelay runTime
                   >> cancel mux_aid
    _ <- traverse atomically getRes
    wait killer

    return (property True)

prop_mux_start_m bearer trigger checkRes (DummyInitiatorResponderApps apps) runTime anyStartAfter = do
    let initMinis = map (appToInfo Mx.InitiatorDirection) apps
        respMinis = map (appToInfo Mx.ResponderDirection) apps
        minRunTime = minimum $ runTime : (map (\a -> daRunTime a) $ filter (\app -> daAction app == DummyAppFail) apps)

    mux <- Mx.new muxVerboseTracer Nothing $ initMinis ++ respMinis
    mux_aid <- async $ Mx.run mux bearer
    getInitRes <- sequence [ Mx.runMiniProtocol
                               mux
                               (daNum app)
                               Mx.InitiatorDirection
                               Mx.StartEagerly
                               (dummyAppToChannel app)
                           | app <- apps
                           ]
    getRespRes <- sequence [ Mx.runMiniProtocol
                               mux
                               (daNum app)
                               Mx.ResponderDirection
                               (unDummyStart $ daStart app)
                               (dummyAppToChannel app)
                           | app <- apps
                           ]

    triggers <- mapM (async . trigger) $
                  filter (\app -> case unDummyStart (daStart app) of
                                       Mx.StartOnDemandAny -> anyStartAfter <= minRunTime
                                       _                   -> daStartAfter app <= minRunTime
                         ) apps
    killer <- async $ (threadDelay runTime) >> Mx.stop mux
    !rcInit <- mapM (checkRes minRunTime) $
                 zip getInitRes $
                   map (\a -> a { daStart = DummyStart Mx.StartEagerly }) apps
    !rcResp <- mapM (checkRes minRunTime) $ zip getRespRes apps
    wait killer
    mapM_ cancel triggers
    void $ waitCatch mux_aid

    return (property $ (conjoin $ map fst rcInit ++ map fst rcResp))

-- | Verify starting and stopping of miniprotocols. Both normal exits and by exception.
prop_mux_start :: DummyApps -> DiffTime -> Property
prop_mux_start apps runTime =
  let (trace, r_e) = (traceEvents &&& traceResult True)
                      (runSimTrace $ prop_mux_start_mX apps runTime)
  in counterexample ( unlines
                    . ("*** TRACE ***" :)
                    . map show
                    $ trace) $
       case r_e of
         Left  e -> counterexample (show e) False
         Right r -> r

-- | Verify restarting of miniprotocols.
prop_mux_restart :: DummyRestartingApps -> Property
prop_mux_restart apps =
  let (trace, r_e) = (traceEvents &&& traceResult True)
                       (runSimTrace $ prop_mux_restart_m apps)
  in counterexample (unlines . map show $ trace) $
       case r_e of
          Left  e -> counterexample (show e) False
          Right r -> r



data WithThreadAndTime a = WithThreadAndTime {
      wtatOccuredAt    :: !Time
    , wtatWithinThread :: !String
    , wtatEvent        :: !a
    }

instance (Show a) => Show (WithThreadAndTime a) where
    show WithThreadAndTime {wtatOccuredAt, wtatWithinThread, wtatEvent} =
        formatToString
          (F.shown F.% ":" %+ F.shown F.% ":" %+ F.shown)
          wtatOccuredAt
          wtatWithinThread
          wtatEvent

verboseTracer :: forall a m.
                       ( MonadAsync m
                       , MonadMonotonicTime m
                       , MonadSay m
                       , Show a
                       )
               => Tracer m a
verboseTracer = threadAndTimeTracer $ show >$< mkTracer say

muxVerboseTracer :: forall m.
                       ( MonadAsync m
                       , MonadMonotonicTime m
                       , MonadSay m
                       )
                 => Mx.Tracers m
muxVerboseTracer = Mx.TracersI verboseTracer verboseTracer verboseTracer

threadAndTimeTracer :: forall a m.
                       ( MonadAsync m
                       , MonadMonotonicTime m
                       )
                    => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = mkTracer $ \s -> do
    !now <- getMonotonicTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now (show tid) s


--
-- mux close test
--


data FaultInjection
    = CleanShutdown
    | CloseOnWrite
    | CloseOnRead
  deriving (Show, Eq)

instance Arbitrary FaultInjection where
    arbitrary = elements [CleanShutdown, CloseOnWrite, CloseOnRead]
    shrink CloseOnRead   = [CleanShutdown, CloseOnWrite]
    shrink CloseOnWrite  = [CleanShutdown]
    shrink CleanShutdown = []


-- | Tag for tracer.
--
data ClientOrServer = Client | Server
    deriving Show


data NetworkCtx sock m b = NetworkCtx {
    ncSocket    :: m sock,
    ncClose     :: sock -> m (),
    ncMuxBearer :: sock -> (Mx.Bearer m -> m b) -> m b
  }


withNetworkCtx :: MonadThrow m => NetworkCtx sock m a -> (Mx.Bearer m -> m a) -> m a
withNetworkCtx NetworkCtx { ncSocket, ncClose, ncMuxBearer } k =
    bracket ncSocket ncClose (\sock -> ncMuxBearer sock k)


-- | What a side's counters must show once the close experiment is over, by
-- what its mini-protocol saw.
data CountersJudgement =
    Exactly Mx.Failures
    -- ^ these failures, exactly
  | AtMostOnePeerClose
    -- ^ nothing, or one close counted: a protocol's last transaction, which
    -- clears its running status, runs after its action has returned, so a
    -- close read in between is counted as a peer close although the
    -- protocol returned normally; on Windows a close arrives as an I/O
    -- error, counted as a read error whether or not a protocol runs
  | Unjudged
    -- ^ an end the experiment does not judge
  deriving Show

close_experiment
    :: forall sock acc req resp m.
       ( Alternative (STM m)
       , MonadAsync       m
       , MonadDelay       m
       , MonadEvaluate    m
       , MonadFork        m
       , MonadLabelledSTM m
       , MonadMask        m
       , MonadTimer       m
       , MonadThrow  (STM m)
       , MonadST          m
       , Serialise req
       , Serialise resp
       , Eq resp
       , Show req
       , Show resp
       )
    => Bool -- 'True' for @m ~ IO@
    -> FaultInjection
    -> Tracer m (ClientOrServer, TraceSendRecv (MsgReqResp req resp))
    -> Tracer m (ClientOrServer, Mx.Trace)
    -> NetworkCtx sock m (Either SomeException (Either [resp] [resp]))
    -> NetworkCtx sock m (Either SomeException ())
    -> [req]
    -> (acc -> req -> (acc, resp))
    -> acc
    -> m Property
close_experiment
#ifdef mingw32_HOST_OS
      iotest
#else
      _iotest
#endif
      fault tracer muxTracer clientCtx serverCtx reqs0 fn acc0 = do
    let clientMuxTracer' = (Client,) `contramap` muxTracer
        serverMuxTracer' = (Server,) `contramap` muxTracer
        clientMuxTracer  = Mx.TracersI clientMuxTracer' nullTracer nullTracer
        serverMuxTracer  = Mx.TracersI serverMuxTracer' nullTracer nullTracer
    clientCounters <- Counters.newCounters
    serverCounters <- Counters.newCounters
    withAsync
      -- run client thread
      (bracket (Mx.new clientMuxTracer (Just clientCounters)
                       [ MiniProtocolInfo {
                           miniProtocolNum,
                           miniProtocolDir = Mx.InitiatorDirectionOnly,
                           miniProtocolLimits = Mx.MiniProtocolLimits maxBound,
                           miniProtocolCapability = Nothing
                         }
                       ])
                Mx.stop $ \mux ->
        withNetworkCtx clientCtx $ \clientBearer ->
          withAsync (Mx.run mux clientBearer) $ \_muxAsync ->
                Mx.runMiniProtocol
                  mux miniProtocolNum
                  Mx.InitiatorDirectionOnly Mx.StartEagerly
                  (\chan -> mkClient >>= runClientCBOR clientTracer chan)
            >>= atomically
      )
      $ \clientAsync ->
        withAsync
          -- run server thread
          (bracket (Mx.new serverMuxTracer (Just serverCounters)
                            [ MiniProtocolInfo {
                                miniProtocolNum,
                                miniProtocolDir = Mx.ResponderDirectionOnly,
                                miniProtocolLimits = Mx.MiniProtocolLimits maxBound,
                                miniProtocolCapability = Nothing
                              }
                            ])
                    Mx.stop $ \mux ->
          withNetworkCtx serverCtx $ \serverBearer  ->
            withAsync (Mx.run mux serverBearer) $ \_muxAsync -> do
                  Mx.runMiniProtocol
                    mux miniProtocolNum
                    Mx.ResponderDirectionOnly Mx.StartOnDemand
                    (\chan -> runServerCBOR serverTracer chan (server acc0))
              >>= atomically
          )
          $ \serverAsync -> do
            -- await for both client and server threads, inspect results

            -- @Left (Left _)@  is the error thrown by 'runMiniProtocol ... >>= atomically'
            -- @Left (Right _)@ is the error return by 'runMiniProtocol'
            (resClient, resServer) <- (,) <$> (reassocE <$> waitCatch clientAsync)
                                          <*> (reassocE <$> waitCatch serverAsync)
            outcome <- case (fault, resClient, resServer) of
              (CleanShutdown, Right (Right resps), Right _)
                 | expected <- expectedResps (List.length resps)
                 , resps == expected
                -> return $ label "CleanShutdown"
                          $ property True

                 | otherwise
                -> return $ counterexample
                              (concat [ show resps
                                      , " ≠ "
                                      , show (expectedResps (List.length resps))
                                      ])
                              False

              -- With empty reqs, CleanShutdown also sends MsgDone immediately.
              -- The server is StartOnDemand and may not be scheduled before the
              -- client socket closes (EOF → BearerClosed → mux Stopped), so it
              -- receives Shutdown Nothing Stopped instead of a normal termination.
              -- On Windows the bearer raises IOException rather than BearerClosed,
              -- so the mux enters Failed state and completionAction returns
              -- Shutdown (Just e) (Failed e) instead of Shutdown Nothing Stopped.
              (CleanShutdown, Right (Right []), Left serverError)
                 | Just e <- fromException (collapsE serverError)
                 , case e of
                     Mx.Shutdown {}     -> True
                     Mx.BearerClosed {} -> True
                     _                  -> False
                -> return $ label "CleanShutdown"
                          $ property True

                 | otherwise
                -> return $ counterexample
                              (show serverError)
                              False

              -- The egress thread clears the Wanton TVar before writeMany sends
              -- the bytes to the bearer.  If the mux run thread (Mx.run) is
              -- cancelled in that window the MsgDone SDU is never transmitted.
              -- The server is blocking on recv waiting for MsgDone; when the
              -- client socket closes it gets BearerClosed → mux Failed.  The
              -- client responses are still correct, so accept the
              -- connection-level server error.
              (CleanShutdown, Right (Right resps), Left serverError)
                 | expected <- expectedResps (List.length resps)
                 , resps == expected
                 , Just e <- fromException (collapsE serverError)
                 , case e of
                     Mx.Shutdown {}     -> True
                     Mx.BearerClosed {} -> True
                     _                  -> False
                -> return $ label "CleanShutdown"
                          $ property True

                 | expected <- expectedResps (List.length resps)
                 , resps /= expected
                -> return $ counterexample
                              (concat [ show resps
                                      , " ≠ "
                                      , show expected
                                      ])
                          $ counterexample
                              (show serverError)
                              False

                 | otherwise
                -> return $ counterexample
                              (show serverError)
                              False

              -- close on read with empty responses is the same as clean
              -- shutdown
              (CloseOnRead, Right (Right resps@[]), Right _)
                 | expected <- expectedResps 0
                 , List.null expected
                -> return $ label ("CloseOnRead: " ++ if null reqs0 then "reqs == 0" else "reqs0 > 0")
                          $ property True

                 | otherwise
                -> return $ counterexample
                              (concat [ show resps
                                      , " ≠ "
                                      , show (expectedResps (List.length resps))
                                      ])
                              False

              -- With empty reqs, CloseOnRead sends MsgDone immediately (same
              -- as CleanShutdown). The server is StartOnDemand and may never
              -- be scheduled before the mux shuts down, so it receives
              -- Shutdown Nothing Stopped instead of a normal termination.
              (CloseOnRead, Right (Right []), Left serverError)
                 | Just e <- fromException (collapsE serverError)
                 , case e of
                     Mx.Shutdown {}     -> True
                     Mx.BearerClosed {} -> True
                     _                  -> False
                -> return $ label ("CloseOnRead: " ++ if null reqs0 then "reqs == 0" else "reqs0 > 0")
                          $ property True

                 | otherwise
                -> return $ counterexample
                              (show serverError)
                              False

              (CloseOnWrite, Right (Left resps), Left serverError)
                 | expected <- expectedResps (List.length resps)
                 , resps == expected
                 , Just e <- fromException (collapsE serverError)
                 , case e of
                     Mx.Shutdown {}     -> True
                     Mx.BearerClosed {} -> True
                     _                  -> False
                -> return $ label ("CloseOnWrite: " ++ if null reqs0 then "reqs == 0" else "reqs0 > 0")
                          $ property True

                 | expected <- expectedResps (List.length resps)
                 , resps /= expected
                -> return $ counterexample
                              (concat [ show resps
                                      , " ≠ "
                                      , show expected
                                      ])
                          $ counterexample
                              (show serverError)
                              False

                 | otherwise
                -> return $ counterexample
                              (show serverError)
                              False
              (CloseOnRead, Right (Left resps), Left serverError)
                 | expected <- expectedResps (List.length resps)
                 , resps == expected
                 , Just e <- fromException (collapsE serverError)
                 , case e of
                     Mx.Shutdown {}     -> True
                     Mx.BearerClosed {} -> True
                     _                  -> False
                -> return $ property True

                 | expected <- expectedResps (List.length resps)
                 , resps /= expected
                -> return $ counterexample
                              (concat [ show resps
                                      , " ≠ "
                                      , show expected
                                      ])
                          $ counterexample
                              (show serverError)
                              False

                 | otherwise
                -> return $ counterexample
                              (show serverError)
                              False
#ifdef mingw32_HOST_OS
              -- these cases fail on Windows for ~1% of cases: when the bearer
              -- gets a Windows "network name no longer available" IOException,
              -- either side can surface it as Shutdown (Just IOException) Failed.
              (_, Left (Right clientError), _)
                 | iotest
                 , Just (Mx.Shutdown (Just e) _) <- fromException clientError
                 , Just (Mx.IOException _ msg) <- fromException e
                 , msg == "recv errored"
                -> return $ label ("client-error: " ++ show fault) True

              (_, Right _, Left (Right serverError))
                 | iotest
                 , Just (Mx.Shutdown (Just e) _) <- fromException serverError
                 , Just (Mx.IOException _ msg) <- fromException e
                 , msg == "recv errored"
                -> return $ label ("server-error: " ++ show fault) True
#endif

              (_, clientRes, serverRes) ->
                return $ counterexample (show fault)
                       $ counterexample ("Client: " ++ show clientRes)
                       $ counterexample ("Server: " ++ show serverRes)
                       $ False
            -- What each side counted, judged by what its mini-protocol saw.
            counts <- (,) <$> atomically (Counters.readCounters clientCounters)
                          <*> atomically (Counters.readCounters serverCounters)
            return $ outcome
                .&&. judged "client" (clientFailures resClient) (fst counts)
                .&&. judged "server" (serverFailures resServer) (snd counts)

  where
    collapsE :: Either a a -> a
    collapsE = either id id

    reassocE :: Either SomeException (Either SomeException a)
             -> Either (Either SomeException SomeException) a
    reassocE (Left e)          = Left (Left e)
    reassocE (Right (Left e))  = Left (Right e)
    reassocE (Right (Right a)) = Right a

    -- A side's counters against what its end says they must show.
    judged :: String -> CountersJudgement -> Mx.Counts -> Property
    judged side judgement Mx.Counts { Mx.mcFailures = failures } =
      counterexample (side ++ " counted " ++ show failures ++ ", judged " ++ show judgement) $
        case judgement of
          Exactly expected   -> failures === expected
          AtMostOnePeerClose ->
            classify (closesCounted failures == 1)
                     (side ++ ": a close counted after the protocol returned") $
                 withoutCloses failures === Mx.noFailures
            .&&. closesCounted failures <= 1
          Unjudged           -> property True

    -- How a peer's close is counted on this platform: as a peer close, or
    -- on Windows, where the socket library reports it under EINVAL rather
    -- than its own errno, as a read error.
    closesCounted :: Mx.Failures -> Word64
    withoutCloses :: Mx.Failures -> Mx.Failures
#ifdef mingw32_HOST_OS
    closesCounted f = Mx.mfPeerClosed f + Mx.mfReadErrors f
    withoutCloses f = f { Mx.mfPeerClosed = 0, Mx.mfReadErrors = 0 }
#else
    closesCounted f = Mx.mfPeerClosed f
    withoutCloses f = f { Mx.mfPeerClosed = 0 }
#endif

    -- The client's close is its own, after its mux is cancelled; its
    -- initiator returning says the server's close, if any, came after it,
    -- or while its exit was in flight.
    clientFailures :: Either (Either SomeException SomeException) a -> CountersJudgement
    clientFailures (Right _) = AtMostOnePeerClose
    clientFailures (Left _)  = Unjudged

    -- The server counts a peer close exactly when the close met its
    -- responder running, which ends the responder with a failed shutdown
    -- carrying the close; a stopped shutdown means nothing was running; a
    -- normal return means the close came after the responder returned, or
    -- while its exit was in flight. The simulated close is lost, as a close
    -- without lingering is, so there the server's read times out instead,
    -- and that is counted as a read timeout. Any other end, as an I/O error
    -- on Windows, is not judged here.
    serverFailures :: Either (Either SomeException SomeException) a -> CountersJudgement
    serverFailures (Right _) = AtMostOnePeerClose
    serverFailures (Left e)  = case fromException (collapsE e) of
      Just (Mx.Shutdown Nothing _) -> Exactly Mx.noFailures
      Just (Mx.Shutdown (Just e') _)
        | Just Mx.BearerClosed {} <- fromException e'
        -> Exactly Mx.noFailures { Mx.mfPeerClosed = 1 }
        | Just Mx.SDUReadTimeout <- fromException e'
        -> Exactly Mx.noFailures { Mx.mfReadTimeouts = 1 }
      _                            -> Unjudged


    clientTracer,
      serverTracer :: Tracer m (TraceSendRecv (MsgReqResp req resp))
    clientTracer = (Client,) `contramap` tracer
    serverTracer = (Server,) `contramap` tracer

    expectedResps :: Int -> [resp]
    expectedResps n = snd $ List.mapAccumL fn acc0 (take n reqs0)

    miniProtocolNum :: Mx.MiniProtocolNum
    miniProtocolNum = Mx.MiniProtocolNum 1

    -- client application; after sending all requests it will either terminate
    -- the protocol (clean shutdown) or close the connection and do early exit.
    mkClient :: m (ReqRespClient req resp m (Either [resp] [resp]))
    mkClient = clientImpl [] reqs0
      where
        clientImpl !resps (req : []) =
          case fault of
            CleanShutdown ->
              return $ SendMsgReq
                req
                (\resp -> return $ SendMsgDone (return $! Right
                                                       $! reverse (resp : resps)))

            CloseOnWrite ->
              return (EarlyExit $! Left
                                $! reverse resps)

            CloseOnRead ->
              return $ SendMsgReq
                req
                (\resp ->
                  return (EarlyExit $! Left
                                    $! reverse (resp : resps)))

        clientImpl !resps (req : reqs) =
          return $ SendMsgReq
            req
            (\resp -> clientImpl (resp : resps) reqs)

        clientImpl !resps [] =
          case fault of
            CloseOnWrite ->
              return $ EarlyExit $! Left
                                 $! reverse resps
            _ ->
              return $ SendMsgDone (return $! Right
                                           $! reverse resps)

    -- server which incrementally computes 'mapAccumL'
    server :: acc -> ReqRespServer req resp m ()
    server acc = ReqRespServer {
        recvMsgReq  = \req -> return $
                        case fn acc req of
                          (acc', resp) -> (resp, server acc'),
        recvMsgDone = return ()
      }


prop_mux_close_io :: FaultInjection
                  -> [Int]
                  -> (Int -> Int -> (Int, Int))
                  -> Int
                  -> Property
prop_mux_close_io fault reqs fn acc = ioProperty $ withIOManager $ \iocp -> do
    serverAddr : _ <- Socket.getAddrInfo
                        Nothing (Just "127.0.0.1") (Just "0")
    bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
            Socket.close
            $ \serverSocket -> do
      associateWithIOManager iocp (Right serverSocket)
      Socket.bind serverSocket (Socket.addrAddress serverAddr)
      Socket.listen serverSocket 1
      let serverCtx :: NetworkCtx Socket.Socket IO
                                  (Either SomeException ())
          serverCtx = NetworkCtx {
              ncSocket = do
                (sock, _) <- Socket.accept serverSocket
                associateWithIOManager iocp (Right sock)
                return sock,
              ncClose  = Socket.close,
              ncMuxBearer = \sd k -> withReadBufferIO (\buffer -> do
                              bearer <- getBearer makeSocketBearer 10 sd buffer
                              k bearer
                            )

            }
          clientCtx :: NetworkCtx Socket.Socket IO
                                  (Either SomeException (Either [Int] [Int]))
          clientCtx = NetworkCtx {
              ncSocket = do
                sock <- Socket.socket Socket.AF_INET Socket.Stream
                                      Socket.defaultProtocol
                associateWithIOManager iocp (Right sock)
                (Socket.getSocketName serverSocket
                  >>= Socket.connect sock)
                return sock,
              ncClose  = Socket.close,
              ncMuxBearer = \sd k -> withReadBufferIO (\buffer -> do
                              bearer <- getBearer makeSocketBearer 10 sd buffer
                              k bearer
                            )

            }
      close_experiment
        True
        fault
        nullTracer
        nullTracer
        {--
          - ((\msg -> (,msg) <$> getMonotonicTime)
          -  `contramapM` Tracer Debug.traceShowM
          - )
          - ((\msg -> (,msg) <$> getMonotonicTime)
          -  `contramapM` Tracer Debug.traceShowM
          - )
          --}
        clientCtx serverCtx
        reqs fn acc


prop_mux_close_sim :: FaultInjection
                   -> Positive Word16
                   -> [Int]
                   -> (Int -> Int -> (Int, Int))
                   -> Int
                   -> Property
prop_mux_close_sim fault (Positive sduSize_) reqs fn acc =
    runSimOrThrow experiment
  where
    experiment :: forall s. IOSim s Property
    experiment = do
      (chann, chann')
        <- atomically $ newConnectedAttenuatedChannelPair
            nullTracer
            nullTracer
            {--
              - ((\msg -> (,(Client,msg)) <$> getMonotonicTime)
              -  `contramapM` Tracer Debug.traceShowM
              - )
              - ((\msg -> (,(Server,msg)) <$> getMonotonicTime)
              -  `contramapM` Tracer Debug.traceShowM
              - )
              --}
            noAttenuation
            noAttenuation
      let sduSize = Mx.SDUSize sduSize_
          sduTimeout = 10
          clientCtx :: NetworkCtx (AttenuatedChannel (IOSim s))
                                  (IOSim s)
                                  (Either SomeException (Either [Int] [Int]))
          clientCtx = NetworkCtx {
              ncSocket = return chann,
              ncClose  = acClose,
              ncMuxBearer = \fd k ->
                               k $ attenuationChannelAsBearer
                                     sduSize sduTimeout fd
            }
          serverCtx :: NetworkCtx (AttenuatedChannel (IOSim s))
                                  (IOSim s)
                                  (Either SomeException ())
          serverCtx = NetworkCtx {
              ncSocket = return chann',
              ncClose  = acClose,
              ncMuxBearer = \fd k ->
                               k $ attenuationChannelAsBearer
                                     sduSize sduTimeout fd
            }
      close_experiment
        False
        fault
        nullTracer
        nullTracer
        {--
          - ((\msg -> (,msg) <$> getMonotonicTime)
          -  `contramapM` Tracer Debug.traceShowM
          - )
          - ((\msg -> (,msg) <$> getMonotonicTime)
          -  `contramapM` Tracer Debug.traceShowM
          - )
          --}
        clientCtx
        serverCtx
        reqs fn acc

    -- in this simulation we don't need attenuation, we inject failures
    -- directly into the client.
    noAttenuation = Attenuation {
        aReadAttenuation  = \_ _ -> (1, AttenuatedChannel.Success),
        aWriteAttenuation = Nothing
      }


newtype NonEmptyByteString = NonEmptyByteString BL.ByteString
  deriving Show

instance Arbitrary NonEmptyByteString where
    arbitrary = do
      bs <- arbitrary `suchThat` (not . BL.null)
      return $ NonEmptyByteString bs

    shrink (NonEmptyByteString bs) =
      [ NonEmptyByteString bs'
      | bs' <- shrink bs
      , not (BL.null bs')
      ]

prop_mux_trailing_bytes
  :: ( Alternative   (STM m)
     , MonadAsync         m
     , MonadDelay         m
     , MonadEvaluate      m
     , MonadFork          m
     , MonadLabelledSTM   m
     , MonadMask          m
     , MonadTimer         m
     , MonadThrow    (STM m)
     , MonadSay           m
     )
  => BL.ByteString
  -> NonEmptyByteString
  -> m Property
prop_mux_trailing_bytes reminder (NonEmptyByteString received) = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    bearer <- getBearer Mx.makeQueueChannelBearer
                (-1)
                QueueChannel { writeQueue = mux_w, readQueue = mux_r }
                Nothing
    mux <- Mx.new Mx.nullTracers Nothing
                  [ MiniProtocolInfo {
                      miniProtocolNum,
                      miniProtocolDir = Mx.ResponderDirectionOnly,
                      miniProtocolLimits = Mx.MiniProtocolLimits maxBound,
                      miniProtocolCapability = Nothing
                    }
                  ]
    withAsync (Mx.run mux bearer) $ \_ -> do
      -- The following sequence represents a remote application sending data
      -- that terminates and restarts a mini-protocol, leaving trailing bytes on
      -- the receiving end after the restart already happened and the bytes were
      -- read from the network.
      --
      -- 1. Send an SDU with a payload of `received` bytes.  This represents the
      -- remote side sending additional data after restarting the mini-protocol.
      -- The initial data for this conversation is in the trailing bytes, which
      -- are assumed to be already received. We inject them in the next step.
      atomically
        $ writeTBQueue mux_r
        $ Mx.encodeSDU
        $ Mx.SDU { Mx.msHeader = Mx.SDUHeader {
                     Mx.mhTimestamp = Mx.RemoteClockModel 0,
                     Mx.mhNum       = miniProtocolNum,
                     Mx.mhDir       = Mx.InitiatorDir,
                     Mx.mhLength    = fromIntegral (BL.length received)
                   },
                   Mx.msBlob = received
                }

      -- 2. Run a mini-protocol which returns `trailing` bytes. This represents
      -- the responder side stopping the mini-protocol with trailing bytes.
      _ <- atomically =<< Mx.runMiniProtocol
              mux
              miniProtocolNum
              Mx.ResponderDirectionOnly
              Mx.StartEagerly
              (\_ -> do
                labelThisThread "resp:1"
                return ((), Just reminder))

      -- 3. Read all bytes from the channel
      r <- atomically =<< Mx.runMiniProtocol
              mux
              miniProtocolNum
              Mx.ResponderDirectionOnly
              Mx.StartEagerly
              (\chan -> do
                labelThisThread "resp:2"
                let expectedLen = BL.length reminder + BL.length received
                    -- `recv` returns whatever is currently available in the
                    -- ingress queue without waiting for more.  If the trailing
                    -- bytes arrive before the demuxer has processed the SDU,
                    -- a single `recv` would return only the trailing bytes and
                    -- miss the SDU payload, making the test non-deterministic.
                    go acc
                      | BL.length acc >= expectedLen = return acc
                      | otherwise = do
                          mbs <- Mx.recv chan
                          go (acc <> fromMaybe BL.empty mbs)
                a <- go BL.empty
                return (Just a, Nothing)
              )

      -- 4. Verify that the trailing bytes were injected before the
      -- additional data (`received` bytes).
      case r of
        Left e    -> throwIO e
        Right bts -> return $ bts === Just (reminder <> received)
  where
    miniProtocolNum :: Mx.MiniProtocolNum
    miniProtocolNum = Mx.MiniProtocolNum 1


prop_mux_trailing_bytes_iosim :: BL.ByteString
                              -> NonEmptyByteString
                              -> Property
prop_mux_trailing_bytes_iosim reminder received =
  let trace = runSimTrace $ prop_mux_trailing_bytes reminder received
  in counterexample (ppTrace_ trace) (case traceResult True trace of
                                       Left e  -> counterexample (show e) False
                                       Right r -> property r)

prop_mux_trailing_bytes_io :: BL.ByteString
                           -> NonEmptyByteString
                           -> Property
prop_mux_trailing_bytes_io reminder received =
  ioProperty $ prop_mux_trailing_bytes reminder received


prop_mux_pure_exception
  :: ( Alternative   (STM m)
     , MonadAsync         m
     , MonadDelay         m
     , MonadEvaluate      m
     , MonadFork          m
     , MonadLabelledSTM   m
     , MonadMask          m
     , MonadTimer         m
     , MonadThrow    (STM m)
     )
  => m Property
prop_mux_pure_exception = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    bearer <- getBearer Mx.makeQueueChannelBearer
                (-1)
                QueueChannel { writeQueue = mux_w, readQueue = mux_r }
                Nothing
    mux <- Mx.new Mx.nullTracers Nothing -- { Mx.tracer = Tracer Debug.traceShowM }
                  [ MiniProtocolInfo {
                      miniProtocolNum,
                      miniProtocolDir = Mx.ResponderDirectionOnly,
                      miniProtocolLimits = Mx.MiniProtocolLimits maxBound,
                      miniProtocolCapability = Nothing
                    }
                  ]
    withAsync (Mx.run mux bearer) $ \_ -> do
      r <- atomically =<< Mx.runMiniProtocol
              mux
              miniProtocolNum
              Mx.ResponderDirectionOnly
              Mx.StartEagerly
              (\_ -> do
                labelThisThread "resp:1"
                throwIO (error "pure exception" :: IOError))

      return $ case r of
        Left e   | Just (_ :: ErrorCall) <- fromException e
                 -> property True
                 | Just (_ :: IOError) <- fromException e
                 -> counterexample "unexpected IOError" False
                 | otherwise
                 -> counterexample "unexpected error" False
        Right {} -> counterexample "unexpected result" False
  where
    miniProtocolNum :: Mx.MiniProtocolNum
    miniProtocolNum = Mx.MiniProtocolNum 1


prop_mux_pure_exception_iosim :: Property
prop_mux_pure_exception_iosim = once $
  let trace = runSimTrace $ prop_mux_pure_exception
  in counterexample (ppTrace_ trace) (case traceResult True trace of
                                       Left e  -> counterexample (show e) False
                                       Right r -> r)

prop_mux_pure_exception_io :: Property
prop_mux_pure_exception_io = once $ ioProperty $ prop_mux_pure_exception

-- compare error types, not the payloads
compareErrors :: Mx.Error -> Mx.Error -> Bool
compareErrors Mx.UnknownMiniProtocol {} Mx.UnknownMiniProtocol {} = True
compareErrors Mx.BearerClosed {}        Mx.BearerClosed {}        = True
compareErrors Mx.IngressQueueOverRun {} Mx.IngressQueueOverRun {} = True
compareErrors Mx.InitiatorOnly {}       Mx.InitiatorOnly {}       = True
compareErrors Mx.IOException {}         Mx.IOException {}         = True
compareErrors Mx.SDUDecodeError {}      Mx.SDUDecodeError {}      = True
compareErrors Mx.SDUReadTimeout {}      Mx.SDUReadTimeout {}      = True
compareErrors Mx.SDUWriteTimeout {}     Mx.SDUWriteTimeout {}     = True
compareErrors Mx.Shutdown {}            Mx.Shutdown {}            = True
compareErrors _ _                                                 = False

-- | A send forces a message up to the soft limit and no further. A responder
-- sends a message with a chunk that throws when evaluated, through a mux
-- whose bearer never completes a write, so the muxer holds its first batch,
-- then a second message; the channel tracer evaluates every event, or none.
-- A chunk within the limit's worth throws in the first send; with the trace
-- evaluated one anywhere else throws there too, as the end of the send
-- counts the whole message; with no trace, one at the limit is reached by
-- the second send's check, counting the buffer from where the batch left
-- it, and one beyond is never reached. 'sendExpected' says which sends
-- return. In IOSim the chunk at the limit is left out: an exception from
-- evaluating a transaction escapes the simulation.
prop_mux_send_forces_prefix_only :: Runner -> (SendCase -> SendOutcome -> Property) -> Property
prop_mux_send_forces_prefix_only runner runWith =
    BaseQC.withNumTests 30 $
    forAllShrink genCase shrinkCase $ \c ->
    classify (inside c) "the throwing chunk is inside the prefix" $
    classify (evaluated c) "the channel tracer evaluates the trace" $
      runWith c (sendExpected c)
  where
    softLimit :: Int
    softLimit = fromIntegral egressSoftBufferLimit

    genCase :: Gen SendCase
    genCase = do
      ev       <- arbitrary
      len      <- choose (1, 20000)
      isInside <- arbitrary
      off      <- if isInside
                    then frequency [ (4, pure least), (2, pure 1), (3, choose (least, softLimit)) ]
                    else choose (0, 20000)
      return SendCase {
               evaluated   = ev,
               chunkLength = len,
               inside      = isInside,
               offset      = off
             }

    shrinkCase :: SendCase -> [SendCase]
    shrinkCase c@SendCase { evaluated, chunkLength, offset } =
      filter runnable $
           [ c { evaluated = False } | evaluated ]
        ++ [ c { chunkLength = len }
           | len <- shrink chunkLength
           , len >= 1
           ]
        ++ [ c { offset = off } | off <- shrink offset ]

    -- In IOSim, not the chunk at the limit, which the second send's check
    -- reaches in a transaction.
    least :: Int
    least = case runner of
                 InIO  -> 0
                 InSim -> 1

    runnable :: SendCase -> Bool
    runnable SendCase { inside, offset } = not inside || offset >= least

prop_mux_send_forces_prefix_only_io :: Property
prop_mux_send_forces_prefix_only_io =
    prop_mux_send_forces_prefix_only InIO $ \c want ->
      ioProperty $ (=== want) <$> sendForcesPrefixOnly c

prop_mux_send_forces_prefix_only_sim :: Property
prop_mux_send_forces_prefix_only_sim =
    prop_mux_send_forces_prefix_only InSim $ \c want ->
      runSimOrThrow (sendForcesPrefixOnly c) === want

-- | Where 'prop_mux_send_forces_prefix_only' runs its sends.
data Runner = InIO | InSim

-- | A case of 'prop_mux_send_forces_prefix_only'.
data SendCase =
     SendCase {
       evaluated   :: Bool,
       -- ^ The channel tracer evaluates every event.
       chunkLength :: Int,
       -- ^ The length of the message's chunks.
       inside      :: Bool,
       -- ^ The throwing chunk is inside the prefix a send forces.
       offset      :: Int
       -- ^ The throwing chunk's offset from the region's boundary: the limit
       -- inside, and beyond it the first byte the second send's check cannot
       -- reach.
     }
  deriving Show

-- | What the sends of a case did: whether the first returned, the second
-- returned, and one threw.
data SendOutcome =
     SendOutcome {
       firstReturned  :: Bool,
       secondReturned :: Bool,
       threw          :: Bool
     }
  deriving (Eq, Show)

-- | The outcome expected of a case.
sendExpected :: SendCase -> SendOutcome
sendExpected SendCase { evaluated, inside, offset }
  | inside, offset > 0 = SendOutcome { firstReturned = False, secondReturned = True,  threw = True }
  | evaluated          = SendOutcome { firstReturned = False, secondReturned = False, threw = True }
  | inside             = SendOutcome { firstReturned = True,  secondReturned = False, threw = True }
  | otherwise          = SendOutcome { firstReturned = True,  secondReturned = False, threw = False }

-- | A pair of queue bearers, each writing what the other reads, for the send
-- properties.
queueBearerPair :: (MonadSTM m, MonadMonotonicTime m, MonadThrow m)
                => m (Mx.Bearer m, Mx.Bearer m)
queueBearerPair = do
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    clientBearer <- getBearer makeQueueChannelBearer (-1)
                      QueueChannel { writeQueue = client_w, readQueue = client_r } Nothing
    serverBearer <- getBearer makeQueueChannelBearer (-1)
                      QueueChannel { writeQueue = client_r, readQueue = client_w } Nothing
    return (clientBearer, serverBearer)

-- | The mini-protocol the send properties run, in a direction.
sendProtocol :: Mx.MiniProtocolNum
sendProtocol = Mx.MiniProtocolNum 2

-- | The mini-protocol's info, in a direction.
sendProtocolInfo :: Mx.MiniProtocolDirection mode -> MiniProtocolInfo mode
sendProtocolInfo dir =
    MiniProtocolInfo {
      miniProtocolNum        = sendProtocol,
      miniProtocolDir        = dir,
      miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 16000000 },
      miniProtocolCapability = Nothing
    }

sendForcesPrefixOnly
  :: forall m.
     ( Alternative   (STM m)
     , MonadAsync         m
     , MonadDelay         m
     , MonadEvaluate      m
     , MonadFork          m
     , MonadLabelledSTM   m
     , MonadMask          m
     , MonadTimer         m
     , MonadThrow    (STM m)
     )
  => SendCase -> m SendOutcome
sendForcesPrefixOnly c@SendCase { evaluated, chunkLength, inside, offset } = do
    (clientBearer, serverBearer) <- queueBearerPair
    serverMux <- Mx.new tracers Nothing [sendProtocolInfo Mx.ResponderDirection]
    clientMux <- Mx.new Mx.nullTracers Nothing [sendProtocolInfo Mx.InitiatorDirection]
    first   <- newTVarIO False
    second  <- newTVarIO False
    failed  <- newTVarIO False
    entered <- newTVarIO False
    let outcome :: STM m SendOutcome
        outcome = SendOutcome <$> readTVar first <*> readTVar second <*> readTVar failed
    withAsync (Mx.run serverMux (stuck serverBearer)) $ \_ ->
      withAsync (Mx.run clientMux clientBearer) $ \_ -> do
        -- A send that reaches the throwing chunk throws in the protocol's
        -- thread; it is caught there, as a failing protocol takes the mux
        -- down.
        let sending :: StrictTVar m Bool -> BL.ByteString -> Mx.ByteChannel m -> m ()
            sending var bs chan = do
              r <- try (Mx.send chan bs)
              case r of
                   Left (_ :: ErrorCall) -> atomically (writeTVar failed True)
                   Right ()              -> atomically (writeTVar var True)
        _ <- Mx.runMiniProtocol serverMux sendProtocol Mx.ResponderDirection Mx.StartOnDemand $ \chan -> do
               _ <- Mx.recv chan
               sending first (message serverBearer) chan
               atomically (writeTVar entered True)
               sending second (BL8.pack "after") chan
               return ((), Nothing)
        _ <- Mx.runMiniProtocol clientMux sendProtocol Mx.InitiatorDirection Mx.StartEagerly $ \chan -> do
               Mx.send chan (BL8.pack "req")
               return ((), Nothing)
        -- The second send has been entered and the outcome has reached the
        -- one expected, within a bound; what must stay false has then stayed
        -- so for a while.
        _ <- timeout 5 $ atomically $ do
               e <- readTVar entered
               o <- outcome
               check (e && o == sendExpected c)
        threadDelay 0.1
        atomically outcome
  where
    softLimit :: Int
    softLimit = fromIntegral egressSoftBufferLimit

    -- The bytes the bearer puts before the throwing chunk, in chunks, and
    -- then the chunk.
    message :: Mx.Bearer m -> BL.ByteString
    message bearer = BL.fromChunks (map (`BS.replicate` 0x61) (chunks (bombAt bearer)))
                     <> error "send: evaluated past the soft limit"

    -- The bytes before the throwing chunk: inside, up to the limit; beyond,
    -- past the limit, the batch the muxer takes, at most a batch size and
    -- an SDU, the limit's worth the check counts past it, and the chunk
    -- that may straddle.
    bombAt :: Mx.Bearer m -> Int
    bombAt bearer
      | inside    = softLimit - offset
      | otherwise = softLimit + 1 + Mx.batchSize bearer
                  + fromIntegral (Mx.getSDUSize (Mx.sduSize bearer))
                  + chunkLength + offset

    chunks :: Int -> [Int]
    chunks n | n <= 0    = []
             | otherwise = min chunkLength n : chunks (n - chunkLength)

    -- Evaluated as it is traced, in the protocol's thread.
    evaluating :: Tracer m (Identity Mx.ChannelTrace)
    evaluating = Tracer (emit (\(Identity ev) -> void (evaluate (length (show ev)))))

    tracers :: Mx.Tracers m
    tracers | evaluated = Mx.nullTracers { Mx.channelTracer = evaluating }
            | otherwise = Mx.nullTracers

    -- A bearer whose writes never complete.
    stuck :: Mx.Bearer m -> Mx.Bearer m
    stuck bearer = bearer { Mx.write     = \_ _ _ -> forever (threadDelay 1)
                          , Mx.writeMany = \_ _ _ -> forever (threadDelay 1) }

-- | The channel trace at the end of a send carries the message's length: a
-- mini-protocol sends messages of drawn lengths, in chunks of a drawn size,
-- through a mux whose bearer drains into another mux, and a channel tracer
-- collecting every event sees each send's start followed by its end with the
-- length. Some chunk sizes divide the soft limit, so that a chunk ends on it
-- and a longer message's forcing stops there exactly.
prop_mux_send_end_length :: Property
prop_mux_send_end_length =
    forAllShrink genCase shrinkCase $ \(chunk, lens) ->
      case traverse project (runSimOrThrow (sendEvents chunk lens)) of
           Left ev    -> counterexample ("unexpected channel event: " ++ show ev) False
           Right seen -> seen === concat [ [Nothing, Just len] | len <- lens ]
  where
    softLimit :: Int
    softLimit = fromIntegral egressSoftBufferLimit

    -- A few messages, up to twice the soft limit, so a send waits behind
    -- one, and few enough for the receiving mux's ingress queue; the limit
    -- is 511 * 513.
    genCase :: Gen (Int, [Int])
    genCase = do
      chunk <- frequency [ (1, pure 511), (1, pure 513), (2, choose (256, 20000)) ]
      n     <- choose (1, 4)
      lens  <- vectorOf n (frequency [ (1, pure softLimit), (1, pure (softLimit + 1))
                                     , (3, choose (1, 2 * softLimit)) ])
      return (chunk, lens)

    shrinkCase :: (Int, [Int]) -> [(Int, [Int])]
    shrinkCase (chunk, lens) =
         [ (chunk, lens') | lens' <- shrinkList (filter (>= 1) . shrink) lens ]
      ++ [ (chunk', lens)
         | chunk' <- shrink chunk
         , chunk' >= 256
         ]

    -- The length the trace at the end of a send carries, and nothing for the
    -- trace at its start.
    project :: Mx.ChannelTrace -> Either Mx.ChannelTrace (Maybe Int)
    project (Mx.TraceChannelSendStart _)   = Right Nothing
    project (Mx.TraceChannelSendEnd _ len) = Right (Just len)
    project ev                             = Left ev

-- | The channel events of a mini-protocol sending messages of the lengths
-- given, in chunks of the length given, through a mux whose bearer drains
-- into another mux.
sendEvents
  :: forall m.
     ( Alternative   (STM m)
     , MonadAsync         m
     , MonadDelay         m
     , MonadEvaluate      m
     , MonadFork          m
     , MonadLabelledSTM   m
     , MonadMask          m
     , MonadTimer         m
     , MonadThrow    (STM m)
     )
  => Int -> [Int] -> m [Mx.ChannelTrace]
sendEvents chunk lens = do
    (clientBearer, serverBearer) <- queueBearerPair
    events <- newTVarIO []
    let collecting :: Tracer m (Identity Mx.ChannelTrace)
        collecting = Tracer (emit (\(Identity ev) -> atomically (modifyTVar events (ev :))))
        tracers :: Mx.Tracers m
        tracers = Mx.nullTracers { Mx.channelTracer = collecting }
    clientMux <- Mx.new tracers Nothing [sendProtocolInfo Mx.InitiatorDirection]
    serverMux <- Mx.new Mx.nullTracers Nothing [sendProtocolInfo Mx.ResponderDirection]
    withAsync (Mx.run clientMux clientBearer) $ \_ ->
      withAsync (Mx.run serverMux serverBearer) $ \_ -> do
        done <- Mx.runMiniProtocol clientMux sendProtocol Mx.InitiatorDirection Mx.StartEagerly $ \chan -> do
                  forM_ lens $ \len -> Mx.send chan (message len)
                  return ((), Nothing)
        _ <- atomically done
        reverse <$> readTVarIO events
  where
    message :: Int -> BL.ByteString
    message len = BL.fromChunks (replicate (len `div` chunk) (BS.replicate chunk 0x61)
                                 ++ [ BS.replicate (len `mod` chunk) 0x61 | len `mod` chunk > 0 ])


--
-- Counters
--

-- | A connection reset, as the socket bearer sees a peer's close with no
-- lingering: the error the socket library raises for the errno.
resetError :: IOException
resetError = errnoToIOError "recv" eCONNRESET Nothing Nothing

-- | The ways a mux can end that the counters tell apart, and one they do
-- not count: the peer closing, read while no mini-protocol is running,
-- after the last finished or after one was killed from outside, in its
-- action or waiting for the muxer to take its last bytes, which ends the
-- mux stopped.
data MuxFailure = ReadTimeout | ReadError | ReadReset | ReadResetIdle | PeerClosed | PeerClosedIdle
                | PeerClosedKilled | PeerClosedKilledDraining | DecodeError | UnknownProtocol
                | InitiatorOnlyData | Overrun | ReadOther | WriteTimeout | WriteError | WriteReset
                | WriteResetIdle | WriteOther | ProtocolFailure | GateTimeout
  deriving (Show, Eq, Enum, Bounded)

-- | A mux that fails in the given way counts that failure, once, and nothing
-- else, and ends failed; a close read with nothing running, after the last
-- protocol finished or after one was killed from outside, counts nothing
-- and ends it stopped, a reset then fails it; a killed protocol cannot be
-- started again. The protocol-level
-- failures come from real SDUs through the demuxer; the timeouts, the I/O
-- errors and the closes are thrown by the bearer, as the socket bearer does,
-- and so is an error of another kind, as an encoding that would not force
-- surfaces in a write; the mini-protocol failure is the initiator's own
-- exception, and the kill an asynchronous exception thrown at its thread,
-- in its action or past it, waiting for the muxer to take its last message.
prop_mux_counters_failure :: MuxFailure -> Property
prop_mux_counters_failure failure =
    counterexample (show failure) $
      runSimOrThrow run === Just (stops failure, expected failure)
  where
    num :: Mx.MiniProtocolNum
    num = Mx.MiniProtocolNum 2

    -- Whether the mux ends stopped rather than failed: only on a close read
    -- with nothing running.
    stops :: MuxFailure -> Bool
    stops f = f `elem` [PeerClosedIdle, PeerClosedKilled, PeerClosedKilledDraining]

    -- Bytes too: nothing is written when the write fails or is held, and
    -- nothing read is counted: an SDU that overruns is not, the write that
    -- would have counted it aborted, and an unknown or unwanted protocol
    -- never is.
    expected :: MuxFailure -> Mx.Counts
    expected f = case f of
      WriteTimeout             -> failed Mx.noFailures { Mx.mfWriteTimeouts = 1 }
      GateTimeout              -> failed Mx.noFailures { Mx.mfWriteTimeouts = 1, Mx.mfGateTimeouts = 1 }
      WriteError               -> failed Mx.noFailures { Mx.mfWriteErrors = 1 }
      WriteReset               -> failed Mx.noFailures { Mx.mfPeerClosed = 1 }
      WriteResetIdle           -> failed Mx.noFailures { Mx.mfPeerClosed = 1 }
      WriteOther               -> failed Mx.noFailures { Mx.mfOtherErrors = 1 }
      ReadTimeout              -> failed Mx.noFailures { Mx.mfReadTimeouts = 1 }
      ReadError                -> failed Mx.noFailures { Mx.mfReadErrors = 1 }
      ReadReset                -> failed Mx.noFailures { Mx.mfPeerClosed = 1 }
      ReadResetIdle            -> failed Mx.noFailures { Mx.mfPeerClosed = 1 }
      ReadOther                -> failed Mx.noFailures { Mx.mfOtherErrors = 1 }
      Overrun                  -> failed Mx.noFailures { Mx.mfOverruns = 1 }
      DecodeError              -> failed Mx.noFailures { Mx.mfProtocolErrors = 1 }
      UnknownProtocol          -> failed Mx.noFailures { Mx.mfProtocolErrors = 1 }
      InitiatorOnlyData        -> failed Mx.noFailures { Mx.mfProtocolErrors = 1 }
      PeerClosed               -> failed Mx.noFailures { Mx.mfPeerClosed = 1 }
      PeerClosedIdle           -> failed Mx.noFailures
      PeerClosedKilled         -> failed Mx.noFailures
      PeerClosedKilledDraining -> failed Mx.noFailures
      ProtocolFailure          -> failed Mx.noFailures
                                    { Mx.mfMiniProtocols = M.singleton (num, Mx.InitiatorDir) 1 }

    -- The failures, and no bytes.
    failed :: Mx.Failures -> Mx.Counts
    failed f = Mx.noCounts { Mx.mcFailures = f }

    -- An SDU as the peer would send it, encoded.
    sdu :: Mx.MiniProtocolNum -> Mx.MiniProtocolDir -> BL.ByteString -> BL.ByteString
    sdu n dir payload = Mx.encodeSDU (peerSDU n dir payload)

    -- A bearer's I/O error, as the socket bearer wraps it.
    ioError' :: Mx.Error
    ioError' = Mx.IOException (userError "an I/O error of no particular kind") "recv errored"

    run :: forall s. IOSim s (Maybe (Bool, Mx.Counts))
    run = do
      counters <- Counters.newCounters
      w <- atomically $ newTBQueue 10
      r <- atomically $ newTBQueue 10
      -- The initiator says when it runs, and a close the bearer throws waits
      -- for that, so the mux fails with a mini-protocol running; or hands
      -- out its thread to be killed, and the close waits for the kill, which
      -- lands in its action, or past it once the mux has traced its end
      -- while the muxer, held in a write, has yet to take its last message.
      running     <- newTVarIO False
      tidVar      <- newEmptyTMVarIO
      killed      <- newTVarIO False
      writing     <- newTVarIO False
      terminating <- newTVarIO False
      base <- getBearer makeQueueChannelBearer (-1)
                QueueChannel { writeQueue = w, readQueue = r } Nothing
      let info :: MiniProtocolInfo Mx.InitiatorMode
          info = MiniProtocolInfo {
              miniProtocolNum        = num,
              miniProtocolDir        = Mx.InitiatorDirectionOnly,
              miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 16 },
              miniProtocolCapability = Nothing }
          -- A second mini-protocol the test never starts, numbered below the
          -- first so the mux's map lists it first: a close while the first
          -- runs finds one idle, then one running.
          idle :: MiniProtocolInfo Mx.InitiatorMode
          idle = info { miniProtocolNum = Mx.MiniProtocolNum 1 }

          -- The mux's trace of the initiator's end, between its action and
          -- its last transaction.
          tracers :: Mx.Tracers (IOSim s)
          tracers = Mx.nullTracers { Mx.tracer = Tracer (emit (\(Identity ev) -> terminated ev)) }

          terminated :: Mx.Trace -> IOSim s ()
          terminated (Mx.TraceTerminating n d)
            | n == num && d == Mx.InitiatorDir = atomically (writeTVar terminating True)
          terminated _ = return ()
      -- A gate is waited at only under scheduled egress.
      budget <- Bucket.newBucket 1e9 65536 Nothing
      let policy = Mx.EgressPolicy { Mx.egressBudget = budget, Mx.egressSlice = Nothing,
                                     Mx.egressLaneOf = \_ _ -> Mx.Scheduled }
      mux <- case failure of
                  GateTimeout -> Mx.newWithEgress policy tracers (Just counters) [info, idle]
                  _           -> Mx.new tracers (Just counters) [info, idle]
      let onceRunning :: Mx.Error -> IOSim s a
          onceRunning e = atomically (readTVar running >>= check) >> throwIO e

          -- Once the initiator has finished its action, its last data held
          -- in a batch, as the write of it can find the peer gone; the
          -- protocol is not idle until that write has returned.
          onceTerminated :: Mx.Error -> IOSim s a
          onceTerminated e = atomically (readTVar terminating >>= check) >> throwIO e

          -- Once the test has killed the initiator and waited on its status.
          onceKilled :: Mx.Error -> IOSim s a
          onceKilled e = atomically (readTVar killed >>= check) >> throwIO e

          -- A write held for good, once it has said it is in progress: the
          -- muxer's first, the message behind it left in the Wanton.
          held :: IOSim s a
          held = atomically (writeTVar writing True) >> atomically retry

          -- The peer closing, as a read reports it.
          closed :: Mx.Error
          closed = Mx.BearerClosed "peer"

          -- A reset, as the socket bearer wraps it on a read or a write.
          readReset, writeReset :: Mx.Error
          readReset  = Mx.IOException resetError "recv errored"
          writeReset = Mx.IOException resetError "sendAll errored"

          -- An exception of a kind the mux does not know: a lazy encoding
          -- that fails, forced by a write, or a bug.
          otherKind :: ErrorCall
          otherKind = ErrorCall "an encoding that would not force"

          bearer :: Mx.Bearer (IOSim s)
          bearer = case failure of
            ReadTimeout              -> base { Mx.read      = \_ _   -> throwIO Mx.SDUReadTimeout }
            ReadError                -> base { Mx.read      = \_ _   -> throwIO ioError' }
            ReadReset                -> base { Mx.read      = \_ _   -> onceRunning readReset }
            ReadResetIdle            -> base { Mx.read      = \_ _   -> throwIO readReset }
            PeerClosed               -> base { Mx.read      = \_ _   -> onceRunning closed }
            PeerClosedIdle           -> base { Mx.read      = \_ _   -> throwIO closed }
            PeerClosedKilled         -> base { Mx.read      = \_ _   -> onceKilled closed }
            PeerClosedKilledDraining -> base { Mx.read      = \_ _   -> onceKilled closed,
                                               Mx.writeMany = \_ _ _ -> held }
            ReadOther                -> base { Mx.read      = \_ _   -> throwIO otherKind }
            WriteTimeout             -> base { Mx.writeMany = \_ _ _ -> throwIO Mx.SDUWriteTimeout }
            GateTimeout              -> base { Mx.awaitWritable = \_ _ -> throwIO Mx.SDUWriteTimeout }
            WriteError               -> base { Mx.writeMany = \_ _ _ -> throwIO ioError' }
            WriteReset               -> base { Mx.writeMany = \_ _ _ -> onceRunning writeReset }
            WriteResetIdle           -> base { Mx.writeMany = \_ _ _ -> onceTerminated writeReset }
            WriteOther               -> base { Mx.writeMany = \_ _ _ -> throwIO otherKind }
            _                        -> base
      withAsync (Mx.run mux bearer) $ \muxA -> do
        let initiator :: (Mx.ByteChannel (IOSim s) -> IOSim s ()) -> IOSim s ()
            initiator body =
              void $ Mx.runMiniProtocol mux num Mx.InitiatorDirectionOnly Mx.StartEagerly
                       (\chan -> body chan >> return ((), Nothing))

            -- Bytes from the peer, as its bearer sent them.
            fromPeer :: BL.ByteString -> IOSim s ()
            fromPeer = atomically . writeTBQueue r

            x :: BL.ByteString
            x = BL8.pack "x"

            nowRunning :: IOSim s ()
            nowRunning = atomically (writeTVar running True)

            protocolBroken :: IOError
            protocolBroken = userError "the peer broke the protocol"

            -- The kill done: the status no longer running, or a second later
            -- should it still be, the killed thread long gone by then, so
            -- the close finds the protocol running; once it is not, a start
            -- of the killed protocol is refused, with the killed status.
            killedAndRefused :: IOSim s ()
            killedAndRefused = do
              ended <- timeout 1 $ atomically $ do
                status <- Mx.miniProtocolStateMap mux M.! (num, Mx.InitiatorDir)
                check (status /= Mx.StatusRunning)
              forM_ ended $ \() -> do
                started <- try $ Mx.runMiniProtocol mux num Mx.InitiatorDirectionOnly Mx.StartEagerly
                                   (\_ -> return ((), Nothing))
                case started of
                     Left (Mx.ProtocolAlreadyRunning _ _ Mx.StatusKilled) -> return ()
                     Left e  -> throwIO e
                     Right _ -> throwIO (userError "a killed protocol was started again")
              atomically (writeTVar killed True)
        case failure of
             DecodeError              -> fromPeer (BL.replicate 3 0)
             UnknownProtocol          -> fromPeer (sdu (Mx.MiniProtocolNum 99) Mx.ResponderDir x)
             -- Data from an initiator, for a responder this mux does not run.
             InitiatorOnlyData        -> fromPeer (sdu num Mx.InitiatorDir x)
             Overrun                  -> fromPeer (sdu num Mx.ResponderDir (BL.replicate 100 0))
             WriteTimeout             -> initiator (\chan -> Mx.send chan x)
             GateTimeout              -> initiator (\chan -> Mx.send chan x)
             WriteError               -> initiator (\chan -> Mx.send chan x)
             WriteReset               -> initiator $ \chan ->
                                           Mx.send chan x >> nowRunning >> void (Mx.recv chan)
             WriteResetIdle           -> initiator (\chan -> Mx.send chan x)
             WriteOther               -> initiator (\chan -> Mx.send chan x)
             PeerClosed               -> initiator (\chan -> nowRunning >> void (Mx.recv chan))
             ReadReset                -> initiator (\chan -> nowRunning >> void (Mx.recv chan))
             ProtocolFailure          -> initiator (\_ -> throwIO protocolBroken)
             PeerClosedKilled         -> do
               initiator $ \chan -> do
                 myThreadId >>= atomically . putTMVar tidVar
                 void (Mx.recv chan)
               tid <- atomically (takeTMVar tidVar)
               -- Killed in its action, as the node's registries kill a
               -- client.
               throwTo tid AsyncCancelled
               killedAndRefused
             PeerClosedKilledDraining -> do
               initiator $ \chan -> do
                 myThreadId >>= atomically . putTMVar tidVar
                 Mx.send chan x
                 -- The muxer holds the first message in its write, so the
                 -- second stays in the Wanton for the job to wait on.
                 atomically (readTVar writing >>= check)
                 Mx.send chan x
               tid <- atomically (takeTMVar tidVar)
               -- Killed past its action, once the mux has traced its end.
               atomically (readTVar terminating >>= check)
               throwTo tid AsyncCancelled
               killedAndRefused
             _                        -> return ()
        done <- timeout 10 (waitCatch muxA)
        case done of
             Nothing    -> return Nothing
             Just ended -> do
               counts <- atomically (Counters.readCounters counters)
               return (Just (either (const False) (const True) ended, counts))

-- | An interval for the loop, in whole seconds from five under its floor of
-- one second to ten above it.
newtype LoopInterval = LoopInterval Int
  deriving Show

instance Arbitrary LoopInterval where
    arbitrary = LoopInterval <$> choose (-5, 10)
    shrink (LoopInterval k) = map LoopInterval (shrink k)

-- | 'withCounters' makes the sets asked for, the remote one and, if
-- asked, the local one, and traces them every interval, a second at least,
-- the remote one first, for as long as its body runs, the values traced
-- being the counters at that instant; without a local set, one event per
-- interval; at an interval of zero or less, no set and no event. The first
-- three rounds are judged off the simulation's trace, so a loop that spins
-- shows as events too early, not as a hang.
prop_mux_counters_loop :: LoopInterval -> Bool -> Property
prop_mux_counters_loop (LoopInterval k) withLocal =
    counterexample (show seen) $
      if k <= 0
         then seen === []
         else map fst seen === concat [ replicate (length perInterval) (roundAt i)
                                      | i <- [1 .. 3 :: Int]
                                      ]
              .&&. map snd seen === concat (replicate 3 perInterval)
  where
    interval :: DiffTime
    interval = fromIntegral k

    delay :: DiffTime
    delay = max 1 interval

    -- The instant of the i-th round.
    roundAt :: Int -> Time
    roundAt i = (fromIntegral i * delay) `addTime` Time 0

    perInterval :: [Mx.CountersTrace]
    perInterval =
        Mx.TraceRemoteCounters (failures Mx.noFailures { Mx.mfPeerClosed = 1 })
      : [ Mx.TraceLocalCounters (failures Mx.noFailures { Mx.mfWriteTimeouts = 1 })
        | withLocal
        ]

    -- The failures, and no bytes.
    failures :: Mx.Failures -> Mx.Counts
    failures f = Mx.noCounts { Mx.mcFailures = f }

    -- The first three rounds of events, each with the time it was traced at.
    seen :: [(Time, Mx.CountersTrace)]
    seen = take (3 * length perInterval) $ selectTraceEventsDynamicWithTime $ runSimTrace $
      Mx.withCounters interval withLocal Nothing (mkTracer traceM) $ \remote local -> do
        -- A failure in each set there is, so the events carry them.
        atomically $ do
          Counters.countDemuxerFailure remote (toException (Mx.BearerClosed "peer"))
          Counters.countMuxerFailure local (toException Mx.SDUWriteTimeout)
        threadDelay (3 * delay + delay / 2)

-- | An SDU from the peer on a protocol and direction, as it would send it.
peerSDU :: Mx.MiniProtocolNum -> Mx.MiniProtocolDir -> BL.ByteString -> Mx.SDU
peerSDU num dir payload = Mx.SDU {
    Mx.msHeader = Mx.SDUHeader { Mx.mhTimestamp = Mx.RemoteClockModel 0
                               , Mx.mhNum       = num
                               , Mx.mhDir       = dir
                               , Mx.mhLength    = fromIntegral (BL.length payload) },
    Mx.msBlob   = payload }

-- | A payload of so many bytes.
filler :: Int -> BL.ByteString
filler bytes = BL.replicate (fromIntegral bytes) 0xaa

-- | A bearer modelled in the simulation: the read, given the mux's timeout
-- function, and the batch write; a single write is a batch of one.
modelledBearer :: (TimeoutFn (IOSim s) -> IOSim s (Mx.SDU, Time))
               -> ([Mx.SDU] -> IOSim s Time)
               -> Mx.Bearer (IOSim s)
modelledBearer readOne writeBatch = Mx.Bearer {
    Mx.write          = \_ _ sdu -> writeBatch [sdu],
    Mx.writeMany      = \_ _ -> writeBatch,
    Mx.read           = \_ -> readOne,
    Mx.sduSize        = Mx.SDUSize 12288,
    Mx.batchSize      = 1000000,
    Mx.name           = "modelled",
    Mx.egressInterval = 0,
    Mx.awaitWritable  = \_ _ -> return ()
  }

-- | A close after every mini-protocol has finished is not a failure, in every
-- schedule: the responder, started on demand by the peer's message, answers
-- it and returns, and once it has and its status is idle the bearer closes.
-- Explored with IOSimPOR, so the close also lands between the responder's
-- exit and its job's deregistration: nothing is counted, and the mux ends
-- as stopped, in every schedule. Run once: the schedules are the input, and
-- nothing else shapes them.
prop_mux_close_after_exit :: Property
prop_mux_close_after_exit =
  once $ exploreSimTrace (\o -> o { explorationScheduleBound = 40 }) sim $ \_ trace ->
    case traceResult False trace of
      Left e  -> counterexample (show e) False
      Right r -> counterexample (show r) $ r === (Mx.noFailures, True)
  where
    num :: Mx.MiniProtocolNum
    num = Mx.MiniProtocolNum 2

    sim :: forall s. IOSim s (Mx.Failures, Bool)
    sim = do
      exploreRaces
      counters <- Counters.newCounters
      -- The peer's message, delivered by the first read; a later read waits
      -- for the responder to have answered and gone idle, then closes.
      pending  <- newTVarIO (Just (peerSDU num Mx.InitiatorDir (filler 100)))
      answered <- newTVarIO False
      let info :: MiniProtocolInfo Mx.ResponderMode
          info = MiniProtocolInfo {
              miniProtocolNum        = num,
              miniProtocolDir        = Mx.ResponderDirectionOnly,
              miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 4000 },
              miniProtocolCapability = Nothing }
      mux <- Mx.new Mx.nullTracers (Just counters) [info]
      let idle :: STM (IOSim s) ()
          idle = do
            readTVar answered >>= check
            status <- Mx.miniProtocolStateMap mux M.! (num, Mx.ResponderDir)
            check (status == Mx.StatusIdle)

          -- The next read: the peer's message, or, once the responder has
          -- answered and gone idle, the close.
          readNext :: IOSim s (Mx.SDU, Time)
          readNext = do
            next <- atomically $ stateTVar pending (, Nothing)
            case next of
                 Just sdu -> (,) sdu <$> getMonotonicTime
                 Nothing  -> atomically idle >> throwIO (Mx.BearerClosed "peer")

          bearer :: Mx.Bearer (IOSim s)
          bearer = modelledBearer (const readNext) (const getMonotonicTime)
      _ <- Mx.runMiniProtocol mux num Mx.ResponderDirectionOnly Mx.StartOnDemand $ \chan -> do
             _ <- Mx.recv chan
             Mx.send chan (BL8.pack "ok")
             atomically (writeTVar answered True)
             return ((), Nothing)
      muxA  <- async (Mx.run mux bearer)
      ended <- waitCatch muxA
      failures <- Mx.mcFailures <$> atomically (Counters.readCounters counters)
      return (failures, either (const False) (const True) ended)

--
-- The counter set under concurrent steps
--

-- | One step against a counter set: a mux that ends with these bytes sent and
-- read, by protocol and direction, or a failure counted.
data CountersOp = RetireMux [((Word16, Bool), Word64)] [((Word16, Bool), Word64)]
                | MuxerDied Int    -- ^ of the n-th of 'muxerErrors'
                | DemuxerDied Int  -- ^ of the n-th of 'demuxerErrors'
                | ProtocolDied Word16 Bool Int
                  -- ^ a mini-protocol, by number and whether our initiator,
                  -- of the n-th of 'protocolErrors'
  deriving Show

-- | One more error of a kind not listed.
otherError :: Mx.Failures -> Mx.Failures
otherError f = f { Mx.mfOtherErrors = Mx.mfOtherErrors f + 1 }

-- | What the muxer can die of, each with what it counts: the counted kinds,
-- and ones that are not.
muxerErrors :: [(SomeException, Mx.Failures -> Mx.Failures)]
muxerErrors =
  [ ( toException Mx.SDUWriteTimeout
    , \f -> f { Mx.mfWriteTimeouts = Mx.mfWriteTimeouts f + 1 } )
  , ( toException wrappedIOError
    , \f -> f { Mx.mfWriteErrors = Mx.mfWriteErrors f + 1 } )
  , ( toException rawIOError
    , \f -> f { Mx.mfWriteErrors = Mx.mfWriteErrors f + 1 } )
  , ( toException (Mx.IOException resetError "sendAll errored")
    , \f -> f { Mx.mfPeerClosed = Mx.mfPeerClosed f + 1 } )
  , ( toException brokenPipeError
    , \f -> f { Mx.mfPeerClosed = Mx.mfPeerClosed f + 1 } )
  , ( toException netDownError
    , \f -> f { Mx.mfWriteErrors = Mx.mfWriteErrors f + 1 } )
  , ( toException Mx.SDUReadTimeout
    , otherError )
  , ( toException (ErrorCall "an encoding that would not force")
    , otherError )
  , ( toException AsyncCancelled
    , id )
  ]

-- | What the demuxer can die of, the mux failing of it, each with what it
-- counts: the counted kinds, and ones that are not.
demuxerErrors :: [(SomeException, Mx.Failures -> Mx.Failures)]
demuxerErrors =
  [ ( toException Mx.SDUReadTimeout
    , \f -> f { Mx.mfReadTimeouts = Mx.mfReadTimeouts f + 1 } )
  , ( toException (Mx.IngressQueueOverRun (Mx.MiniProtocolNum 2) Mx.InitiatorDir)
    , \f -> f { Mx.mfOverruns = Mx.mfOverruns f + 1 } )
  , ( toException (Mx.UnknownMiniProtocol (Mx.MiniProtocolNum 99))
    , \f -> f { Mx.mfProtocolErrors = Mx.mfProtocolErrors f + 1 } )
  , ( toException (Mx.InitiatorOnly (Mx.MiniProtocolNum 2))
    , \f -> f { Mx.mfProtocolErrors = Mx.mfProtocolErrors f + 1 } )
  , ( toException (Mx.SDUDecodeError "x")
    , \f -> f { Mx.mfProtocolErrors = Mx.mfProtocolErrors f + 1 } )
  , ( toException (Mx.BearerClosed "peer")
    , \f -> f { Mx.mfPeerClosed = Mx.mfPeerClosed f + 1 } )
  , ( toException (Mx.IOException resetError "recv errored")
    , \f -> f { Mx.mfPeerClosed = Mx.mfPeerClosed f + 1 } )
  , ( toException resetError
    , \f -> f { Mx.mfPeerClosed = Mx.mfPeerClosed f + 1 } )
  , ( toException vanishedError
    , \f -> f { Mx.mfReadErrors = Mx.mfReadErrors f + 1 } )
  , ( toException netDownError
    , \f -> f { Mx.mfReadErrors = Mx.mfReadErrors f + 1 } )
  , ( toException wrappedIOError
    , \f -> f { Mx.mfReadErrors = Mx.mfReadErrors f + 1 } )
  , ( toException rawIOError
    , \f -> f { Mx.mfReadErrors = Mx.mfReadErrors f + 1 } )
  , ( toException Mx.SDUWriteTimeout
    , otherError )
  , ( toException (ErrorCall "a bug")
    , otherError )
  , ( toException AsyncCancelled
    , id )
  ]

-- | What a mini-protocol's thread can die of, each with whether it counts:
-- its own exception does; a cancellation and the mux's own errors do not.
protocolErrors :: [(SomeException, Bool)]
protocolErrors =
  [ (toException (userError "the peer broke the protocol"), True)
  , (toException AsyncCancelled, False)
  , (toException (Mx.BearerClosed "the mux died"), False)
  , (toException (Mx.BlockedOnCompletionVar (Mx.MiniProtocolNum 2)), False)
  ]

-- | A bearer's I/O error as the socket bearer wraps it, and as the
-- simulation's channel throws it.
wrappedIOError :: Mx.Error
wrappedIOError = Mx.IOException rawIOError "recv errored"

rawIOError :: IOException
rawIOError = userError "an I/O error of no particular kind"

-- | A broken pipe, as a write finds a peer gone, and a connection vanished
-- without an errno, as the simulation's channel throws for a connection
-- gone or a fault it injects: no close, no errno saying so.
brokenPipeError, vanishedError :: IOException
brokenPipeError = errnoToIOError "sendAll" ePIPE Nothing Nothing
vanishedError = resourceVanishedIOError "recv" "resource vanished"

-- | The network down, which base files under the vanished kind too: not a
-- close, its errno says.
netDownError :: IOException
netDownError = errnoToIOError "recv" eNETDOWN Nothing Nothing

-- | Byte counts as a generator makes them: a protocol number, whether our
-- side is the initiator, and the bytes.
genByteCounts :: Gen [((Word16, Bool), Word64)]
genByteCounts = listOf $ (,) <$> ((,) <$> choose (0, 3) <*> arbitrary) <*> choose (1, 1000000)

instance Arbitrary CountersOp where
    arbitrary = frequency
      [ (3, RetireMux <$> genByteCounts <*> genByteCounts)
      , (1, MuxerDied <$> choose (0, length muxerErrors - 1))
      , (2, DemuxerDied <$> choose (0, length demuxerErrors - 1))
      , (1, ProtocolDied <$> choose (0, 3) <*> arbitrary <*> choose (0, length protocolErrors - 1))
      ]
    shrink (RetireMux s r) =
         [ RetireMux s' r | s' <- shrinkList (const []) s ]
      ++ [ RetireMux s r' | r' <- shrinkList (const []) r ]
    shrink _ = []

-- | A byte-count key as the mux builds its own.
countersKey :: (Word16, Bool) -> (Mx.MiniProtocolNum, Mx.MiniProtocolDir)
countersKey (n, initiator) =
    (Mx.MiniProtocolNum n, if initiator then Mx.InitiatorDir else Mx.ResponderDir)

countersMap :: [((Word16, Bool), Word64)] -> Mx.ByteCounts
countersMap = M.fromListWith (+) . map (Bifunctor.first countersKey)

-- | The keys a mux with these sent and read counts needs, and one more for a
-- step that grows it, each once.
slotKeys :: [((Word16, Bool), Word64)] -> [((Word16, Bool), Word64)]
         -> [(Mx.MiniProtocolNum, Mx.MiniProtocolDir)]
slotKeys sent recv =
    nub (countersKey (0, True) : M.keys (countersMap sent `M.union` countersMap recv))

-- | Set a mux's slots to these counts, as its muxers would have.
fillSlots :: MonadSTM m => Counters.ByteSlots m -> Mx.ByteCounts -> STM m ()
fillSlots slots counts =
    forM_ (M.toList counts) $ \(k, b) ->
      forM_ (M.lookup k slots) $ \slot -> modifyTVar slot (\es -> es { Mx.esSent = b })

-- | Ingress queues under these keys, nothing read.
newQueues :: MonadSTM m
          => [(Mx.MiniProtocolNum, Mx.MiniProtocolDir)]
          -> m (M.Map (Mx.MiniProtocolNum, Mx.MiniProtocolDir) (Mx.IngressQueue m))
newQueues keys = M.fromList <$> mapM (\k -> (,) k <$> Mx.newIngressQueue) keys

-- | Set the queues' read counts, as the mux's demuxer would have.
fillQueues :: MonadSTM m
           => M.Map (Mx.MiniProtocolNum, Mx.MiniProtocolDir) (Mx.IngressQueue m)
           -> Mx.ByteCounts
           -> STM m ()
fillQueues queues counts =
    forM_ (M.toList counts) $ \(k, b) ->
      forM_ (M.lookup k queues) $ \q -> modifyTVar q (\iq -> iq { Mx.iqReceived = b })

-- | Apply one step to a counter set: a mux that registers, writes its cells
-- and ends, or a failure counted.
countersStep :: (MonadSTM m, MonadMask m) => Mx.Counters m -> CountersOp -> m ()
countersStep counters op = case op of
  RetireMux sent recv -> do
    slots  <- atomically $ Counters.newByteSlots (slotKeys sent recv)
    queues <- newQueues (slotKeys sent recv)
    Counters.withByteCells (Just counters) slots queues $
      atomically $ do
        fillSlots slots (countersMap sent)
        fillQueues queues (countersMap recv)
  MuxerDied i   ->
    let (err, _) = muxerErrors !! i
    in atomically $ Counters.countMuxerFailure (Just counters) err
  DemuxerDied i ->
    let (err, _) = demuxerErrors !! i
    in atomically $ Counters.countDemuxerFailure (Just counters) err
  ProtocolDied n initiator i ->
    let (num, dir) = countersKey (n, initiator)
        (err, _)   = protocolErrors !! i
    in atomically $ Counters.countMiniProtocolFailure (Just counters) num dir err

-- | What a counter set reads after these steps: every mux's bytes, ended or
-- not, and every counted failure.
countersModel :: [CountersOp] -> Mx.Counts
countersModel ops = Mx.Counts {
    Mx.mcFailures = foldr failureOf Mx.noFailures ops,
    Mx.mcSent     = M.unionsWith (+) [ countersMap s | RetireMux s _ <- ops ],
    Mx.mcReceived = M.unionsWith (+) [ countersMap r | RetireMux _ r <- ops ]
  }
  where
    failureOf :: CountersOp -> Mx.Failures -> Mx.Failures
    failureOf op = case op of
      RetireMux {}  -> id
      MuxerDied i   -> case muxerErrors !! i of (_, count) -> count
      DemuxerDied i -> case demuxerErrors !! i of (_, count) -> count
      ProtocolDied n initiator i
        | snd (protocolErrors !! i) ->
            \f -> f { Mx.mfMiniProtocols =
                        M.insertWith (+) (countersKey (n, initiator)) 1 (Mx.mfMiniProtocols f) }
        | otherwise -> id

-- | A counter set's history for a snapshot: steps done, and muxes that are
-- still running, each holding its cells, written once.
data SplitOp = Ended CountersOp
             | Live [((Word16, Bool), Word64)] [((Word16, Bool), Word64)]
  deriving Show

instance Arbitrary SplitOp where
    arbitrary = frequency
      [ (2, Ended <$> arbitrary)
      , (3, Live <$> genByteCounts <*> genByteCounts)
      ]
    shrink (Ended op) = map Ended (shrink op)
    shrink (Live s r) =
         [ Live s' r | s' <- shrinkList (const []) s ]
      ++ [ Live s r' | r' <- shrinkList (const []) r ]

-- | One step taken while a snapshot runs, before its @k@-th transaction.
data SplitStep = SplitStep Int Step
  deriving Show

data Step = StepEnd Int        -- ^ the n-th live mux ends, modulo how many there are
          | StepStart [((Word16, Bool), Word64)] [((Word16, Bool), Word64)]
                               -- ^ a mux starts, with these bytes
          | StepGrow Int Word64 -- ^ the n-th live mux sends this much more
          | StepFail CountersOp -- ^ a failure is counted
  deriving Show

instance Arbitrary SplitStep where
    arbitrary = SplitStep <$> choose (0, 12) <*> frequency
      [ (3, StepEnd <$> arbitrary)
      , (1, StepStart <$> genByteCounts <*> genByteCounts)
      , (2, StepGrow <$> arbitrary <*> choose (1, 1000000))
      , (1, StepFail <$> (arbitrary `suchThat` isFailure))
      ]
      where
        isFailure :: CountersOp -> Bool
        isFailure op = case op of
          RetireMux {} -> False
          _            -> True
    shrink (SplitStep k s) = [ SplitStep k' s | k' <- shrink k ]

-- | A live mux the harness holds open: its thread, the flag that ends it, and
-- its slots.
data LiveMux m = LiveMux (Async m ()) (StrictTVar m Bool) (Counters.ByteSlots m)

-- | A snapshot in small transactions sees one concurrent step whole or not
-- at all: with the step taken before the snapshot's @k@-th transaction, what
-- it reads is the exact total before the step or after it, so a mux that
-- ends in between is counted once, never twice and never not at all; and it
-- runs one transaction for the totals and one per counter of each live mux,
-- a slot or a queue.
prop_mux_counters_snapshot_split :: [SplitOp] -> SplitStep -> Property
prop_mux_counters_snapshot_split ops (SplitStep k step) =
    classify injected                              "a step taken mid-snapshot" $
    classify (injected && isEnd && liveBefore > 0) "a mux ends mid-snapshot" $
    classify (liveBefore > 0)                      "live muxes" $
         counterexample ("transactions " ++ show txs
                           ++ ", counters at the totals " ++ show countersAtTotals)
           (txs === 1 + countersAtTotals)
    .&&. counterexample ("read " ++ show result ++ "\nbefore " ++ show totalBefore
                           ++ "\nafter " ++ show totalAfter)
           (if injected
               then result == totalBefore || result == totalAfter
               else result == totalBefore)
  where
    txs :: Int
    result :: Mx.Counts
    injected :: Bool
    (txs, result, injected) = runSimOrThrow run

    isEnd :: Bool
    isEnd = case step of
      StepEnd {} -> True
      _          -> False

    liveBefore :: Int
    liveBefore = length [ () | Live {} <- ops ]

    -- The counters of the live muxes when the totals are read, a slot and a
    -- queue per key of each: a step before the first transaction has already
    -- started or ended a mux.
    countersAtTotals :: Int
    countersAtTotals
      | injected && k == 0 = sum liveCounters + case step of
                               StepStart s r               -> countersOf s r
                               StepEnd i  | liveBefore > 0 -> negate (endedCounters i)
                               _                           -> 0
      | otherwise = sum liveCounters

    -- The counters of each live mux, in order, and of the one a step ends,
    -- the i-th modulo how many there are.
    liveCounters :: [Int]
    liveCounters = [ countersOf s r | Live s r <- ops ]

    endedCounters :: Int -> Int
    endedCounters i = liveCounters !! (i `mod` liveBefore)

    countersOf :: [((Word16, Bool), Word64)] -> [((Word16, Bool), Word64)] -> Int
    countersOf s r = 2 * length (slotKeys s r)

    asOp :: SplitOp -> CountersOp
    asOp (Ended op) = op
    asOp (Live s r) = RetireMux s r

    totalBefore, totalAfter :: Mx.Counts
    totalBefore = countersModel (map asOp ops)
    totalAfter  = countersModel (map asOp ops ++ case step of
               StepStart s r                 -> [RetireMux s r]
               StepGrow _ d | liveBefore > 0 -> [RetireMux [((0, True), d)] []]
               StepFail op                   -> [op]
               _                             -> [])

    run :: forall s. IOSim s (Int, Mx.Counts, Bool)
    run = do
      counters <- Counters.newCounters
      mapM_ (countersStep counters) [ op | Ended op <- ops ]
      lives <- forM [ (s, r) | Live s r <- ops ] (startLive counters)
      liveVar     <- newTVarIO lives
      txCount     <- newTVarIO (0 :: Int)
      injectedVar <- newTVarIO False
      let inject :: IOSim s ()
          inject = do
            ls <- readTVarIO liveVar
            case step of
              StepEnd i | not (null ls) -> do
                let n = i `mod` length ls
                    LiveMux a stop _ = ls !! n
                atomically (writeTVar stop True)
                wait a
                atomically (writeTVar liveVar (take n ls ++ drop (n + 1) ls))
              StepStart s r -> do
                lm <- startLive counters (s, r)
                atomically (modifyTVar liveVar (++ [lm]))
              StepGrow i d | not (null ls) -> do
                let LiveMux _ _ slots = ls !! (i `mod` length ls)
                atomically $
                  forM_ (M.lookup (countersKey (0, True)) slots) $ \slot ->
                    modifyTVar slot (\es -> es { Mx.esSent = Mx.esSent es + d })
              StepFail op -> countersStep counters op
              _ -> return ()

          runTx :: forall a. STM (IOSim s) a -> IOSim s a
          runTx tx = do
            n <- atomically $ stateTVar txCount (\c -> (c, c + 1))
            when (n == k) $ do
              inject
              atomically (writeTVar injectedVar True)
            atomically tx
      got <- Counters.snapshotCountersWith runTx counters
      ls  <- readTVarIO liveVar
      forM_ ls $ \(LiveMux a stop _) -> atomically (writeTVar stop True) >> wait a
      (,,) <$> readTVarIO txCount <*> pure got <*> readTVarIO injectedVar

    -- A mux that registers, writes its cells and holds them until told to end.
    startLive :: Mx.Counters (IOSim s)
              -> ([((Word16, Bool), Word64)], [((Word16, Bool), Word64)])
              -> IOSim s (LiveMux (IOSim s))
    startLive counters (s, r) = do
      ready  <- newTVarIO False
      stop   <- newTVarIO False
      slots  <- atomically $ Counters.newByteSlots (slotKeys s r)
      queues <- newQueues (slotKeys s r)
      a <- async $ Counters.withByteCells (Just counters) slots queues $ do
        atomically $ do
          fillSlots slots (countersMap s)
          fillQueues queues (countersMap r)
          writeTVar ready True
        atomically (readTVar ready >>= check)
        atomically (readTVar stop >>= check)
      atomically (readTVar ready >>= check)
      return (LiveMux a stop slots)

--
-- The muxer's write and the count after it: the bytes the bearer took are
-- counted once the write returns, so a cancellation that lands in the
-- bearer's tail, after the take, loses the last batch's count alone, and a
-- write blocked part way through still takes the cancellation. The same
-- for the demuxer's read and the queueing that counts it.
--

-- | Where a cancellation lands: in the bearer's tail, the hand-off made and
-- the count not yet, or once the count is made.
data Cancelled = InTheTail | AfterTheCount
  deriving (Eq, Show)

instance Arbitrary Cancelled where
    arbitrary = elements [InTheTail, AfterTheCount]
    shrink InTheTail     = [AfterTheCount]
    shrink AfterTheCount = []

-- | Batches of messages for the muxer, each message on a mini-protocol
-- number and our side's direction, the muxer to take each batch whole.
newtype Batches = Batches [[(Word16, Bool, Int)]]
  deriving Show

instance Arbitrary Batches where
    arbitrary = Batches <$> (choose (1, 3) >>= \n -> vectorOf n batch)
      where
        batch :: Gen [(Word16, Bool, Int)]
        batch = choose (1, 3) >>= \n -> vectorOf n msg

        msg :: Gen (Word16, Bool, Int)
        msg = (,,) <$> choose (0, 3) <*> arbitrary <*> choose (1, 30000)
    shrink (Batches batches) =
        [ Batches bs
        | bs <- shrinkList shrinkBatch batches
        , not (null bs)
        ]
      where
        shrinkBatch :: [(Word16, Bool, Int)] -> [[(Word16, Bool, Int)]]
        shrinkBatch msgs = [ ms | ms <- shrinkList shrinkMsg msgs, not (null ms) ]

        shrinkMsg :: (Word16, Bool, Int) -> [(Word16, Bool, Int)]
        shrinkMsg (n, initiator, bytes) = [ (n, initiator, b) | b <- shrink bytes, b >= 1 ]

-- | The batches, and where the cancellation lands, in the bearer's tail of
-- the last write or once the muxer has counted it.
data WriteCase = WriteCase Batches Cancelled
  deriving Show

instance Arbitrary WriteCase where
    arbitrary = WriteCase <$> arbitrary <*> arbitrary
    shrink (WriteCase batches cancelled) =
         [ WriteCase bs cancelled | bs <- shrink batches ]
      ++ [ WriteCase batches c | c <- shrink cancelled ]

-- | The bytes a bearer took, headers included, by mini-protocol and direction
-- as the SDU headers carry them.
writtenBytes :: [Mx.SDU] -> Mx.ByteCounts
writtenBytes sdus = M.fromListWith (+)
  [ ((Mx.mhNum h, Mx.mhDir h), fromIntegral Mx.msHeaderLength + fromIntegral (Mx.mhLength h))
  | sdu <- sdus
  , let h = Mx.msHeader sdu
  ]

-- | A muxer running over a modelled bearer: messages queued for it, and the
-- bytes it counted.
data WriteMuxer s = WriteMuxer {
    muxer   :: Async (IOSim s) (),
    enqueue :: [(Word16, Bool, Int)] -> IOSim s (),
    -- ^ queue messages in one transaction, so a muxer at its queue takes
    -- them whole, as one batch
    counted :: IOSim s Mx.ByteCounts
  }

-- | The muxer over a modelled bearer whose batch write is @write@, with a
-- slot for every protocol and direction a message may be on.
newWriteMuxer :: forall s. ([Mx.SDU] -> IOSim s ()) -> IOSim s (WriteMuxer s)
newWriteMuxer write = do
    q         <- newTBQueueIO 100
    byteSlots <- atomically $ Counters.newByteSlots
                   [ (Mx.MiniProtocolNum n, dir)
                   | n <- [0 .. 3]
                   , dir <- [Mx.InitiatorDir, Mx.ResponderDir]
                   ]
    let bearer :: Mx.Bearer (IOSim s)
        bearer = modelledBearer (const (error "the muxer does not read"))
                                (\sdus -> write sdus >> getMonotonicTime)
    mx <- async (void (Egress.muxer q nullTracer Nothing Egress.Unscheduled bearer))
    return WriteMuxer {
        muxer   = mx,
        enqueue = \msgs -> atomically $ forM_ msgs $ \(n, initiator, bytes) -> do
          w <- newTVar (BL.replicate (fromIntegral bytes) 0x55)
          let dir = if initiator then Mx.InitiatorDir else Mx.ResponderDir
              -- the protocol's own variable, as in a mux
              wanton = Egress.Wanton { Egress.want = w,
                                       Egress.held = byteSlots M.! (Mx.MiniProtocolNum n, dir) }
          writeTBQueue q (Egress.TLSRDemand (Mx.MiniProtocolNum n) dir wanton),
        counted = atomically (Counters.readSlots byteSlots)
      }

-- | The bearer takes each batch, says so, and holds it in its tail until
-- released; each next batch is queued while the previous one is held, so
-- the muxer takes it whole. Cancelled in the tail of the last write, the
-- batches before it are counted and the last is not; cancelled once the
-- last is counted, every batch is: by protocol and direction as the SDUs
-- carry them.
prop_mux_written_is_counted :: WriteCase -> Property
prop_mux_written_is_counted (WriteCase (Batches batches) cancelled) =
  case traceResult False (runSimTrace sim) of
    Left e -> counterexample (show e) False
    Right (taken, counted) ->
      counterexample ("taken " ++ show taken) $
        counted === M.unionsWith (+) (case cancelled of
                                        InTheTail     -> drop 1 taken
                                        AfterTheCount -> taken)
  where
    sim :: forall s. IOSim s ([Mx.ByteCounts], Mx.ByteCounts)
    sim = do
      -- Each batch's bytes as the bearer took it, the last first, and the
      -- writes released from the bearer's tail so far.
      taken    <- newTVarIO []
      released <- newTVarIO (0 :: Int)
      WriteMuxer { muxer, enqueue, counted } <- newWriteMuxer $ \sdus -> do
        k <- atomically $ do
          modifyTVar taken (writtenBytes sdus :)
          length <$> readTVar taken
        atomically $ readTVar released >>= check . (>= k)
      forM_ (zip [0 ..] batches) $ \(k, batch) -> do
        -- The batch once the previous write is held, then that write
        -- released: the muxer counts it and takes the batch at its queue.
        atomically $ readTVar taken >>= check . (== k) . length
        enqueue batch
        atomically $ writeTVar released k
      atomically $ readTVar taken >>= check . (== length batches) . length
      when (cancelled == AfterTheCount) $ do
        atomically $ writeTVar released (length batches)
        -- The muxer counts the batch and blocks at its empty queue; time
        -- passes only once every thread is blocked.
        threadDelay 1
      cancel muxer
      (,) <$> readTVarIO taken <*> counted

-- | The bearer takes part of the batch and then waits for room that never
-- comes, as a socket with a full send buffer does. Cancelled then, the
-- muxer is gone in the same instant, and the half-sent batch is not
-- counted.
prop_mux_partial_write_cancelled :: Batches -> Property
prop_mux_partial_write_cancelled (Batches batches) =
  case traceResult False (runSimTrace sim) of
    Left e -> counterexample (show e) False
    Right r@(waited, counted) ->
      counterexample (show r) $
           counterexample "the cancellation waited" (waited === 0)
      .&&. counted === M.empty
  where
    sim :: forall s. IOSim s (DiffTime, Mx.ByteCounts)
    sim = do
      written <- newTVarIO (0 :: Int)
      room    <- newTVarIO False
      WriteMuxer { muxer, enqueue, counted } <- newWriteMuxer $ \sdus -> do
        atomically $ modifyTVar written (+ max 1 (fromIntegral (sum (writtenBytes sdus)) `div` 2))
        atomically $ readTVar room >>= check
      enqueue (concat batches)
      -- Cancelled once the bearer has taken part of the batch and waits.
      atomically $ readTVar written >>= check . (> 0)
      t0 <- getMonotonicTime
      cancel muxer
      t1 <- getMonotonicTime
      (,) (t1 `diffTime` t0) <$> counted

-- | SDUs the peer sends, each on a mini-protocol the mux runs, from the
-- peer's responder to our initiator, small enough to fit the ingress limit
-- together; and where the cancellation lands, in the bearer's tail of the
-- last read or once the demuxer has queued the SDU, which counts it.
data ReadCase = ReadCase [(Word16, Int)] Cancelled
  deriving Show

readNums :: [Word16]
readNums = [2, 3]

instance Arbitrary ReadCase where
    arbitrary = ReadCase <$> (choose (1, 3) >>= \n -> vectorOf n sdu) <*> arbitrary
      where
        sdu :: Gen (Word16, Int)
        sdu = (,) <$> elements readNums <*> choose (1, 1000)
    shrink (ReadCase sdus cancelled) =
         [ ReadCase ss cancelled
         | ss <- shrinkList shrinkSdu sdus
         , not (null ss)
         ]
      ++ [ ReadCase sdus c | c <- shrink cancelled ]
      where
        shrinkSdu :: (Word16, Int) -> [(Word16, Int)]
        shrinkSdu (n, bytes) = [ (n, b) | b <- shrink bytes, b >= 1 ]

-- | The bearer delivers the SDUs one per read, says so, and then blocks:
-- after the last, or, the cancellation to land in its tail, before it
-- hands the last one over. Cancelled in the tail, the SDUs before the last
-- are counted and retired with the mux and the last is not; cancelled once
-- the last is queued, every SDU is: by protocol and our direction, the
-- initiator's.
prop_mux_read_is_counted :: ReadCase -> Property
prop_mux_read_is_counted (ReadCase sdus cancelled) =
  case traceResult False (runSimTrace sim) of
    Left e -> counterexample (show e) False
    Right (delivered, counted) ->
      counterexample ("delivered " ++ show delivered) $
        counted === M.unionsWith (+) (case cancelled of
                                        InTheTail     -> drop 1 delivered
                                        AfterTheCount -> delivered)
  where
    sim :: forall s. IOSim s ([Mx.ByteCounts], Mx.ByteCounts)
    sim = do
      counters  <- Counters.newCounters
      pending   <- newTVarIO [ peerSDU (Mx.MiniProtocolNum n) Mx.ResponderDir (filler bytes)
                             | (n, bytes) <- sdus
                             ]
      -- Each delivery's bytes, the last first.
      delivered <- newTVarIO []
      let -- The next SDU, counted as delivered as it is taken; the last one
          -- held in the tail when the cancellation is to land there, and
          -- the reads after it blocked for good.
          deliver :: IOSim s (Mx.SDU, Time)
          deliver = do
            (sdu, final) <- atomically $ do
              queue <- readTVar pending
              case queue of
                   []         -> retry
                   sdu : rest -> do
                     writeTVar pending rest
                     modifyTVar delivered (M.singleton (ourKey sdu) (sduBytes sdu) :)
                     return (sdu, null rest)
            when (final && cancelled == InTheTail) $ atomically retry
            t <- getMonotonicTime
            return (sdu, t)

          bearer :: Mx.Bearer (IOSim s)
          bearer = modelledBearer (const deliver) (const (error "the demuxer does not write"))

          apps :: [MiniProtocolInfo Mx.InitiatorMode]
          apps = [ MiniProtocolInfo {
                       miniProtocolNum        = Mx.MiniProtocolNum n,
                       miniProtocolDir        = Mx.InitiatorDirectionOnly,
                       miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 4000 },
                       miniProtocolCapability = Nothing }
                 | n <- readNums
                 ]
      mux  <- Mx.new Mx.nullTracers (Just counters) apps
      muxA <- async (Mx.run mux bearer)
      atomically $ readTVar pending >>= check . null
      -- The demuxer queues the last SDU and blocks at its next read; time
      -- passes only once every thread is blocked.
      when (cancelled == AfterTheCount) $ threadDelay 1
      cancel muxA
      counted <- Mx.mcReceived <$> atomically (Counters.readCounters counters)
      (,) <$> readTVarIO delivered <*> pure counted

    -- The key the mux counts it under: our side of the protocol.
    ourKey :: Mx.SDU -> (Mx.MiniProtocolNum, Mx.MiniProtocolDir)
    ourKey sdu = (Mx.mhNum (Mx.msHeader sdu), Mx.InitiatorDir)

    sduBytes :: Mx.SDU -> Word64
    sduBytes sdu = fromIntegral Mx.msHeaderLength + fromIntegral (Mx.mhLength (Mx.msHeader sdu))

-- | Where the peer's SDU lands against the read's deadline, before, on or
-- after it, with the SDU's protocol and size.
data DeadlineCase = DeadlineCase Ordering Word16 Int
  deriving Show

instance Arbitrary DeadlineCase where
    arbitrary = DeadlineCase <$> elements [LT, EQ, GT] <*> elements readNums <*> choose (1, 1000)
    shrink (DeadlineCase landing n bytes) =
      [ DeadlineCase landing n b | b <- shrink bytes, b >= 1 ]

-- | How the mux stands once the read's deadline and the SDU's arrival are
-- both past.
data Fate = Running | TimedOut | Died String
  deriving (Eq, Show)

-- | A read whose SDU lands before, on or after its deadline: before, the SDU
-- is read and counted and the mux reads on, a second SDU half a second later
-- counted too; after, the read times out, the timeout is counted and the
-- mux fails of it; on the deadline one or the other, never anything else.
-- The modelled bearer times its reads with the function the mux gives it,
-- as the socket bearer does, and the SDUs arrive from a thread delayed by
-- the case. Run in plain IOSim, whose one schedule runs the writer, then the
-- timeout's thread, then the demuxer: with the read masked, io-sim's
-- timeout exception leaks past its frame there (IntersectMBO/io-sim#269)
-- and the demuxer dies of it once the first SDU is queued, unreported, the
-- job pool passing no asynchronous exception on, so the mux runs on deaf
-- and the second SDU is never read.
prop_mux_read_on_the_deadline :: DeadlineCase -> Property
prop_mux_read_on_the_deadline (DeadlineCase landing n bytes) =
    tabulate "landing, outcome" [show landing ++ ", " ++ outcome] $
    counterexample (show got) $ case landing of
      LT -> got === (bothRead, 0, Running)
      GT -> got === (M.empty, 1, TimedOut)
      EQ -> counterexample "neither both read nor timed out" $
              got == (bothRead, 0, Running) || got == (M.empty, 1, TimedOut)
  where
    deadline, arrival :: DiffTime
    deadline = 1
    arrival = case landing of
      LT -> deadline / 2
      EQ -> deadline
      GT -> deadline * 2

    num :: Mx.MiniProtocolNum
    num = Mx.MiniProtocolNum n

    -- Both SDUs' bytes under our side of the protocol, the initiator's.
    bothRead :: Mx.ByteCounts
    bothRead = M.singleton (num, Mx.InitiatorDir)
                           (2 * (fromIntegral Mx.msHeaderLength + fromIntegral bytes))

    -- The outcome, for the table.
    outcome :: String
    outcome = case got of
      (_, _, Died e) -> "died of " ++ takeWhile (/= ' ') e
      (r, t, fate)   -> readLabel ++ ", " ++ show t ++ " timeout, " ++ show fate
        where
          readLabel :: String
          readLabel
            | M.null r      = "nothing read"
            | r == bothRead = "both read"
            | otherwise     = "one read"

    -- The bytes read for our initiator, the read timeouts counted, and the
    -- mux's fate.
    got :: (Mx.ByteCounts, Word64, Fate)
    got = runSimOrThrow sim

    sim :: forall s. IOSim s (Mx.ByteCounts, Word64, Fate)
    sim = do
      counters  <- Counters.newCounters
      -- SDUs arrived, and SDUs delivered to the demuxer.
      arrived   <- newTVarIO (0 :: Int)
      delivered <- newTVarIO (0 :: Int)
      _ <- forkIO $ do
        threadDelay arrival
        atomically (writeTVar arrived 1)
        threadDelay (deadline / 2)
        atomically (writeTVar arrived 2)
      let -- The next SDU once it has arrived, within the deadline; after the
          -- second the reads block for good.
          readNext :: TimeoutFn (IOSim s) -> IOSim s (Mx.SDU, Time)
          readNext timeoutFn = do
            done <- readTVarIO delivered
            when (done >= 2) $ atomically retry
            r <- timeoutFn deadline (atomically (readTVar arrived >>= check . (> done)))
            case r of
                 Nothing -> throwIO Mx.SDUReadTimeout
                 Just () -> do
                   atomically (writeTVar delivered (done + 1))
                   (,) (peerSDU num Mx.ResponderDir (filler bytes)) <$> getMonotonicTime

          bearer :: Mx.Bearer (IOSim s)
          bearer = modelledBearer readNext (const (error "the demuxer does not write"))

          apps :: [MiniProtocolInfo Mx.InitiatorMode]
          apps = [ MiniProtocolInfo {
                       miniProtocolNum        = num,
                       miniProtocolDir        = Mx.InitiatorDirectionOnly,
                       miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 4000 },
                       miniProtocolCapability = Nothing }
                 ]
      mux  <- Mx.new Mx.nullTracers (Just counters) apps
      muxA <- async (Mx.run mux bearer)
      -- Past the deadline and both arrivals; a mux still running ends here.
      threadDelay (3 * deadline)
      fate <- poll muxA >>= \ended -> case ended of
        Nothing        -> return Running
        Just (Right _) -> return (Died "stopped")
        Just (Left e)  -> return $ case fromException e of
          Just Mx.SDUReadTimeout -> TimedOut
          _                      -> Died (show e)
      cancel muxA
      counts <- atomically (Counters.readCounters counters)
      return (Mx.mcReceived counts, Mx.mfReadTimeouts (Mx.mcFailures counts), fate)

--
-- Egress bucket
--
-- The arithmetic is the pure 'Bucket.grantAt', checked on its own.  The queue
-- is checked by driving concurrent requests through a real bucket in IOSim and
-- comparing every grant instant, exactly, with a replay through the pure core
-- in (rank, arrival) order: rate, work conservation, priority and FIFO all
-- follow.  Liveness under cancellation is checked separately.

-- | 10 kB/s .. 1 GB/s: schedules from microseconds to seconds.  'byteEps' is
-- sized for the top of this range.
genRate :: Gen Double
genRate = do
    e <- choose (4, 8 :: Int)
    m <- choose (1, 9.99 :: Double)
    return (m * 10 ^^ e)

genCapacity :: Gen Int
genCapacity = (4096 *) <$> choose (1, 8)

-- | Exactly @n@ picoseconds: @1e-12 :: DiffTime@ is the resolution.
picos :: Integer -> DiffTime
picos n = fromIntegral n * 1e-12

-- | A 'Time' as whole picoseconds.  'DiffTime' is fixed point, so going
-- through 'Rational' loses nothing.
picosOf :: Time -> Integer
picosOf t = round (toRational (t `diffTime` Time 0) * 1e12)

-- | Sizes straddle the capacity; an oversized request is the one case that
-- puts the bucket in debt.
genBytes :: Int -> Gen Int
genBytes cap = frequency [ (3, choose (1, cap))
                         , (2, choose (cap, 2 * cap))
                         , (1, choose (1, 64)) ]

--
-- Pure core
--

-- | One take from a bucket in any state, from long full to deep in debt.
data BucketTake = BucketTake {
    tkRate :: !Double,
    tkCap  :: !Int,
    tkNow  :: !Time,
    tkFull :: !Time,
    tkNeed :: !Int
  }
  deriving Show

instance Arbitrary BucketTake where
    arbitrary = do
      rate <- genRate
      cap  <- genCapacity
      now  <- Time . realToFrac <$> choose (0, 100 :: Double)
      let fillTime = fromIntegral cap / rate :: Double
      d    <- choose (-2 * fillTime, 3 * fillTime)
      need <- genBytes cap
      return BucketTake { tkRate = rate, tkCap = cap, tkNow = now,
                          tkFull = realToFrac d `addTime` now, tkNeed = need }

    shrink tk@BucketTake { tkRate, tkCap, tkNow, tkFull, tkNeed } =
         -- the bucket's state is the offset of @tkFull@ from @tkNow@, so move
         -- @tkNow@ to the origin and carry @tkFull@ with it
         [ tk { tkNow = Time 0, tkFull = (tkFull `diffTime` tkNow) `addTime` Time 0 }
         | tkNow /= Time 0 ]
      ++ [ tk { tkFull = tkNow } | tkFull /= tkNow ]           -- exactly full
         -- then halve what is left, so an offset that must stay non-zero
         -- still reaches a small round number
      ++ [ tk { tkFull = (offset / 2) `addTime` tkNow }
         | let offset = tkFull `diffTime` tkNow, abs offset > 1e-12 ]
      ++ [ tk { tkNeed = n }
         | n <- [1, tkCap, tkCap + 1], n < tkNeed ]            -- the credit boundary
      ++ [ tk { tkCap = 4096 } | tkCap > 4096 ]
      ++ [ tk { tkRate = r } | r <- [1e4, 1e6], r < tkRate ]

-- | Slack in bytes, at a given rate, for the picosecond truncation in the
-- pure core: @realToFrac :: Double -> DiffTime@ truncates, so each conversion
-- can lose a whole picosecond's worth of bytes, and a law that recomputes a
-- level stacks two of them.  The constant term covers float error at low
-- rates, where a picosecond is worth almost nothing.
byteEps :: Double -> Double
byteEps rate = 4 * rate * 1e-12 + 1e-9

-- | Over every place in the queue and every tier: a tier's place maps back to
-- it, the places past the tiers have none, every place has its own name, a
-- tier's place is named after it, and the floor serves exactly the credited
-- tiers -- partners, pool relays, strangers -- in a class each; and the
-- names, which metric names carry, are the ones dashboards know.
prop_tiers :: Property
prop_tiers = once $
       counterexample "a tier's place is not its own"
         (conjoin [ Mx.numberTier (Mx.tierNumber t) === Just t | t <- tiers ])
  .&&. counterexample "a place past the tiers has one"
         (conjoin [ Mx.numberTier n === Nothing | n <- places, n > Mx.tierNumber maxBound ])
  .&&. counterexample "two places share a name" (length (nub names) === length names)
  .&&. counterexample "a tier's place is not named after it"
         (conjoin [ Mx.numberName (Mx.tierNumber t) === Mx.tierName t | t <- tiers ])
  .&&. counterexample "the floor serves other than the credited"
         (conjoin [ counterexample (Mx.numberName n) $
                      isJust (floorClass n) === (Mx.numberTier n `elem` map Just credited)
                  | n <- places ])
  .&&. counterexample "credited tiers share a floor class"
         (length (nub [ floorClass (Mx.tierNumber t) | t <- credited ]) === length credited)
    -- the names are in metric names, so dashboards depend on them
  .&&. counterexample "a metric name changed"
         (map Mx.numberName [0 .. 4] ++ [Mx.numberName maxBound]
            === ["localRoot", "partner", "pool", "stranger", "rest", "unranked"])
  where
    tiers    = [minBound .. maxBound] :: [Mx.Tier]
    places   = [minBound .. maxBound] :: [Word8]
    names    = map Mx.numberName places
    credited = [Mx.PartnerTier, Mx.PoolTier, Mx.StrangerTier]

-- | The cases that matter for the pure laws: an oversized request is served
-- on credit, and 'Bucket.tokenLevel' saturates once the bucket is full, so a
-- law stated over the level alone says little about those grants.
labelTake :: BucketTake -> Property -> Property
labelTake BucketTake { tkRate, tkCap, tkNow, tkFull, tkNeed } =
      classify (tkNeed >= tkCap) "oversized"
    . classify (ready == tkNow)  "granted at once"
    . classify (level < 0)       "in debt"
  where
    (ready, _) = Bucket.grantAt tkRate tkCap tkFull tkNow tkNeed
    level      = Bucket.tokenLevel tkRate tkCap tkFull tkNow

-- | Grants at the earliest instant the bytes are there: never before the
-- request, and if it waits, exactly when the level reaches the target.
prop_grantAt_ready :: BucketTake -> Property
prop_grantAt_ready tk@BucketTake { tkRate, tkCap, tkNow, tkFull, tkNeed } =
    labelTake tk $
    counterexample (show (ready, level, target)) $
         ready >= tkNow
      && level >= target - eps
      && (ready == tkNow || level <= target + eps)
  where
    (ready, _) = Bucket.grantAt tkRate tkCap tkFull tkNow tkNeed
    level      = Bucket.tokenLevel tkRate tkCap tkFull ready
    target     = fromIntegral (min tkNeed tkCap) :: Double
    eps        = byteEps tkRate

-- | A take that must wait, held as the generator's own inputs so that
-- shrinking any of them still waits: the bucket is full again @bwExtra@
-- picoseconds past the instant its level would reach the target, so the target
-- is out of reach at @bwNow@ and 'Bucket.grantAt' cannot serve it there.
data BucketWait = BucketWait {
    bwRate  :: !Double,
    bwCap   :: !Int,
    bwNeed  :: !Int,
    bwNow   :: !Time,
    bwExtra :: !Integer   -- ^ picoseconds past the deficit, at least one
  }
  deriving Show

waitTake :: BucketWait -> BucketTake
waitTake BucketWait { bwRate, bwCap, bwNeed, bwNow, bwExtra } =
    BucketTake { tkRate = bwRate, tkCap = bwCap, tkNeed = bwNeed,
                 tkNow  = bwNow,
                 tkFull = (deficit spare + picos bwExtra) `addTime` bwNow }
  where
    -- the bucket reaches the target this far below its capacity
    spare = bwCap - min bwNeed bwCap

    -- the same expression as the accrual inside 'Bucket.grantAt', so the
    -- offset is exact and the wait is a whole number of picoseconds
    deficit :: Int -> DiffTime
    deficit n = realToFrac (fromIntegral n / bwRate)

instance Arbitrary BucketWait where
    arbitrary = do
      rate  <- genRate
      cap   <- genCapacity
      need  <- genBytes cap
      now   <- Time . realToFrac <$> choose (0, 100 :: Double)
      let perByte = max 1 (ceiling (1e12 / rate))                     :: Integer
          perFill = max 1 (ceiling (1e12 * fromIntegral cap / rate))  :: Integer
      extra <- frequency [ (1, return 1)                  -- tightest wait
                         , (3, choose (1, perByte))       -- under a byte
                         , (3, choose (1, perFill))       -- part of a fill
                         , (1, choose (1, 3 * perFill)) ] -- in debt
      return BucketWait { bwRate = rate, bwCap = cap, bwNeed = need,
                          bwNow = now, bwExtra = extra }

    shrink bw@BucketWait { bwRate, bwCap, bwNeed, bwNow, bwExtra } =
         [ bw { bwNow   = Time 0 } | bwNow /= Time 0 ]
      ++ [ bw { bwExtra = e } | e <- shrinkIntegral bwExtra, e >= 1 ]
      ++ [ bw { bwNeed  = n } | n <- [1, bwCap, bwCap + 1], n < bwNeed ]
      ++ [ bw { bwCap   = 4096 } | bwCap > 4096 ]
      ++ [ bw { bwRate  = r } | r <- [1e4, 1e6], r < bwRate ]

-- | A grant that waited is not late: one byte's accrual before it, the bucket
-- was still short of the target.  'prop_grantAt_ready' cannot check this,
-- because 'Bucket.tokenLevel' clamps at the capacity and a request of at least
-- the capacity targets all of it: there the level reads as the target at
-- @ready@ and at every instant after, so an arbitrarily late grant passes.
-- Before @ready@ the level is still rising.
--
-- @probe@ negates a converted positive, as the implementation's own accrual
-- does: 'realToFrac' floors to the picosecond, so converting the negative
-- overshoots by one.  For the tightest waits @probe@ falls before @tkNow@,
-- which is sound: 'Bucket.tokenLevel' is linear there and @probe@ stays below
-- @tkFull@, so it is clear of the clamp.
prop_grantAt_tight :: BucketWait -> Property
prop_grantAt_tight bw =
    labelTake tk $
    counterexample (show (ready, probe, level, target)) $
         ready > tkNow          -- the generator guarantees a wait, so a grant
                                -- that never waits is caught here
      && level <= target - 1 + byteEps tkRate
  where
    tk@BucketTake { tkRate, tkCap, tkNow, tkFull, tkNeed } = waitTake bw
    (ready, _) = Bucket.grantAt tkRate tkCap tkFull tkNow tkNeed
    probe      = negate (realToFrac (1 / tkRate)) `addTime` ready
    level      = Bucket.tokenLevel tkRate tkCap tkFull probe
    target     = fromIntegral (min tkNeed tkCap) :: Double

-- | Taking lowers the level, as of the grant instant, by exactly the bytes
-- granted.
prop_grantAt_take :: BucketTake -> Property
prop_grantAt_take tk@BucketTake { tkRate, tkCap, tkNow, tkFull, tkNeed } =
    labelTake tk $
    classify (tkFull <= ready) "full at the grant" $
    counterexample (show (levelBefore, levelAfter)) $
      abs (levelAfter - (levelBefore - fromIntegral tkNeed)) <= byteEps tkRate
  where
    (ready, full') = Bucket.grantAt tkRate tkCap tkFull tkNow tkNeed
    levelBefore = Bucket.tokenLevel tkRate tkCap tkFull ready
    levelAfter  = Bucket.tokenLevel tkRate tkCap full' ready

-- | A rate of zero grants everything at once.
prop_grantAt_disabled :: BucketTake -> Property
prop_grantAt_disabled BucketTake { tkCap, tkNow, tkFull, tkNeed } =
    fst (Bucket.grantAt 0 tkCap tkFull tkNow tkNeed) === tkNow

-- | A short bearer wakes no earlier than its bytes are there and less than a
-- microsecond later.
prop_wakeAt :: BucketTake -> Property
prop_wakeAt BucketTake { tkRate, tkCap, tkNow, tkFull, tkNeed } =
    counterexample (show (ready, wake)) $
      ready == tkNow || (wake >= ready && wake `diffTime` ready < 1e-6)
  where
    (ready, _) = Bucket.grantAt tkRate tkCap tkFull tkNow tkNeed
    wake       = Bucket.wakeAt tkNow ready

-- | A rate change keeps the token level.
prop_atRate :: BucketTake -> Property
prop_atRate BucketTake { tkRate, tkCap, tkNow, tkFull } =
    forAll genRate $ \rate' ->
      let full'  = Bucket.atRate tkRate rate' tkCap tkNow tkFull
          levelBefore = Bucket.tokenLevel tkRate tkCap tkFull tkNow
          levelAfter  = Bucket.tokenLevel rate'  tkCap full'  tkNow
      in counterexample (show (levelBefore, levelAfter)) $
           abs (levelAfter - levelBefore) <= byteEps (max tkRate rate')

-- | Taking on credit lowers the level at @now@ by exactly the bytes, whatever
-- the state of the bucket -- there is no wait to move the instant.
prop_chargeAt :: BucketTake -> Property
prop_chargeAt tk@BucketTake { tkRate, tkCap, tkNow, tkFull, tkNeed } =
    labelTake tk $
    counterexample (show (levelBefore, levelAfter)) $
      abs (levelAfter - (levelBefore - fromIntegral tkNeed)) <= byteEps tkRate
  where
    full'       = Bucket.chargeAt tkRate tkFull tkNow tkNeed
    levelBefore = Bucket.tokenLevel tkRate tkCap tkFull tkNow
    levelAfter  = Bucket.tokenLevel tkRate tkCap full' tkNow

-- | A rotation, a period number, and two offsets into that period.
data RotationCase = RotationCase {
    rcRotation :: !Bucket.Rotation,
    rcPeriod   :: !Word64,
    rcOffsets  :: !(Integer, Integer)   -- ^ picoseconds into the period
  }
  deriving Show

instance Arbitrary RotationCase where
    arbitrary = do
      seed   <- arbitrary
      -- a millisecond to a day
      period <- picos <$> choose (1000000000, 86400000000000000)
      p      <- choose (0, 10000)
      let inside = choose (0, picosOf (period `addTime` Time 0) - 1)
      offs   <- (,) <$> inside <*> inside
      return RotationCase { rcRotation = Bucket.Rotation seed period maxBound,
                            rcPeriod = p, rcOffsets = offs }

    shrink rc@RotationCase { rcRotation = Bucket.Rotation seed period _, rcPeriod, rcOffsets } =
         [ rc { rcRotation = Bucket.Rotation 0 period maxBound } | seed /= 0 ]
      ++ [ rc { rcPeriod = 0 } | rcPeriod /= 0 ]
      ++ [ rc { rcOffsets = (0, snd rcOffsets) } | fst rcOffsets /= 0 ]
      ++ [ rc { rcOffsets = (fst rcOffsets, 0) } | snd rcOffsets /= 0 ]

-- | Within a period a bearer keeps its place, and it fits in 24 bits; across
-- a boundary the places are re-dealt: of a hundred bearers, at most a few keep
-- theirs (each does with probability 2^-24).
prop_rotatedRank :: RotationCase -> Property
prop_rotatedRank RotationCase { rcRotation = ro, rcPeriod, rcOffsets = (o1, o2) } =
    counterexample (show (kept, map (rank t1) bearers)) $
         all (\b -> rank t1 b == rank t2 b) bearers
      && all (\b -> rank t1 b < 2 ^ (24 :: Int)) bearers
      && length kept <= 3
  where
    bearers = [0 .. 99]
    at p o  = picos (p * picosOf (Bucket.roPeriod ro `addTime` Time 0) + o) `addTime` Time 0
    t1      = at (fromIntegral rcPeriod) o1
    t2      = at (fromIntegral rcPeriod) o2
    t3      = at (fromIntegral rcPeriod + 1) o1
    rank t b = Bucket.rotatedRank ro b t
    kept    = [ b | b <- bearers, rank t1 b == rank t3 b ]

-- | A rotation with a period of zero is no rotation: a bearer queues exactly
-- where it would without one.
-- | From the threshold up a tier queues as without a rotation, so its order
-- is the tickets' and backlogged bearers take turns, and so do the local
-- roots, tier 0, whatever the threshold; below it the dealt place still
-- decides. The tier byte is untouched either way.
prop_queueRank_turns :: Word64 -> Word64 -> TierArg -> Word8 -> Positive Integer
                     -> NonNegative Integer -> Property
prop_queueRank_turns seed bearer (TierArg tier) from (Positive period) (NonNegative ps) =
    classify (tier >= from) "takes turns" $
    classify (tier == 0 && from > 0) "a local root below the threshold" $
    if tier >= from || tier == 0
       then with === Bucket.queueRank Nothing bearer (Bucket.Rank tier) t
       else with === (fromIntegral tier `shiftL` 24 .|. Bucket.rotatedRank ro bearer t)
  where
    ro   = Bucket.Rotation seed (picos period) from
    with = Bucket.queueRank (Just ro) bearer (Bucket.Rank tier) t
    t    = Time (picos ps)

-- | A tier for the rank laws: the local roots often, the small tiers often,
-- and any byte.
newtype TierArg = TierArg Word8
  deriving Show

instance Arbitrary TierArg where
  arbitrary = TierArg <$> frequency [ (1, pure 0), (1, choose (1, 5)), (1, arbitrary) ]
  shrink (TierArg t) = [ TierArg t' | t' <- shrink t ]

prop_queueRank_zeroPeriod :: Word64 -> Word64 -> Word8 -> NonNegative Integer
                          -> Property
prop_queueRank_zeroPeriod seed bearer tier (NonNegative ps) =
    Bucket.queueRank (Just (Bucket.Rotation seed 0 maxBound)) bearer (Bucket.Rank tier) t
      === Bucket.queueRank Nothing bearer (Bucket.Rank tier) t
  where
    t = picos ps `addTime` Time 0

-- | Grants at tiers and bearers, recorded in time order, then counted at
-- @now@: a tier counts each bearer it granted within the window once,
-- however often, and none granted only before it.
prop_served_window :: ServedGrants -> Property
prop_served_window (ServedGrants window now grants) =
    classify (any (\(_, _, t) -> now `diffTime` t == window) grants) "a grant on the boundary" $
    classify (any (\(_, _, t) -> now `diffTime` t > window) grants)  "a grant aged out" $
    classify (M.size oracle > 1)                                     "several tiers" $
    classify (any (> 1) (M.elems oracle))                            "several bearers in a tier" $
      Bucket.servedByTier (Bucket.servedWithin window now recorded) === oracle
  where
    recorded = Bucket.bsServed $ List.foldl'
                 (\st (tier, b, t) -> Bucket.recordServed tier b t 0 0 st)
                 Bucket.emptyBucketStats grants
    oracle = servedOracle window now grants

-- | The served record with nothing else running: replayed exactly against the
-- model after every grant, never holding a grant two windows old, and
-- counting at each grant's instant what the oracle counts over the whole
-- history.
prop_served_bounded :: ServedHistory -> Property
prop_served_bounded (ServedHistory grants) =
    classify (snd (servedModel (map plain grants)) > Time 0)     "pruned" $
    classify (any (== w) gaps)                                   "a gap of exactly the window" $
    classify (any (\(st, (_, _, t)) -> any (\s -> t `diffTime` s > w) (lasts st))
                  (zip states grants))                           "a grant kept past one window" $
      conjoin
        [ counterexample ("after grant " ++ show i) $
               (Bucket.bsServed st, Bucket.bsServedPruned st) === servedModel (map plain prefix)
          .&&. counterexample "older than two windows"
                 (all (\s -> t `diffTime` s <= 2 * w) (lasts st))
          .&&. Bucket.servedByTier (Bucket.servedWithin w t (Bucket.bsServed st))
                 === servedOracle w t prefix
        | (i, st, prefix, (_, _, t)) <- zip4 [1 :: Int ..] states (drop 1 (List.inits grants)) grants ]
  where
    w      = Bucket.servedWindow
    states = drop 1 (scanl (\st (tier, b, t) -> Bucket.recordServed tier b t 0 0 st)
                           Bucket.emptyBucketStats grants)
    lasts  = map Bucket.svLast . M.elems . Bucket.bsServed
    gaps   = zipWith (\(_, _, a) (_, _, b) -> b `diffTime` a) grants (drop 1 grants)
    zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
    zip4 _ _ _ _                             = []

-- | The served record and when it was last pruned, after grants in time
-- order, each with its token wait and the bearer's own time: once a window
-- after the last prune, a grant drops every bearer not granted within the
-- window.
servedModel :: [(Word8, Word64, Time, DiffTime, DiffTime)]
            -> (M.Map (Word8, Word64) Bucket.Served, Time)
servedModel = List.foldl' step (M.empty, Time 0)
  where
    w = Bucket.servedWindow
    step (m, pruned) (tier, b, t, waited, own)
      | t `diffTime` pruned >= w
      = (record (M.filter (\sv -> t `diffTime` Bucket.svLast sv <= w) m), t)
      | otherwise
      = (record m, pruned)
      where
        record = M.alter (Just . Bucket.servedStep t waited own) (tier, b)

-- | A grant that neither waited nor reported any own time.
plain :: (Word8, Word64, Time) -> (Word8, Word64, Time, DiffTime, DiffTime)
plain (tier, b, t) = (tier, b, t, 0, 0)

-- | Grants in time order, gaps in whole seconds with exactly the window
-- weighted up, so prunes land on the boundary and past it.
newtype ServedHistory = ServedHistory [(Word8, Word64, Time)]
  deriving Show

instance Arbitrary ServedHistory where
    arbitrary = do
      let w = round (realToFrac Bucket.servedWindow :: Double) :: Integer
      gaps <- listOf1 (frequency [ (3, choose (0, 5)), (2, choose (6, w - 1))
                                 , (2, pure w), (1, choose (w + 1, 3 * w)) ])
      let times = map (Time . fromIntegral) (scanl1 (+) gaps)
      ServedHistory <$> mapM (\t -> (,,) <$> elements [0, 1, 2, 3, 4, 255]
                                         <*> (fromIntegral <$> choose (0, 5 :: Int))
                                         <*> pure t) times
    shrink (ServedHistory grants) =
      [ ServedHistory gs | gs <- shrinkList (const []) grants, not (null gs) ]

-- | Per tier, the bearers with a grant within @window@ of @now@.
servedOracle :: DiffTime -> Time -> [(Word8, Word64, Time)] -> M.Map Word8 Int
servedOracle window now grants =
    M.fromListWith (+)
      [ (tier, 1)
      | (tier, b) <- Set.toList (Set.fromList [ (tier, b) | (tier, b, _) <- grants ])
      , any (\(tier', b', t) -> tier' == tier && b' == b && now `diffTime` t <= window)
            grants ]

-- | The same grants through a real bucket, every one on the fast path, and
-- the counters' snapshot at @now@: its served count per tier is the oracle's
-- over 'Bucket.servedWindow'. The times are stretched so that the window
-- splits them.
prop_served_counters :: ServedGrants -> Property
prop_served_counters (ServedGrants _ now0 grants0) =
    classify (any (\(_, _, t) -> now `diffTime` t == Bucket.servedWindow) grants) "a grant on the boundary" $
    classify (any (\(_, _, t) -> now `diffTime` t > Bucket.servedWindow) grants)  "a grant aged out" $
      counted === M.toList (servedOracle Bucket.servedWindow now grants)
  where
    stretch (Time d) = Time (4 * d)
    now    = stretch now0
    grants = [ (tier, b, stretch t) | (tier, b, t) <- grants0 ]
    counted = runSimOrThrow $ do
      bucket <- Bucket.newBucket 1e12 (1024 * 1024) Nothing
      hs <- mapM (const (Bucket.registerBearer bucket)) [0 .. 5 :: Int]
      forM_ grants $ \(tier, b, t) -> do
        t0 <- getMonotonicTime
        threadDelay (t `diffTime` t0)
        let h = hs !! fromIntegral b
        atomically $ Bucket.setRank h (Bucket.Rank tier)
        Bucket.awaitGrant h 1
      t0 <- getMonotonicTime
      threadDelay (now `diffTime` t0)
      sc <- atomically $ Counters.schedulingCounts now (bucket, Nothing)
      return [ (tier, Counters.tcServed c) | (tier, c) <- Counters.scTiers sc
                                           , Counters.tcServed c > 0 ]

-- | Grants that never queue record the tier their bearer's rule gives at
-- the grant, asked every time. A rule over a variable per bearer, changed
-- and reinstalled between requests, is replayed exactly: the record, and
-- the rules asked once per request.
prop_served_fast_tier :: FastTiers -> Property
prop_served_fast_tier (FastTiers n events) =
    classify (any changes events)    "a rule's answer changed" $
    classify (any reinstalls events) "a rule installed again" $
      observed === (served, asks)
  where
    (served, asks) = fastTierOracle events
    reinstalls (_, FastReinstall _) = True
    reinstalls _                    = False
    changes (_, FastSetTier _ _) = True
    changes _                    = False
    observed = runSimOrThrow $ do
      bucket <- Bucket.newBucket 1e12 (1024 * 1024) Nothing
      asked  <- newTVarIO (0 :: Int)
      let rule v = \_ -> modifyTVar asked (+ 1) >> readTVar v
      hvs <- forM [1 .. n] $ \_ -> do
        h <- Bucket.registerBearer bucket
        v <- newTVarIO (Bucket.Rank 0)
        atomically $ Bucket.setRankSource h (rule v)
        return (h, v)
      forM_ events $ \(t, e) -> do
        t0 <- getMonotonicTime
        threadDelay (t `diffTime` t0)
        case e of
             FastRequest b    -> void $ Bucket.awaitGrant (fst (hvs !! b)) 1
             FastSetTier b r  -> atomically $ writeTVar (snd (hvs !! b)) (Bucket.Rank r)
             FastReinstall b  -> let (h, v) = hvs !! b
                                 in atomically $ Bucket.setRankSource h (rule v)
      now <- getMonotonicTime
      (st, _, _, _) <- atomically $ Bucket.bucketSnapshot bucket now
      (,) (Bucket.bsServed st) <$> readTVarIO asked

-- | The record and the number of asks: every request asks, and records the
-- tier the rule gives then.
fastTierOracle :: [(Time, FastEvent)] -> (M.Map (Word8, Word64) Bucket.Served, Int)
fastTierOracle = go M.empty [] 0
  where
    go _ served asks [] = (fst (servedModel (map plain (reverse served))), asks)
    go tiers served asks ((t, e) : rest) = case e of
      FastSetTier b r -> go (M.insert b r tiers) served asks rest
      FastReinstall _ -> go tiers served asks rest
      FastRequest b   -> go tiers ((M.findWithDefault 0 b tiers, fromIntegral b, t) : served)
                            (asks + 1) rest

data FastEvent = FastRequest Int        -- ^ the bearer asks for one byte
               | FastSetTier Int Word8  -- ^ the bearer's rule gives this tier from now
               | FastReinstall Int      -- ^ the bearer's rule is installed again
  deriving (Eq, Show)

-- | Bearers, and their events in time order on a one-second grid, with gaps
-- of exactly a refresh weighted up.
data FastTiers = FastTiers Int [(Time, FastEvent)]
  deriving Show

instance Arbitrary FastTiers where
    arbitrary = do
      n   <- choose (1, 3)
      len <- choose (1, 30)
      let refresh = round (realToFrac Bucket.tierRefresh :: Double) :: Integer
          steps :: Int -> Time -> Gen [(Time, FastEvent)]
          steps 0 _ = return []
          steps k t = do
            gap <- frequency [ (3, return 0), (4, choose (1, refresh - 1))
                             , (3, return refresh), (1, choose (refresh + 1, 2 * refresh)) ]
            e   <- frequency [ (3, FastRequest <$> choose (0, n - 1))
                             , (1, FastSetTier <$> choose (0, n - 1) <*> choose (0, 3))
                             , (1, FastReinstall <$> choose (0, n - 1)) ]
            let t' = fromIntegral gap `addTime` t
            ((t', e) :) <$> steps (k - 1) t'
      FastTiers n <$> steps len (Time 0)

    shrink (FastTiers n events) =
         [ FastTiers n es | es <- shrinkList (const []) events, not (null es) ]
      ++ [ FastTiers n (take i events ++ [(t, FastSetTier b 0)] ++ drop (i + 1) events)
         | (i, (t, FastSetTier b r)) <- zip [0 ..] events, r /= 0 ]

-- | A bearer's standing after grants in time order, each with its token wait
-- and the bearer's own time. The sums decay, so the verdicts of
-- 'Bucket.slowedBy' are those of a ratio over the recent window: scaling
-- every time by a power of two leaves them unchanged, exactly; a bearer that
-- only waited is slowed by every bound and one that never waited by none; an
-- idle window carries @e@ less of the standing into the next grant; and
-- after a long idle a grant stands alone.
prop_slowdown_laws :: SlowHistory -> Property
prop_slowdown_laws (SlowHistory k grants) =
    classify (any (\(_, w, _) -> w == 0) grants) "a grant that did not wait" $
    classify (any (\(_, _, o) -> o == 0) grants) "a grant with no own time" $
    classify (or verdicts)                        "slowed by some bound" $
    classify (and verdicts)                       "slowed by every bound" $
    conjoin
      [ counterexample "a sum is negative"
          (Bucket.svWaited standing >= 0 && Bucket.svOwn standing >= 0)
      , counterexample "slowed by a bound but not by a smaller one"
          (and (zipWith (>=) verdicts (drop 1 verdicts)))
      , counterexample "scaling every time changed a verdict"
          (map (`Bucket.slowedBy` standingOf scaled) Bucket.slowdownBounds === verdicts)
      , counterexample "waited only, yet not slowed by every bound"
          (not (all (\(_, _, o) -> o == 0) grants && any (\(_, w, _) -> w > 0) grants) || and verdicts)
      , counterexample "never waited, yet slowed"
          (any (\(_, w, _) -> w > 0) grants || not (or verdicts))
      , counterexample "an idle window does not carry e less" $
          let rested = Bucket.servedStep (Bucket.servedWindow `addTime` lastT) 0 0 (Just standing)
          in (Bucket.svWaited rested, Bucket.svOwn rested)
               === (Bucket.svWaited standing * exp (-1), Bucket.svOwn standing * exp (-1))
      , counterexample "after a long idle a grant does not stand alone" $
          let alone = Bucket.servedStep ((100 * Bucket.servedWindow) `addTime` lastT) w0 o0 (Just standing)
          in close (Bucket.svWaited alone) (realToFrac w0) && close (Bucket.svOwn alone) (realToFrac o0)
      ]
  where
    standing     = standingOf grants
    verdicts     = map (`Bucket.slowedBy` standing) Bucket.slowdownBounds
    scaled       = [ (t, w * 2 ^ k, o * 2 ^ k) | (t, w, o) <- grants ]
    lastT        = (\(t, _, _) -> t) (last grants)
    (_, w0, o0)  = head grants
    close a b    = abs (a - b) <= 1e-12 * max 1 b
    standingOf   = fromJust . List.foldl' (\sv_m (t, w, o) -> Just (Bucket.servedStep t w o sv_m)) Nothing

-- | One bearer's grants in time order, gaps of up to two windows, and a
-- scale: whole microseconds, waits and own times of the same size so the
-- verdicts fall on both sides of every bound and, at times, exactly on one.
data SlowHistory = SlowHistory Int [(Time, DiffTime, DiffTime)]
  deriving Show

instance Arbitrary SlowHistory where
    arbitrary = do
      k    <- choose (0, 20)
      n    <- frequency [ (1, choose (1, 3)), (2, choose (1, 30)) ]
      gaps <- vectorOf n (frequency [ (2, pure 0), (3, choose (1, 10000000))
                                    , (1, choose (1, 120000000 :: Int)) ])
      let times = map (\us -> Time (fromIntegral us / 1000000)) (scanl1 (+) gaps)
      grants <- forM times $ \t -> do
        own    <- genOwn
        waited <- frequency [ (1, pure 0), (3, genOwn)
                            , (2, (\f -> (realToFrac f - 1) * own) <$> elements Bucket.slowdownBounds) ]
        return (t, waited, own)
      return (SlowHistory k grants)
      where
        genOwn = frequency [ (2, pure 0)
                           , (3, (\us -> fromIntegral (us :: Int) / 1000000) <$> choose (1, 10000000)) ]
    shrink (SlowHistory k grants) =
         [ SlowHistory k gs | gs <- shrinkList (const []) grants, not (null gs) ]
      ++ [ SlowHistory 0 grants | k /= 0 ]

-- | Per tier, 'Bucket.slowedByTier' counts the bearers whose standing is
-- slowed by at least each bound; the count is stated literally.
prop_slowed_by_tier :: Standings -> Property
prop_slowed_by_tier (Standings m) =
    classify (M.size m > 1)         "several bearers" $
    classify (any onBound (M.elems m)) "a standing exactly on a bound" $
      Bucket.slowedByTier m === M.fromListWith (zipWith (+))
        [ (tier, [ if Bucket.svWaited sv > 0 && Bucket.svWaited sv >= (f - 1) * Bucket.svOwn sv
                      then 1 else 0
                 | f <- Bucket.slowdownBounds ])
        | ((tier, _), sv) <- M.toList m ]
  where
    onBound sv = any (\f -> Bucket.svWaited sv == (f - 1) * Bucket.svOwn sv) Bucket.slowdownBounds

newtype Standings = Standings (M.Map (Word8, Word64) Bucket.Served)
  deriving Show

instance Arbitrary Standings where
    arbitrary = do
      n <- choose (0, 12)
      Standings . M.fromList <$> vectorOf n ((,) <$> key <*> standing)
      where
        key = (,) <$> elements [0, 1, 2, 3, 4, 255] <*> (fromIntegral <$> choose (0, 5 :: Int))
        standing = do
          own    <- frequency [ (1, pure 0), (3, choose (0, 20)) ]
          waited <- frequency [ (1, pure 0), (3, choose (0, 20))
                              , (2, (\f -> (f - 1) * own) <$> elements Bucket.slowdownBounds) ]
          return (Bucket.Served (Time 0) waited own)
    shrink (Standings m) = [ Standings (M.fromList kvs) | kvs <- shrinkList (const []) (M.toList m) ]

-- | Bearers at fixed tiers, each reporting a gate wait and a write with
-- every request, served by a bucket slow enough to queue them; the counters'
-- snapshot counts per tier the bearers served within the window and, of
-- those, the ones slowed by each bound, as the model says from the waits the
-- harness saw.
prop_slowed_counters :: SlowRun -> Property
prop_slowed_counters SlowRun { srRate, srCapacity, srBearers, srAfter } =
    classify (any (\(_, _, asked, granted, _, _) -> granted > asked) grants) "a grant that waited" $
    classify (any (\(_, _, _, granted, _, _) -> now `diffTime` granted > Bucket.servedWindow) grants)
             "a grant aged out" $
    classify (any (\(_, _, slowed) -> any ((> 0) . snd) slowed) counted) "a bearer slowed" $
      counted === oracle
  where
    (now, grants, counted) = runSimOrThrow $ do
      bucket <- Bucket.newBucket srRate srCapacity Nothing
      -- registered in order, so bearer @i@ has id @i@
      hs <- mapM (const (Bucket.registerBearer bucket)) srBearers
      gs <- forConcurrently (zip hs srBearers) $ \(h, SlowBearer { sbTier, sbStart, sbTakes }) -> do
        atomically $ Bucket.setRank h (Bucket.Rank sbTier)
        threadDelay sbStart
        forM sbTakes $ \(gap, bytes, gate, wrote) -> do
          threadDelay gap
          asked <- getMonotonicTime
          void $ Bucket.awaitGrantWaited Nothing gate wrote h bytes
          granted <- getMonotonicTime
          return (asked, granted, gate, wrote)
      threadDelay srAfter
      end <- getMonotonicTime
      sc  <- atomically $ Counters.schedulingCounts end (bucket, Nothing)
      return ( end
             , [ (sbTier b, i, asked, granted, gate, wrote)
               | (i, b, g) <- zip3 [0 ..] srBearers gs, (asked, granted, gate, wrote) <- g ]
             , [ (t, Counters.tcServed c, Counters.tcSlowed c)
               | (t, c) <- Counters.scTiers sc, Counters.tcServed c > 0 ] )
    standings = fst (servedModel (List.sortOn (\(_, _, t, _, _) -> t)
                       [ (tier, i, granted, granted `diffTime` asked, gate + wrote)
                       | (tier, i, asked, granted, gate, wrote) <- grants ]))
    recent    = M.filter (\sv -> now `diffTime` Bucket.svLast sv <= Bucket.servedWindow) standings
    oracle    = [ (tier, length svs, [ (f, length (filter (Bucket.slowedBy f) svs))
                                     | f <- Bucket.slowdownBounds ])
                | tier <- nub [ t | (t, _) <- M.keys recent ]
                , let svs = [ sv | ((t, _), sv) <- M.toList recent, t == tier ] ]

data SlowBearer = SlowBearer {
    sbTier  :: !Word8,
    sbStart :: !DiffTime,
    sbTakes :: ![(DiffTime, Int, DiffTime, DiffTime)]
      -- ^ after the previous grant: the gap before asking, the bytes, the
      -- gate wait and the write reported with the request
  }
  deriving Show

data SlowRun = SlowRun {
    srRate     :: !Double,
    srCapacity :: !Int,
    srBearers  :: ![SlowBearer],
    srAfter    :: !DiffTime        -- ^ from the last grant to the snapshot
  }
  deriving Show

instance Arbitrary SlowRun where
    arbitrary = do
      rate <- genRate
      cap  <- genCapacity
      n    <- choose (1, 4)
      -- asking again at once is weighted up: that is what makes bearers
      -- queue; the gate waits and writes are short next to a queued wait
      bearers <- vectorOf n $ do
        tier  <- elements [0, 1, 2, 3, 4, 255]
        start <- millis 1000
        takes <- choose (1, 10 :: Int) >>= \m -> vectorOf m $
          (,,,) <$> frequency [ (3, pure 0), (2, millis 500) ] <*> genBytes cap
                <*> frequency [ (1, pure 0), (2, millis 200) ]
                <*> frequency [ (1, pure 0), (2, millis 20) ]
        return (SlowBearer tier start takes)
      rest <- elements [0, w / 2, w, 2 * w]
      return (SlowRun rate cap bearers rest)
      where
        w = Bucket.servedWindow
        millis m = (\k -> fromIntegral (k :: Int) / 1000) <$> choose (0, m)
    shrink run@SlowRun { srBearers, srAfter } =
         [ run { srBearers = bs } | bs <- shrinkList shrinkBearer srBearers, not (null bs) ]
      ++ [ run { srAfter = 0 } | srAfter /= 0 ]
      where
        shrinkBearer b =
             [ b { sbTakes = ts } | ts <- shrinkList shrinkTake (sbTakes b), not (null ts) ]
          ++ [ b { sbTier = 0 } | sbTier b /= 0 ]
          ++ [ b { sbStart = 0 } | sbStart b /= 0 ]
        shrinkTake (gap, bytes, gate, wrote) =
             [ (0, bytes, gate, wrote) | gap /= 0 ]
          ++ [ (gap, bytes, 0, wrote)  | gate /= 0 ]
          ++ [ (gap, bytes, gate, 0)   | wrote /= 0 ]
          ++ [ (gap, 1024, gate, wrote) | bytes > 1024 ]

-- | A window, the instant of the count, and grants in time order before it,
-- on a coarse grid so that some land exactly on the window's edge.
data ServedGrants = ServedGrants DiffTime Time [(Word8, Word64, Time)]
  deriving Show

instance Arbitrary ServedGrants where
    arbitrary = do
      window <- (fromIntegral :: Int -> DiffTime) <$> choose (0, 30)
      steps  <- listOf (choose (0, 3 :: Int))
      let times = map (\s -> Time (fromIntegral s)) (scanl1 (+) steps)
      grants <- mapM (\t -> (,,) <$> elements [0, 1, 2, 3, 4, 255]
                                  <*> (fromIntegral <$> choose (0, 5 :: Int))
                                  <*> pure t) times
      extra  <- choose (0, 10 :: Int)
      let lastT = maybe (Time 0) (\(_, _, t) -> t) (lastMaybe grants)
      return (ServedGrants window (fromIntegral extra `addTime` lastT) grants)
      where
        lastMaybe xs = if null xs then Nothing else Just (last xs)
    shrink (ServedGrants window now grants) =
      [ ServedGrants window now gs | gs <- shrinkList (const []) grants ]
      ++ [ ServedGrants w now grants | w <- [0, window / 2], w < window ]

--
-- Queue
--

-- | One bearer: its rank, when it first asks, and the takes it makes -- all
-- on one handle, as the muxer does, each one asking again after its previous
-- grant.  Times are counted in rounds, a round being one picosecond per bearer
-- in the run; see 'askAt'.
data BucketBearer = BucketBearer {
    bbRank  :: !Word8,
    bbStart :: !Integer,           -- ^ rounds before the first request
    bbFirst :: !Int,               -- ^ bytes of the first take
    bbMore  :: ![(Integer, Int, Word8)],
                                   -- ^ rounds after a grant, then bytes and rank
    bbKill  :: !(Maybe Int),       -- ^ cancel it this far, in 256ths, into
                                   --   the first wait the replay gives it
    bbSlice :: !Bool               -- ^ asks the slice, borrowing the budget;
                                   --   only when the schedule has a slice
  }
  deriving (Eq, Show)

-- | A bucket and the bearers taking from it.
data BucketSched = BucketSched {
    schRate     :: !Double,        -- ^ bytes/s
    schCapacity :: !Int,           -- ^ bytes
    schRotation :: !(Maybe Bucket.Rotation),
    schSlice    :: !(Maybe Int),   -- ^ the slice's share of the rate, percent
    schFloor    :: !(Maybe Int),   -- ^ the floor's share of the rate, percent
    schBearers  :: ![BucketBearer]
  }
  deriving Show

instance Arbitrary BucketSched where
    arbitrary = do
      rate <- genRate
      cap  <- genCapacity
      n    <- choose (1, 12)
      BucketSched rate cap
        <$> frequency [ (1, return Nothing), (2, Just <$> genRotation rate cap) ]
        <*> frequency [ (1, return Nothing), (1, Just <$> choose (5, 50)) ]
        <*> pure Nothing
        <*> vectorOf n (genBucketBearer rate cap n)

    shrink sch@BucketSched { schRate, schCapacity, schRotation, schSlice, schFloor, schBearers } =
         [ sch { schBearers = bs }
         | bs <- shrinkList shrinkBearer schBearers, not (null bs) ]
      ++ [ sch { schSlice = Nothing } | Just _ <- [schSlice] ]
      ++ [ sch { schFloor = Nothing } | Just _ <- [schFloor] ]
      ++ [ sch { schRotation = Nothing } | Just _ <- [schRotation] ]
      ++ [ sch { schCapacity = 4096 } | schCapacity > 4096 ]
      ++ [ sch { schRate = r } | r <- [1e4, 1e6], r < schRate ]
      where
        shrinkBearer b =
             [ b { bbRank  = 0 }  | bbRank b /= 0 ]
          ++ [ b { bbStart = 0 }  | bbStart b /= 0 ]
          ++ [ b { bbMore  = ms } | ms <- shrinkList shrinkTake (bbMore b) ]
          ++ [ b { bbFirst = sz } | sz <- [1024, schCapacity], sz < bbFirst b ]
          ++ [ b { bbKill  = Nothing } | Just _ <- [bbKill b] ]
          ++ [ b { bbKill  = Just f }
             | Just f0 <- [bbKill b], f <- [0, 128], f < f0 ]
          ++ [ b { bbSlice = False } | bbSlice b ]

        shrinkTake (k, sz, r) =
             [ (0, sz, r)  | k /= 0 ]
          ++ [ (k, sz', r) | sz' <- [1024, schCapacity], sz' < sz ]
          ++ [ (k, sz, 0)  | r /= 0 ]

-- | Periods of a quarter to four fill times, so that schedules straddle a
-- boundary.
genRotation :: Double -> Int -> Gen Bucket.Rotation
genRotation rate cap = do
    seed <- arbitrary
    f    <- choose (0.25, 4 :: Double)
    return (Bucket.Rotation seed (realToFrac (f * fromIntegral cap / rate)) maxBound)

-- | Few ranks, so ties are common.  Asking again at once, and starting in the
-- opening burst, are both weighted up: those are what make bearers queue.
genBucketBearer :: Double -> Int -> Int -> Gen BucketBearer
genBucketBearer rate cap n =
    BucketBearer <$> choose (0, 3)
                 <*> genRounds
                 <*> genBytes cap
                 <*> (do extra <- frequency [ (3, return 0), (4, choose (1, 3)) ]
                         vectorOf extra ((,,) <$> genRounds <*> genBytes cap <*> choose (0, 3)))
                 <*> frequency [ (3, return Nothing)
                               , (1, Just <$> choose (0, 255))
                               , (1, Just <$> choose (192, 255)) ]
                 <*> arbitrary
  where
    fillPs   = ceiling (1e12 * fromIntegral cap / rate) :: Integer
    rounds k = k `div` fromIntegral n
    genRounds = frequency [ (4, return 0)                      -- at once
                          , (3, choose (0, rounds fillPs))
                          , (1, choose (0, rounds (4 * fillPs))) ]

-- | One request: which bearer made it and which of its takes this is, its rank
-- and size, when it was made, and what that bearer has still to ask for.
data Arrival = Arrival {
    arBearer :: !Int,
    arTake   :: !Int,
    arRank   :: !Word8,
    arBytes  :: !Int,
    arAt     :: !Time,
    arMore   :: ![(Integer, Int, Word8)],
    arSlice  :: !Bool              -- ^ asks the slice, borrowing the budget
  }
  deriving (Eq, Show)

-- | When bearer @i@ of @n@ asks, @k@ rounds after @t@: the first instant past
-- @t@ congruent to @i@ modulo @n@ picoseconds, plus @k@ whole rounds.  Every
-- request a bearer makes therefore lands on a picosecond no other bearer can
-- use, so no two requests in a run are ever simultaneous, tickets are handed
-- out in request order, and the replay never has to guess whose came first.
askAt :: Int -> Int -> Integer -> Time -> Time
askAt n i k t = picos ((k + 1) * fromIntegral n + delta) `addTime` t
  where
    delta = (fromIntegral i - picosOf t) `mod` fromIntegral n

-- | The first request of every bearer.
arrivals :: BucketSched -> [Arrival]
arrivals BucketSched { schSlice, schBearers } =
    [ Arrival { arBearer = i, arTake = 0, arRank = bbRank, arBytes = bbFirst,
                arAt = askAt n i bbStart (Time 0), arMore = bbMore,
                arSlice = bbSlice && isJust schSlice }
    | (i, BucketBearer { bbRank, bbStart, bbFirst, bbMore, bbSlice }) <- zip [0 ..] schBearers
    ]
  where
    n = length schBearers

-- | The same bearer's next request, once this one is granted at @t@.
nextArrival :: Int -> Arrival -> Time -> Maybe Arrival
nextArrival n a t =
    case arMore a of
      []                -> Nothing
      (k, bytes, rank) : more -> Just a { arTake  = arTake a + 1
                                  , arBytes = bytes
                                  , arRank  = rank
                                  , arAt    = askAt n (arBearer a) k t
                                  , arMore  = more }

labelBucket :: BucketSched -> Property -> Property
labelBucket BucketSched { schRotation, schSlice, schFloor, schBearers } =
      classify (any (not . null . bbMore) schBearers) "reuses a handle"
    . classify (isJust schFloor)                      "with a floor"
    . classify (any (\b -> any (\(_, _, r) -> r /= bbRank b) (bbMore b)) schBearers)
               "changes rank between takes"
    . classify (isJust schRotation)                   "rotating"
    . classify (isJust schSlice)                      "with a slice"

-- | Grant instants from a real bucket in IOSim, by bearer and take.  Each
-- bearer keeps one handle for all of its takes.
runBucketSched :: BucketSched -> [((Int, Int), Time)]
runBucketSched = fst . runBucketSchedStats

-- | 'runBucketSched', with the budget's and the slice's counters at the end.
runBucketSchedStats :: BucketSched
                    -> ([((Int, Int), Time)], (Bucket.BucketStats, Maybe Bucket.BucketStats))
runBucketSchedStats sch@BucketSched { schRate, schCapacity, schRotation, schSlice, schFloor,
                                      schBearers } =
    runSimOrThrow $ do
      bucket <- Bucket.newBucket schRate schCapacity schRotation
      forM_ schFloor $ \pct -> Bucket.attachFloor bucket (fromIntegral pct / 100)
      slice  <- traverse (\pct -> Bucket.newBucket (schRate * fromIntegral pct / 100)
                                                  schCapacity Nothing) schSlice
      -- registered on the budget in schedule order, so bearer @i@ has id @i@,
      -- as the replay assumes; every bearer also has a slice handle if there
      -- is a slice
      hs <- mapM (const (Bucket.registerBearer bucket)) schBearers
      ss <- mapM (const (traverse Bucket.registerBearer slice)) schBearers
      grants <- forConcurrently (zip3 hs ss (arrivals sch)) $ \(h, s_m, a0) -> do
        -- the rank as a rule over a variable, as the node does: the variable
        -- holds each take's rank before it is asked for
        rankVar <- newTVarIO (Bucket.Rank (arRank a0))
        atomically $ Bucket.setRankSource h (\_ -> readTVar rankVar)
        forM_ s_m $ \s -> atomically $ Bucket.setRankSource s (\_ -> readTVar rankVar)
        -- what the slice lane does: its own bucket, charged to the budget on
        -- credit, or the budget's idle capacity
        let grant a = case s_m of
              Just s | arSlice a -> void $ Bucket.awaitGrantBorrowing s bucket (arBytes a)
              _                  -> Bucket.awaitGrant h (arBytes a)
            takeAll a = do
              now <- getMonotonicTime
              threadDelay (arAt a `diffTime` now)
              atomically $ writeTVar rankVar (Bucket.Rank (arRank a))
              grant a
              t <- getMonotonicTime
              (((arBearer a, arTake a), t) :)
                <$> maybe (return []) takeAll (nextArrival n a t)
        takeAll a0
      now <- getMonotonicTime
      stats <- atomically $ do
        (b, _, _, _) <- Bucket.bucketSnapshot bucket now
        s' <- traverse (\sl -> (\(st, _, _, _) -> st) <$> Bucket.bucketSnapshot sl now) slice
        return (b, s')
      return (concat grants, stats)
  where
    n = length schBearers

-- | How a replayed grant was paid for.
data ReplayPay = FromBudget | FromSlice | Borrowed
  deriving (Eq, Show)

-- | What the replay saw: a grant, a slice head that went to sleep on the
-- budget's ready instant, or two events in the same instant whose order the
-- replay cannot know.
data ReplayEvent = RGrant !ReplayPay ((Int, Int), (Time, Time))
                 | RSleptOnBudget
                 | RTie
                 | RBudgetQueue !Time !Int !(Int, Int) !Bool
                   -- ^ the budget's queue changed: when, its new length, whose
                   -- request, and whether it joined
                 | RSliceQueue !Time !Int !(Int, Int) !Bool
                   -- ^ the same, for the slice's queue
  deriving (Eq, Show)

-- | Grant instants from the pure core, with each take's request instant
-- alongside.  Each bucket has its own queue, served lowest (rank, request)
-- first: the head takes at once if its bytes are there, else sleeps until
-- 'Bucket.wakeAt' -- unless a lower key arrives first and takes the head.  A
-- slice head that is short borrows the budget if nobody is queued on it and
-- the bytes are there; short on both, it sleeps until the earlier of the two,
-- and nothing but its own queue wakes it.  A slice-paid grant is charged to
-- the budget on credit.  A granted bearer re-joins with its next take.
--
-- The two queues run concurrently, so two checks in the same instant, one on
-- each queue, happen in an order the replay cannot see. Those end the replay
-- with 'RTie'.
replayEvents :: BucketSched -> [ReplayEvent]
replayEvents sch@BucketSched { schRate, schCapacity, schRotation, schSlice, schBearers } =
    go (Time 0) (Time 0) (Time 0) (List.sortOn arAt (arrivals sch)) [] [] Nothing Nothing Nothing
  where
    n         = length schBearers
    sliceRate = maybe 0 (\pct -> schRate * fromIntegral pct / 100) schSlice

    -- the rank a request holds at @t@, as each bucket computes it: on the
    -- budget the place dealt at its last refresh at or before @t@, or where
    -- it queued, since a waiting request asks again every
    -- 'Bucket.tierRefresh' and moves to the place of the period it then
    -- finds itself in, checking the queue in the same instant. (A head
    -- whose wake falls in the very instant of its refresh checks first and
    -- refreshes after; the replay does not model that coincidence, which
    -- needs a token instant to land on a whole microsecond seven seconds
    -- after a request.) The slice deals no places.
    keyB t a = ( Bucket.queueRank schRotation (fromIntegral (arBearer a))
                                  (Bucket.Rank (arRank a)) (refreshUpTo (arAt a) t)
               , arAt a )
    -- the same with the refreshes at @t@ still to come
    keyBefore t a = ( Bucket.queueRank schRotation (fromIntegral (arBearer a))
                                       (Bucket.Rank (arRank a)) (refreshBefore (arAt a) t)
                    , arAt a )
    keyS a = (Bucket.queueRank Nothing 0 (Bucket.Rank (arRank a)) (arAt a), arAt a)

    refreshBefore asked t = refreshNth asked (max 0 (ceiling (waited asked t / period) - 1))
    refreshUpTo   asked t = refreshNth asked (max 0 (floor   (waited asked t / period)))
    refreshNth asked k    = (fromIntegral (k :: Int) * Bucket.tierRefresh) `addTime` asked
    waited asked t        = realToFrac (t `diffTime` asked) :: Double
    period                = realToFrac Bucket.tierRefresh :: Double

    -- the next refresh of any request waiting on the budget after @now@,
    -- and whose it is; no two share an instant, since requests do not
    nextRefresh now wb = case [ (refreshNth (arAt a) (floor (waited (arAt a) now / period) + 1), a) | a <- wb ] of
                              [] -> Nothing
                              rs -> Just (List.minimumBy (\(t, _) (t', _) -> compare t t') rs)

    headBy k waiting = case List.sortOn k waiting of
                            []    -> Nothing
                            h : _ -> Just h

    enqueue = List.insertBy (\x y -> compare (arAt x) (arAt y))

    next h now pending = maybe pending (`enqueue` pending) (nextArrival n h now)

    -- pending: not yet requested, by request instant
    -- wb, ws:  the budget's and the slice's queues; each head is the lowest key
    -- kb, ks:  when each queue's short head checks again
    -- lst:     the instant and queue of the last check
    go now fb fs pending wb ws kb ks lst
      -- the budget's head checks
      | Just h <- headBy (keyB now) wb, Nothing <- kb =
          if crossed True then [RTie] else
          let (ready, fb') = Bucket.grantAt schRate schCapacity fb now (arBytes h)
              lst'         = Just (now, True)
          in if ready == now
                then RGrant FromBudget (grant h)
                   : RBudgetQueue now (length wb - 1) (arBearer h, arTake h) False
                   : go now fb' fs (next h now pending) (List.delete h wb) ws Nothing ks lst'
                else go now fb fs pending wb ws (Just (Bucket.wakeAt now ready)) ks lst'
      -- the slice's head checks: its own bucket, else the budget's idle capacity
      | Just h <- headBy keyS ws, Nothing <- ks =
          let need          = arBytes h
              (readyS, fs') = Bucket.grantAt sliceRate schCapacity fs now need
              (readyB, fb') = Bucket.grantAt schRate schCapacity fb now need
              idle          = null wb
          in if crossed False then [RTie] else
             if readyS == now
                then RGrant FromSlice (grant h)
                   : RSliceQueue now (length ws - 1) (arBearer h, arTake h) False
                   : go now (Bucket.chargeAt schRate fb now need) fs'
                        (next h now pending) wb (List.delete h ws) kb Nothing
                        (Just (now, False))
             else if idle && readyB == now
                then RGrant Borrowed (grant h)
                   : RSliceQueue now (length ws - 1) (arBearer h, arTake h) False
                   : go now fb' fs (next h now pending) wb (List.delete h ws) kb Nothing
                        (Just (now, False))
             else [ RSleptOnBudget | idle, readyB < readyS ]
               ++ go now fb fs pending wb ws kb
                     (Just (Bucket.wakeAt now (if idle then min readyS readyB else readyS)))
                     (Just (now, False))
      -- a waiting request's refresh before both wakes and the next request:
      -- if its key moves, whoever is then the budget's head checks at once,
      -- the request itself if it stays or becomes the head, or the one its
      -- move hands the head to
      | Just (r, a) <- nextRefresh now wb, all (r <) kb, all (r <) ks, all ((r <) . arAt) (take 1 pending) =
          let moved   = keyBefore r a /= keyB r a
              headWas = fmap keyOf (headBy (keyBefore r) wb)
              headNow = fmap keyOf (headBy (keyB r) wb)
              checks  = moved && (headNow /= headWas || headNow == Just (keyOf a))
              keyOf x = (arBearer x, arTake x)
          in go r fb fs pending wb ws (if checks then Nothing else kb) ks lst
      -- a request before both wakes joins its own queue; a lower key takes
      -- that queue's head
      | p : ps <- pending, all (arAt p <) kb, all (arAt p <) ks =
          if arSlice p
             then let displaces = maybe False ((keyS p <) . keyS) (headBy keyS ws)
                  in RSliceQueue (arAt p) (length ws + 1) (arBearer p, arTake p) True
                   : go (arAt p) fb fs ps wb (p : ws) kb (if displaces then Nothing else ks) lst
             else let displaces = maybe False ((keyB (arAt p) p <) . keyB (arAt p)) (headBy (keyB (arAt p)) wb)
                  in RBudgetQueue (arAt p) (length wb + 1) (arBearer p, arTake p) True
                   : go (arAt p) fb fs ps (p : wb) ws (if displaces then Nothing else kb) ks lst
      -- the earlier wake fires
      | otherwise =
          case (kb, ks) of
               (Just b, Just s') | b == s'   -> [RTie]
                                 | b < s'    -> go b  fb fs pending wb ws Nothing ks lst
                                 | otherwise -> go s' fb fs pending wb ws kb Nothing lst
               (Just b, Nothing)  -> go b  fb fs pending wb ws Nothing ks lst
               (Nothing, Just s') -> go s' fb fs pending wb ws kb Nothing lst
               (Nothing, Nothing) -> []
      where
        grant h = ((arBearer h, arTake h), (arAt h, now))
        -- the last check was on the other queue, in this same instant
        crossed onBudget = case lst of
                                Just (t, q) -> t == now && q /= onBudget
                                Nothing     -> False

-- | The grants of 'replayEvents', whatever it could not order.
replayRun :: BucketSched -> [((Int, Int), (Time, Time))]
replayRun sch = [ g | RGrant _ g <- replayEvents sch ]

-- | The counters each bucket should hold after the replay's grants.
replayStats :: BucketSched -> (Bucket.BucketStats, Maybe Bucket.BucketStats)
replayStats sch@BucketSched { schSlice, schBearers } =
    ( withBursts budgetQueue (stats (queuedIn budgetQueue) [ g | RGrant FromBudget g <- events ] [])
    , withBursts sliceQueue (stats (queuedIn sliceQueue)
                                   [ g | RGrant pay g <- events, pay /= FromBudget ]
                                   [ g | RGrant Borrowed g <- events ]) <$ schSlice )
  where
    events = replayEvents sch

    -- the budget's queue as the bucket sees it: a request that joins an empty
    -- queue and is granted at once took the fast path and never queued
    budgetQueue = dropFastPath [ (t, n, k, j) | RBudgetQueue t n k j <- events ]
    sliceQueue  = dropFastPath [ (t, n, k, j) | RSliceQueue t n k j <- events ]

    dropFastPath ((t, 1, k, True) : (t', 0, k', False) : rest)
      | t == t' && k == k' = dropFastPath rest
    dropFastPath (e : rest) = e : dropFastPath rest
    dropFastPath []         = []

    -- busy periods from the credited queue's lengths, in the order they
    -- changed: a bearer at rest rank or beyond joins and leaves unseen
    withBursts q st = go 0 Nothing Nothing 0 st (creditedLengths 0 q)
      where
        creditedLengths _ [] = []
        creditedLengths c ((t, _, k, joined) : rest)
          | rankOf k < 4 = let c' = if joined then c + 1 else c - 1
                           in (t, c') : creditedLengths c' rest
          | otherwise    = creditedLengths c rest
        go _ _ _ _ acc [] = acc
        go n0 since cSince peak acc ((t, n1) : rest) =
          let started = n0 == 0 && n1 >= 1
              since'  = if started then Just t else since
              peak'   = if started then n1 else max peak n1
              acc1    = if started then acc { Bucket.bsBursts = Bucket.bsBursts acc + 1 } else acc
              (cSince', acc2)
                | n0 < 2 && n1 >= 2, Nothing <- cSince = (Just t, acc1)
                | n0 >= 2 && n1 < 2, Just c <- cSince  =
                    (Nothing, acc1 { Bucket.bsContendedTime =
                                       Bucket.bsContendedTime acc1 + (t `diffTime` c) })
                | otherwise = (cSince, acc1)
          in if n0 >= 1 && n1 == 0
                then case since' of
                       Just s0 ->
                         let len = t `diffTime` s0
                             acc3 = acc2 {
                               Bucket.bsBusyTime    = Bucket.bsBusyTime acc2 + len,
                               Bucket.bsBurstsOver  =
                                 zipWith (\b c -> if len > b then c + 1 else c)
                                         Bucket.burstBounds (Bucket.bsBurstsOver acc2),
                               Bucket.bsBurstsWider =
                                 zipWith (\w c -> if peak' >= w then c + 1 else c)
                                         Bucket.burstWidths (Bucket.bsBurstsWider acc2) }
                         in go n1 Nothing cSince' 0 acc3 rest
                       Nothing -> go n1 Nothing cSince' 0 acc2 rest
                else go n1 since' cSince' peak' acc2 rest

    -- bytes and rank of take @k@ of bearer @b@
    bytesOf (b, k) = let bb = schBearers !! b
                     in if k == 0 then bbFirst bb else (\(_, sz, _) -> sz) (bbMore bb !! (k - 1))
    rankOf  (b, k) = let bb = schBearers !! b
                     in if k == 0 then bbRank bb else (\(_, _, r) -> r) (bbMore bb !! (k - 1))

    -- the requests that joined a queue: the fast path leaves no trace
    queuedIn q = Set.fromList [ k | (_, _, k, True) <- q ]

    stats queued gs borrowed =
      let waits = [ granted `diffTime` asked | (_, (asked, granted)) <- gs ]
          bytesIn = sum . map (fromIntegral . bytesOf . fst)
      in Bucket.BucketStats {
           Bucket.bsBytes        = bytesIn gs,
           Bucket.bsBatches      = fromIntegral (length gs),
           Bucket.bsBorrowed     = bytesIn borrowed,
           Bucket.bsCredited     = 0,
           Bucket.bsWaitTokens   = sum waits,
           Bucket.bsWaitWritable = 0,
           Bucket.bsWaitsOver    = [ fromIntegral (length (filter (> b) waits))
                                   | b <- Bucket.waitBounds ],
           Bucket.bsBursts        = 0,
           Bucket.bsBusyTime      = 0,
           Bucket.bsContendedTime = 0,
           Bucket.bsBurstsOver    = map (const 0) Bucket.burstBounds,
           Bucket.bsBurstsWider   = map (const 0) Bucket.burstWidths,
           Bucket.bsTiers         = M.fromListWith (<>)
                                      [ (rankOf k, Bucket.TierGrants (fromIntegral (bytesOf k)) 1)
                                      | (k, _) <- gs, k `Set.member` queued ],
           Bucket.bsServed        = served,
           Bucket.bsServedPruned  = pruned,
           Bucket.bsBurstsDropped = 0
         }
      where
        -- the harness reports no gate waits or writes, so a standing is the
        -- token waits alone
        (served, pruned) = servedModel (List.sortOn (\(_, _, t, _, _) -> t)
                             [ (tier, fromIntegral (fst k), granted, granted `diffTime` asked, 0)
                             | (k, tier, asked, granted) <- servedTiers queued gs ])

    -- the tier each grant records: its rank, asked as it joined the queue
    -- or, on the fast path, at the grant
    servedTiers _queued gs = [ (k, rankOf k, asked, granted) | (k, (asked, granted)) <- gs ]

replaySched :: BucketSched -> [((Int, Int), Time)]
replaySched = map (\(k, (_, granted)) -> (k, granted)) . replayRun

-- | Where each take is granted when nothing ever waits, so the chain follows
-- from the schedule alone.
instantGrants :: BucketSched -> [((Int, Int), Time)]
instantGrants sch@BucketSched { schBearers } = concatMap chain (arrivals sch)
  where
    n = length schBearers
    chain a = ((arBearer a, arTake a), arAt a)
            : maybe [] chain (nextArrival n a (arAt a))

-- | Whatever the schedule, the counters the budget and the slice keep hold no
-- thunks: one left behind per grant would grow with every batch sent.
prop_bucket_stats_no_thunks :: BucketSched -> Property
prop_bucket_stats_no_thunks sch =
    labelBucket sch $
    case runBucketSchedStats sch of
      (_, (!budget, slice_m)) ->
             noThunksIn "budget" budget
        .&&. case slice_m of
               Just !slice -> noThunksIn "slice" slice
               Nothing     -> property True
  where
    noThunksIn what st = counterexample (what ++ ": " ++ show (unsafeNoThunks st))
                                        (isNothing (unsafeNoThunks st))

-- | Every grant happens exactly when the replay says.
prop_bucket_schedule :: BucketSched -> Property
prop_bucket_schedule sch
  | RTie `elem` events = discard
  | otherwise =
      labelBucket sch $
      classify (queued == 0)                  "nothing queued" $
      classify (paid Borrowed)                "borrowed" $
      classify (paid FromSlice)               "slice paid, charged on credit" $
      classify (RSleptOnBudget `elem` events) "slice slept on the budget" $
      classify (Bucket.bsBursts (fst (replayStats sch)) > 0) "the budget was busy" $
      classify (Bucket.bsContendedTime (fst (replayStats sch)) > 0) "bearers contended" $
        (List.sortOn fst grants === List.sortOn fst (replaySched sch)
         .&&. counterexample "counters" (stats === replayStats sch))
  where
    events = replayEvents sch
    (grants, stats) = runBucketSchedStats sch
    paid k = any (\e -> case e of { RGrant k' _ -> k' == k; _ -> False }) events
    queued = length [ () | (_, (asked, granted)) <- replayRun sch
                         , granted > asked ]

-- | Killing a bearer mid-wait hands the queue on: every other bearer still
-- completes all of its takes, and the cancellation itself returns.  A
-- cancelled head must wake its successor, or the bucket wedges.
--
-- The instant comes from the replay, so a cancellation always lands inside
-- 'Bucket.awaitGrant' rather than whenever a fixed delay happens to fall:
-- @bbKill@ says how far into the bearer's first wait to strike, and a bearer
-- the replay never makes wait is left alone.  Waiting on the cancellations as
-- well as on the survivors keeps the property meaningful even when every
-- bearer is killed, which would otherwise leave nothing to wait for.
prop_bucket_cancel :: BucketSched -> Property
prop_bucket_cancel sch0 = prop_bucket_cancel' sch0 { schSlice = Nothing }

prop_bucket_cancel' :: BucketSched -> Property
prop_bucket_cancel' sch@BucketSched { schRate, schCapacity, schRotation, schFloor, schBearers } =
    labelBucket sch
      $ classify (not (null killed))    "kills someone"
      $ classify (any killsHead killed) "kills the next to be served"
      $ counterexample ("kept " ++ show (length kept))
      -- and once everyone is done, killed or served, no busy period is left
      -- open: a cancelled waiter must close it as it leaves the queue; nor is
      -- anyone left in the floor's queue
      $ outcome === Just (length kept, False, 0)
  where
    n    = length schBearers
    run  = replayRun sch

    plan   = [ (a, bbKill b >>= strikeAt (arBearer a))
             | (a, b) <- zip (arrivals sch) schBearers ]
    killed = [ (a, t) | (a, Just t)  <- plan ]
    kept   = [ a      | (a, Nothing) <- plan ]

    -- @f@ 256ths into the first wait this bearer has, if it has one
    strikeAt b f =
      case List.sortOn fst [ (tk, (asked, granted))
                           | ((b', tk), (asked, granted)) <- run
                           , b' == b, granted > asked ] of
        []                        -> Nothing
        (_, (asked, granted)) : _ -> Just (part asked granted f)

    part asked granted f =
      picos (picosOf asked
              + ((picosOf granted - picosOf asked) * fromIntegral f) `div` 256)
        `addTime` Time 0

    -- of the bearers waiting at @t@, the one the queue is about to serve
    killsHead (a, t) =
      case [ (granted, b) | ((b, _), (asked, granted)) <- run
                          , asked <= t, t < granted ] of
        [] -> False
        ws -> snd (minimum ws) == arBearer a

    -- generous: the last first-request, every gap, four times the fluid time
    -- for every byte, and a second
    limit = maximum [ arAt a `diffTime` Time 0 | a <- arrivals sch ]
          + picos (sum [ (k + 2) * fromIntegral n
                       | b <- schBearers, (k, _, _) <- bbMore b ])
          + realToFrac (4 * bytes / schRate) + 1
    bytes = sum [ fromIntegral (bbFirst b) + sum (map (\(_, sz, _) -> fromIntegral sz) (bbMore b))
                | b <- schBearers ] :: Double

    outcome = runSimOrThrow $ do
      bucket <- Bucket.newBucket schRate schCapacity schRotation
      forM_ schFloor $ \pct -> Bucket.attachFloor bucket (fromIntegral pct / 100)
      hs <- mapM (const (Bucket.registerBearer bucket)) schBearers
      as <- forM (zip hs plan) $ \(h, (a0, kill)) -> do
        atomically $ Bucket.setRank h (Bucket.Rank (arRank a0))
        let takeAll a = do
              now <- getMonotonicTime
              threadDelay (arAt a `diffTime` now)
              Bucket.awaitGrant h (arBytes a)
              t <- getMonotonicTime
              maybe (return ()) takeAll (nextArrival n a t)
        asy <- async (takeAll a0)
        return (kill, asy)

      killers <- forM [ (asy, t) | (Just t, asy) <- as ] $ \(asy, t) ->
        async $ do
          now <- getMonotonicTime
          threadDelay (t `diffTime` now)
          cancel asy

      done <- timeout limit
        (do mapM_ wait killers                        -- each cancel returns
            mapM wait [ asy | (Nothing, asy) <- as ]) -- survivors finish
      case done of
           Nothing -> return Nothing
           Just rs -> do
             t0 <- getMonotonicTime
             (st0, _, _, _) <- atomically $ Bucket.bucketSnapshot bucket t0
             threadDelay 1
             t1 <- getMonotonicTime
             (st1, _, _, _) <- atomically $ Bucket.bucketSnapshot bucket t1
             pending <- atomically $ Bucket.floorPending bucket
             return (Just (length rs, Bucket.bsBusyTime st1 /= Bucket.bsBusyTime st0, pending))

-- | 'prop_bucket_cancel' with a floor attached: the generated ranks 1 to 3
-- are credited, so cancellations land on bearers waiting in both queues.
prop_bucket_cancel_floor :: BucketSched -> Positive Int -> Property
prop_bucket_cancel_floor sch0 (Positive pct) =
    prop_bucket_cancel' sch0 { schSlice = Nothing, schFloor = Just (5 + pct `mod` 30) }

-- | A disabled budget disables its floor with it: still nothing ever waits.
prop_bucket_disabled_floor :: BucketSched -> Positive Int -> Property
prop_bucket_disabled_floor sch (Positive pct) =
    prop_bucket_disabled sch { schFloor = Just (5 + pct `mod` 30) }

-- | A floor nobody is eligible for adds nothing to the budget's path: with
-- every rank at 0 or above 3, the schedule still matches the replay exactly.
-- The replay orders by rank value, so moving ranks 1 to 3 up by three keeps
-- its prediction and takes every bearer out of the floor's classes.
prop_bucket_schedule_idle_floor :: BucketSched -> Positive Int -> Property
prop_bucket_schedule_idle_floor sch (Positive pct) =
    prop_bucket_schedule sch { schFloor   = Just (5 + pct `mod` 30)
                             , schBearers = map uncredited (schBearers sch) }
  where
    uncredited b = b { bbRank = up (bbRank b)
                     , bbMore = [ (k, sz, up r) | (k, sz, r) <- bbMore b ] }
    up r = if r == 0 then 0 else r + 3

-- | A rate of zero disables the bucket: nothing ever waits.
prop_bucket_disabled :: BucketSched -> Property
prop_bucket_disabled sch =
    labelBucket sch $
      List.sortOn fst (runBucketSched sch { schRate = 0 })
        === List.sortOn fst (instantGrants sch)

-- | 'Bucket.setBucketRate' takes effect: later grants are paced by the new
-- rate.  (A bearer already asleep is not woken early.)
prop_bucket_rate_change :: BucketSched -> Property
prop_bucket_rate_change BucketSched { schRate, schCapacity } =
    counterexample (show (spent, expected)) (abs (spent - expected) <= tolerance)
  where
    n         = 4 :: Int
    newRate   = 8 * schRate
    expected  = fromIntegral (n * schCapacity) / newRate
    tolerance = expected * 0.02 + fromIntegral n * 1e-6
    spent = runSimOrThrow $ do
      bucket <- Bucket.newBucket schRate schCapacity Nothing
      h      <- Bucket.registerBearer bucket
      Bucket.awaitGrant h schCapacity        -- drains the full bucket
      t0 <- getMonotonicTime
      atomically $ Bucket.setBucketRate bucket t0 newRate
      replicateM_ n (Bucket.awaitGrant h schCapacity)
      t1 <- getMonotonicTime
      return (realToFrac (t1 `diffTime` t0) :: Double)


--
-- Egress lanes
--
-- A scheduled server mux over the Queues bearer, in IOSim: a bulk responder
-- protocol saturating the scheduled lane, a small request protocol the server
-- initiates on the direct lane, and a stream it initiates on a reserved slice.
-- The client mux is unscheduled. The same run with every protocol on the
-- scheduled lane is the control: there the requests do queue behind the bulk.

data LaneCase = LaneCase {
    lcRate     :: !Double,   -- ^ budget, bytes/s
    lcSlicePct :: !Int,      -- ^ the slice's share of it
    lcBatches  :: !Int       -- ^ the bulk protocol serves this many batches
  }
  deriving Show

instance Arbitrary LaneCase where
    arbitrary = do
      e   <- choose (5, 7 :: Int)
      m   <- choose (1, 9.99 :: Double)
      pct <- choose (5, 30)
      n   <- choose (20, 200)
      return LaneCase { lcRate = m * 10 ^^ e, lcSlicePct = pct, lcBatches = n }
    shrink LaneCase { lcRate, lcSlicePct, lcBatches } =
         [ LaneCase r lcSlicePct lcBatches | r <- [1e5, 1e6], r < lcRate ]
      ++ [ LaneCase lcRate p lcBatches | p <- [5, 15], p < lcSlicePct ]
      ++ [ LaneCase lcRate lcSlicePct n | n <- [20, 50], n < lcBatches ]

-- | The Queues bearer writes two SDUs per batch.
laneBatch :: Int
laneBatch = 2 * fromIntegral (Mx.getSDUSize laneSduSize)

-- | What a run measured: the bulk's transfer time, the longest probe round
-- trip, and the bytes the slice stream delivered while the bulk was running.
data LaneRun = LaneRun {
    lrBulkTime   :: !Double,
    lrProbeMax   :: !Double,
    lrProbes     :: !Int,
    lrSliced     :: !Int,      -- ^ slice bytes delivered while the bulk ran
    lrIdle       :: !Double,   -- ^ seconds the link was idle before the bulk
    lrSlicedIdle :: !Int,      -- ^ slice bytes delivered in that time
    lrCharged    :: !(M.Map Mx.MiniProtocolNum Int),
    -- ^ what the charge sink was handed, per mini-protocol
    lrExpected   :: !(M.Map Mx.MiniProtocolNum Int)
    -- ^ what the bearer wrote on scheduled-lane protocols, headers included
  }
  deriving Show

laneSduSize :: Mx.SDUSize
laneSduSize = Mx.SDUSize 12288

runLanes :: (Mx.MiniProtocolNum -> Mx.MiniProtocolDir -> Mx.Lane) -> LaneCase -> LaneRun
runLanes = runLanesWith True

-- | 'runLanes' with or without a slice bucket: without one, the rule's
-- 'Mx.Slice' traffic is scheduled, ranked and charged like the bulk.
runLanesWith :: Bool -> (Mx.MiniProtocolNum -> Mx.MiniProtocolDir -> Mx.Lane) -> LaneCase -> LaneRun
runLanesWith withSlice laneOf LaneCase { lcRate, lcSlicePct, lcBatches } = runSimOrThrow $ do
    let cap    = 2 * laneBatch                                    -- two batches
        lcBulk = lcBatches * laneBatch
        -- a probe every quarter of a batch's worth of budget
        probeEvery = realToFrac (fromIntegral laneBatch / lcRate / 4) :: DiffTime
        -- the bulk starts after thirty batches' worth of idle link, during
        -- which the slice stream has the budget to itself
        idlePhase  = realToFrac (30 * fromIntegral laneBatch / lcRate) :: DiffTime
    budget <- Bucket.newBucket lcRate cap Nothing
    slice  <- Bucket.newBucket (lcRate * fromIntegral lcSlicePct / 100) cap Nothing

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    clientBearer <- getBearer makeQueueChannelBearer (-1)
                      QueueChannel { writeQueue = client_w, readQueue = client_r } Nothing
    serverBearer <- getBearer makeQueueChannelBearer (-1)
                      QueueChannel { writeQueue = client_r, readQueue = client_w } Nothing

    stopVar   <- newTVarIO False
    probeVar  <- newTVarIO (0 :: Int, 0 :: Double)     -- probes done, longest round trip
    slicedVar <- newTVarIO (0 :: Int)
    -- the oracle for the charge sink: every SDU the server bearer writes, by
    -- mini-protocol and direction, header included
    writtenVar <- newTVarIO (M.empty :: M.Map (Mx.MiniProtocolNum, Mx.MiniProtocolDir) Int)
    chargedVar <- newTVarIO (M.empty :: M.Map Mx.MiniProtocolNum Int)
    let sduBytes sdu = fromIntegral Mx.msHeaderLength + fromIntegral (Mx.msLength sdu)
        recorded = serverBearer {
          Mx.writeMany = \tr to sdus -> do
            atomically $ modifyTVar writtenVar $ \m ->
              foldr (\sdu -> M.insertWith (+) (Mx.mhNum (Mx.msHeader sdu), Mx.mhDir (Mx.msHeader sdu))
                                                (sduBytes sdu))
                    m sdus
            Mx.writeMany serverBearer tr to sdus }

    let info :: Mx.MiniProtocolNum -> Mx.MiniProtocolDirection Mx.InitiatorResponderMode
             -> MiniProtocolInfo Mx.InitiatorResponderMode
        info num dir = MiniProtocolInfo {
            miniProtocolNum = num, miniProtocolDir = dir,
            miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = 16000000 },
            miniProtocolCapability = Nothing }
        bulkN = Mx.MiniProtocolNum 2
        probeN = Mx.MiniProtocolNum 3
        sliceN = Mx.MiniProtocolNum 4

        policy = Mx.EgressPolicy { Mx.egressBudget = budget,
                                   Mx.egressSlice = if withSlice then Just slice else Nothing,
                                   Mx.egressLaneOf = laneOf }
    serverMux <- Mx.newWithEgress policy Mx.nullTracers Nothing
                   [ info bulkN Mx.ResponderDirection, info probeN Mx.InitiatorDirection
                   , info sliceN Mx.InitiatorDirection ]
    atomically $ Mx.setEgressChargeSink serverMux $ \_ num n ->
      modifyTVar chargedVar (M.insertWith (+) num n)
    clientMux <- Mx.new Mx.nullTracers Nothing
                   [ info bulkN Mx.InitiatorDirection, info probeN Mx.ResponderDirection
                   , info sliceN Mx.ResponderDirection ]

    let untilStopped act = do
          stopped <- readTVarIO stopVar
          unless stopped act

        -- the bulk: one request, lcBulk bytes back in 8 kB messages
        bulkServer chan = do
          _ <- Mx.recv chan
          forM_ (chunks lcBulk) $ \n -> Mx.send chan (BL.replicate (fromIntegral n) 0x78)
          return ((), Nothing)
        bulkClient chan = do
          threadDelay idlePhase
          slicedIdle <- readTVarIO slicedVar
          t0 <- getMonotonicTime
          Mx.send chan (BL8.pack "req")
          drain chan lcBulk
          t1 <- getMonotonicTime
          return ((realToFrac (t1 `diffTime` t0) :: Double, slicedIdle), Nothing)

        -- the probe: 32 bytes out, 32 back, every quarter batch
        probeServer chan = do
          let loop = untilStopped $ do
                t0 <- getMonotonicTime
                Mx.send chan (BL.replicate 32 0x70)
                drain chan 32
                t1 <- getMonotonicTime
                atomically $ modifyTVar probeVar $ \(n, mx) ->
                  (n + 1, max mx (realToFrac (t1 `diffTime` t0)))
                threadDelay probeEvery
                loop
          loop
          return ((), Nothing)
        echo chan = do
          let loop = do
                mbs <- Mx.recv chan
                case mbs of
                  Nothing -> return ()
                  Just bs -> Mx.send chan bs >> loop
          loop
          return ((), Nothing)

        -- the slice stream: 4 kB messages as fast as the lane takes them,
        -- many times what the run can carry, so a lane that took them
        -- unthrottled would still run out rather than keep the budget in
        -- debt for ever
        sliceServer chan = do
          let loop n = when (n > 0) $ untilStopped $
                         Mx.send chan (BL.replicate 4096 0x74) >> loop (n - 1)
          loop (10 * (30 + lcBatches) * laneBatch `div` 4096)
          return ((), Nothing)
        sink chan = do
          let loop = do
                mbs <- Mx.recv chan
                case mbs of
                  Nothing -> return ()
                  Just bs -> do
                    stopped <- readTVarIO stopVar
                    unless stopped $
                      atomically $ modifyTVar slicedVar (+ fromIntegral (BL.length bs))
                    loop
          loop
          return ((), Nothing)

    withAsync (Mx.run serverMux recorded) $ \_ ->
      withAsync (Mx.run clientMux clientBearer) $ \_ -> do
        _ <- Mx.runMiniProtocol serverMux bulkN  Mx.ResponderDirection Mx.StartOnDemand bulkServer
        _ <- Mx.runMiniProtocol serverMux probeN Mx.InitiatorDirection Mx.StartEagerly probeServer
        _ <- Mx.runMiniProtocol serverMux sliceN Mx.InitiatorDirection Mx.StartEagerly sliceServer
        _ <- Mx.runMiniProtocol clientMux probeN Mx.ResponderDirection Mx.StartOnDemand echo
        _ <- Mx.runMiniProtocol clientMux sliceN Mx.ResponderDirection Mx.StartOnDemand sink
        bulk <- Mx.runMiniProtocol clientMux bulkN Mx.InitiatorDirection Mx.StartEagerly bulkClient
        r <- atomically bulk
        atomically $ writeTVar stopVar True
        (probes, probeMax) <- readTVarIO probeVar
        sliced <- readTVarIO slicedVar
        Mx.stop serverMux
        Mx.stop clientMux
        written <- readTVarIO writtenVar
        charged <- readTVarIO chargedVar
        let effective Mx.Slice | not withSlice = Mx.Scheduled
            effective lane                     = lane
            expected = M.fromListWith (+) [ (num, n) | ((num, dir), n) <- M.toList written
                                                     , effective (laneOf num dir) == Mx.Scheduled ]
        case r of
          Left e -> throwIO e
          Right (bulkTime, slicedIdle) ->
            return LaneRun { lrBulkTime = bulkTime, lrProbeMax = probeMax, lrProbes = probes,
                             lrSliced = sliced - slicedIdle,
                             lrIdle = realToFrac idlePhase, lrSlicedIdle = slicedIdle,
                             lrCharged = charged, lrExpected = expected }
  where
    chunks n | n <= 0    = []
             | otherwise = min 8192 n : chunks (n - 8192)

    -- read until @n@ bytes have arrived
    drain :: Monad m => Mx.ByteChannel m -> Int -> m ()
    drain _    n | n <= 0 = return ()
    drain chan n = do
      mbs <- Mx.recv chan
      case mbs of
        Nothing -> return ()
        Just bs -> drain chan (n - fromIntegral (BL.length bs))

-- | With the lanes: no probe ever waits (in IOSim the direct path takes no
-- simulated time at all); while the link is idle the slice stream borrows the
-- budget and runs near the link rate; while the bulk runs the slice delivers
-- its share -- no less than most of it, no more than its bucket allows, which
-- is its capacity's burst plus its rate -- and the bulk gets the rest.
-- Without the lanes, everything scheduled, the probes queue behind the bulk's
-- batches, which is the failure the lanes exist to prevent. Without a slice
-- bucket the slice lane's traffic is scheduled: charged beside the bulk, and
-- the probes still never wait.
prop_mux_lanes :: LaneCase -> Property
prop_mux_lanes lc@LaneCase { lcRate, lcSlicePct, lcBatches } =
    counterexample (show (lc, lanes, control)) $
         counterexample "probe waited"            (lrProbeMax lanes < batchTime / 100)
    .&&. counterexample "slice did not borrow the idle link"
           (fromIntegral (lrSlicedIdle lanes) >= 0.7 * lcRate * lrIdle lanes)
    .&&. counterexample "no probes"               (lrProbes lanes > 0)
    .&&. counterexample "slice short of its share" (sliced >= 0.7 * slice * bulkTime)
    .&&. counterexample "slice over its bucket"    (sliced <= cap + slice * bulkTime + batch)
    .&&. counterexample "bulk faster than budget"  (bulkTime >= 0.9 * fluid)
    .&&. counterexample "bulk starved"             (bulkTime <= 1.3 * fluid / (1 - share))
    .&&. counterexample "control did not queue"    (lrProbeMax control >= batchTime / 8)
    -- the charge sink sees exactly the scheduled lane's bytes: the bulk alone
    -- with the lanes, everything in the control, headers included, never a
    -- direct or slice byte
    .&&. counterexample "charged /= scheduled bytes"  (lrCharged lanes === lrExpected lanes)
    .&&. counterexample "control charged /= all bytes" (lrCharged control === lrExpected control)
    .&&. counterexample "no slice: charged /= scheduled and slice bytes"
           (lrCharged noSlice === lrExpected noSlice)
    .&&. counterexample "no slice: probe waited"       (lrProbeMax noSlice < batchTime / 100)
  where
    lanes     = runLanes laneRule lc
    control   = runLanes (\_ _ -> Mx.Scheduled) lc
    noSlice   = runLanesWith False laneRule lc
    share     = fromIntegral lcSlicePct / 100
    slice     = share * lcRate
    bulkTime  = lrBulkTime lanes
    sliced    = fromIntegral (lrSliced lanes) :: Double
    batch     = fromIntegral laneBatch :: Double
    batchTime = batch / lcRate
    fluid     = fromIntegral lcBatches * batchTime
    cap       = 2 * batch                     -- the slice bucket's capacity, as 'runLanes' sizes it

    laneRule (Mx.MiniProtocolNum 4) Mx.InitiatorDir = Mx.Slice
    laneRule num dir                                = Mx.directionSplit num dir

-- | Through the muxer, every scheduled batch reports to the budget the time
-- it waited at the writability gate and the time the batch before it took to
-- write. Over a bearer whose gate and writes take a known time, with one
-- message at a time and a pause between them so that each is one batch, the
-- bearer's standing is exactly the model's over those times, and the gate
-- waits sum to one per message; nothing waits for tokens.
prop_mux_scheduled_own_time :: OwnTimeCase -> Property
prop_mux_scheduled_own_time oc@OwnTimeCase { otGate, otWrite, otMsgs } =
    counterexample (show oc) $
    classify (otGate == 0)       "no gate wait" $
    classify (otWrite == 0)      "writes take no time" $
    classify (length otMsgs > 1) "several batches" $
    case runSimOrThrow run of
      (st, sent) ->
             counterexample "standing" (Bucket.bsServed st === fst (servedModel (model sent)))
        .&&. counterexample "gate waits"
               (Bucket.bsWaitWritable st === fromIntegral (length otMsgs) * otGate)
  where
    num   = Mx.MiniProtocolNum 2
    pause = otGate + otWrite + 0.001

    -- the bearer is the budget's first and has no rank: unranked, id 0;
    -- a batch is granted once its gate opens, and reports the gate and the
    -- write before it, none before the first
    model sent = [ (255, 0, otGate `addTime` t, 0, otGate + (if k == 0 then 0 else otWrite))
                 | (k, t) <- zip [0 :: Int ..] sent ]

    run :: IOSim s (Bucket.BucketStats, [Time])
    run = do
      w <- atomically $ newTBQueue 2
      r <- atomically $ newTBQueue 2
      base <- getBearer makeQueueChannelBearer (-1)
                QueueChannel { writeQueue = w, readQueue = r } Nothing
      let bearer = base
            { Mx.awaitWritable = \_ _ -> threadDelay otGate
            , Mx.writeMany     = \tr to sdus -> threadDelay otWrite >> Mx.writeMany base tr to sdus }
          info :: MiniProtocolInfo Mx.InitiatorMode
          info = MiniProtocolInfo {
              miniProtocolNum        = num,
              miniProtocolDir        = Mx.InitiatorDirectionOnly,
              miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 16 },
              miniProtocolCapability = Nothing }
      budget <- Bucket.newBucket 1e9 65536 Nothing
      let policy = Mx.EgressPolicy { Mx.egressBudget = budget, Mx.egressSlice = Nothing,
                                     Mx.egressLaneOf = \_ _ -> Mx.Scheduled }
      mux <- Mx.newWithEgress policy Mx.nullTracers Nothing [info]
      -- the peer takes everything
      _ <- async $ forever $ atomically (readTBQueue w)
      withAsync (Mx.run mux bearer) $ \_ -> do
        sent <- newTVarIO []
        _ <- Mx.runMiniProtocol mux num Mx.InitiatorDirectionOnly Mx.StartEagerly $ \chan -> do
          forM_ otMsgs $ \n -> do
            t <- getMonotonicTime
            Mx.send chan (BL.replicate (fromIntegral n) 7)
            atomically $ modifyTVar sent (t :)
            threadDelay pause
          return ((), Nothing)
        atomically $ readTVar sent >>= check . (== length otMsgs) . length
        threadDelay pause
        now <- getMonotonicTime
        (st, _, _, _) <- atomically $ Bucket.bucketSnapshot budget now
        (,) st . reverse <$> readTVarIO sent

-- | A gate wait and a write time, whole microseconds up to 50 ms and often
-- none, and messages of one SDU each: the queue bearer's SDU is 1280 bytes.
data OwnTimeCase = OwnTimeCase {
    otGate  :: !DiffTime,
    otWrite :: !DiffTime,
    otMsgs  :: ![Int]
  }
  deriving Show

instance Arbitrary OwnTimeCase where
    arbitrary = OwnTimeCase <$> micros <*> micros
                            <*> listOf1 (choose (1, 1280))
      where
        micros = frequency [ (1, pure 0)
                           , (3, (\us -> fromIntegral (us :: Int) / 1000000) <$> choose (1, 50000)) ]
    shrink oc@OwnTimeCase { otGate, otWrite, otMsgs } =
         [ oc { otMsgs = ms } | ms <- shrinkList (const []) otMsgs, not (null ms) ]
      ++ [ oc { otGate = 0 }  | otGate /= 0 ]
      ++ [ oc { otWrite = 0 } | otWrite /= 0 ]

-- | Own requests through a bearer whose peer stops and starts draining: the
-- gate is shut and the link stalls in the closed phases, and in the open ones
-- the link drains at a rate, so a batch may block half written.
data GateCase = GateCase {
    gcMsgs   :: ![(Int, Int)],   -- ^ ms after the previous send, and bytes
    gcPhases :: ![Int],          -- ^ phase lengths in ms, closed first, alternating
    gcRate   :: !Int             -- ^ bytes per ms the open link drains
  }
  deriving Show

instance Arbitrary GateCase where
    arbitrary = GateCase <$> listOf1 ((,) <$> choose (0, 20)
                                          <*> frequency [ (1, choose (1, 2000))
                                                        , (2, choose (1, 200000)) ])
                         <*> listOf1 (choose (1, 40))
                         <*> choose (1000, 50000)
    shrink gc@GateCase { gcMsgs, gcPhases } =
         [ gc { gcMsgs = ms }   | ms <- shrinkList shrinkMsg gcMsgs, not (null ms) ]
      ++ [ gc { gcPhases = ps } | ps <- shrinkList shrinkIntegral gcPhases, not (null ps), all (> 0) ps ]
      where
        shrinkMsg (d, n) = [ (d', n) | d' <- shrinkIntegral d ] ++ [ (d, n') | n' <- shrinkIntegral n, n' > 0 ]

-- | The node's own requests are charged to the budget on credit, never made
-- to wait for tokens -- but only for bytes handed to the bearer, and only once
-- the bearer can take them. Sampled every 100 us while the phases run: no byte
-- is handed over while the gate is shut, and the budget's credited bytes never
-- run ahead of the bytes handed over; once the link stays open, the two are
-- equal and every message has gone.
prop_mux_direct_gate :: GateCase -> Property
prop_mux_direct_gate gc@GateCase { gcMsgs, gcPhases, gcRate } =
    counterexample (show gc) $
    classify (any (\(_, n) -> n > 12288) gcMsgs) "a message of several SDUs" $
    classify (any (\(_, n) -> n <= 2000) gcMsgs) "a small message" $
    classify (length gcPhases > 2) "the gate shuts again" $
      case runSimOrThrow run of
           (closedWrites, ahead, final) ->
                  counterexample "handed over while shut" (closedWrites === [])
             .&&. counterexample "charged ahead of the write" (ahead === [])
             .&&. counterexample "at the end, credited /= handed or a message missing"
                    (final === Just (True, sum (map snd gcMsgs)))
  where
    num = Mx.MiniProtocolNum 2

    run :: IOSim s ([(Time, Int)], [(Time, Int, Int)], Maybe (Bool, Int))
    run = do
      open    <- newTVarIO False
      handed  <- newTVarIO (0 :: Int)
      w <- atomically $ newTBQueue 2
      r <- atomically $ newTBQueue 2
      base <- getBearer makeQueueChannelBearer (-1)
                QueueChannel { writeQueue = w, readQueue = r } Nothing
      let bearer = base
            { Mx.awaitWritable = \_ _ -> atomically (readTVar open >>= check)
            , Mx.writeMany     = \tr to sdus -> do
                t <- Mx.writeMany base tr to sdus
                atomically $ modifyTVar handed
                  (+ sum [ 8 + fromIntegral (BL.length (Mx.msBlob s)) | s <- sdus ])
                return t }
          info :: MiniProtocolInfo Mx.InitiatorMode
          info = MiniProtocolInfo {
              miniProtocolNum        = num,
              miniProtocolDir        = Mx.InitiatorDirectionOnly,
              miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 16 },
              miniProtocolCapability = Nothing }
      budget <- Bucket.newBucket 1e9 65536 Nothing
      let policy = Mx.EgressPolicy { Mx.egressBudget = budget, Mx.egressSlice = Nothing,
                                     Mx.egressLaneOf = Mx.directionSplit }
          credited = (\(st, _, _, _) -> fromIntegral (Bucket.bsCredited st))
                       <$> (getMonotonicTime >>= atomically . Bucket.bucketSnapshot budget)
      mux <- Mx.newWithEgress policy Mx.nullTracers Nothing [info]
      -- the peer: reads at the link rate while open, not at all while shut
      _ <- async $ forever $ do
        bs <- atomically $ do readTVar open >>= check; readTBQueue w
        threadDelay (realToFrac (fromIntegral (BL.length bs) / fromIntegral gcRate / 1000 :: Double))
      withAsync (Mx.run mux bearer) $ \_ -> do
        received <- newTVarIO (0 :: Int)
        _ <- Mx.runMiniProtocol mux num Mx.InitiatorDirectionOnly Mx.StartEagerly $ \chan -> do
          forM_ gcMsgs $ \(d, n) -> do
            threadDelay (fromIntegral d / 1000)
            Mx.send chan (BL.replicate (fromIntegral n) 7)
            atomically $ modifyTVar received (+ n)
          return ((), Nothing)
        -- the phases, sampled
        samples <- fmap concat $ forM (zip (cycle [False, True]) gcPhases) $ \(isOpen, ms) -> do
          atomically $ writeTVar open isOpen
          forM [1 .. ms * 10] $ \_ -> do
            h0 <- readTVarIO handed
            threadDelay 0.0001
            h1 <- readTVarIO handed
            c  <- credited
            now <- getMonotonicTime
            return (now, isOpen, h0, h1, c)
        let closedWrites = [ (t, h1 - h0) | (t, False, h0, h1, _) <- samples, h1 /= h0 ]
            ahead        = [ (t, c, h1) | (t, _, _, h1, c) <- samples, c > h1 ]
        -- then open for good, until every message is handed over
        atomically $ writeTVar open True
        final <- timeout 60 $ do
          atomically $ readTVar received >>= check . (== sum (map snd gcMsgs))
          threadDelay 1
          (,) <$> ((==) <$> credited <*> readTVarIO handed) <*> readTVarIO received
        return (closedWrites, ahead, final)

-- | Several muxes share one budget; one of them, the head of its queue, is
-- torn down mid-wait.
data HeadKill = HeadKill {
    hkRate    :: !Double,        -- ^ budget, bytes/s
    hkMuxes   :: !Int,           -- ^ muxes, the victim among them
    hkBatches :: !Int,           -- ^ batches each serves
    hkAt      :: !Int,           -- ^ tear the victim down this far, in 256ths,
                                 --   into its own transfer
    hkHow     :: !TearDown
  }
  deriving Show

data TearDown = StopMux | CancelRun
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary HeadKill where
    arbitrary = HeadKill <$> ((* 1e5) <$> choose (1, 99))
                         <*> choose (2, 5)
                         <*> choose (10, 60)
                         <*> choose (16, 240)
                         <*> arbitraryBoundedEnum
    shrink hk@HeadKill { hkMuxes, hkBatches, hkHow } =
         [ hk { hkMuxes = 2 }       | hkMuxes > 2 ]
      ++ [ hk { hkBatches = 10 }    | hkBatches > 10 ]
      ++ [ hk { hkHow = CancelRun } | hkHow == StopMux ]

-- | Tearing down the head of the budget's queue hands the queue on: every
-- other mux still delivers all of its bulk. The victim has rank 0 and the rest
-- rank 1, so whenever the victim waits for tokens it is the head, with the
-- others waiting behind it on their own wake variables; writes to the Queues
-- bearer take no simulated time, so at any instant of its transfer it is
-- asleep at the head. A head that leaves without waking its successor wedges
-- everyone behind it.
prop_mux_egress_head_torn_down :: HeadKill -> Property
prop_mux_egress_head_torn_down hk@HeadKill { hkRate, hkMuxes, hkBatches, hkAt, hkHow } =
    counterexample (show hk) $
    tabulate "tear-down" [show hkHow] $
      runSimOrThrow run === Just (hkMuxes - 1)
  where
    bulk     = hkBatches * laneBatch
    batch    = fromIntegral laneBatch :: Double
    victimT  = realToFrac (fromIntegral hkBatches * batch / hkRate) :: DiffTime
    killAt   = victimT * fromIntegral hkAt / 256
    -- everyone's bulk at the budget rate, twice over, the 2 s drain, a margin
    deadline = realToFrac (2 * fromIntegral (hkMuxes * hkBatches) * batch / hkRate) + 5

    run :: IOSim s (Maybe Int)
    run = do
      budget <- Bucket.newBucket hkRate (2 * laneBatch) Nothing
      let policy = Mx.EgressPolicy { Mx.egressBudget = budget, Mx.egressSlice = Nothing,
                                     Mx.egressLaneOf = Mx.directionSplit }
      pairs <- forM [0 .. hkMuxes - 1] $ \i -> do
        (serverMux, clientMux, serverBearer, clientBearer) <- headKillPair policy
        atomically $ Mx.setEgressRank serverMux (Bucket.Rank (if i == 0 then 0 else 1))
        serverA <- async (Mx.run serverMux serverBearer)
        _       <- async (Mx.run clientMux clientBearer)
        _ <- Mx.runMiniProtocol serverMux bulkNum Mx.ResponderDirection Mx.StartOnDemand
               (headKillServer bulk)
        done <- Mx.runMiniProtocol clientMux bulkNum Mx.InitiatorDirection Mx.StartEagerly
                  (headKillClient bulk)
        return (serverMux, serverA, done)
      case pairs of
           [] -> return Nothing
           (victimMux, victimA, _) : others -> do
             _ <- async $ do
               threadDelay killAt
               case hkHow of
                    StopMux   -> Mx.stop victimMux
                    CancelRun -> cancel victimA
             timeout deadline $
               length <$> mapM (\(_, _, done) -> atomically done) others

-- | A demoted peer's last message: a protocol sends it and completes while
-- the budget is busy with another connection's bulk, so the muxer holds the
-- batch waiting for its grant. As the connection manager does on release,
-- the connection's thread is cancelled the moment the protocol reports
-- completion; completion waits for the held batch to be written, so the
-- peer has the whole message. Or the mux is stopped as soon as the message
-- is sent, as on a timeout: the drain waits up to two seconds for the held
-- batch, and the bulk ahead of it takes at most one.
data FinalCase = FinalCase {
    fcRate    :: !Double,   -- ^ budget, bytes/s
    fcBatches :: !Int,      -- ^ the other connection's bulk, in batches
    fcFinal   :: !Int,      -- ^ the last message, bytes
    fcRank    :: !Word8,    -- ^ the demoted peer's tier
    fcHow     :: !FinalEnd
  }
  deriving Show

data FinalEnd = CancelOnCompletion | StopWhenSent
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary FinalCase where
  arbitrary = FinalCase <$> choose (1e5, 1e6) <*> choose (2, 20)
                        <*> choose (1, 20000) <*> choose (1, 4)
                        <*> elements [minBound .. maxBound]
  shrink fc@FinalCase { fcBatches, fcFinal } =
       [ fc { fcBatches = b } | b <- shrink fcBatches, b >= 2 ]
    ++ [ fc { fcFinal = f } | f <- shrink fcFinal, f >= 1 ]

prop_mux_final_message_before_release :: FinalCase -> Property
prop_mux_final_message_before_release fc@FinalCase { fcRate, fcBatches, fcFinal, fcRank, fcHow } =
    counterexample (show fc) $
    tabulate "end" [show fcHow] $
      runSimOrThrow run === Just fcFinal
  where
    -- stopped, the bulk ahead must clear within a second of the drain's two
    bulk     = case fcHow of
                    CancelOnCompletion -> fcBatches * laneBatch
                    StopWhenSent       -> min (fcBatches * laneBatch) (floor (fcRate * 0.5))
    deadline = realToFrac (2 * fromIntegral (bulk + fcFinal) / fcRate) + 5 :: DiffTime

    run :: IOSim s (Maybe Int)
    run = do
      budget <- Bucket.newBucket fcRate (2 * laneBatch) Nothing
      let policy = Mx.EgressPolicy { Mx.egressBudget = budget, Mx.egressSlice = Nothing,
                                     Mx.egressLaneOf = Mx.directionSplit }
      -- the busy connection: a local root taking the budget
      (busyMux, busyClient, busyBearer, busyClientBearer) <- headKillPair policy
      atomically $ Mx.setEgressRank busyMux (Bucket.Rank 0)
      _ <- async (Mx.run busyMux busyBearer)
      _ <- async (Mx.run busyClient busyClientBearer)
      _ <- Mx.runMiniProtocol busyMux bulkNum Mx.ResponderDirection Mx.StartOnDemand
             (headKillServer bulk)
      _ <- Mx.runMiniProtocol busyClient bulkNum Mx.InitiatorDirection Mx.StartEagerly
             (headKillClient bulk)
      -- the demoted peer: one request, one last message, done
      (peerMux, peerClient, peerBearer, peerClientBearer) <- headKillPair policy
      atomically $ Mx.setEgressRank peerMux (Bucket.Rank fcRank)
      peerA <- async (Mx.run peerMux peerBearer)
      _     <- async (Mx.run peerClient peerClientBearer)
      threadDelay 0.001     -- the bulk has the budget first
      sent <- newTVarIO False
      completion <- Mx.runMiniProtocol peerMux bulkNum Mx.ResponderDirection Mx.StartOnDemand $ \chan -> do
                      _ <- Mx.recv chan
                      Mx.send chan (BL.replicate (fromIntegral fcFinal) 0x78)
                      atomically (writeTVar sent True)
                      return ((), Nothing)
      received <- newTVarIO 0
      _ <- Mx.runMiniProtocol peerClient bulkNum Mx.InitiatorDirection Mx.StartEagerly $ \chan -> do
             Mx.send chan (BL8.pack "req")
             let go = do
                   mbs <- Mx.recv chan
                   case mbs of
                        Nothing -> return ()
                        Just bs -> atomically (modifyTVar received (+ fromIntegral (BL.length bs))) >> go
             go
             return ((), Nothing)
      case fcHow of
           -- the release: cancel the connection as soon as the protocol is done
           CancelOnCompletion -> do
             _ <- atomically completion
             cancel peerA
           -- a stop with the message sent, not yet written
           StopWhenSent -> do
             atomically (readTVar sent >>= check)
             Mx.stop peerMux
      timeout deadline $ atomically $ do
        n <- readTVar received
        check (n >= fcFinal)
        return n


bulkNum :: Mx.MiniProtocolNum
bulkNum = Mx.MiniProtocolNum 2

-- | A scheduled server mux and a plain client mux over a pair of Queues
-- bearers, each with the bulk protocol.
headKillPair :: Mx.EgressPolicy (IOSim s)
         -> IOSim s ( Mx.Mux Mx.InitiatorResponderMode (IOSim s)
                    , Mx.Mux Mx.InitiatorResponderMode (IOSim s)
                    , Mx.Bearer (IOSim s), Mx.Bearer (IOSim s) )
headKillPair policy = do
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    clientBearer <- getBearer makeQueueChannelBearer (-1)
                      QueueChannel { writeQueue = client_w, readQueue = client_r } Nothing
    serverBearer <- getBearer makeQueueChannelBearer (-1)
                      QueueChannel { writeQueue = client_r, readQueue = client_w } Nothing
    let info dir = MiniProtocolInfo {
            miniProtocolNum = bulkNum, miniProtocolDir = dir,
            miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = 16000000 },
            miniProtocolCapability = Nothing }
    serverMux <- Mx.newWithEgress policy Mx.nullTracers Nothing [info Mx.ResponderDirection]
    clientMux <- Mx.new Mx.nullTracers Nothing [info Mx.InitiatorDirection]
    return (serverMux, clientMux, serverBearer, clientBearer)

-- | One request, then @n@ bytes back in 8 kB messages.
headKillServer :: Int -> Mx.ByteChannel (IOSim s) -> IOSim s ((), Maybe BL.ByteString)
headKillServer n chan = do
    _ <- Mx.recv chan
    forM_ (chunksOf8k n) $ \k -> Mx.send chan (BL.replicate (fromIntegral k) 0x78)
    return ((), Nothing)
  where
    chunksOf8k m | m <= 0    = []
                 | otherwise = min 8192 m : chunksOf8k (m - 8192)

-- | Ask, then read until @n@ bytes have arrived.
headKillClient :: Int -> Mx.ByteChannel (IOSim s) -> IOSim s ((), Maybe BL.ByteString)
headKillClient n chan = do
    Mx.send chan (BL8.pack "req")
    let go m | m <= 0    = return ()
             | otherwise = do
                 mbs <- Mx.recv chan
                 case mbs of
                      Nothing -> return ()
                      Just bs -> go (m - fromIntegral (BL.length bs))
    go n
    return ((), Nothing)

--
-- Egress counters
--

-- | A snapshot loop over a budget that one bearer takes from and the direct
-- lane charges on credit.
data CountersCase = CountersCase {
    ccInterval :: !Integer,        -- ^ seconds
    ccRate     :: !Double,         -- ^ bytes/s
    ccTakes    :: ![(Integer, Int)],
      -- ^ milliseconds before each take, and its bytes
    ccCredits  :: ![(Integer, Int)]
      -- ^ milliseconds before each charge on credit, and its bytes
  }
  deriving Show

instance Arbitrary CountersCase where
    arbitrary = do
      interval <- choose (1, 10)
      rate     <- (* 1e4) <$> choose (1, 100)
      let step = (,) <$> choose (0, 500) <*> choose (1, 8192)
      CountersCase interval rate <$> listOf step <*> listOf step
    shrink cc@CountersCase { ccTakes, ccCredits } =
         [ cc { ccTakes = ts }   | ts <- shrinkList (const []) ccTakes ]
      ++ [ cc { ccCredits = cs } | cs <- shrinkList (const []) ccCredits ]

-- | Snapshots arrive exactly every interval; every counter only grows; and
-- once the traffic is over, the last snapshot holds exactly what was sent.
prop_egress_counters_loop :: CountersCase -> Property
prop_egress_counters_loop cc@CountersCase { ccInterval, ccRate, ccTakes, ccCredits } =
    counterexample (show cc) $
    counterexample (show snapshots) $
         counterexample "snapshot times"
           (map fst snapshots === [ (fromIntegral k * interval) `addTime` Time 0
                                  | k <- [1 .. length snapshots] ])
    .&&. counterexample "counters shrank" (and (zipWith grows cs (drop 1 cs)))
    .&&. counterexample "final counters"
           (fmap (\c -> (Mx.scScheduledBytes c, Mx.scScheduledBatches c, Mx.scDirectBytes c))
                 (lastMaybe cs)
              === Just (sum (map (fromIntegral . snd) ccTakes), fromIntegral (length ccTakes),
                        sum (map (fromIntegral . snd) ccCredits)) )
  where
    interval = fromIntegral ccInterval :: DiffTime
    -- time for all the traffic at the budget rate, and three snapshots after
    horizon  = realToFrac (fromIntegral (sum (map snd (ccTakes ++ ccCredits))) / ccRate)
             + fromIntegral (length (ccTakes ++ ccCredits)) * 0.5
             + 3 * interval + interval / 2

    cs = map snd snapshots
    lastMaybe xs = if null xs then Nothing else Just (last xs)

    grows a b =  Mx.scScheduledBytes a   <= Mx.scScheduledBytes b
              && Mx.scScheduledBatches a <= Mx.scScheduledBatches b
              && Mx.scDirectBytes a      <= Mx.scDirectBytes b
              && Mx.scWaitTokens a       <= Mx.scWaitTokens b
              && and (zipWith (<=) (map snd (Mx.scWaitsOver a)) (map snd (Mx.scWaitsOver b)))

    snapshots :: [(Time, Mx.SchedulingCounts)]
    snapshots = runSimOrThrow $ do
      budget <- Bucket.newBucket ccRate 16384 Nothing
      h      <- Bucket.registerBearer budget
      seen   <- newTVarIO []
      remote <- Counters.newCounters
      let tracer = mkTracer $ \ev -> case ev of
            Mx.TraceEgressCounters c -> do
              t <- getMonotonicTime
              atomically $ modifyTVar seen ((t, c) :)
            _ -> return ()
      _ <- async (Counters.countersLoop (Just remote) Nothing (Just (budget, Nothing)) interval tracer)
      _ <- async $ forM_ ccTakes $ \(ms, bytes) -> do
             threadDelay (fromIntegral ms / 1000)
             Bucket.awaitGrant h bytes
      _ <- async $ forM_ ccCredits $ \(ms, bytes) -> do
             threadDelay (fromIntegral ms / 1000)
             now <- getMonotonicTime
             atomically $ Bucket.takeOnCredit budget now bytes
      threadDelay horizon
      reverse <$> readTVarIO seen
