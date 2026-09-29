{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PackageImports             #-}
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
import Codec.Serialise (Serialise (..))
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Exception (ErrorCall (..))
import Control.Monad
import Data.Binary.Put qualified as Bin
import Data.Bits
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8 (pack)
import Data.List (dropWhileEnd, nub)
import Data.List qualified as List
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Tuple (swap)
import Data.Word
import System.Random.SplitMix qualified as SM
import Test.Cardano.Base.QuickCheck qualified as BaseQC
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
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

import Test.Mux.ReqResp

import Network.Mux (Mux)
import Network.Mux qualified as Mx
import Network.Mux.Bearer as Mx
import Network.Mux.Bearer.AttenuatedChannel as AttenuatedChannel
import Network.Mux.Bearer.Pipe qualified as Mx
import Network.Mux.Bearer.Queues as Mx
import Network.Mux.Codec qualified as Mx
import Network.Mux.Egress.Bucket qualified as Bucket
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
  , testGroup "Egress bucket"
    [ testProperty "grantAt: earliest instant"    prop_grantAt_ready
    , testProperty "grantAt: not late"            prop_grantAt_tight
    , testProperty "grantAt: takes what it grants" prop_grantAt_take
    , testProperty "grantAt: rate zero disables"  prop_grantAt_disabled
    , testProperty "wakeAt: within a microsecond" prop_wakeAt
    , testProperty "atRate: keeps the level"      prop_atRate
    , testProperty "rotatedRank: re-dealt each period" prop_rotatedRank
    , testProperty "queueRank: zero period is none"   prop_queueRank_zeroPeriod
    , testProperty "chargeAt: takes without waiting"  prop_chargeAt
    , testProperty "schedule matches the replay"  prop_bucket_schedule
    , testProperty "cancellation is safe"         prop_bucket_cancel
    , testProperty "disabled bucket never waits"  prop_bucket_disabled
    , testProperty "rate change takes effect"     prop_bucket_rate_change
    ]
  , testGroup "Egress lanes"
    [ testProperty "own requests never wait, the slice gets its share"
                   (BaseQC.withNumTests 30 prop_mux_lanes)
    , testProperty "a torn-down head hands the queue on"
                   prop_mux_egress_head_torn_down
    , testProperty "own requests wait at the gate, charged once written"
                   (BaseQC.withNumTests 30 prop_mux_direct_gate)
    , testProperty "counters are snapshotted every interval"
                   prop_egress_counters_loop
    ]
  , testGroup "Counters"
    [ testProperty "each failure is counted once, where it belongs"
                   prop_mux_counters_failure
    , testProperty "both sides are snapshotted every interval"
                   prop_mux_counters_loop
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
    show d = printf "DummyPayload %d\n" (BL.length $ unDummyPayload d)

-- |
-- Generate a byte string of a given size.
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
genLargeByteString chunkSize  size | chunkSize < size = do
  chunk <- genByteString chunkSize
  return $ BL.concat $
        replicate (size `div` chunkSize) chunk
      ++
        [BL.take (fromIntegral $ size `mod` chunkSize) chunk]
genLargeByteString _chunkSize size = genByteString size

-- |
-- Large Int values, but not too large, up to @1024*1024@.
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
    show a = printf "InvalidSDU 0x%08x 0x%04x 0x%04x 0x%04x 0x%02x\n"
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
                , (8, (DummyCapability . Just) <$> choose (0, 7))
                , (1, (DummyCapability . Just) <$> arbitrary)
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

        clientTracer' = contramap (Mx.WithBearer "client") activeTracer
        serverTracer' = contramap (Mx.WithBearer "server") activeTracer
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

    clientMux <- Mx.new clientTracer [clientApp]

    serverMux <- Mx.new serverTracer [serverApp]

    withAsync (Mx.run clientMux clientBearer) $ \clientAsync ->
      withAsync (Mx.run serverMux serverBearer) $ \serverAsync -> do

        r <- step clientMux clientApp serverMux serverApp messages
        Mx.stop serverMux
        Mx.stop clientMux
        wait serverAsync
        wait clientAsync
        return $ r

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

        clientTracer' = contramap (Mx.WithBearer "client") activeTracer
        serverTracer' = contramap (Mx.WithBearer "server") activeTracer
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


    clientMux <- Mx.new clientTracer clientApps
    clientAsync <- async $ Mx.run clientMux clientBearer

    serverMux <- Mx.new serverTracer serverApps
    serverAsync <- async $ Mx.run serverMux serverBearer

    r <- step clientMux clientApps serverMux serverApps messages
    Mx.stop clientMux
    Mx.stop serverMux
    wait serverAsync
    wait clientAsync
    return r

  where
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

        clientTracer' = contramap (Mx.WithBearer "client") activeTracer
        serverTracer' = contramap (Mx.WithBearer "server") activeTracer
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
      clientMux <- Mx.new clientTracer clientBundle
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
      serverMux <- Mx.new serverTracer serverBundle
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
    let clientTracer' = contramap (Mx.WithBearer "client") activeTracer
        serverTracer' = contramap (Mx.WithBearer "server") activeTracer
        clientTracer  = Mx.TracersI clientTracer' clientTracer' clientTracer'
        serverTracer  = Mx.TracersI serverTracer' serverTracer' serverTracer'
        protNum = [1..]
        respApps' = zip protNum respApps
        initApps' = zip protNum initApps

    respMux <- Mx.new serverTracer $ map (\(pn,_) ->
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

    initMux <- Mx.new clientTracer $ map (\(pn,_) ->
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

        clientTracer' = contramap (Mx.WithBearer "client") activeTracer
        serverTracer' = contramap (Mx.WithBearer "server") activeTracer
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

    serverMux <- Mx.new serverTracer [serverApp2, serverApp3]
    serverMux_aid <- async $ Mx.run serverMux serverBearer
    serverRes2 <- Mx.runMiniProtocol serverMux (miniProtocolNum serverApp2) (miniProtocolDir serverApp2)
                   Mx.StartOnDemand server_short
    serverRes3 <- Mx.runMiniProtocol serverMux (miniProtocolNum serverApp3) (miniProtocolDir serverApp3)
                   Mx.StartOnDemand server_long

    clientMux <- Mx.new clientTracer [clientApp2, clientApp3]
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

        (client_w, said, waitServerRes, mux) <- plainServer server_mps (serverRsp stopVar)

        writeSdu client_w $! unDummyPayload sdu

        atomically $! putTMVar stopVar $ unDummyPayload sdu

        _ <- atomically waitServerRes
        Mx.stop mux
        res <- waitCatch said
        case res of
            Left e  ->
                case fromException e of
                    Just me ->
                      return $ case me of
                        Mx.IngressQueueOverRun {} -> property True
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

        (client_w, said, waitServerRes, mux) <- plainServer server_mps (serverRsp stopVar)

        atomically $! putTMVar stopVar $! unDummyPayload sdu
        writeSdu client_w $ unDummyPayload sdu

        _ <- atomically waitServerRes
        Mx.stop mux
        res <- waitCatch said
        case res of
            Left e  ->
                case fromException e of
                    Just me -> case err_m of
                                    Just err -> return $ counterexample (show me)
                                                       $ me `compareErrors` err
                                    Nothing  -> return $ counterexample (show me) False
                    Nothing -> return $ counterexample (show e) False
            Right _ -> return $ counterexample "expected an exception" $ isNothing err_m

    run (ArbitraryInvalidSDU badSdu err) = do
        stopVar <- newEmptyTMVarIO

        let server_mps = MiniProtocolInfo {
                            miniProtocolNum = Mx.MiniProtocolNum 2,
                            miniProtocolDir = Mx.ResponderDirectionOnly,
                            miniProtocolLimits = defaultMiniProtocolLimits,
                            miniProtocolCapability = Nothing
                          }

        (client_w, said, waitServerRes, mux) <- plainServer server_mps (serverRsp stopVar)

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
        case res of
            Left e  ->
                case fromException e of
                    Just me -> return $ counterexample (show me)
                                      $ me `compareErrors` err
                    Nothing -> return $ counterexample (show e) False
            Right _ -> return $ counterexample "expected an exception" False

    plainServer serverApp server_mp = do
        server_w <- atomically $ newTBQueue 10
        server_r <- atomically $ newTBQueue 10

        let serverTracer' = contramap (Mx.WithBearer "server") activeTracer
            serverTracer  = Mx.TracersI serverTracer' serverTracer' serverTracer'

        serverBearer <- getBearer makeQueueChannelBearer
                          (-1)
                          QueueChannel { writeQueue = server_w,
                                         readQueue  = server_r
                                       }
                          Nothing

        serverMux <- Mx.new serverTracer [serverApp]
        serverRes <- Mx.runMiniProtocol serverMux (Mx.miniProtocolNum serverApp) (Mx.miniProtocolDir serverApp)
                 Mx.StartEagerly server_mp

        said <- async $ Mx.run serverMux serverBearer
        return (server_r, said, serverRes, serverMux)

    -- Server that expects to receive a specific ByteString.
    -- Doesn't send a reply.
    serverRsp stopVar chan =
        atomically (takeTMVar stopVar) >>= loop
      where
        loop e | e == BL.empty = return ((), Nothing)
        loop e = do
            msg_m <- Mx.recv chan
            case msg_m of
                 Just msg ->
                     case BL.stripPrefix msg e of
                          Just e' -> loop e'
                          Nothing -> error "recv corruption"
                 Nothing -> error "eof corruption"

    writeSdu _ payload | payload == BL.empty = return ()
    writeSdu queue payload = do
        let (!frag, !rest) = BL.splitAt 0xffff payload
            sdu' = Mx.SDU
                    (Mx.SDUHeader
                      (Mx.RemoteClockModel 0)
                      (Mx.MiniProtocolNum 2)
                      Mx.InitiatorDir
                      (fromIntegral $ BL.length frag))
                    frag
            !pkt = Mx.encodeSDU (sdu' :: Mx.SDU)

        atomically $ writeTBQueue queue pkt
        writeSdu queue rest

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
        nums <- listOf1 $ arbitrary
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
        nums <- listOf1 $ arbitrary
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
                                          (printf "%s ≰ %s" (show minRunTime) (show totTime))
                                          (minRunTime <= totTime)
                                        , r)
                      Right _ -> return (counterexample
                                          (printf "%s ≱ %s" (show minRunTime) (show totTime))
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

    mux <- Mx.new Mx.nullTracers minis
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

    mux <- Mx.new Mx.nullTracers minis
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

    mux <- Mx.new Mx.nullTracers $ initMinis ++ respMinis
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

    mux <- Mx.new Mx.nullTracers minis
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

    mux <- Mx.new muxVerboseTracer minis
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

    mux <- Mx.new muxVerboseTracer minis
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

    mux <- Mx.new muxVerboseTracer $ initMinis ++ respMinis
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
        printf "%s: %s: %s" (show wtatOccuredAt) (show wtatWithinThread) (show wtatEvent)

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
    withAsync
      -- run client thread
      (bracket (Mx.new clientMuxTracer
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
          (bracket ( Mx.new serverMuxTracer
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
            case (fault, resClient, resServer) of
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
              -- this fails on Windows for ~1% of cases
              (_, Right _, Left (Right serverError))
                 | iotest
                 , Just (Mx.Shutdown (Just e) _) <- fromException serverError
                 , Just Mx.IOException {} <- fromException e
                -> return $ label ("server-error: " ++ show fault) True
#endif

              (_, clientRes, serverRes) ->
                return $ counterexample (show fault)
                       $ counterexample ("Client: " ++ show clientRes)
                       $ counterexample ("Server: " ++ show serverRes)
                       $ False

  where
    collapsE :: Either a a -> a
    collapsE = either id id

    reassocE :: Either SomeException (Either SomeException a)
             -> Either (Either SomeException SomeException) a
    reassocE (Left e)          = Left (Left e)
    reassocE (Right (Left e))  = Left (Right e)
    reassocE (Right (Right a)) = Right a


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
    mux <- Mx.new Mx.nullTracers
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
    mux <- Mx.new Mx.nullTracers -- { Mx.tracer = Tracer Debug.traceShowM }
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
      return RotationCase { rcRotation = Bucket.Rotation seed period,
                            rcPeriod = p, rcOffsets = offs }

    shrink rc@RotationCase { rcRotation = Bucket.Rotation seed period, rcPeriod, rcOffsets } =
         [ rc { rcRotation = Bucket.Rotation 0 period } | seed /= 0 ]
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
prop_queueRank_zeroPeriod :: Word64 -> Word64 -> Word8 -> NonNegative Integer
                          -> Property
prop_queueRank_zeroPeriod seed bearer tier (NonNegative ps) =
    Bucket.queueRank (Just (Bucket.Rotation seed 0)) bearer (Bucket.Rank tier) t
      === Bucket.queueRank Nothing bearer (Bucket.Rank tier) t
  where
    t = picos ps `addTime` Time 0

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
    bbMore  :: ![(Integer, Int)],  -- ^ rounds after a grant, then bytes
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
        <*> vectorOf n (genBucketBearer rate cap n)

    shrink sch@BucketSched { schRate, schCapacity, schRotation, schSlice, schBearers } =
         [ sch { schBearers = bs }
         | bs <- shrinkList shrinkBearer schBearers, not (null bs) ]
      ++ [ sch { schSlice = Nothing } | Just _ <- [schSlice] ]
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

        shrinkTake (k, sz) =
             [ (0, sz)  | k /= 0 ]
          ++ [ (k, sz') | sz' <- [1024, schCapacity], sz' < sz ]

-- | Periods of a quarter to four fill times, so that schedules straddle a
-- boundary.
genRotation :: Double -> Int -> Gen Bucket.Rotation
genRotation rate cap = do
    seed <- arbitrary
    f    <- choose (0.25, 4 :: Double)
    return (Bucket.Rotation seed (realToFrac (f * fromIntegral cap / rate)))

-- | Few ranks, so ties are common.  Asking again at once, and starting in the
-- opening burst, are both weighted up: those are what make bearers queue.
genBucketBearer :: Double -> Int -> Int -> Gen BucketBearer
genBucketBearer rate cap n =
    BucketBearer <$> choose (0, 3)
                 <*> genRounds
                 <*> genBytes cap
                 <*> (do extra <- frequency [ (3, return 0), (4, choose (1, 3)) ]
                         vectorOf extra ((,) <$> genRounds <*> genBytes cap))
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
    arMore   :: ![(Integer, Int)],
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
      (k, bytes) : more -> Just a { arTake  = arTake a + 1
                                  , arBytes = bytes
                                  , arAt    = askAt n (arBearer a) k t
                                  , arMore  = more }

labelBucket :: BucketSched -> Property -> Property
labelBucket BucketSched { schRotation, schSlice, schBearers } =
      classify (any (not . null . bbMore) schBearers) "reuses a handle"
    . classify (isJust schRotation)                   "rotating"
    . classify (isJust schSlice)                      "with a slice"

-- | Grant instants from a real bucket in IOSim, by bearer and take.  Each
-- bearer keeps one handle for all of its takes.
runBucketSched :: BucketSched -> [((Int, Int), Time)]
runBucketSched = fst . runBucketSchedStats

-- | 'runBucketSched', with the budget's and the slice's counters at the end.
runBucketSchedStats :: BucketSched
                    -> ([((Int, Int), Time)], (Bucket.BucketStats, Maybe Bucket.BucketStats))
runBucketSchedStats sch@BucketSched { schRate, schCapacity, schRotation, schSlice, schBearers } =
    runSimOrThrow $ do
      bucket <- Bucket.newBucket schRate schCapacity schRotation
      slice  <- traverse (\pct -> Bucket.newBucket (schRate * fromIntegral pct / 100)
                                                  schCapacity Nothing) schSlice
      -- registered on the budget in schedule order, so bearer @i@ has id @i@,
      -- as the replay assumes; every bearer also has a slice handle if there
      -- is a slice
      hs <- mapM (const (Bucket.registerBearer bucket)) schBearers
      ss <- mapM (const (traverse Bucket.registerBearer slice)) schBearers
      grants <- forConcurrently (zip3 hs ss (arrivals sch)) $ \(h, s_m, a0) -> do
        atomically $ Bucket.setRank h (Bucket.Rank (arRank a0))
        forM_ s_m $ \s -> atomically $ Bucket.setRank s (Bucket.Rank (arRank a0))
        -- what the slice lane does: its own bucket, charged to the budget on
        -- credit, or the budget's idle capacity
        let grant a = case s_m of
              Just s | arSlice a -> void $ Bucket.awaitGrantBorrowing s bucket (arBytes a)
              _                  -> Bucket.awaitGrant h (arBytes a)
            takeAll a = do
              now <- getMonotonicTime
              threadDelay (arAt a `diffTime` now)
              grant a
              t <- getMonotonicTime
              (((arBearer a, arTake a), t) :)
                <$> maybe (return []) takeAll (nextArrival n a t)
        takeAll a0
      now <- getMonotonicTime
      stats <- atomically $ do
        (b, _, _) <- Bucket.bucketSnapshot bucket now
        s' <- traverse (\sl -> (\(st, _, _) -> st) <$> Bucket.bucketSnapshot sl now) slice
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

    -- the rank a request queues with, as each bucket computes it
    keyB a = ( Bucket.queueRank schRotation (fromIntegral (arBearer a))
                                (Bucket.Rank (arRank a)) (arAt a)
             , arAt a )
    keyS a = (Bucket.queueRank Nothing 0 (Bucket.Rank (arRank a)) (arAt a), arAt a)

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
      | Just h <- headBy keyB wb, Nothing <- kb =
          if crossed True then [RTie] else
          let (ready, fb') = Bucket.grantAt schRate schCapacity fb now (arBytes h)
              lst'         = Just (now, True)
          in if ready == now
                then RGrant FromBudget (grant h)
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
                   : go now (Bucket.chargeAt schRate fb now need) fs'
                        (next h now pending) wb (List.delete h ws) kb Nothing
                        (Just (now, False))
             else if idle && readyB == now
                then RGrant Borrowed (grant h)
                   : go now fb' fs (next h now pending) wb (List.delete h ws) kb Nothing
                        (Just (now, False))
             else [ RSleptOnBudget | idle, readyB < readyS ]
               ++ go now fb fs pending wb ws kb
                     (Just (Bucket.wakeAt now (if idle then min readyS readyB else readyS)))
                     (Just (now, False))
      -- a request before both wakes joins its own queue; a lower key takes
      -- that queue's head
      | p : ps <- pending, all (arAt p <) kb, all (arAt p <) ks =
          if arSlice p
             then let displaces = maybe False ((keyS p <) . keyS) (headBy keyS ws)
                  in go (arAt p) fb fs ps wb (p : ws) kb (if displaces then Nothing else ks) lst
             else let displaces = maybe False ((keyB p <) . keyB) (headBy keyB wb)
                  in go (arAt p) fb fs ps (p : wb) ws (if displaces then Nothing else kb) ks lst
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
    ( stats [ g | RGrant FromBudget g <- events ] []
    , stats [ g | RGrant pay g <- events, pay /= FromBudget ]
            [ g | RGrant Borrowed g <- events ] <$ schSlice )
  where
    events = replayEvents sch

    -- bytes of take @k@ of bearer @b@
    bytesOf (b, k) = let bb = schBearers !! b
                     in if k == 0 then bbFirst bb else snd (bbMore bb !! (k - 1))

    stats gs borrowed =
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
                                   | b <- Bucket.waitBounds ]
         }

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
prop_bucket_cancel' sch@BucketSched { schRate, schCapacity, schRotation, schBearers } =
    labelBucket sch
      $ classify (not (null killed))    "kills someone"
      $ classify (any killsHead killed) "kills the next to be served"
      $ counterexample ("kept " ++ show (length kept))
      $ outcome === Just (length kept)
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
                       | b <- schBearers, (k, _) <- bbMore b ])
          + realToFrac (4 * bytes / schRate) + 1
    bytes = sum [ fromIntegral (bbFirst b) + sum (map (fromIntegral . snd) (bbMore b))
                | b <- schBearers ] :: Double

    outcome = runSimOrThrow $ do
      bucket <- Bucket.newBucket schRate schCapacity schRotation
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

      fmap length <$> timeout limit
        (do mapM_ wait killers                        -- each cancel returns
            mapM wait [ asy | (Nothing, asy) <- as ]) -- survivors finish

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
    lrSlicedIdle :: !Int       -- ^ slice bytes delivered in that time
  }
  deriving Show

laneSduSize :: Mx.SDUSize
laneSduSize = Mx.SDUSize 12288

runLanes :: (Mx.MiniProtocolNum -> Mx.MiniProtocolDir -> Mx.Lane) -> LaneCase -> LaneRun
runLanes laneOf LaneCase { lcRate, lcSlicePct, lcBatches } = runSimOrThrow $ do
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

    let info :: Mx.MiniProtocolNum -> Mx.MiniProtocolDirection Mx.InitiatorResponderMode
             -> MiniProtocolInfo Mx.InitiatorResponderMode
        info num dir = MiniProtocolInfo {
            miniProtocolNum = num, miniProtocolDir = dir,
            miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = 16000000 },
            miniProtocolCapability = Nothing }
        bulkN = Mx.MiniProtocolNum 2
        probeN = Mx.MiniProtocolNum 3
        sliceN = Mx.MiniProtocolNum 4

        policy = Mx.EgressPolicy { Mx.egressBudget = budget, Mx.egressSlice = Just slice,
                                   Mx.egressLaneOf = laneOf }
    serverMux <- Mx.newWithEgress policy Mx.nullTracers
                   [ info bulkN Mx.ResponderDirection, info probeN Mx.InitiatorDirection
                   , info sliceN Mx.InitiatorDirection ]
    clientMux <- Mx.new Mx.nullTracers
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

        -- the slice stream: 4 kB messages as fast as the lane takes them
        sliceServer chan = do
          let loop = untilStopped $ Mx.send chan (BL.replicate 4096 0x74) >> loop
          loop
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

    withAsync (Mx.run serverMux serverBearer) $ \_ ->
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
        case r of
          Left e -> throwIO e
          Right (bulkTime, slicedIdle) ->
            return LaneRun { lrBulkTime = bulkTime, lrProbeMax = probeMax, lrProbes = probes,
                             lrSliced = sliced - slicedIdle,
                             lrIdle = realToFrac idlePhase, lrSlicedIdle = slicedIdle }
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
-- batches, which is the failure the lanes exist to prevent.
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
  where
    lanes     = runLanes laneRule lc
    control   = runLanes (\_ _ -> Mx.Scheduled) lc
    share     = fromIntegral lcSlicePct / 100
    slice     = share * lcRate
    bulkTime  = lrBulkTime lanes
    sliced    = fromIntegral (lrSliced lanes) :: Double
    batch     = fromIntegral laneBatch :: Double
    batchTime = batch / lcRate
    fluid     = fromIntegral lcBatches * batchTime
    cap       = 2 * batch                     -- the slice bucket's capacity, as 'runLanes' sizes it

    laneRule (Mx.MiniProtocolNum 4) Mx.InitiatorDir = Mx.Slice
    laneRule num dir                             = Mx.directionSplit num dir

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
          credited = (\(st, _, _) -> fromIntegral (Bucket.bsCredited st))
                       <$> (getMonotonicTime >>= atomically . Bucket.bucketSnapshot budget)
      mux <- Mx.newWithEgress policy Mx.nullTracers [info]
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
    serverMux <- Mx.newWithEgress policy Mx.nullTracers [info Mx.ResponderDirection]
    clientMux <- Mx.new Mx.nullTracers [info Mx.InitiatorDirection]
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
-- Counters
--

-- | The ways a mux can fail that the counters distinguish.
data MuxFailure = ReadTimeout | PeerClosed | DecodeError | UnknownProtocol
                | InitiatorOnlyData | Overrun | WriteTimeout | GateTimeout
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary MuxFailure where
    arbitrary = arbitraryBoundedEnum

-- | A mux that fails in the given way counts that failure, once, and nothing
-- else. The protocol-level failures come from real SDUs through the demuxer;
-- the timeouts and the close are thrown by the bearer, as the socket bearer
-- does.
prop_mux_counters_failure :: MuxFailure -> Property
prop_mux_counters_failure failure =
    counterexample (show failure) $
      runSimOrThrow run === Just (expected failure)
  where
    num = Mx.MiniProtocolNum 2

    expected :: MuxFailure -> (Mx.EgressCounts, Mx.IngressCounts)
    expected f = case f of
      WriteTimeout      -> (Mx.EgressCounts 1 0 Nothing, Mx.IngressCounts 0 0 0 0)
      -- a write timeout, at the gate of a scheduled mux
      GateTimeout       -> (Mx.EgressCounts 1 1 Nothing, Mx.IngressCounts 0 0 0 0)
      ReadTimeout       -> (Mx.EgressCounts 0 0 Nothing, Mx.IngressCounts 1 0 0 0)
      Overrun           -> (Mx.EgressCounts 0 0 Nothing, Mx.IngressCounts 0 1 0 0)
      DecodeError       -> (Mx.EgressCounts 0 0 Nothing, Mx.IngressCounts 0 0 1 0)
      UnknownProtocol   -> (Mx.EgressCounts 0 0 Nothing, Mx.IngressCounts 0 0 1 0)
      InitiatorOnlyData -> (Mx.EgressCounts 0 0 Nothing, Mx.IngressCounts 0 0 1 0)
      PeerClosed        -> (Mx.EgressCounts 0 0 Nothing, Mx.IngressCounts 0 0 0 1)

    -- an SDU as the peer would send it
    sdu n dir payload = Mx.encodeSDU Mx.SDU {
        Mx.msHeader = Mx.SDUHeader { Mx.mhTimestamp = Mx.RemoteClockModel 0
                                   , Mx.mhNum       = n
                                   , Mx.mhDir       = dir
                                   , Mx.mhLength    = fromIntegral (BL.length payload) },
        Mx.msBlob   = payload }

    run :: IOSim s (Maybe (Mx.EgressCounts, Mx.IngressCounts))
    run = do
      counters <- Mx.newMuxCounters
      w <- atomically $ newTBQueue 10
      r <- atomically $ newTBQueue 10
      base <- getBearer makeQueueChannelBearer (-1)
                QueueChannel { writeQueue = w, readQueue = r } Nothing
      let bearer = case failure of
            ReadTimeout  -> base { Mx.read      = \_ _   -> throwIO Mx.SDUReadTimeout }
            PeerClosed   -> base { Mx.read      = \_ _   -> throwIO (Mx.BearerClosed "peer") }
            WriteTimeout -> base { Mx.writeMany = \_ _ _ -> throwIO Mx.SDUWriteTimeout }
            GateTimeout  -> base { Mx.awaitWritable = \_ _ -> throwIO Mx.SDUWriteTimeout }
            _            -> base
          info :: MiniProtocolInfo Mx.InitiatorMode
          info = MiniProtocolInfo {
              miniProtocolNum        = num,
              miniProtocolDir        = Mx.InitiatorDirectionOnly,
              miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 16 },
              miniProtocolCapability = Nothing }
      budget <- Bucket.newBucket 1e9 65536 Nothing
      let policy = Mx.EgressPolicy { Mx.egressBudget = budget, Mx.egressSlice = Nothing,
                                     Mx.egressLaneOf = \_ _ -> Mx.Scheduled }
      mux <- Mx.withCounters counters <$>
               case failure of
                    GateTimeout -> Mx.newWithEgress policy Mx.nullTracers [info]
                    _           -> Mx.new Mx.nullTracers [info]
      withAsync (Mx.run mux bearer) $ \muxA -> do
        case failure of
             DecodeError       -> atomically $ writeTBQueue r (BL.replicate 3 0)
             UnknownProtocol   -> atomically $ writeTBQueue r (sdu (Mx.MiniProtocolNum 99) Mx.ResponderDir (BL8.pack "x"))
             -- data from an initiator, for a responder this mux does not run
             InitiatorOnlyData -> atomically $ writeTBQueue r (sdu num Mx.InitiatorDir (BL8.pack "x"))
             Overrun           -> atomically $ writeTBQueue r (sdu num Mx.ResponderDir (BL.replicate 100 0))
             _ | failure `elem` [WriteTimeout, GateTimeout] ->
               void $ Mx.runMiniProtocol mux num Mx.InitiatorDirectionOnly Mx.StartEagerly
                        (\chan -> Mx.send chan (BL8.pack "x") >> return ((), Nothing))
             _                 -> return ()
        done <- timeout 10 (waitCatch muxA)
        case done of
             Nothing -> return Nothing
             Just _  -> Just <$> atomically (Mx.readMuxCounters counters)

-- | The loop traces both sides of both counter sets every interval, and the
-- values it traces are the counters at that instant.
prop_mux_counters_loop :: Positive Int -> Property
prop_mux_counters_loop (Positive k) =
    counterexample (show seen) $
         map fst seen === concat [ replicate 4 ((fromIntegral i * interval) `addTime` Time 0)
                                 | i <- [1 .. 3 :: Int] ]
    .&&. map snd seen === concat (replicate 3
           [ Mx.TraceRemoteEgress (Mx.EgressCounts 0 0 Nothing)
           , Mx.TraceRemoteIngress (Mx.IngressCounts 0 0 0 1)
           , Mx.TraceLocalEgress (Mx.EgressCounts 1 0 Nothing)
           , Mx.TraceLocalIngress (Mx.IngressCounts 0 0 0 0) ])
  where
    interval = fromIntegral (1 + k `mod` 10) :: DiffTime

    seen :: [(Time, Mx.CountersTrace)]
    seen = runSimOrThrow $ do
      remote <- Mx.newMuxCounters
      local  <- Mx.newMuxCounters
      atomically $ do
        Mx.countDemuxerFailure remote (toException (Mx.BearerClosed "peer"))
        Mx.countMuxerFailure local (toException Mx.SDUWriteTimeout)
      v <- newTVarIO []
      let tracer = mkTracer $ \ev -> do
            t <- getMonotonicTime
            atomically $ modifyTVar v ((t, ev) :)
      _ <- async (Mx.countersLoop remote local Nothing interval tracer)
      threadDelay (3 * interval + interval / 2)
      reverse <$> readTVarIO v
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
      remote <- Mx.newMuxCounters
      local  <- Mx.newMuxCounters
      let tracer = mkTracer $ \ev -> case ev of
            Mx.TraceRemoteEgress Mx.EgressCounts { Mx.ecScheduling = Just c } -> do
              t <- getMonotonicTime
              atomically $ modifyTVar seen ((t, c) :)
            _ -> return ()
      _ <- async (Mx.countersLoop remote local (Just (budget, Nothing)) interval tracer)
      _ <- async $ forM_ ccTakes $ \(ms, bytes) -> do
             threadDelay (fromIntegral ms / 1000)
             Bucket.awaitGrant h bytes
      _ <- async $ forM_ ccCredits $ \(ms, bytes) -> do
             threadDelay (fromIntegral ms / 1000)
             now <- getMonotonicTime
             atomically $ Bucket.takeOnCredit budget now bytes
      threadDelay horizon
      reverse <$> readTVarIO seen
