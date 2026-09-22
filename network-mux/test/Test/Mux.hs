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
import Data.Maybe (fromMaybe, isNothing)
import Data.Tuple (swap)
import Data.Word
import Formatting (formatToString, (%), (%+))
import Formatting qualified as F
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
    , testProperty "schedule matches the replay"  prop_bucket_schedule
    , testProperty "cancellation is safe"         prop_bucket_cancel
    , testProperty "disabled bucket never waits"  prop_bucket_disabled
    , testProperty "rate change takes effect"     prop_bucket_rate_change
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

    clientMux <- Mx.new clientTracer [clientApp]

    serverMux <- Mx.new serverTracer [serverApp]

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
    let clientTracer' = contramap (Mx.WithBearer ("client" :: String)) activeTracer
        serverTracer' = contramap (Mx.WithBearer ("server" :: String)) activeTracer
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

    plainServer :: MiniProtocolInfo  mode
                -> (Mx.ByteChannel m -> m (a, Maybe BL.ByteString))
                -> m ( StrictTBQueue m BL.ByteString
                     , Async m ()
                     , STM m (Either SomeException a), Mx.Mux mode m
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

        serverMux <- Mx.new serverTracer [serverApp]
        serverRes <- Mx.runMiniProtocol serverMux
                                        (Mx.miniProtocolNum serverApp)
                                        (Mx.miniProtocolDir serverApp)
                                        Mx.StartEagerly
                                        server_mp

        said <- async $ Mx.run serverMux serverBearer
        return (server_r, said, serverRes, serverMux)

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
    writeSdu _ payload | BL.null payload = return ()
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

--
-- Queue
--

-- | One bearer: its rank, when it first asks, and the takes it makes -- all
-- on one handle, as the muxer does, each one asking again after its previous
-- grant.  Times are counted in rounds, a round being one picosecond per bearer
-- in the run; see 'askAt'.
data BucketBearer = BucketBearer {
    bbRank  :: !Word32,
    bbStart :: !Integer,           -- ^ rounds before the first request
    bbFirst :: !Int,               -- ^ bytes of the first take
    bbMore  :: ![(Integer, Int)],  -- ^ rounds after a grant, then bytes
    bbKill  :: !(Maybe Int)        -- ^ cancel it this far, in 256ths, into
                                   --   the first wait the replay gives it
  }
  deriving (Eq, Show)

-- | A bucket and the bearers taking from it.
data BucketSched = BucketSched {
    schRate     :: !Double,        -- ^ bytes/s
    schCapacity :: !Int,           -- ^ bytes
    schBearers  :: ![BucketBearer]
  }
  deriving Show

instance Arbitrary BucketSched where
    arbitrary = do
      rate <- genRate
      cap  <- genCapacity
      n    <- choose (1, 12)
      BucketSched rate cap <$> vectorOf n (genBucketBearer rate cap n)

    shrink sch@BucketSched { schRate, schCapacity, schBearers } =
         [ sch { schBearers = bs }
         | bs <- shrinkList shrinkBearer schBearers, not (null bs) ]
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

        shrinkTake (k, sz) =
             [ (0, sz)  | k /= 0 ]
          ++ [ (k, sz') | sz' <- [1024, schCapacity], sz' < sz ]

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
    arRank   :: !Word32,
    arBytes  :: !Int,
    arAt     :: !Time,
    arMore   :: ![(Integer, Int)]
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
arrivals BucketSched { schBearers } =
    [ Arrival { arBearer = i, arTake = 0, arRank = bbRank, arBytes = bbFirst,
                arAt = askAt n i bbStart (Time 0), arMore = bbMore }
    | (i, BucketBearer { bbRank, bbStart, bbFirst, bbMore }) <- zip [0 ..] schBearers
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
labelBucket BucketSched { schBearers } =
    classify (any (not . null . bbMore) schBearers) "reuses a handle"

-- | Grant instants from a real bucket in IOSim, by bearer and take.  Each
-- bearer keeps one handle for all of its takes.
runBucketSched :: BucketSched -> [((Int, Int), Time)]
runBucketSched sch@BucketSched { schRate, schCapacity, schBearers } =
    concat $ runSimOrThrow $ do
      bucket <- Bucket.newBucket schRate schCapacity
      forConcurrently (arrivals sch) $ \a0 -> do
        h <- Bucket.registerBearer bucket
        atomically $ Bucket.setRank h (Bucket.Rank (arRank a0))
        let takeAll a = do
              now <- getMonotonicTime
              threadDelay (arAt a `diffTime` now)
              Bucket.awaitGrant h (arBytes a)
              t <- getMonotonicTime
              (((arBearer a, arTake a), t) :)
                <$> maybe (return []) takeAll (nextArrival n a t)
        takeAll a0
  where
    n = length schBearers

-- | Grant instants from the pure core, with each take's request instant
-- alongside: the head of the queue is the lowest (rank, request); it takes at
-- once if its bytes are there, else sleeps until 'Bucket.wakeAt' -- unless a
-- lower key arrives first and takes the head.  A granted bearer re-joins with
-- its next take.
replayRun :: BucketSched -> [((Int, Int), (Time, Time))]
replayRun sch@BucketSched { schRate, schCapacity, schBearers } =
    go (Time 0) (Time 0) (List.sortOn arAt (arrivals sch)) [] Nothing
  where
    n = length schBearers

    key a = (arRank a, arAt a)

    headOf waiting = case List.sortOn key waiting of
                          []    -> Nothing
                          h : _ -> Just h

    enqueue = List.insertBy (\x y -> compare (arAt x) (arAt y))

    -- pending: not yet requested, by request instant
    -- waiting: queued; the head is the lowest key
    -- wake:    when a short head checks again
    go now full pending waiting wake
      -- a head that has not checked yet checks now
      | Just h <- headOf waiting, Nothing <- wake =
          let (ready, full') = Bucket.grantAt schRate schCapacity full now
                                              (arBytes h)
          in if ready == now
                then ((arBearer h, arTake h), (arAt h, now))
                   : go now full'
                        (maybe pending (`enqueue` pending) (nextArrival n h now))
                        (List.delete h waiting) Nothing
                else go now full pending waiting (Just (Bucket.wakeAt now ready))
      -- a request before the head wakes; a lower key takes the head
      | p : ps <- pending, all (arAt p <) wake =
          let displaces = maybe False ((key p <) . key) (headOf waiting)
          in go (arAt p) full ps (p : waiting) (if displaces then Nothing else wake)
      -- the head wakes and takes
      | Just w <- wake = go w full pending waiting Nothing
      | otherwise = []

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
prop_bucket_schedule sch =
    labelBucket sch $
    classify (queued == 0) "nothing queued" $
      List.sortOn fst (runBucketSched sch) === List.sortOn fst (replaySched sch)
  where
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
prop_bucket_cancel sch@BucketSched { schRate, schCapacity, schBearers } =
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
      bucket <- Bucket.newBucket schRate schCapacity
      as <- forM plan $ \(a0, kill) -> do
        h <- Bucket.registerBearer bucket
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
      bucket <- Bucket.newBucket schRate schCapacity
      h      <- Bucket.registerBearer bucket
      Bucket.awaitGrant h schCapacity        -- drains the full bucket
      t0 <- getMonotonicTime
      atomically $ Bucket.setBucketRate bucket t0 newRate
      replicateM_ n (Bucket.awaitGrant h schCapacity)
      t1 <- getMonotonicTime
      return (realToFrac (t1 `diffTime` t0) :: Double)
