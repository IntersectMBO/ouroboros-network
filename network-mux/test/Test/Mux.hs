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
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8 (pack)
import Data.Functor.Identity (Identity (..))
import Data.List (dropWhileEnd, nub)
import Data.List qualified as List
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Tuple (swap)
import Data.Word
import Foreign.C.Error (Errno (..), eCONNRESET)
import Formatting (formatToString, (%), (%+))
import Formatting qualified as F
import GHC.IO.Exception (IOException (..))
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
    [ testProperty "each failure is counted once, where it belongs"
                   prop_mux_counters_failure
    , testProperty "the sets given are snapshotted every interval"
                   prop_mux_counters_loop
    , testProperty "a close after every mini-protocol finished counts nothing"
                   prop_mux_close_after_exit
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
        counts <- atomically (Mx.readMuxCounters counters)
        case res of
            Left e  ->
                case fromException e of
                    Just me ->
                      return $ case me of
                        -- The overrun is counted.
                        Mx.IngressQueueOverRun {} ->
                          counted counts Mx.noCounts { Mx.mcFailures = Mx.noFailures { Mx.mfOverruns = 1 } }
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
        counts <- atomically (Mx.readMuxCounters counters)
        case res of
            Left e  ->
                case fromException e of
                    Just me -> case err_m of
                                    Just err -> return $ counterexample (show me)
                                                       $ me `compareErrors` err
                                    Nothing  -> return $ counterexample (show me) False
                    Nothing -> return $ counterexample (show e) False
            -- A valid SDU counts nothing.
            Right _ -> return $ counterexample "expected an exception" (isNothing err_m)
                          .&&. counted counts Mx.noCounts

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
        counts <- atomically (Mx.readMuxCounters counters)
        case res of
            Left e  ->
                case fromException e of
                    -- A protocol error.
                    Just me -> return $ counterexample (show me) (me `compareErrors` err)
                                   .&&. counted counts Mx.noCounts { Mx.mcFailures = Mx.noFailures { Mx.mfProtocolErrors = 1 } }
                    Nothing -> return $ counterexample (show e) False
            Right _ -> return $ counterexample "expected an exception" False

    plainServer :: MiniProtocolInfo  mode
                -> (Mx.ByteChannel m -> m (a, Maybe BL.ByteString))
                -> m ( StrictTBQueue m BL.ByteString
                     , Async m ()
                     , STM m (Either SomeException a), Mx.Mux mode m
                     , Mx.MuxCounters m
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

        counters  <- Mx.newMuxCounters
        serverMux <- Mx.withCounters counters <$> Mx.new serverTracer [serverApp]
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

    -- The counters once the mux has ended.
    counted :: Mx.MuxCounts -> Mx.MuxCounts -> Property
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


-- | What a side's counters must show once the close experiment is over, by
-- what its mini-protocol saw.
data CountersJudgement =
    Exactly Mx.MuxFailures
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
    clientCounters <- Mx.newMuxCounters
    serverCounters <- Mx.newMuxCounters
    withAsync
      -- run client thread
      (bracket (Mx.withCounters clientCounters <$> Mx.new clientMuxTracer
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
          (bracket (Mx.withCounters serverCounters <$> Mx.new serverMuxTracer
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
            counts <- (,) <$> atomically (Mx.readMuxCounters clientCounters)
                          <*> atomically (Mx.readMuxCounters serverCounters)
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
    judged :: String -> CountersJudgement -> Mx.MuxCounts -> Property
    judged side judgement Mx.MuxCounts { Mx.mcFailures = failures } =
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
    -- on Windows, where the socket library reports it as an I/O error with
    -- neither an errno nor the vanished kind, as a read error.
    closesCounted :: Mx.MuxFailures -> Word64
    withoutCloses :: Mx.MuxFailures -> Mx.MuxFailures
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
    serverMux <- Mx.new tracers [sendProtocolInfo Mx.ResponderDirection]
    clientMux <- Mx.new Mx.nullTracers [sendProtocolInfo Mx.InitiatorDirection]
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
    clientMux <- Mx.new tracers [sendProtocolInfo Mx.InitiatorDirection]
    serverMux <- Mx.new Mx.nullTracers [sendProtocolInfo Mx.ResponderDirection]
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
-- lingering.
resetError :: IOException
resetError = (userError "connection reset by peer") { ioe_errno = Just reset }
  where Errno reset = eCONNRESET

-- | The ways a mux can end that the counters tell apart, and three they do
-- not count: the peer closing, or resetting, met by a read or a write
-- while no mini-protocol is running.
data MuxFailure = ReadTimeout | ReadError | ReadReset | ReadResetIdle | PeerClosed | PeerClosedIdle
                | DecodeError | UnknownProtocol | InitiatorOnlyData | Overrun | ReadOther
                | WriteTimeout | WriteError | WriteReset | WriteResetIdle | WriteOther
                | ProtocolFailure
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary MuxFailure where
    arbitrary = arbitraryBoundedEnum

-- | A mux that fails in the given way counts that failure, once, and nothing
-- else; a close with nothing running counts nothing. The protocol-level
-- failures come from real SDUs through the demuxer; the timeouts, the I/O
-- errors and the closes are thrown by the bearer, as the socket bearer does,
-- and so is an error of another kind, as an encoding that would not force
-- surfaces in a write; the mini-protocol failure is the initiator's own
-- exception.
prop_mux_counters_failure :: MuxFailure -> Property
prop_mux_counters_failure failure =
    counterexample (show failure) $
      runSimOrThrow run === Just (expected failure)
  where
    num :: Mx.MiniProtocolNum
    num = Mx.MiniProtocolNum 2

    expected :: MuxFailure -> Mx.MuxCounts
    expected f = case f of
      WriteTimeout      -> failed Mx.noFailures { Mx.mfWriteTimeouts = 1 }
      WriteError        -> failed Mx.noFailures { Mx.mfWriteErrors = 1 }
      WriteReset        -> failed Mx.noFailures { Mx.mfPeerClosed = 1 }
      WriteResetIdle    -> failed Mx.noFailures
      WriteOther        -> failed Mx.noFailures { Mx.mfOtherErrors = 1 }
      ReadTimeout       -> failed Mx.noFailures { Mx.mfReadTimeouts = 1 }
      ReadError         -> failed Mx.noFailures { Mx.mfReadErrors = 1 }
      ReadReset         -> failed Mx.noFailures { Mx.mfPeerClosed = 1 }
      ReadResetIdle     -> failed Mx.noFailures
      ReadOther         -> failed Mx.noFailures { Mx.mfOtherErrors = 1 }
      Overrun           -> failed Mx.noFailures { Mx.mfOverruns = 1 }
      DecodeError       -> failed Mx.noFailures { Mx.mfProtocolErrors = 1 }
      UnknownProtocol   -> failed Mx.noFailures { Mx.mfProtocolErrors = 1 }
      InitiatorOnlyData -> failed Mx.noFailures { Mx.mfProtocolErrors = 1 }
      PeerClosed        -> failed Mx.noFailures { Mx.mfPeerClosed = 1 }
      PeerClosedIdle    -> failed Mx.noFailures
      ProtocolFailure   -> failed Mx.noFailures
                             { Mx.mfMiniProtocols = M.singleton (num, Mx.InitiatorDir) 1 }

    failed :: Mx.MuxFailures -> Mx.MuxCounts
    failed f = Mx.noCounts { Mx.mcFailures = f }

    -- An SDU as the peer would send it.
    sdu :: Mx.MiniProtocolNum -> Mx.MiniProtocolDir -> BL.ByteString -> BL.ByteString
    sdu n dir payload = Mx.encodeSDU Mx.SDU {
        Mx.msHeader = Mx.SDUHeader { Mx.mhTimestamp = Mx.RemoteClockModel 0
                                   , Mx.mhNum       = n
                                   , Mx.mhDir       = dir
                                   , Mx.mhLength    = fromIntegral (BL.length payload) },
        Mx.msBlob   = payload }

    -- A bearer's I/O error, as the socket bearer wraps it.
    ioError' :: Mx.Error
    ioError' = Mx.IOException (userError "connection reset by peer") "recv errored"

    run :: forall s. IOSim s (Maybe Mx.MuxCounts)
    run = do
      counters <- Mx.newMuxCounters
      w <- atomically $ newTBQueue 10
      r <- atomically $ newTBQueue 10
      -- The initiator says when it runs, and a close the bearer throws waits
      -- for that, so the mux fails with a mini-protocol running.
      running <- newTVarIO False
      base <- getBearer makeQueueChannelBearer (-1)
                QueueChannel { writeQueue = w, readQueue = r } Nothing
      let info :: MiniProtocolInfo Mx.InitiatorMode
          info = MiniProtocolInfo {
              miniProtocolNum        = num,
              miniProtocolDir        = Mx.InitiatorDirectionOnly,
              miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 16 },
              miniProtocolCapability = Nothing }
      mux <- Mx.withCounters counters <$> Mx.new Mx.nullTracers [info]
      let onceRunning :: Mx.Error -> IOSim s a
          onceRunning e = atomically (readTVar running >>= check) >> throwIO e

          -- Once the initiator has finished and gone idle, as the write of
          -- its last data can find the peer gone.
          onceIdle :: Mx.Error -> IOSim s a
          onceIdle e = do
            atomically $ do
              status <- Mx.miniProtocolStateMap mux M.! (num, Mx.InitiatorDir)
              check (status == Mx.StatusIdle)
            throwIO e

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
            ReadTimeout    -> base { Mx.read      = \_ _   -> throwIO Mx.SDUReadTimeout }
            ReadError      -> base { Mx.read      = \_ _   -> throwIO ioError' }
            ReadReset      -> base { Mx.read      = \_ _   -> onceRunning readReset }
            ReadResetIdle  -> base { Mx.read      = \_ _   -> throwIO readReset }
            PeerClosed     -> base { Mx.read      = \_ _   -> onceRunning (Mx.BearerClosed "peer") }
            PeerClosedIdle -> base { Mx.read      = \_ _   -> throwIO (Mx.BearerClosed "peer") }
            ReadOther      -> base { Mx.read      = \_ _   -> throwIO otherKind }
            WriteTimeout   -> base { Mx.writeMany = \_ _ _ -> throwIO Mx.SDUWriteTimeout }
            WriteError     -> base { Mx.writeMany = \_ _ _ -> throwIO ioError' }
            WriteReset     -> base { Mx.writeMany = \_ _ _ -> onceRunning writeReset }
            WriteResetIdle -> base { Mx.writeMany = \_ _ _ -> onceIdle writeReset }
            WriteOther     -> base { Mx.writeMany = \_ _ _ -> throwIO otherKind }
            _              -> base
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
        case failure of
             DecodeError       -> fromPeer (BL.replicate 3 0)
             UnknownProtocol   -> fromPeer (sdu (Mx.MiniProtocolNum 99) Mx.ResponderDir x)
             -- Data from an initiator, for a responder this mux does not run.
             InitiatorOnlyData -> fromPeer (sdu num Mx.InitiatorDir x)
             Overrun           -> fromPeer (sdu num Mx.ResponderDir (BL.replicate 100 0))
             WriteTimeout      -> initiator (\chan -> Mx.send chan x)
             WriteError        -> initiator (\chan -> Mx.send chan x)
             WriteReset        -> initiator $ \chan ->
                                    Mx.send chan x >> nowRunning >> void (Mx.recv chan)
             WriteResetIdle    -> initiator (\chan -> Mx.send chan x)
             WriteOther        -> initiator (\chan -> Mx.send chan x)
             PeerClosed        -> initiator (\chan -> nowRunning >> void (Mx.recv chan))
             ReadReset         -> initiator (\chan -> nowRunning >> void (Mx.recv chan))
             ProtocolFailure   -> initiator (\_ -> throwIO protocolBroken)
             _                 -> return ()
        done <- timeout 10 (waitCatch muxA)
        case done of
             Nothing -> return Nothing
             Just _  -> Just <$> atomically (Mx.readMuxCounters counters)

-- | An interval for the loop, in whole seconds from five under its floor of
-- one second to ten above it.
newtype LoopInterval = LoopInterval Int
  deriving Show

instance Arbitrary LoopInterval where
    arbitrary = LoopInterval <$> choose (-5, 10)
    shrink (LoopInterval k) = map LoopInterval (shrink k)

-- | The loop traces the counter sets it is given every interval, a second
-- at least, the node-to-node one first, and the values it traces are the
-- counters at that instant; without a node-to-client set, one event per
-- interval. The first three rounds are judged off the simulation's trace,
-- so a loop that spins shows as events too early, not as a hang.
prop_mux_counters_loop :: LoopInterval -> Bool -> Property
prop_mux_counters_loop (LoopInterval k) withLocal =
    counterexample (show seen) $
         map fst seen === concat [ replicate (length perInterval) (roundAt i)
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
    failures :: Mx.MuxFailures -> Mx.MuxCounts
    failures f = Mx.noCounts { Mx.mcFailures = f }

    -- The first three rounds of events, each with the time it was traced at.
    seen :: [(Time, Mx.CountersTrace)]
    seen = take (3 * length perInterval) $ selectTraceEventsDynamicWithTime $ runSimTrace $ do
      remote <- Mx.newMuxCounters
      local  <- Mx.newMuxCounters
      atomically $ do
        Counters.countDemuxerFailure (Just remote) True (toException (Mx.BearerClosed "peer"))
        Counters.countMuxerFailure (Just local) True (toException Mx.SDUWriteTimeout)
      _ <- async (Mx.countersLoop (Just remote) (if withLocal then Just local else Nothing)
                                  interval (mkTracer traceM))
      threadDelay (3 * delay + delay / 2)

-- | An SDU from the peer on a protocol and direction, as it would send it.
peerSDU :: Mx.MiniProtocolNum -> Mx.MiniProtocolDir -> Int -> Mx.SDU
peerSDU num dir bytes = Mx.SDU {
    Mx.msHeader = Mx.SDUHeader { Mx.mhTimestamp = Mx.RemoteClockModel 0
                               , Mx.mhNum       = num
                               , Mx.mhDir       = dir
                               , Mx.mhLength    = fromIntegral bytes },
    Mx.msBlob   = BL.replicate (fromIntegral bytes) 0xaa }

-- | A peer closing after our responder has finished: the protocol's number
-- and the size of the message the responder answers.
data CloseCase = CloseCase Word16 Int
  deriving Show

instance Arbitrary CloseCase where
    arbitrary = CloseCase <$> elements [2, 3] <*> choose (1, 1000)
    shrink (CloseCase n bytes) = [ CloseCase n b | b <- shrink bytes, b >= 1 ]

-- | A close after every mini-protocol has finished is not a failure, in every
-- schedule: the responder, started on demand by the peer's message, answers
-- it and returns, and once it has and its status is idle the bearer closes.
-- Explored with IOSimPOR, so the close also lands between the responder's
-- exit and its job's deregistration: nothing is counted, and the mux ends
-- as stopped, in every schedule.
prop_mux_close_after_exit :: CloseCase -> Property
prop_mux_close_after_exit (CloseCase n bytes) =
  exploreSimTrace (\o -> o { explorationScheduleBound = 40 }) sim $ \_ trace ->
    case traceResult False trace of
      Left e  -> counterexample (show e) False
      Right r -> counterexample (show r) $ r === (Mx.noFailures, True)
  where
    num :: Mx.MiniProtocolNum
    num = Mx.MiniProtocolNum n

    sim :: forall s. IOSim s (Mx.MuxFailures, Bool)
    sim = do
      exploreRaces
      counters <- Mx.newMuxCounters
      -- The peer's message, delivered by the first read; a later read waits
      -- for the responder to have answered and gone idle, then closes.
      pending  <- newTVarIO (Just (peerSDU num Mx.InitiatorDir bytes))
      answered <- newTVarIO False
      let info :: MiniProtocolInfo Mx.ResponderMode
          info = MiniProtocolInfo {
              miniProtocolNum        = num,
              miniProtocolDir        = Mx.ResponderDirectionOnly,
              miniProtocolLimits     = MiniProtocolLimits { maximumIngressQueue = 4000 },
              miniProtocolCapability = Nothing }
      mux <- Mx.withCounters counters <$> Mx.new Mx.nullTracers [info]
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
          bearer = Mx.Bearer {
              Mx.write          = \_ _ _ -> getMonotonicTime,
              Mx.writeMany      = \_ _ _ -> getMonotonicTime,
              Mx.read           = \_ _ -> readNext,
              Mx.sduSize        = Mx.SDUSize 12288,
              Mx.batchSize      = 1000000,
              Mx.name           = "modelled",
              Mx.egressInterval = 0
            }
      _ <- Mx.runMiniProtocol mux num Mx.ResponderDirectionOnly Mx.StartOnDemand $ \chan -> do
             _ <- Mx.recv chan
             Mx.send chan (BL8.pack "ok")
             atomically (writeTVar answered True)
             return ((), Nothing)
      muxA  <- async (Mx.run mux bearer)
      ended <- waitCatch muxA
      failures <- Mx.mcFailures <$> atomically (Mx.readMuxCounters counters)
      return (failures, either (const False) (const True) ended)

