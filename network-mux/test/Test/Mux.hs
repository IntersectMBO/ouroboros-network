{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

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
import Control.Monad
import Data.Binary.Put qualified as Bin
import Data.Bits
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8 (pack)
import Data.List (dropWhileEnd, nub)
import Data.List qualified as List
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Tuple (swap)
import Data.Word
import System.Random.SplitMix qualified as SM
import Test.QuickCheck hiding ((.&.))
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
import System.Win32.NamedPipes qualified as Win32.NamedPipes
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
import Network.Mux.Types (MiniProtocolInfo (..), MiniProtocolLimits (..))
import Network.Mux.Types qualified as Mx
import Network.Socket qualified as Socket
import Text.Show.Functions ()
-- import qualified Debug.Trace as Debug

tests :: TestTree
tests =
  testGroup "Mux"
  [ testProperty "mux send receive"        prop_mux_snd_recv
  , testProperty "mux send receive bidir"  prop_mux_snd_recv_bi
  , testProperty "mux send receive compat" prop_mux_snd_recv_compat
  , testProperty "1 miniprotocol Queue"    (withMaxSuccess 50 prop_mux_1_mini_Queue)
  , testProperty "2 miniprotocols Queue"   (withMaxSuccess 50 prop_mux_2_minis_Queue)
  , testProperty "1 miniprotocol Pipe"     (withMaxSuccess 50 prop_mux_1_mini_Pipe)
  , testProperty "2 miniprotocols Pipe"    (withMaxSuccess 50 prop_mux_2_minis_Pipe)
  , testProperty "starvation"              prop_mux_starvation
  , testProperty "demuxing (Sim)"          prop_demux_sdu_sim
  , testProperty "demuxing (IO)"           prop_demux_sdu_io
  , testProperty "mux start and stop"      prop_mux_start
  , testProperty "mux restart"             prop_mux_restart
  , testProperty "mux close (Sim)"         prop_mux_close_sim
  , testProperty "mux close (IO)"          (withMaxSuccess 50 prop_mux_close_io)
  , testGroup "Generators"
    [ testProperty "genByteString"         prop_arbitrary_genByteString
    , testProperty "genLargeByteString"    prop_arbitrary_genLargeByteString
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
--activeTracer = showTracing _sayTracer

_sayTracer :: MonadSay m => Tracer m String
_sayTracer = Tracer say

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

prop_arbitrary_genByteString :: (NonNegative (Small Int)) -> Property
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
                      , invalidLength
                      , validSdu
                      , tooLargeSdu
                      ]
      where
        hdrLength = 8

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
            len <- arbitrary
            p <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) (mid .|. mode) len
                                          (hdrLength + fromIntegral len) p)
                                         (Mx.Shutdown (Just . toException $ Mx.UnknownMiniProtocol (Mx.MiniProtocolNum 0)) (Mx.Failed (toException $ Mx.UnknownMiniProtocol (Mx.MiniProtocolNum 0))))
        invalidLength = do
            ts  <- arbitrary
            mid <- arbitrary
            realLen <- choose (1, hdrLength)
            p <- arbitrary

            if realLen < hdrLength
              then return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) mid 0 realLen p)
                              (Mx.Shutdown (Just . toException $ Mx.SDUDecodeError "") (Mx.Failed (toException $ Mx.SDUDecodeError "") ))
              else return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) mid 0 realLen p)
                              (Mx.Shutdown (Just . toException $ Mx.SDUDecodeError "short sdu") (Mx.Failed (toException $ Mx.SDUDecodeError "short sdu")))

instance Arbitrary Mx.BearerState where
     arbitrary = elements [Mx.Mature, Mx.Dead]



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
                         clientTracer
                         QueueChannel { writeQueue = client_w, readQueue = client_r }
        serverBearer = queueChannelAsBearer
                         sduLen
                         serverTracer
                         QueueChannel { writeQueue = server_w, readQueue = server_r }

        clientTracer = contramap (Mx.WithBearer "client") activeTracer
        serverTracer = contramap (Mx.WithBearer "server") activeTracer

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

    clientMux <- Mx.new [clientApp]

    serverMux <- Mx.new [serverApp]

    withAsync (Mx.run clientTracer clientMux clientBearer) $ \clientAsync ->
      withAsync (Mx.run serverTracer serverMux serverBearer) $ \serverAsync -> do

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
                     -> Property
prop_mux_snd_recv_bi (DummyRun messages) = ioProperty $ do
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10

    let server_w = client_r
        server_r = client_w

        clientTracer = contramap (Mx.WithBearer "client") activeTracer
        serverTracer = contramap (Mx.WithBearer "server") activeTracer

    clientBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      clientTracer
                      QueueChannel { writeQueue = client_w, readQueue = client_r }
    serverBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      serverTracer
                      QueueChannel { writeQueue = server_w, readQueue = server_r }

    let clientApps = [ MiniProtocolInfo {
                        miniProtocolNum = Mx.MiniProtocolNum 2,
                        miniProtocolDir = Mx.InitiatorDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits,
                        miniProtocolCapability = Nothing
                       }
                     , MiniProtocolInfo {
                        miniProtocolNum = Mx.MiniProtocolNum 2,
                        miniProtocolDir = Mx.ResponderDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits,
                        miniProtocolCapability = Nothing
                      }
                     ]

        serverApps = [ MiniProtocolInfo {
                        miniProtocolNum = Mx.MiniProtocolNum 2,
                        miniProtocolDir = Mx.ResponderDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits,
                        miniProtocolCapability = Nothing
                       }
                     , MiniProtocolInfo {
                        miniProtocolNum = Mx.MiniProtocolNum 2,
                        miniProtocolDir = Mx.InitiatorDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits,
                        miniProtocolCapability = Nothing
                       }
                     ]


    clientMux <- Mx.new clientApps
    clientAsync <- async $ Mx.run clientTracer clientMux clientBearer

    serverMux <- Mx.new serverApps
    serverAsync <- async $ Mx.run serverTracer serverMux serverBearer

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
    endMpsVar <- atomically $ newTVar 2

    let server_w = client_r
        server_r = client_w

        clientTracer = contramap (Mx.WithBearer "client") activeTracer
        serverTracer = contramap (Mx.WithBearer "server") activeTracer


    clientBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      clientTracer
                      QueueChannel { writeQueue = client_w, readQueue = client_r }
    serverBearer <- getBearer makeQueueChannelBearer
                     (-1)
                     serverTracer
                     QueueChannel { writeQueue = server_w, readQueue = server_r }
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
      clientMux <- Mx.new clientBundle
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
      withAsync (Mx.run clientTracer clientMux clientBearer) $ \aid -> do
        _ <- atomically res
        Mx.stop clientMux
        wait aid

    serverAsync <- async $ do
      serverMux <- Mx.new serverBundle
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
      withAsync (Mx.run serverTracer serverMux serverBearer) $ \aid -> do
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
        (result, trailing) <- runClient nullTracer clientChan (reqRespClient requests)
        atomically (putTMVar clientResultVar result)
        (,trailing) <$> end

    serverApp :: StrictTMVar IO Bool
              -> Mx.ByteChannel IO
              -> IO ((), Maybe BL.ByteString)
    serverApp serverResultVar serverChan = do
        (result, trailing) <- runServer nullTracer serverChan (reqRespServer responses)
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
    clientApp clientChan = runClient nullTracer clientChan (reqRespClient requests)

    serverApp :: Mx.ByteChannel IO
              -> IO (Bool, Maybe BL.ByteString)
    serverApp serverChan = runServer nullTracer serverChan (reqRespServer responses)

--
-- Running with queues and pipes
--

-- Run applications continuation
type RunMuxApplications
    =  [Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)]
    -> [Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)]
    -> IO Bool


runMuxApplication :: [Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)]
                  -> Mx.Bearer IO
                  -> [Mx.ByteChannel IO -> IO (Bool, Maybe BL.ByteString)]
                  -> Mx.Bearer IO
                  -> IO Bool
runMuxApplication initApps initBearer respApps respBearer = do
    let clientTracer = contramap (Mx.WithBearer "client") activeTracer
        serverTracer = contramap (Mx.WithBearer "server") activeTracer
        protNum = [1..]
        respApps' = zip protNum respApps
        initApps' = zip protNum initApps

    respMux <- Mx.new $ map (\(pn,_) ->
          MiniProtocolInfo {
            miniProtocolNum        = Mx.MiniProtocolNum pn,
            miniProtocolDir        = Mx.ResponderDirectionOnly,
            miniProtocolLimits     = defaultMiniProtocolLimits,
            miniProtocolCapability = Nothing
          }
        )
        respApps'
    respAsync <- async $ Mx.run serverTracer respMux respBearer
    getRespRes <- sequence [ Mx.runMiniProtocol
                              respMux
                              (Mx.MiniProtocolNum pn)
                              Mx.ResponderDirectionOnly
                              Mx.StartOnDemand
                              app
                           | (pn, app) <- respApps'
                           ]

    initMux <- Mx.new $ map (\(pn,_) ->
          MiniProtocolInfo {
            miniProtocolNum        = Mx.MiniProtocolNum pn,
            miniProtocolDir        = Mx.InitiatorDirectionOnly,
            miniProtocolLimits     = defaultMiniProtocolLimits,
            miniProtocolCapability = Nothing
          }
        )
        initApps'
    initAsync <- async $ Mx.run clientTracer initMux initBearer
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

runWithQueues :: RunMuxApplications
runWithQueues initApps respApps = do
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    let server_w = client_r
        server_r = client_w

        clientTracer = contramap (Mx.WithBearer "client") activeTracer
        serverTracer = contramap (Mx.WithBearer "server") activeTracer

    clientBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      clientTracer
                      QueueChannel { writeQueue = client_w, readQueue = client_r }
    serverBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      serverTracer
                      QueueChannel { writeQueue = server_w, readQueue = server_r }
    runMuxApplication initApps clientBearer respApps serverBearer

runWithPipe :: RunMuxApplications
runWithPipe initApps respApps =
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

             clientBearer <- getBearer makePipeChannelBearer (-1) clientTracer clientChannel
             serverBearer <- getBearer makePipeChannelBearer (-1) serverTracer serverChannel

             Win32.Async.connectNamedPipe hSrv
             runMuxApplication initApps clientBearer respApps serverBearer
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

        clientBearer <- getBearer makePipeChannelBearer (-1) clientTracer clientChannel
        serverBearer <- getBearer makePipeChannelBearer (-1) serverTracer serverChannel
        runMuxApplication initApps clientBearer respApps serverBearer

#endif
  where
    clientTracer = contramap (Mx.WithBearer "client") activeTracer
    serverTracer = contramap (Mx.WithBearer "server") activeTracer


-- | Verify that it is possible to run two miniprotocols over the same bearer.
-- Makes sure that messages are delivered to the correct miniprotocol in order.
--
test_mux_1_mini :: RunMuxApplications
                -> DummyTrace
                -> IO Bool
test_mux_1_mini run msgTrace = do
    (clientApp, serverApp) <- setupMiniReqRsp (return ()) msgTrace
    run [clientApp] [serverApp]


prop_mux_1_mini_Queue :: DummyTrace -> Property
prop_mux_1_mini_Queue = ioProperty . test_mux_1_mini runWithQueues

prop_mux_1_mini_Pipe :: DummyTrace -> Property
prop_mux_1_mini_Pipe = ioProperty . test_mux_1_mini runWithPipe

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


prop_mux_2_minis_Queue :: DummyTrace
                       -> DummyTrace
                       -> Property
prop_mux_2_minis_Queue a b = ioProperty $ test_mux_2_minis runWithQueues a b

prop_mux_2_minis_Pipe :: DummyTrace
                      -> DummyTrace
                      -> Property
prop_mux_2_minis_Pipe a b = ioProperty $ test_mux_2_minis runWithPipe a b


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
    activeMpsVar <- atomically $ newTVar 0
    traceHeaderVar <- newTVarIO []
    let headerTracer =
          Tracer $ \e -> case e of
            Mx.TraceRecvHeaderEnd header
              -> atomically (modifyTVar traceHeaderVar (header:))
            _ -> return ()

    let server_w = client_r
        server_r = client_w

        clientTracer = contramap (Mx.WithBearer "client") activeTracer
        serverTracer = contramap (Mx.WithBearer "server") activeTracer


    clientBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      clientTracer
                      QueueChannel { writeQueue = client_w, readQueue = client_r }
    serverBearer <- getBearer makeQueueChannelBearer
                      (-1)
                      serverTracer
                      QueueChannel { writeQueue = server_w, readQueue = server_r }
    (client_short, server_short) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                         $ DummyTrace [(request, response1)]
    (client_long, server_long) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                         $ DummyTrace [(request, response1)]


    let clientApp2 = MiniProtocolInfo {
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

    serverMux <- Mx.new [serverApp2, serverApp3]
    serverMux_aid <- async $ Mx.run serverTracer serverMux serverBearer
    serverRes2 <- Mx.runMiniProtocol serverMux (miniProtocolNum serverApp2) (miniProtocolDir serverApp2)
                   Mx.StartOnDemand server_short
    serverRes3 <- Mx.runMiniProtocol serverMux (miniProtocolNum serverApp3) (miniProtocolDir serverApp3)
                   Mx.StartOnDemand server_long

    clientMux <- Mx.new [clientApp2, clientApp3]
    clientMux_aid <- async $ Mx.run (clientTracer <> headerTracer) clientMux clientBearer
    clientRes2 <- Mx.runMiniProtocol clientMux (miniProtocolNum clientApp2) (miniProtocolDir clientApp2)
                   Mx.StartEagerly client_short
    clientRes3 <- Mx.runMiniProtocol clientMux (miniProtocolNum clientApp3) (miniProtocolDir clientApp3)
                   Mx.StartEagerly client_long


    -- Fetch results
    srvRes2 <- atomically serverRes2
    srvRes3 <- atomically serverRes3
    cliRes2 <- atomically clientRes2
    cliRes3 <- atomically clientRes3

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
                  MonadAsync m
               => ArbitrarySDU
               -> Mx.Bearer m
               -> StrictTMVar m (MiniProtocolInfo Mx.ResponderMode)
               -> m (Either SomeException () -> Property)
prop_demux_sdu a Mx.Bearer { write } signalServer = do
    r <- run a
    return $   tabulate "SDU type" [stateLabel a]
             . tabulate "SDU Violation " [violationLabel a]
             . r
  where
    run (ArbitraryValidSDU sdu (Just Mx.IngressQueueOverRun {})) = do
        -- To trigger MuxIngressQueueOverRun we use a special test protocol
        -- with an ingress queue which is less than 0xffff so that it can be
        -- triggered by a single segment.
        let server_mps = MiniProtocolInfo {
                           miniProtocolNum = Mx.MiniProtocolNum 2,
                           miniProtocolDir = Mx.ResponderDirectionOnly,
                           miniProtocolLimits = smallMiniProtocolLimits,
                           miniProtocolCapability = Nothing
                         }
        atomically $ writeTMVar signalServer server_mps
        writeSdu $ unDummyPayload sdu
        return \case
                 Left (fromException -> Just (Mx.Shutdown (Just e) status))
                   | Mx.Failed (fromException -> Just (Mx.IngressQueueOverRun {})) <- status -> property True
                   | otherwise -> counterexample (show e) False
                 Left e -> counterexample (show e) False
                 Right _ -> counterexample "expected an exception" False

    run (ArbitraryValidSDU sdu err_m) = do
      let server_mps = MiniProtocolInfo {
                          miniProtocolNum = Mx.MiniProtocolNum 2,
                          miniProtocolDir = Mx.ResponderDirectionOnly,
                          miniProtocolLimits = defaultMiniProtocolLimits,
                          miniProtocolCapability = Nothing
                        }
      atomically $ writeTMVar signalServer server_mps
      writeSdu $ unDummyPayload sdu
      return \case
                Left (fromException -> Just me)
                  | Just err <- err_m ->
                      counterexample (show me <> " /= " <> show err) $ me `compareErrors` err
                  | Nothing <- err_m -> counterexample (show me <> " /= Nothing") False
                Left other -> counterexample ("protocolAction exception: " <> show other) False
                Right _ -> counterexample "expected an exception" $ isNothing err_m

    run (ArbitraryInvalidSDU badSdu err) = do
      let server_mps = MiniProtocolInfo {
                          miniProtocolNum = Mx.MiniProtocolNum 2,
                          miniProtocolDir = Mx.ResponderDirectionOnly,
                          miniProtocolLimits = defaultMiniProtocolLimits,
                          miniProtocolCapability = Nothing
                        }
      atomically $ writeTMVar signalServer server_mps
      let frag = BL.take (fromIntegral (isRealLength badSdu))
                         (encodeInvalidMuxSDU badSdu)
          sdu = makeSDU frag True

      void $ write (\_ ma -> Just <$> ma) sdu
      return \case
               Left (fromException -> Just me) ->
                 counterexample (show me <> " /= " <> show err) $
                      me `compareErrors` err
                   || case me of
                        (Mx.Shutdown (Just (fromException -> Just Mx.SDUReadTimeout)) _status)
                          | isRealLength badSdu < 8 -> True
                        _otherwise -> False
               Left e -> counterexample ("protocolAction exception: " <> show e) False
               Right _ -> counterexample "expected an exception" False

    writeSdu payload | payload == BL.empty = return ()
    writeSdu payload = do
        let (!frag, !rest) = BL.splitAt 0xffff payload
            sdu = makeSDU frag False
        void $ write (\_ ma -> Just <$> ma) sdu
        writeSdu rest

    makeSDU frag custom =
      let sduHdr = Mx.SDUHeader
                     (Mx.RemoteClockModel 0)
                     (Mx.MiniProtocolNum mid)
                     Mx.InitiatorDir
                     fragLen
          hdrBuf = Bin.runPut (enc sduHdr) in
      if custom
        then Mx.SDU sduHdr frag -- ^ for sending out pre-encoded 'bad' SDU
        else Mx.SDU sduHdr (hdrBuf <> frag)
      where
        fragLen = fromIntegral $ BL.length frag
        mid = 2

        enc sduHdr = do
            Bin.putWord32be $ Mx.unRemoteClockModel $ Mx.mhTimestamp sduHdr
            Bin.putWord16be $ mid
            Bin.putWord16be fragLen

    stateLabel (ArbitraryInvalidSDU _ _) = "Invalid"
    stateLabel (ArbitraryValidSDU _ _)   = "Valid"

    violationLabel (ArbitraryValidSDU _ err_m) = sduViolation err_m
    violationLabel (ArbitraryInvalidSDU _ err) = sduViolation $ Just err

    sduViolation (Just Mx.IngressQueueOverRun {}) = "ingress queue overrun"
    sduViolation (Just e)                         = show e
    sduViolation Nothing                          = "none"

prop_demux_sdu_sim :: ArbitrarySDU
                   -> Property
prop_demux_sdu_sim badSdu =
    let payload =
          case badSdu of
            ArbitraryInvalidSDU (InvalidSDU { isLength, isPattern }) _ ->
              BL.replicate (fromIntegral isLength) isPattern
            ArbitraryValidSDU dummyPayload _ -> unDummyPayload dummyPayload
        r_e = runSimStrictShutdown do
                let serverTracer = contramap (Mx.WithBearer "server") activeTracer
                    clientTracer = contramap (Mx.WithBearer "client") activeTracer
                (miniInfoV, serverInfoV) <- (,) <$> newEmptyTMVarIO <*> newEmptyTMVarIO
                (server_w, server_r) <- atomically $ (,) <$> newTBQueue 10 <*> newTBQueue 10
                serverBearer <- getBearer makeQueueChannelBearer
                                  (-1)
                                  serverTracer
                                  QueueChannel { writeQueue = server_w,
                                                 readQueue  = server_r
                                               }
                server <- async $ plainServer miniInfoV serverInfoV payload serverTracer serverBearer
                clientBearer <- getBearer (makeQueueChannelBearer' True)
                                  (-1)
                                  clientTracer
                                  QueueChannel { writeQueue = server_r,
                                                 readQueue  = server_w
                                               }
                resultFn <- prop_demux_sdu badSdu clientBearer miniInfoV
                (mux, resultPromise) <- atomically $ takeTMVar serverInfoV
                result <- atomically resultPromise
                Mx.stop mux
                void . waitCatch $ server
                return $ resultFn result

    in case r_e of
         Left  e -> counterexample (show e) False
         Right r -> r

prop_demux_sdu_io :: ArbitrarySDU
                  -> Property
prop_demux_sdu_io badSdu = ioProperty do
  ad <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  muxAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
  Socket.setSocketOption ad Socket.ReuseAddr 1
  Socket.bind ad (Socket.addrAddress muxAddress)
  addr <- Socket.getSocketName ad
  Socket.listen ad 1
  (miniInfoV, serverInfoV) <- (,) <$> newEmptyTMVarIO <*> newEmptyTMVarIO

  let payload = case badSdu of
        ArbitraryInvalidSDU (InvalidSDU { isLength, isPattern }) _ ->
          BL.replicate (fromIntegral isLength) isPattern
        ArbitraryValidSDU dummyPayload _ -> unDummyPayload dummyPayload
      serverTracer = contramap (Mx.WithBearer "server") activeTracer

  void . async $ bracket (fst <$> Socket.accept ad)
                         Socket.close
                         \sd -> do
                           serverBearer <- getBearer makeSocketBearer 0.005 serverTracer sd
                           plainServer miniInfoV serverInfoV payload serverTracer serverBearer

  clientSd <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.connect clientSd addr
  let clientTracer = contramap (Mx.WithBearer "client") activeTracer
  clientBearer <- getBearer (makeSocketBearer' True) 0 clientTracer clientSd
  resultFn <- prop_demux_sdu badSdu clientBearer miniInfoV
  (mux, resultPromise) <- atomically $ takeTMVar serverInfoV
  result <- atomically resultPromise
  Mx.stop mux
  Socket.close clientSd
  return $ resultFn result

plainServer :: ( Alternative (STM m)
               , MonadAsync m
               , MonadFork m
               , MonadLabelledSTM m
               , MonadMask m
               , MonadThrow (STM m)
               , MonadTimer m
               )
            => StrictTMVar m (MiniProtocolInfo mode)
            -> StrictTMVar m (Mux mode m, STM m (Either SomeException ()))
            -> BL.ByteString
            -> Tracer m Mx.Trace
            -> Bearer m
            -> m ()
plainServer miniInfoV serverInfoV payload serverTracer bearer = do
  miniInfo <- atomically $ takeTMVar miniInfoV
  mux <- Mx.new [miniInfo]
  resultPromise <- Mx.runMiniProtocol mux (Mx.miniProtocolNum miniInfo) (Mx.miniProtocolDir miniInfo)
                     Mx.StartEagerly protocolAction
  atomically $ putTMVar serverInfoV (mux, resultPromise)
  Mx.run serverTracer mux bearer
  where
    -- Server that expects to receive a specific ByteString.
    -- Doesn't send a reply.
    protocolAction chan =
      let loop rest False | rest == BL.empty = return ((), Nothing)
          loop rest _first = do
            msg_m <- Mx.recv chan
            case msg_m of
              Just msg -> do
                case BL.stripPrefix msg rest of
                 Just rest' -> loop rest' False
                 Nothing    -> error "recv corruption"
              Nothing -> error "eof corruption"
      in loop payload True

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
    let chan = Mx.bearerAsChannel bearer (daNum app) Mx.InitiatorDir
    traceWith verboseTracer $ "app waiting " ++ (show $ daNum app)
    threadDelay (daStartAfter app)
    traceWith verboseTracer $ "app starting " ++ (show $ daNum app)
    Mx.send chan $ BL.singleton 0xa5
    return ()

prop_mux_start_mX :: forall m.
                       ( Alternative (STM m)
                       , MonadAsync m
                       , MonadDelay m
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
        nullTracer
        QueueChannel { writeQueue = mux_w, readQueue = mux_r }
    peerBearer <-
      getBearer makeQueueChannelBearer
        (-1)
        nullTracer
        QueueChannel { writeQueue = mux_r, readQueue = mux_w }
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
                nullTracer
                QueueChannel { writeQueue = mux_w, readQueue = mux_r }
    let minis = map (appToInfo Mx.InitiatorDirectionOnly . fst) apps

    mux <- Mx.new minis
    mux_aid <- async $ Mx.run nullTracer mux bearer
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
        nullTracer
        QueueChannel { writeQueue = mux_w, readQueue = mux_r }
    peerBearer <-
      getBearer makeQueueChannelBearer
        (-1)
        nullTracer
        QueueChannel { writeQueue = mux_r, readQueue = mux_w }
    let apps = map fst rapps
        minis = map (appToInfo Mx.ResponderDirectionOnly) apps

    mux <- Mx.new minis
    mux_aid <- async $ Mx.run nullTracer mux bearer
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
        nullTracer
        QueueChannel { writeQueue = mux_w, readQueue = mux_r }
    peerBearer <-
      getBearer makeQueueChannelBearer
        (-1)
        nullTracer
        QueueChannel { writeQueue = mux_r, readQueue = mux_w }
    let apps = map fst rapps
        initMinis = map (appToInfo Mx.InitiatorDirection) apps
        respMinis = map (appToInfo Mx.ResponderDirection) apps

    mux <- Mx.new $ initMinis ++ respMinis
    mux_aid <- async $ Mx.run nullTracer mux bearer
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

    mux <- Mx.new minis
    mux_aid <- async $ Mx.run nullTracer mux bearer
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

    mux <- Mx.new minis
    mux_aid <- async $ Mx.run verboseTracer mux bearer
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

    mux <- Mx.new minis
    mux_aid <- async $ Mx.run verboseTracer mux bearer
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

    mux <- Mx.new $ initMinis ++ respMinis
    mux_aid <- async $ Mx.run verboseTracer mux bearer
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
verboseTracer = threadAndTimeTracer $ showTracing $ Tracer say

threadAndTimeTracer :: forall a m.
                       ( MonadAsync m
                       , MonadMonotonicTime m
                       )
                    => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = Tracer $ \s -> do
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


data NetworkCtx sock m = NetworkCtx {
    ncSocket    :: m sock,
    ncClose     :: sock -> m (),
    ncMuxBearer :: sock -> m (Mx.Bearer m)
  }


withNetworkCtx :: MonadThrow m => NetworkCtx sock m -> (Mx.Bearer m -> m a) -> m a
withNetworkCtx NetworkCtx { ncSocket, ncClose, ncMuxBearer } k =
    bracket ncSocket ncClose (\sock -> ncMuxBearer sock >>= k)


close_experiment
    :: forall sock acc req resp m.
       ( Alternative (STM m)
       , MonadAsync       m
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
    -> NetworkCtx sock m
    -> NetworkCtx sock m
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
    withAsync
      -- run client thread
      (bracket (Mx.new [ MiniProtocolInfo {
                           miniProtocolNum,
                           miniProtocolDir = Mx.InitiatorDirectionOnly,
                           miniProtocolLimits = Mx.MiniProtocolLimits maxBound,
                           miniProtocolCapability = Nothing
                         }
                       ])
                Mx.stop $ \mux ->
        withNetworkCtx clientCtx $ \clientBearer ->
          withAsync (Mx.run ((Client,) `contramap` muxTracer) mux clientBearer) $ \_muxAsync ->
                Mx.runMiniProtocol
                  mux miniProtocolNum
                  Mx.InitiatorDirectionOnly Mx.StartEagerly
                  (\chan -> mkClient >>= runClient clientTracer chan)
            >>= atomically
      )
      $ \clientAsync ->
        withAsync
          -- run server thread
          (bracket ( Mx.new [ MiniProtocolInfo {
                                miniProtocolNum,
                                miniProtocolDir = Mx.ResponderDirectionOnly,
                                miniProtocolLimits = Mx.MiniProtocolLimits maxBound,
                                miniProtocolCapability = Nothing
                              }
                            ])
                    Mx.stop $ \mux ->
          withNetworkCtx serverCtx $ \serverBearer  ->
            withAsync (Mx.run ((Server,) `contramap` muxTracer) mux serverBearer) $ \_muxAsync -> do
                  Mx.runMiniProtocol
                    mux miniProtocolNum
                    Mx.ResponderDirectionOnly Mx.StartOnDemand
                    (\chan -> runServer serverTracer chan (server acc0))
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
                -> return $ property True

                 | otherwise
                -> return $ counterexample
                              (concat [ show resps
                                      , " ≠ "
                                      , show (expectedResps (List.length resps))
                                      ])
                              False

              -- close on read with empty responses is the same as clean
              -- shutdown
              (CloseOnRead, Right (Right resps@[]), Right _)
                 | expected <- expectedResps 0
                 , List.null expected
                -> return $ property True

                 | otherwise
                -> return $ counterexample
                              (concat [ show resps
                                      , " ≠ "
                                      , show (expectedResps (List.length resps))
                                      ])
                              False

              (CloseOnWrite, Right (Left resps), Left serverError)
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
      let serverCtx = NetworkCtx {
              ncSocket = do
                (sock, _) <- Socket.accept serverSocket
                associateWithIOManager iocp (Right sock)
                return sock,
              ncClose  = Socket.close,
              ncMuxBearer = getBearer makeSocketBearer 10 nullTracer
            }
          clientCtx = NetworkCtx {
              ncSocket = do
                sock <- Socket.socket Socket.AF_INET Socket.Stream
                                      Socket.defaultProtocol
                associateWithIOManager iocp (Right sock)
                (Socket.getSocketName serverSocket
                  >>= Socket.connect sock)
                  `onException`
                    Socket.close sock
                return sock,
              ncClose  = Socket.close,
              ncMuxBearer = getBearer makeSocketBearer 10 nullTracer
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
    experiment :: IOSim s Property
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
          clientCtx = NetworkCtx {
              ncSocket = return chann,
              ncClose  = acClose,
              ncMuxBearer = pure
                          . attenuationChannelAsBearer
                              sduSize sduTimeout
                              nullTracer
            }
          serverCtx = NetworkCtx {
              ncSocket = return chann',
              ncClose  = acClose,
              ncMuxBearer = pure
                          . attenuationChannelAsBearer
                              sduSize sduTimeout
                              nullTracer
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
