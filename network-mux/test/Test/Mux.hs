{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -Wno-orphans            #-}

module Test.Mux
    ( tests
    ) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Control.Monad
import qualified Data.Binary.Put as Bin
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack)
import           Data.List (dropWhileEnd, nub)
import qualified Data.Map as M
import           Data.Tuple (swap)
import           Data.Word
import           Test.QuickCheck hiding ((.&.))
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Printf
import qualified System.Random.SplitMix as SM

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer

#if defined(mingw32_HOST_OS)
import qualified System.Win32.NamedPipes as Win32.NamedPipes
import qualified System.Win32.File       as Win32.File
import qualified System.Win32.Async      as Win32.Async
import           System.IOManager
#else
import           System.IO (hClose)
import           System.Process (createPipe)
#endif

import           Test.Mux.ReqResp

import           Network.Mux
import qualified Network.Mux.Compat as Compat
import           Network.Mux.Codec
import           Network.Mux.Channel
import           Network.Mux.Types ( muxBearerAsChannel, MiniProtocolDir(..), MuxSDU(..), MuxSDUHeader(..)
                                   , RemoteClockModel(..), SDUSize (..) )
import           Network.Mux.Bearer.Queues
import           Network.Mux.Bearer.Pipe

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
      isTimestamp  :: !RemoteClockModel
    , isIdAndMode  :: !Word16
    , isLength     :: !Word16
    , isRealLength :: !Int
    , isPattern    :: !Word8
    }

instance Show InvalidSDU where
    show a = printf "InvalidSDU 0x%08x 0x%04x 0x%04x 0x%04x 0x%02x\n"
                    (unRemoteClockModel $ isTimestamp a)
                    (isIdAndMode a)
                    (isLength a)
                    (isRealLength a)
                    (isPattern a)

data ArbitrarySDU = ArbitraryInvalidSDU InvalidSDU Compat.MuxErrorType
                  | ArbitraryValidSDU DummyPayload (Maybe Compat.MuxErrorType)
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
            return $ ArbitraryValidSDU (DummyPayload pl) (Just Compat.MuxIngressQueueOverRun)

        unknownMiniProtocol = do
            ts  <- arbitrary
            mid <- choose (6, 0x7fff) -- ClientChainSynWithBlocks with 5 is the highest valid mid
            mode <- oneof [return 0x0, return 0x8000]
            len <- arbitrary
            p <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (RemoteClockModel ts) (mid .|. mode) len
                                          (8 + fromIntegral len) p)
                                         Compat.MuxUnknownMiniProtocol
        invalidLenght = do
            ts  <- arbitrary
            mid <- arbitrary
            len <- arbitrary
            realLen <- choose (0, 7) -- Size of mux header is 8
            p <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (RemoteClockModel ts) mid len realLen p)
                                         Compat.MuxDecodeError

instance Arbitrary Compat.MuxBearerState where
     arbitrary = elements [ Compat.Mature
                          , Compat.Dead
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
    let sduLen = SDUSize 1260

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10

    let server_w = client_r
        server_r = client_w

        clientBearer = queuesAsMuxBearer clientTracer client_w client_r sduLen
        serverBearer = queuesAsMuxBearer serverTracer server_w server_r sduLen

        clientTracer = contramap (Compat.WithMuxBearer "client") activeTracer
        serverTracer = contramap (Compat.WithMuxBearer "server") activeTracer

        clientApp = MiniProtocolInfo {
                       miniProtocolNum = MiniProtocolNum 2,
                       miniProtocolDir = InitiatorDirectionOnly,
                       miniProtocolLimits = defaultMiniProtocolLimits
                     }

        serverApp = MiniProtocolInfo {
                       miniProtocolNum = MiniProtocolNum 2,
                       miniProtocolDir = ResponderDirectionOnly,
                       miniProtocolLimits = defaultMiniProtocolLimits
                     }

    clientMux <- newMux $ MiniProtocolBundle [clientApp]

    serverMux <- newMux $ MiniProtocolBundle [serverApp]

    withAsync (runMux clientTracer clientMux clientBearer) $ \clientAsync -> 
      withAsync (runMux serverTracer serverMux serverBearer) $ \serverAsync -> do

        r <- step clientMux clientApp serverMux serverApp messages
        stopMux serverMux
        stopMux clientMux
        wait serverAsync
        wait clientAsync
        return $ r

  where
    step _ _ _ _ [] = return $ property True
    step clientMux clientApp serverMux serverApp (msgs:msgss) = do
        (client_mp, server_mp) <- setupMiniReqRsp (return ()) msgs

        clientRes <- runMiniProtocol clientMux (miniProtocolNum clientApp) (miniProtocolDir clientApp)
                   StartEagerly client_mp
        serverRes <- runMiniProtocol serverMux (miniProtocolNum serverApp) (miniProtocolDir serverApp)
                   StartEagerly server_mp
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
    let sduLen = SDUSize 1260

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10

    let server_w = client_r
        server_r = client_w

        clientBearer = queuesAsMuxBearer clientTracer client_w client_r sduLen
        serverBearer = queuesAsMuxBearer serverTracer server_w server_r sduLen

        clientTracer = contramap (Compat.WithMuxBearer "client") activeTracer
        serverTracer = contramap (Compat.WithMuxBearer "server") activeTracer

    let clientApps = [ MiniProtocolInfo {
                        miniProtocolNum = MiniProtocolNum 2,
                        miniProtocolDir = InitiatorDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits
                       }
                     , MiniProtocolInfo {
                        miniProtocolNum = MiniProtocolNum 2,
                        miniProtocolDir = ResponderDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits
                      }
                     ]

        serverApps = [ MiniProtocolInfo {
                        miniProtocolNum = MiniProtocolNum 2,
                        miniProtocolDir = ResponderDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits
                       }
                     , MiniProtocolInfo {
                        miniProtocolNum = MiniProtocolNum 2,
                        miniProtocolDir = InitiatorDirection,
                        miniProtocolLimits = defaultMiniProtocolLimits
                       }
                     ]


    clientMux <- newMux $ MiniProtocolBundle clientApps
    clientAsync <- async $ runMux clientTracer clientMux clientBearer

    serverMux <- newMux $ MiniProtocolBundle serverApps
    serverAsync <- async $ runMux serverTracer serverMux serverBearer

    r <- step clientMux clientApps serverMux serverApps messages
    stopMux clientMux
    stopMux serverMux
    wait serverAsync
    wait clientAsync
    return r

  where
    step _ _ _ _ [] = return $ property True
    step clientMux clientApps serverMux serverApps (msgs:msgss) = do
        (client_mp, server_mp) <- setupMiniReqRsp (return ()) msgs
        clientRes <- sequence
          [ runMiniProtocol
            clientMux
            miniProtocolNum
            miniProtocolDir
            strat
            chan
          | MiniProtocolInfo {miniProtocolNum, miniProtocolDir} <- clientApps
          , (strat, chan) <- case miniProtocolDir of
                              InitiatorDirection -> [(StartEagerly, client_mp)]
                              _                  -> [(StartOnDemand, server_mp)]
          ]
        serverRes <- sequence
          [ runMiniProtocol
            serverMux
            miniProtocolNum
            miniProtocolDir
            strat
            chan
          | MiniProtocolInfo {miniProtocolNum, miniProtocolDir} <- serverApps
          , (strat, chan) <- case miniProtocolDir of
                              InitiatorDirection -> [(StartEagerly, client_mp)]
                              _                  -> [(StartOnDemand, server_mp)]
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
    let sduLen = SDUSize 1260

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    endMpsVar <- atomically $ newTVar 2

    let server_w = client_r
        server_r = client_w

        clientBearer = queuesAsMuxBearer clientTracer client_w client_r sduLen
        serverBearer = queuesAsMuxBearer serverTracer server_w server_r sduLen

        clientTracer = contramap (Compat.WithMuxBearer "client") activeTracer
        serverTracer = contramap (Compat.WithMuxBearer "server") activeTracer

    (verify, client_mp, server_mp) <- setupMiniReqRspCompat
                                        (return ()) endMpsVar messages

    let clientApp = Compat.MuxApplication
                      [ Compat.MuxMiniProtocol {
                          Compat.miniProtocolNum    = Compat.MiniProtocolNum 2,
                          Compat.miniProtocolLimits = defaultMiniProtocolLimits,
                          Compat.miniProtocolRun    = Compat.InitiatorProtocolOnly client_mp
                        }
                      ]

        serverApp = Compat.MuxApplication
                      [ Compat.MuxMiniProtocol {
                          Compat.miniProtocolNum    = Compat.MiniProtocolNum 2,
                          Compat.miniProtocolLimits = defaultMiniProtocolLimits,
                          Compat.miniProtocolRun    = Compat.ResponderProtocolOnly server_mp
                        }
                      ]

    clientAsync <- async $ Compat.muxStart clientTracer clientApp clientBearer
    serverAsync <- async $ Compat.muxStart serverTracer serverApp serverBearer

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
                            , Channel IO -> IO ((), Maybe BL.ByteString)
                            , Channel IO -> IO ((), Maybe BL.ByteString)
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
              -> Channel IO
              -> IO ((), Maybe BL.ByteString)
    clientApp clientResultVar clientChan = do
        (result, trailing) <- runClient nullTracer clientChan (reqRespClient requests)
        atomically (putTMVar clientResultVar result)
        (,trailing) <$> end

    serverApp :: StrictTMVar IO Bool
              -> Channel IO
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
                -> IO ( Channel IO -> IO (Bool, Maybe BL.ByteString)
                      , Channel IO -> IO (Bool, Maybe BL.ByteString)
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

    clientApp :: Channel IO
              -> IO (Bool, Maybe BL.ByteString)
    clientApp clientChan = runClient nullTracer clientChan (reqRespClient requests)

    serverApp :: Channel IO
              -> IO (Bool, Maybe BL.ByteString)
    serverApp serverChan = runServer nullTracer serverChan (reqRespServer responses)

--
-- Running with queues and pipes
--

-- Run applications continuation
type RunMuxApplications
    =  [Channel IO -> IO (Bool, Maybe BL.ByteString)]
    -> [Channel IO -> IO (Bool, Maybe BL.ByteString)]
    -> IO Bool


runMuxApplication :: [Channel IO -> IO (Bool, Maybe BL.ByteString)]
                  -> MuxBearer IO
                  -> [Channel IO -> IO (Bool, Maybe BL.ByteString)]
                  -> MuxBearer IO
                  -> IO Bool
runMuxApplication initApps initBearer respApps respBearer = do
    let clientTracer = contramap (Compat.WithMuxBearer "client") activeTracer
        serverTracer = contramap (Compat.WithMuxBearer "server") activeTracer
        protNum = [1..]
        respApps' = zip protNum respApps
        initApps' = zip protNum initApps

    respMux <- newMux $ MiniProtocolBundle $ map (\(pn,_) ->
        MiniProtocolInfo (MiniProtocolNum pn) ResponderDirectionOnly defaultMiniProtocolLimits)
        respApps'
    respAsync <- async $ runMux serverTracer respMux respBearer
    getRespRes <- sequence [ runMiniProtocol
                              respMux
                              (MiniProtocolNum pn)
                              ResponderDirectionOnly
                              StartOnDemand
                              app
                           | (pn, app) <- respApps'
                           ]

    initMux <- newMux $ MiniProtocolBundle $ map (\(pn,_) ->
        MiniProtocolInfo (MiniProtocolNum pn) InitiatorDirectionOnly defaultMiniProtocolLimits)
        initApps'
    initAsync <- async $ runMux clientTracer initMux initBearer
    getInitRes <- sequence [ runMiniProtocol
                              initMux
                              (MiniProtocolNum pn)
                              InitiatorDirectionOnly
                              StartEagerly
                              app
                           | (pn, app) <- initApps'
                           ]

    initRes <- mapM getResult getInitRes
    respRes <- mapM getResult getRespRes
    stopMux initMux
    stopMux respMux
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
    let sduLen = SDUSize 14000
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    let server_w = client_r
        server_r = client_w

        clientBearer = queuesAsMuxBearer clientTracer client_w client_r sduLen
        serverBearer = queuesAsMuxBearer serverTracer server_w server_r sduLen

        clientTracer = contramap (Compat.WithMuxBearer "client") activeTracer
        serverTracer = contramap (Compat.WithMuxBearer "server") activeTracer

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

             let clientChannel = pipeChannelFromNamedPipe hCli
                 serverChannel = pipeChannelFromNamedPipe hSrv

                 clientBearer  = pipeAsMuxBearer clientTracer clientChannel
                 serverBearer  = pipeAsMuxBearer serverTracer serverChannel

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
        let clientChannel = pipeChannelFromHandles rCli wSrv
            serverChannel = pipeChannelFromHandles rSrv wCli

            clientBearer  = pipeAsMuxBearer clientTracer clientChannel
            serverBearer  = pipeAsMuxBearer serverTracer serverChannel
        runMuxApplication initApps clientBearer respApps serverBearer

#endif
  where
    clientTracer = contramap (Compat.WithMuxBearer "client") activeTracer
    serverTracer = contramap (Compat.WithMuxBearer "server") activeTracer


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
    let sduLen = SDUSize 1260 in
    (BL.length (unDummyPayload response0) > 2 * fromIntegral (getSDUSize sduLen)) &&
    (BL.length (unDummyPayload response1) > 2 * fromIntegral (getSDUSize sduLen)) ==>
    ioProperty $ do
    let request       = DummyPayload $ BL.replicate 4 0xa

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    activeMpsVar <- atomically $ newTVar 0
    traceHeaderVar <- newTVarIO []
    let headerTracer =
          Tracer $ \e -> case e of
            Compat.MuxTraceRecvHeaderEnd header
              -> atomically (modifyTVar traceHeaderVar (header:))
            _ -> return ()

    let server_w = client_r
        server_r = client_w

        clientBearer = queuesAsMuxBearer clientTracer client_w client_r sduLen
        serverBearer = queuesAsMuxBearer serverTracer server_w server_r sduLen

        clientTracer = contramap (Compat.WithMuxBearer "client") activeTracer
        serverTracer = contramap (Compat.WithMuxBearer "server") activeTracer

    (client_short, server_short) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                         $ DummyTrace [(request, response1)]
    (client_long, server_long) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                         $ DummyTrace [(request, response1)]


    let clientApp2 = MiniProtocolInfo {
                         miniProtocolNum = MiniProtocolNum 2,
                         miniProtocolDir = InitiatorDirectionOnly,
                         miniProtocolLimits = defaultMiniProtocolLimits
                       }
        clientApp3 = MiniProtocolInfo {
                         miniProtocolNum = MiniProtocolNum 3,
                         miniProtocolDir = InitiatorDirectionOnly,
                         miniProtocolLimits = defaultMiniProtocolLimits
                       }

        serverApp2 = MiniProtocolInfo {
                         miniProtocolNum = MiniProtocolNum 2,
                         miniProtocolDir = ResponderDirectionOnly,
                         miniProtocolLimits = defaultMiniProtocolLimits
                       }
        serverApp3 = MiniProtocolInfo {
                         miniProtocolNum = MiniProtocolNum 3,
                         miniProtocolDir = ResponderDirectionOnly,
                         miniProtocolLimits = defaultMiniProtocolLimits
                       }

    serverMux <- newMux $ MiniProtocolBundle [serverApp2, serverApp3]
    serverMux_aid <- async $ runMux serverTracer serverMux serverBearer
    serverRes2 <- runMiniProtocol serverMux (miniProtocolNum serverApp2) (miniProtocolDir serverApp2)
                   StartOnDemand server_short
    serverRes3 <- runMiniProtocol serverMux (miniProtocolNum serverApp3) (miniProtocolDir serverApp3)
                   StartOnDemand server_long

    clientMux <- newMux $ MiniProtocolBundle [clientApp2, clientApp3]
    clientMux_aid <- async $ runMux (clientTracer <> headerTracer) clientMux clientBearer
    clientRes2 <- runMiniProtocol clientMux (miniProtocolNum clientApp2) (miniProtocolDir clientApp2)
                   StartEagerly client_short
    clientRes3 <- runMiniProtocol clientMux (miniProtocolNum clientApp3) (miniProtocolDir clientApp3)
                   StartEagerly client_long


    -- Fetch results
    srvRes2 <- atomically serverRes2
    srvRes3 <- atomically serverRes3
    cliRes2 <- atomically clientRes2
    cliRes3 <- atomically clientRes3

    -- First verify that all messages where received correctly
    let res_short = case (srvRes2, cliRes2) of
                         (Left _, _) -> False
                         (_, Left _) -> False
                         (Right a, Right b) -> a && b
    let res_long  = case (srvRes3, cliRes3) of
                         (Left _, _) -> False
                         (_, Left _) -> False
                         (Right a, Right b) -> a && b

    stopMux serverMux
    stopMux clientMux
    _ <- waitBoth serverMux_aid clientMux_aid

    -- Then look at the message trace to check for starvation.
    trace <- atomically $ readTVar traceHeaderVar
    let es = map mhNum (take 100 (reverse trace))
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
        alternates []     = True
        alternates (_:[]) = True
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
        Bin.putWord32be $ unRemoteClockModel $ isTimestamp sdu
        Bin.putWord16be $ isIdAndMode sdu
        Bin.putWord16be $ isLength sdu

-- | Verify ingress processing of valid and invalid SDUs.
--
prop_demux_sdu :: forall m.
                    ( MonadAsync m
                    , MonadFork m
                    , MonadMask m
                    , MonadSay m
                    , MonadThrow (STM m)
                    , MonadTime m
                    , MonadTimer m
                    )
                 => ArbitrarySDU
                 -> m Property
prop_demux_sdu a = do
    r <- run a
    return $ tabulate "SDU type" [stateLabel a] $
             tabulate "SDU Violation " [violationLabel a] r
  where
    run (ArbitraryValidSDU sdu (Just Compat.MuxIngressQueueOverRun)) = do
        stopVar <- newEmptyTMVarIO

        -- To trigger Compat.MuxIngressQueueOverRun we use a special test protocol
        -- with an ingress queue which is less than 0xffff so that it can be
        -- triggered by a single segment.
        let server_mps = MiniProtocolInfo {
                           miniProtocolNum = MiniProtocolNum 2,
                           miniProtocolDir = ResponderDirectionOnly,
                           miniProtocolLimits = smallMiniProtocolLimits
                         }

        (client_w, said, waitServerRes, mux) <- plainServer server_mps (serverRsp stopVar)

        writeSdu client_w $! unDummyPayload sdu

        atomically $! putTMVar stopVar $ unDummyPayload sdu

        _ <- atomically waitServerRes
        stopMux mux
        res <- waitCatch said
        case res of
            Left e  ->
                case fromException e of
                    Just me -> return $ Compat.errorType me === Compat.MuxIngressQueueOverRun
                    Nothing -> return $ property False
            Right _ -> return $ property False

    run (ArbitraryValidSDU sdu err_m) = do
        stopVar <- newEmptyTMVarIO

        let server_mps = MiniProtocolInfo {
                            miniProtocolNum = MiniProtocolNum 2,
                            miniProtocolDir = ResponderDirectionOnly,
                            miniProtocolLimits = defaultMiniProtocolLimits
                          }

        (client_w, said, waitServerRes, mux) <- plainServer server_mps (serverRsp stopVar)

        atomically $! putTMVar stopVar $! unDummyPayload sdu
        writeSdu client_w $ unDummyPayload sdu

        _ <- atomically waitServerRes
        stopMux mux
        res <- waitCatch said
        case res of
            Left e  ->
                case fromException e of
                    Just me -> case err_m of
                                    Just err -> return $ Compat.errorType me === err
                                    Nothing  -> return $ property False
                    Nothing -> return $ property False
            Right _ -> return $ err_m === Nothing

    run (ArbitraryInvalidSDU badSdu err) = do
        stopVar <- newEmptyTMVarIO

        let server_mps = MiniProtocolInfo {
                            miniProtocolNum = MiniProtocolNum 2,
                            miniProtocolDir = ResponderDirectionOnly,
                            miniProtocolLimits = defaultMiniProtocolLimits
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
        stopMux mux
        res <- waitCatch said
        case res of
            Left e  ->
                case fromException e of
                    Just me -> return $ Compat.errorType me === err
                    Nothing -> return $ counterexample ("unexpected: " ++ show e) False
            Right _ -> return $ counterexample "expected an exception" False

    plainServer serverApp server_mp = do
        server_w <- atomically $ newTBQueue 10
        server_r <- atomically $ newTBQueue 10

        let serverBearer = queuesAsMuxBearer serverTracer server_w server_r (SDUSize 1280)
            serverTracer = contramap (Compat.WithMuxBearer "server") activeTracer

        serverMux <- newMux $ MiniProtocolBundle [serverApp]
        serverRes <- runMiniProtocol serverMux (miniProtocolNum serverApp) (miniProtocolDir serverApp)
                 StartEagerly server_mp

        said <- async $ runMux serverTracer serverMux serverBearer
        return (server_r, said, serverRes, serverMux)

    -- Server that expects to receive a specific ByteString.
    -- Doesn't send a reply.
    serverRsp stopVar chan =
        atomically (takeTMVar stopVar) >>= loop
      where
        loop e | e == BL.empty = return ((), Nothing)
        loop e = do
            msg_m <- recv chan
            case msg_m of
                 Just msg ->
                     case BL.stripPrefix msg e of
                          Just e' -> loop e'
                          Nothing -> error "recv corruption"
                 Nothing -> error "eof corruption"

    writeSdu _ payload | payload == BL.empty = return ()
    writeSdu queue payload = do
        let (!frag, !rest) = BL.splitAt 0xffff payload
            sdu' = MuxSDU
                    (MuxSDUHeader
                      (RemoteClockModel 0)
                      (Compat.MiniProtocolNum 2)
                      Compat.InitiatorDir
                      (fromIntegral $ BL.length frag))
                    frag
            !pkt = encodeMuxSDU (sdu' :: MuxSDU)

        atomically $ writeTBQueue queue pkt
        writeSdu queue rest

    stateLabel (ArbitraryInvalidSDU _ _) = "Invalid"
    stateLabel (ArbitraryValidSDU _ _)   = "Valid"

    violationLabel (ArbitraryValidSDU _ err_m) = sduViolation err_m
    violationLabel (ArbitraryInvalidSDU _ err) = sduViolation $ Just err

    sduViolation (Just Compat.MuxUnknownMiniProtocol) = "unknown miniprotocol"
    sduViolation (Just Compat.MuxDecodeError        ) = "decode error"
    sduViolation (Just Compat.MuxIngressQueueOverRun) = "ingress queue overrun"
    sduViolation (Just _                     ) = "unknown violation"
    sduViolation Nothing                       = "none"

prop_demux_sdu_sim :: ArbitrarySDU
                     -> Property
prop_demux_sdu_sim badSdu =
    let r_e =  runSimStrictShutdown $ prop_demux_sdu badSdu in
    case r_e of
         Left  _ -> property False
         Right r -> r

prop_demux_sdu_io :: ArbitrarySDU
                    -> Property
prop_demux_sdu_io badSdu = ioProperty $ prop_demux_sdu badSdu

instance Arbitrary MiniProtocolNum where
    arbitrary = do
        n <- arbitrary
        return $ MiniProtocolNum $ 0x7fff .&. n

    shrink (MiniProtocolNum n) = MiniProtocolNum <$> shrink n

instance Arbitrary MuxMode where
    arbitrary = elements [InitiatorMode, ResponderMode, InitiatorResponderMode]

    shrink InitiatorResponderMode = [InitiatorMode, ResponderMode]
    shrink _                      = []

data DummyAppResult = DummyAppSucceed | DummyAppFail deriving (Eq, Show)

instance Arbitrary DummyAppResult where
    arbitrary = elements [DummyAppSucceed, DummyAppFail]

instance Arbitrary DiffTime where
    arbitrary = fromIntegral <$> choose (0, 100::Word16)

    shrink = map (fromRational . getNonNegative)
           . shrink
           . NonNegative
           . toRational

data DummyApp = DummyApp {
      daNum        :: !MiniProtocolNum
    , daAction     :: !DummyAppResult
    , daRunTime    :: !DiffTime
    , daStartAfter :: !DiffTime
    } deriving (Eq, Show)

instance Arbitrary DummyApp where
    arbitrary = DummyApp <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

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
             InitiatorMode          -> return $ DummyInitiatorApps apps
             ResponderMode          -> frequency [ (3, return $ DummyResponderApps apps)
                                                 , (1, return $ DummyResponderAppsKillMux apps)
                                                 ]
             InitiatorResponderMode -> return $ DummyInitiatorResponderApps apps

      where
        genApp num = DummyApp num <$> arbitrary <*> arbitrary <*> arbitrary

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
                     , MonadCatch m
                     , MonadTimer m
                     )
                  => DummyApp
                  -> (Channel m -> m ((), Maybe BL.ByteString))
dummyAppToChannel DummyApp {daAction, daRunTime} = \_ -> do
    threadDelay daRunTime
    case daAction of
         DummyAppSucceed -> return ((), Nothing)
         DummyAppFail    -> throwIO $ MuxError (MuxShutdown Nothing) "App Fail"

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
             InitiatorMode          -> return $ DummyRestartingInitiatorApps apps
             ResponderMode          -> return $ DummyRestartingResponderApps apps
             InitiatorResponderMode -> return $ DummyRestartingInitiatorResponderApps apps
      where
        genApp num = do
            app <- DummyApp num DummyAppSucceed <$> arbitrary <*> arbitrary
            restarts <- choose (0, 5)
            return (app, restarts)


dummyRestartingAppToChannel :: forall a m.
                     ( MonadAsync m
                     , MonadCatch m
                     , MonadTimer m
                     )
                  => (DummyApp, a)
                  -> (Channel m -> m ((DummyApp, a), Maybe BL.ByteString))
dummyRestartingAppToChannel (app, r) = \_ -> do
    threadDelay $ daRunTime app
    case daAction app of
         DummyAppSucceed -> return ((app, r), Nothing)
         DummyAppFail    -> throwIO $ MuxError (MuxShutdown Nothing) "App Fail"


appToInfo :: MiniProtocolDirection mode -> DummyApp -> MiniProtocolInfo mode
appToInfo d da = MiniProtocolInfo (daNum da) d defaultMiniProtocolLimits

triggerApp :: forall m.
              ( MonadAsync m
              , MonadSay m
              , MonadTime m
              , MonadTimer m
              )
            => MuxBearer m
            -> DummyApp
            -> m ()
triggerApp bearer app = do
    let chan = muxBearerAsChannel bearer (daNum app) InitiatorDir
    traceWith verboseTracer $ "app waiting " ++ (show $ daNum app)
    threadDelay (daStartAfter app)
    traceWith verboseTracer $ "app starting " ++ (show $ daNum app)
    send chan $ BL.singleton 0xa5
    return ()

prop_mux_start_mX :: forall m.
                       ( MonadAsync m
                       , MonadFork m
                       , MonadMask m
                       , MonadSay m
                       , MonadThrow (STM m)
                       , MonadTime m
                       , MonadTimer m
                       )
                    => DummyApps
                    -> DiffTime
                    -> m Property
prop_mux_start_mX apps runTime = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    let bearer = queuesAsMuxBearer nullTracer mux_w mux_r (SDUSize 1234)
        peerBearer = queuesAsMuxBearer nullTracer mux_r mux_w (SDUSize 1234)
    prop_mux_start_m bearer (triggerApp peerBearer) checkRes apps runTime

  where
    checkRes :: StartOnDemandOrEagerly
             -> DiffTime
             -> ((STM m (Either SomeException ())), DummyApp)
             -> m (Property, Either SomeException ())
    checkRes startStrat minRunTime (get,da) = do
        let totTime = case startStrat of
                           StartOnDemand -> daRunTime da + daStartAfter da
                           StartEagerly  -> daRunTime da
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
                       ( MonadAsync m
                       , MonadFork m
                       , MonadMask m
                       , MonadSay m
                       , MonadThrow (STM m)
                       , MonadTime m
                       , MonadTimer m
                       )
                    => DummyRestartingApps
                    -> m Property
prop_mux_restart_m (DummyRestartingInitiatorApps apps) = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    let bearer = queuesAsMuxBearer nullTracer mux_w mux_r (SDUSize 1234)
        MiniProtocolBundle minis = MiniProtocolBundle $ map (appToInfo InitiatorDirectionOnly . fst) apps

    mux <- newMux $ MiniProtocolBundle minis
    mux_aid <- async $ runMux nullTracer mux bearer
    getRes <- sequence [ runMiniProtocol
                           mux
                          (daNum $ fst app)
                          InitiatorDirectionOnly
                          StartEagerly
                          (dummyRestartingAppToChannel app)
                       | app <- apps
                       ]
    r <- runRestartingApps mux $ M.fromList $ zip (map (daNum . fst) apps) getRes
    stopMux mux
    void $ waitCatch mux_aid
    return $ property r

  where
    runRestartingApps :: Mux InitiatorMode m
                      -> M.Map MiniProtocolNum (STM m (Either SomeException (DummyApp, Int)))
                      -> m Bool
    runRestartingApps _ ops | M.null ops = return True
    runRestartingApps mux ops = do
        appResult <- atomically $ foldr (<|>) retry $ M.elems ops
        case appResult of
             Left _ -> return False
             Right (app, 0) -> do
                 runRestartingApps mux $ M.delete (daNum app) ops
             Right (app, restarts) -> do
                 op <- runMiniProtocol mux (daNum app) InitiatorDirectionOnly StartEagerly
                         (dummyRestartingAppToChannel (app, restarts - 1))
                 runRestartingApps mux $ M.insert (daNum app) op ops

prop_mux_restart_m (DummyRestartingResponderApps rapps) = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    let sduSize = SDUSize 1234
        bearer = queuesAsMuxBearer nullTracer mux_w mux_r sduSize
        peerBearer = queuesAsMuxBearer nullTracer mux_r mux_w sduSize
        apps = map fst rapps
        MiniProtocolBundle minis = MiniProtocolBundle $ map (appToInfo ResponderDirectionOnly) apps

    mux <- newMux $ MiniProtocolBundle minis
    mux_aid <- async $ runMux nullTracer mux bearer
    getRes <- sequence [ runMiniProtocol
                           mux
                          (daNum $ fst app)
                          ResponderDirectionOnly
                          StartEagerly
                          (dummyRestartingAppToChannel app)
                       | app <- rapps
                       ]
    triggers <- mapM (async . (triggerApp peerBearer)) apps
    r <- runRestartingApps mux $ M.fromList $ zip (map daNum apps) getRes
    stopMux mux
    void $ waitCatch mux_aid
    mapM_ cancel triggers
    return $ property r
  where
    runRestartingApps :: Mux ResponderMode m
                      -> M.Map MiniProtocolNum (STM m (Either SomeException (DummyApp, Int)))
                      -> m Bool
    runRestartingApps _ ops | M.null ops = return True
    runRestartingApps mux ops = do
        appResult <- atomically $ foldr (<|>) retry $ M.elems ops
        case appResult of
             Left _ -> return False
             Right (app, 0) -> do
                 runRestartingApps mux $ M.delete (daNum app) ops
             Right (app, restarts) -> do
                 op <- runMiniProtocol mux (daNum app) ResponderDirectionOnly StartOnDemand
                           (dummyRestartingAppToChannel (app, restarts - 1))
                 runRestartingApps mux $ M.insert (daNum app) op ops

prop_mux_restart_m (DummyRestartingInitiatorResponderApps rapps) = do
    mux_w <- atomically $ newTBQueue 10
    mux_r <- atomically $ newTBQueue 10
    let sduSize = SDUSize 1234
        bearer = queuesAsMuxBearer nullTracer mux_w mux_r sduSize
        peerBearer = queuesAsMuxBearer nullTracer mux_r mux_w sduSize
        apps = map fst rapps
        initMinis = map (appToInfo InitiatorDirection) apps
        respMinis = map (appToInfo ResponderDirection) apps

    mux <- newMux $ MiniProtocolBundle $ initMinis ++ respMinis
    mux_aid <- async $ runMux nullTracer mux bearer
    getInitRes <- sequence [ runMiniProtocol
                               mux
                               (daNum $ fst app)
                               InitiatorDirection
                               StartEagerly
                               (dummyRestartingAppToChannel (fst app, (InitiatorDirection, snd app)))
                           | app <- rapps
                           ]
    getRespRes <- sequence [ runMiniProtocol
                               mux
                               (daNum $ fst app)
                               ResponderDirection
                               StartOnDemand
                               (dummyRestartingAppToChannel (fst app, (ResponderDirection, snd app)))
                           | app <- rapps
                           ]

    triggers <- mapM (async . triggerApp peerBearer) apps
    let gi = M.fromList $ map (\(n, g) -> ((InitiatorDirection, n), g)) $ zip (map daNum apps) getInitRes
        gr = M.fromList $ map (\(n, g) -> ((ResponderDirection, n), g)) $ zip (map daNum apps) getRespRes
    r <- runRestartingApps mux $ gi <> gr
    stopMux mux
    void $ waitCatch mux_aid
    mapM_ cancel triggers
    return $ property r
  where
    runRestartingApps :: Mux InitiatorResponderMode m
                      -> M.Map (MiniProtocolDirection InitiatorResponderMode, MiniProtocolNum)
                               (STM m (Either SomeException (DummyApp, (MiniProtocolDirection InitiatorResponderMode, Int))))
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
                                  InitiatorDirection -> StartEagerly
                                  ResponderDirection -> StartOnDemand
                 op <- runMiniProtocol mux (daNum app) dir strat (dummyRestartingAppToChannel (app, (dir, restarts - 1)))
                 runRestartingApps mux $ M.insert opKey op ops



prop_mux_start_m :: forall m.
                       ( MonadAsync m
                       , MonadFork m
                       , MonadMask m
                       , MonadSay m
                       , MonadThrow (STM m)
                       , MonadTime m
                       , MonadTimer m
                       )
                    => MuxBearer m
                    -> (DummyApp -> m ())
                    -> (    StartOnDemandOrEagerly
                         -> DiffTime
                         -> ((STM m (Either SomeException ())), DummyApp)
                         -> m (Property, Either SomeException ())
                       )
                    -> DummyApps
                    -> DiffTime
                    -> m Property
prop_mux_start_m bearer _ checkRes (DummyInitiatorApps apps) runTime = do
    let MiniProtocolBundle minis = MiniProtocolBundle $ map (appToInfo InitiatorDirectionOnly) apps
        minRunTime = minimum $ runTime : (map daRunTime $ filter (\app -> daAction app == DummyAppFail) apps)

    mux <- newMux $ MiniProtocolBundle minis
    mux_aid <- async $ runMux nullTracer mux bearer
    killer <- async $ (threadDelay runTime) >> stopMux mux
    getRes <- sequence [ runMiniProtocol
                           mux
                          (daNum app)
                          InitiatorDirectionOnly
                          StartEagerly
                          (dummyAppToChannel app)
                       | app <- apps
                       ]
    rc <- mapM (checkRes StartEagerly minRunTime) $ zip getRes apps
    wait killer
    void $ waitCatch mux_aid

    return (conjoin $ map fst rc)

prop_mux_start_m bearer trigger checkRes (DummyResponderApps apps) runTime = do
    let MiniProtocolBundle minis = MiniProtocolBundle $ map (appToInfo ResponderDirectionOnly) apps
        minRunTime = minimum $ runTime : (map (\a -> daRunTime a + daStartAfter a) $ filter (\app -> daAction app == DummyAppFail) apps)

    mux <- newMux $ MiniProtocolBundle minis
    mux_aid <- async $ runMux verboseTracer mux bearer
    getRes <- sequence [ runMiniProtocol
                           mux
                          (daNum app)
                          ResponderDirectionOnly
                          StartOnDemand
                          (dummyAppToChannel app)
                       | app <- apps
                       ]

    triggers <- mapM (async . trigger) $ filter (\app -> daStartAfter app <= minRunTime) apps
    killer <- async $ (threadDelay runTime) >> stopMux mux
    rc <- mapM (checkRes StartOnDemand minRunTime) $ zip getRes apps
    wait killer
    mapM_ cancel triggers
    void $ waitCatch mux_aid

    return (conjoin $ map fst rc)

prop_mux_start_m bearer _trigger _checkRes (DummyResponderAppsKillMux apps) runTime = do
    -- Start a mini-protocol on demand, but kill mux before the application is
    -- triggered.  This test assures that mini-protocol completion action does
    -- not deadlocks.
    let MiniProtocolBundle minis = MiniProtocolBundle $ map (appToInfo ResponderDirectionOnly) apps

    mux <- newMux $ MiniProtocolBundle minis
    mux_aid <- async $ runMux verboseTracer mux bearer
    getRes <- sequence [ runMiniProtocol
                           mux
                          (daNum app)
                          ResponderDirectionOnly
                          StartOnDemand
                          (dummyAppToChannel app)
                       | app <- apps
                       ]

    killer <- async $ threadDelay runTime
                   >> cancel mux_aid
    _ <- traverse atomically getRes
    wait killer

    return (property True)

prop_mux_start_m bearer trigger checkRes (DummyInitiatorResponderApps apps) runTime = do
    let initMinis = map (appToInfo InitiatorDirection) apps
        respMinis = map (appToInfo ResponderDirection) apps
        minRunTime = minimum $ runTime : (map (\a -> daRunTime a) $ filter (\app -> daAction app == DummyAppFail) apps)

    mux <- newMux $ MiniProtocolBundle $ initMinis ++ respMinis
    mux_aid <- async $ runMux verboseTracer mux bearer
    getInitRes <- sequence [ runMiniProtocol
                               mux
                               (daNum app)
                               InitiatorDirection
                               StartEagerly
                               (dummyAppToChannel app)
                           | app <- apps
                           ]
    getRespRes <- sequence [ runMiniProtocol
                               mux
                               (daNum app)
                               ResponderDirection
                               StartOnDemand
                               (dummyAppToChannel app)
                           | app <- apps
                           ]

    triggers <- mapM (async . trigger) $ filter (\app -> daStartAfter app <= minRunTime) apps
    killer <- async $ (threadDelay runTime) >> stopMux mux
    !rcInit <- mapM (checkRes StartEagerly minRunTime) $ zip getInitRes apps
    !rcResp <- mapM (checkRes StartOnDemand minRunTime) $ zip getRespRes apps
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
                       , MonadSay m
                       , MonadTime m
                       , Show a
                       )
               => Tracer m a
verboseTracer = threadAndTimeTracer $ showTracing $ Tracer say

threadAndTimeTracer :: forall a m.
                       ( MonadAsync m
                       , MonadTime m
                       )
                    => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getMonotonicTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now (show tid) s
