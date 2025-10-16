{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans                 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Ouroboros.Network.Socket (tests) where

import Data.ByteString.Lazy qualified as BL

import Data.Void (Void)
import Network.Socket qualified as Socket

import Control.Concurrent (ThreadId)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork hiding (ThreadId)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer

import Network.Mux qualified as Mx

import Ouroboros.Network.Mux
import Ouroboros.Network.Snocket hiding (Accept (..))
import Ouroboros.Network.Socket

-- import Cardano.Network.NodeToNode

import Ouroboros.Network.Block (Tip, decodeTip, encodeTip)
import Ouroboros.Network.IOManager
import Ouroboros.Network.Magic
import Ouroboros.Network.Mock.Chain (Chain, ChainUpdate, Point)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mock.ChainGenerators (TestBlockChainAndUpdates (..))
import Ouroboros.Network.Mock.ProducerState qualified as CPS
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Examples qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Server qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import Ouroboros.Network.Server.Simple qualified as Server.Simple
import Ouroboros.Network.Util.ShowProxy

import Test.Ouroboros.Network.Serialise

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf
import Text.Show.Functions ()

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Control.Monad.Class.MonadST
import Data.Text (Text)
import Data.Text qualified as T
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Handshake.Acceptable
import Ouroboros.Network.Handshake.Queryable
import Ouroboros.Network.Protocol.Handshake.Codec qualified as Handshake
import Ouroboros.Network.Protocol.Handshake.Type (Handshake)

--
-- Simple testing versioning scheme
--

data TestVersion = TestVersion
  deriving (Eq, Ord, Enum, Bounded, Show)

newtype TestVersionData = TestVersionData { networkMagic :: NetworkMagic }
  deriving (Show, Eq)

instance Acceptable TestVersionData where
    -- | Check that both side use the same 'networkMagic'.  Choose smaller one
    -- from both 'diffusionMode's, e.g. if one is running in 'InitiatorOnlyMode'
    -- agree on it. Agree on the same 'PeerSharing' value.
    acceptableVersion local remote
      | networkMagic local == networkMagic remote
      = Accept remote
      | otherwise
      = Refuse $ T.pack $ "version data mismatch: "
                       ++ show local
                       ++ " /= " ++ show remote

instance Queryable TestVersionData where
    queryVersion = const False


handshakeCodec :: MonadST m
               => Codec (Handshake TestVersion CBOR.Term)
                        CBOR.DeserialiseFailure m BL.ByteString
handshakeCodec = Handshake.codecHandshake CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm :: TestVersion -> CBOR.Term
    encodeTerm TestVersion = CBOR.TInt 1

    decodeTerm :: CBOR.Term -> Either (Text, Maybe Int) TestVersion
    decodeTerm (CBOR.TInt 1) = Right TestVersion
    decodeTerm (CBOR.TInt n) = Left ( T.pack "decode TestVersion: unknown tag: "
                                        <> T.pack (show n)
                                    , Just n
                                    )
    decodeTerm _ = Left ( T.pack "decode TestVersion: unexpected term"
                        , Nothing)

testVersionCodecCBORTerm
  :: TestVersion
  -> CodecCBORTerm Text TestVersionData
testVersionCodecCBORTerm !_ =
    CodecCBORTerm { encodeTerm = encodeTerm, decodeTerm = decodeTerm }
  where
    encodeTerm :: TestVersionData -> CBOR.Term
    encodeTerm TestVersionData {networkMagic}
      = CBOR.TList
          [ CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)
          ]

    decodeTerm :: CBOR.Term -> Either Text TestVersionData
    decodeTerm (CBOR.TList [CBOR.TInt x])
      | x >= 0
      , x <= 0xffffffff
      = Right
          TestVersionData {
              networkMagic = NetworkMagic (fromIntegral x)
            }
      | otherwise
      = Left $ T.pack $ "networkMagic out of bound: " <> show x
    decodeTerm t
      = Left $ T.pack $ "unknown encoding: " ++ show t

testVersionDataCodec :: VersionDataCodec TestVersion TestVersionData
testVersionDataCodec = mkVersionedCodecCBORTerm testVersionCodecCBORTerm

--
-- The list of all tests
--

tests :: TestTree
tests =
  testGroup "Socket"
  [ testProperty "socket sync demo" prop_socket_demo
  ]


defaultMiniProtocolLimit :: Int
defaultMiniProtocolLimit = 3000000

-- | The bundle of mini-protocols in our test protocol: only chain sync
--
testProtocols1 :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
               -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
testProtocols1 chainSync =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolStart  = StartOnDemandAny,
        miniProtocolLimits = MiniProtocolLimits {
                               maximumIngressQueue = defaultMiniProtocolLimit
                             },
        miniProtocolRun    = chainSync
      }
    ]


--
-- Properties
--

-- | Test chain-sync over a socket bearer
prop_socket_demo :: TestBlockChainAndUpdates -> Property
prop_socket_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates


demo :: forall block .
        ( Chain.HasHeader block
        , Serialise (Chain.HeaderHash block)
        , Serialise block
        , Eq block
        , Show block
        , ShowProxy block
        )
     => Chain block -> [ChainUpdate block block] -> IO Bool
demo chain0 updates = withIOManager $ \iocp -> do
    producerAddressInfo:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    consumerAddressInfo:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")

    let producerAddress = Socket.addrAddress producerAddressInfo
        consumerAddress = Socket.addrAddress consumerAddressInfo

    producerVar <- newTVarIO (CPS.initChainProducerState chain0)
    consumerVar <- newTVarIO chain0
    done <- newEmptyTMVarIO

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        initiatorApp
          :: OuroborosApplicationWithMinimalCtx Mx.InitiatorMode Socket.SockAddr
                                                BL.ByteString IO () Void
        initiatorApp = testProtocols1 chainSyncInitator

        chainSyncInitator =
          InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ctx ->
            ( nullTracer
            , codecChainSync
            , ChainSync.chainSyncClientPeer
                (ChainSync.chainSyncClientExample consumerVar
                   (consumerClient done target consumerVar))
            )

        server :: ChainSync.ChainSyncServer block (Point block) (Tip block) IO ()
        server = ChainSync.chainSyncServerExample () producerVar id

        responderApp
          :: OuroborosApplicationWithMinimalCtx Mx.ResponderMode Socket.SockAddr
                                                BL.ByteString IO Void ()
        responderApp = testProtocols1 chainSyncResponder

        chainSyncResponder =
          ResponderProtocolOnly $ mkMiniProtocolCbFromPeer $ \_ctx ->
            ( nullTracer
            , codecChainSync
            , ChainSync.chainSyncServerPeer server
            )

        codecChainSync = ChainSync.codecChainSync encode             decode
                                                  encode             decode
                                                  (encodeTip encode) (decodeTip decode)

    Server.Simple.with
      (socketSnocket iocp)
      makeSocketBearer
      ((. Just) <$> configureSocket)
      producerAddress
      HandshakeArguments {
        haHandshakeTracer  = nullTracer,
        haBearerTracer     = nullTracer,
        haHandshakeCodec   = handshakeCodec,
        haVersionDataCodec = testVersionDataCodec,
        haAcceptVersion    = acceptableVersion,
        haQueryVersion     = queryVersion,
        haTimeLimits       = noTimeLimitsHandshake

      }
      (simpleSingletonVersions
        (maxBound :: TestVersion)
        TestVersionData { networkMagic = NetworkMagic 0 }
        (\_ -> SomeResponderApplication responderApp))
      $ \producerAddress' _ -> do
      withAsync
        (connectToNode
          (socketSnocket iocp)
          makeSocketBearer
          ConnectToArgs {
            ctaHandshakeCodec      = handshakeCodec,
            ctaHandshakeTimeLimits = noTimeLimitsHandshake,
            ctaVersionDataCodec    = testVersionDataCodec,
            ctaConnectTracers      = nullNetworkConnectTracers,
            ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
          }
          (`configureSocket` Nothing)
          (simpleSingletonVersions
            (maxBound :: TestVersion)
            TestVersionData { networkMagic  = NetworkMagic 0 }
            (\_ -> initiatorApp))
          (Just consumerAddress)
          producerAddress')
        $ \ _connAsync -> do
          void $ forkIO $ sequence_
              [ do
                  threadDelay 10e-4 -- just to provide interest
                  atomically $ do
                    p <- readTVar producerVar
                    let Just p' = CPS.applyChainUpdate update p
                    writeTVar producerVar p'
              | update <- updates
              ]

          atomically $ takeTMVar done

  where
    checkTip target consumerVar = atomically $ do
      chain <- readTVar consumerVar
      return (Chain.headPoint chain == target)

    -- A simple chain-sync client which runs until it recieves an update to
    -- a given point (either as a roll forward or as a roll backward).
    consumerClient :: StrictTMVar IO Bool
                   -> Point block
                   -> StrictTVar IO (Chain block)
                   -> ChainSync.Client block (Point block) (Tip block) IO ()
    consumerClient done target chain =
      ChainSync.Client
        { ChainSync.rollforward = \_ -> checkTip target chain >>= \b ->
            if b then do
                    atomically $ putTMVar done True
                    pure $ Left ()
                 else
                    pure $ Right $ consumerClient done target chain
        , ChainSync.rollbackward = \_ _ -> checkTip target chain >>= \b ->
            if b then do
                    atomically $ putTMVar done True
                    pure $ Left ()
                 else
                    pure $ Right $ consumerClient done target chain
        , ChainSync.points = \_ -> pure $ Right $ consumerClient done target chain
        }

data WithThreadAndTime a = WithThreadAndTime {
      wtatOccuredAt    :: !UTCTime
    , wtatWithinThread :: !ThreadId
    , wtatEvent        :: !a
    }

instance (Show a) => Show (WithThreadAndTime a) where
    show WithThreadAndTime {wtatOccuredAt, wtatWithinThread, wtatEvent} =
        printf "%s: %s: %s" (show wtatOccuredAt) (show wtatWithinThread) (show wtatEvent)

_verboseTracer :: Show a => Tracer IO a
_verboseTracer = threadAndTimeTracer $ showTracing stdoutTracer

threadAndTimeTracer :: Tracer IO (WithThreadAndTime a) -> Tracer IO a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getCurrentTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now tid s
