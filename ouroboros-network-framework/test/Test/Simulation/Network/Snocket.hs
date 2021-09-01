{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Simulation.Network.Snocket
  ( tests
  , BearerInfoScript(..)
  , NonFailingBearerInfoScript(..)
  , AbsBearerInfo
  , toBearerInfo
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), contramap, contramapM, nullTracer)

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise

import           Data.ByteString.Lazy (ByteString)
import           Data.List (scanl', takeWhile, unlines)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Foldable (traverse_)
import           Data.Monoid (Any (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)
import           Data.Word (Word16)
import           Text.Printf

import           Ouroboros.Network.Driver.Simple
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Util.ShowProxy
import           Ouroboros.Network.Testing.Utils (Delay (..))
import           Simulation.Network.Snocket

import           Network.Mux
import           Network.Mux.Types (SDUSize (..))
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec.CBOR
import           Network.TypedProtocol.ReqResp.Type
import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Server

import           Test.Ouroboros.Network.Orphans ()  -- ShowProxy ReqResp instance

import           Test.QuickCheck hiding (Result (..))
import           Test.QuickCheck.Instances.ByteString
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
    testGroup "Simulation.Network.Snocket"
    [ testGroup "generators"
      [ testProperty "shrinker AbsBearerInfo" prop_shrinker_AbsBearerInfo
      , testProperty "shrinker BearerInfoScript" prop_shrinker_BearerInfoScript
      , testProperty "generator NonFailingBearerInfoScript"
          prop_generator_NonFailingBeararInfoScript
      ]
    , testProperty "client-server" prop_client_server
    ]

type TestAddr      = TestAddress Int
type TestFD      m = FD m TestAddr
type TestSnocket m = Snocket m (TestFD m) TestAddr

pingServer :: forall payload m. Applicative m
           => ReqRespServer payload payload m ()
pingServer = ReqRespServer {
        recvMsgReq  = \req -> pure (req, pingServer),
        recvMsgDone = pure ()
      }

pingClient :: ( Applicative m
              , Eq payload
              )
           => [payload]
           -> ReqRespClient payload payload m Bool
pingClient = go True
  where
    go !res []       = SendMsgDone (pure res)
    go !res (a : as) = SendMsgReq a $ \a' -> pure (go (a == a' && res) as)


codecReqResp :: forall req resp m.
                ( MonadST m
                , Serialise req
                , Serialise resp
                )
             => Codec (ReqResp req resp) CBOR.DeserialiseFailure m ByteString
codecReqResp = mkCodecCborLazyBS encodeMsg decodeMsg
  where
    encodeMsg :: forall (pr :: PeerRole) st st'.
                 PeerHasAgency pr st
              -> Message (ReqResp req resp) st st'
              -> CBOR.Encoding
    encodeMsg (ClientAgency TokIdle) (MsgReq req) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 1
      <> Serialise.encode req
    encodeMsg (ServerAgency TokBusy) (MsgResp resp) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 2
      <> Serialise.encode resp
    encodeMsg (ClientAgency TokIdle) MsgDone =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 3

    decodeMsg :: forall (pr :: PeerRole) s (st :: ReqResp req resp).
                 PeerHasAgency pr st
              -> CBOR.Decoder s (SomeMessage st)
    decodeMsg stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (ClientAgency TokIdle, 2, 1) -> do
          payload <- Serialise.decode
          return (SomeMessage $ MsgReq payload)
        (ServerAgency TokBusy, 2, 2) -> do
          payload <- Serialise.decode
          return (SomeMessage $ MsgResp payload)
        (ClientAgency TokIdle, 1, 3) ->
          return (SomeMessage MsgDone)
        _ ->
          fail (printf "codecReqResp (%s) unexpected key (%d, %d)"
                       (show stok) key len)


untilSuccess :: ( MonadCatch m
                , MonadDelay m
                , MonadSay   m
                )
             => m a
             -> m a
untilSuccess go =
    go `catch` \(_ :: SomeException) -> do
      threadDelay 1
      untilSuccess go


clientServerSimulation
    :: forall m payload.
       ( MonadAsync       m
       , MonadFork        m
       , MonadLabelledSTM m
       , MonadMask        m
       , MonadSay         m
       , MonadST          m
       , MonadThrow  (STM m)
       , MonadTime        m
       , MonadTimer       m

       , Serialise payload
       , Eq payload
       , Show payload
       , Ord (Async m ())
       )
    => Script BearerInfo
    -> [payload]
    -> m (Maybe Bool)
clientServerSimulation script payloads =
    withSnocket nullTracer script $ \snocket ->
      withAsync (server snocket) $ \_serverAsync -> do
        res <- untilSuccess (client snocket)
        return (Just res)

  where
    reqRespProtocolNum :: MiniProtocolNum
    reqRespProtocolNum = MiniProtocolNum 0

    serverAddr :: TestAddr
    serverAddr = TestAddress 1

    serverPeer :: Peer (ReqResp payload payload) AsServer StIdle m ()
    serverPeer = reqRespServerPeer pingServer

    clientPeer :: Peer (ReqResp payload payload) AsClient StIdle m Bool
    clientPeer = reqRespClientPeer (pingClient payloads)

    server :: TestSnocket m
           -> m ()
    server snocket = do
        labelThisThread "server"
        threadsVar <- newTVarIO Set.empty
        bracket (open snocket TestFamily)
                (close snocket)
                (\fd -> do
                  bind snocket fd serverAddr
                  listen snocket fd
                  accept snocket fd >>= acceptLoop threadsVar)
          `finally` do
            threads <- atomically (readTVar threadsVar)
            traverse_ cancel threads
      where
        acceptLoop :: StrictTVar m (Set (Async m ()))
                   -> Accept m (TestFD m) TestAddr
                   -> m ()
        acceptLoop threadsVar accept0 = do
          (accepted, accept1) <- runAccept accept0
          case accepted of
            Accepted fd' remoteAddr -> do
              bearer <- toBearer snocket 10 nullTracer fd'
              thread <- async $ handleConnection bearer remoteAddr
                                `finally`
                               close snocket fd'
              atomically $
                modifyTVar threadsVar (Set.insert thread)
              acceptLoop threadsVar accept1
            AcceptFailure _err ->
              acceptLoop threadsVar accept1

        handleConnection :: MuxBearer m -> TestAddr -> m ()
        handleConnection bearer remoteAddr = do
          labelThisThread "server-handler"
          bracket
            (newMux (MiniProtocolBundle
                      [ MiniProtocolInfo {
                            miniProtocolNum    = reqRespProtocolNum,
                            miniProtocolDir    = ResponderDirectionOnly,
                            miniProtocolLimits = MiniProtocolLimits maxBound
                          }
                      ]))
            stopMux
            $ \mux -> do
              let connId = ConnectionId {
                      localAddress  = serverAddr,
                      remoteAddress = remoteAddr
                    }
                  tr = (connId,) `contramap`
                       traceTime (   Tracer (say . show)
                                  -- <> Tracer Debug.traceShowM
                                 )

              resSTM <- runMiniProtocol
                          mux reqRespProtocolNum
                          ResponderDirectionOnly
                          StartOnDemand
                          (\channel -> runPeer tr codecReqResp
                                               (fromChannel channel)
                                               serverPeer)
              withAsync
                (do labelThisThread "server-mux"
                    runMux (("server", connId,)
                             `contramap`
                             traceTime (Tracer (say . show)))
                           mux bearer)
                $ \_muxThread -> do
                  res <- atomically resSTM
                  say $ "SERVER HANDLER " ++ show res


    client :: TestSnocket m
           -> m Bool
    client snocket = do
        labelThisThread "client"
        bracket (openToConnect snocket serverAddr)
                (close snocket)
                $ \fd -> do
                  connect snocket fd serverAddr
                  mux <- newMux (MiniProtocolBundle
                                  [ MiniProtocolInfo {
                                        miniProtocolNum    = reqRespProtocolNum,
                                        miniProtocolDir    = InitiatorDirectionOnly,
                                        miniProtocolLimits = MiniProtocolLimits maxBound
                                      }
                                  ])
                  localAddr <- getLocalAddr snocket fd
                  let connId = ConnectionId {
                          localAddress  = localAddr,
                          remoteAddress = serverAddr
                        }
                      tr = (connId,) `contramap`
                           traceTime (   Tracer (say . show)
                                      -- <> Tracer Debug.traceShowM
                                     )
                  resSTM <- runMiniProtocol
                              mux reqRespProtocolNum
                              InitiatorDirectionOnly
                              StartEagerly
                              (\channel -> runPeer tr codecReqResp
                                                   (fromChannel channel)
                                                   clientPeer)
                  bearer <- toBearer snocket 10 nullTracer fd

                  -- kill mux as soon as the client returns
                  withAsync
                    (do labelThisThread "client-mux"
                        runMux (("client", connId,)
                                 `contramap`
                                 traceTime (Tracer (say . show)))
                               mux bearer)
                    $ \_ -> do
                      res <- atomically resSTM
                      stopMux mux
                      case res of
                        (Left err) -> throwIO err
                        (Right b)  -> return b
                        -- Right _         -> error "client: mux died"

--
-- Generators
--

data AbsDelay = SmallDelay
              | NormalDelay
              | LargeDelay

  deriving (Eq, Ord, Show)

delay :: AbsDelay -> DiffTime
delay SmallDelay  = 0.1
delay NormalDelay = 1
delay LargeDelay  = 20

instance Arbitrary AbsDelay where
    arbitrary = frequency
      [ (1, pure SmallDelay)
      , (2, pure NormalDelay)
      , (1, pure LargeDelay)
      ]
    shrink SmallDelay  = []
    shrink NormalDelay = [SmallDelay]
    shrink LargeDelay  = [SmallDelay, NormalDelay]

data AbsSpeed = SlowSpeed
              | NormalSpeed
              | FastSpeed
    deriving (Eq, Ord, Show)

instance Arbitrary AbsSpeed where
    arbitrary = frequency
      [ (1, pure SlowSpeed)
      , (2, pure NormalSpeed)
      , (1, pure FastSpeed)
      ]
    shrink SlowSpeed   = [FastSpeed, NormalSpeed]
    shrink NormalSpeed = [FastSpeed]
    shrink FastSpeed   = []

speedToRational :: AbsSpeed -> Rational
speedToRational SlowSpeed   = 3057    -- 12228 / 4
speedToRational NormalSpeed = 48912   -- 12228 * 4
speedToRational FastSpeed   = 1048576 -- 1Mb/s

delayAtSpeed :: AbsSpeed -> Size -> DiffTime
delayAtSpeed speed size = fromRational (toRational size / speedToRational speed)


data AbsSDUSize = SmallSDU
                | NormalSDU
                | LargeSDU

  deriving (Eq, Show)

instance Arbitrary AbsSDUSize where
    arbitrary = elements [SmallSDU, NormalSDU, LargeSDU]
    shrink SmallSDU  = [LargeSDU, NormalSDU]
    shrink NormalSDU = [LargeSDU]
    shrink LargeSDU  = []

toSduSize :: AbsSDUSize -> SDUSize
toSduSize SmallSDU  = SDUSize 1_024
toSduSize NormalSDU = SDUSize 12_228
toSduSize LargeSDU  = SDUSize 32_768

data AbsAttenuation =
    NoAttenuation    AbsSpeed
  | SpeedAttenuation AbsSpeed Time DiffTime
  | ErrorInterval    AbsSpeed Time DiffTime
  deriving (Eq, Show)

-- | At most `Time 20s`.
--
genTime :: Gen Time
genTime = Time . getDelay <$> arbitrary

-- | At most `1_000`s.
--
genLongDelay :: Gen DiffTime
genLongDelay = getDelay <$> resize 1_000 arbitrary

instance Arbitrary AbsAttenuation where
    arbitrary =
      frequency
        [ (2, NoAttenuation <$> arbitrary)
        , (1, SpeedAttenuation <$> arbitrary `suchThat` (> SlowSpeed)
                               <*> genTime
                               <*> genLongDelay
          )
        , (1, ErrorInterval <$> arbitrary
                            <*> genTime
                            <*> genLongDelay
          )
        ]

    shrink (NoAttenuation speed) =
      [NoAttenuation speed' | speed' <- shrink speed ]
    shrink (SpeedAttenuation speed time len) =
      [ if len' < 1
         then NoAttenuation speed
         else SpeedAttenuation speed time len'
      | Delay len' <- shrink (Delay len)
      ]
    shrink (ErrorInterval speed time len) =
      [ if len' < 1
         then NoAttenuation speed
         else SpeedAttenuation speed time len'
      | Delay len' <- shrink (Delay len)
      ]


attenuation :: AbsAttenuation
            -> Time -> Size -> (DiffTime, SuccessOrFailure)
attenuation (NoAttenuation speed) =
   \_ size -> (delayAtSpeed speed size, Success)
attenuation (SpeedAttenuation normalSpeed from len) =
    \t size ->
      let speed = if t < from || t >= len `addTime` from
                    then normalSpeed
                    else SlowSpeed
      in ( delayAtSpeed speed size
         , Success
         )
attenuation (ErrorInterval speed from len) =
    \t size ->
        ( delayAtSpeed speed size
        , if t < from || t >= len `addTime` from
            then Success
            else Failure
        )

data AbsBearerInfo = AbsBearerInfo
    { abiConnectionDelay      :: !AbsDelay
    , abiInboundAttenuation   :: !AbsAttenuation
    , abiOutboundAttenuation  :: !AbsAttenuation
    , abiInboundWriteFailure  :: !(Maybe Int)
    , abiOutboundWriteFailure :: !(Maybe Int)
    , abiSDUSize              :: !AbsSDUSize
    }
  deriving (Eq, Show)

canFail :: AbsBearerInfo -> Bool
canFail abi = getAny $
      case abiInboundAttenuation abi of
         NoAttenuation {} -> Any False
         _                -> Any True
    <> case abiOutboundAttenuation abi of
         NoAttenuation {} -> Any False
         _                -> Any True
    <> case abiInboundWriteFailure abi of
         Nothing          -> Any False
         _                -> Any True
    <> case abiOutboundWriteFailure abi of
         Nothing          -> Any False
         _                -> Any True

toBearerInfo :: AbsBearerInfo -> BearerInfo
toBearerInfo abi =
    BearerInfo {
        biConnectionDelay      = delay (abiConnectionDelay abi),
        biInboundAttenuation   = attenuation (abiInboundAttenuation abi),
        biOutboundAttenuation  = attenuation (abiOutboundAttenuation abi),
        biInboundWriteFailure  = abiInboundWriteFailure abi,
        biOutboundWriteFailure = abiOutboundWriteFailure abi,
        biSDUSize              = toSduSize (abiSDUSize abi)
      }


instance Arbitrary AbsBearerInfo where
    arbitrary =
        AbsBearerInfo <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> genWriteFailure
                      <*> genWriteFailure
                      <*> arbitrary
      where
        genWriteFailure =
          frequency
            [ (2, pure Nothing)
            , (1, Just <$> arbitrarySizedNatural)
            ]

    shrink abi =
      [ abi { abiConnectionDelay = connDelay }
      | connDelay <- shrink (abiConnectionDelay abi) ]
      ++
      [ abi { abiInboundAttenuation = a }
      | a <- shrink (abiInboundAttenuation abi) ]
      ++
      [ abi { abiOutboundAttenuation = a }
      | a <- shrink (abiOutboundAttenuation abi) ]
      ++
      [ abi { abiInboundWriteFailure = a }
      | a <- shrink (abiInboundWriteFailure abi) ]
      ++
      [ abi { abiOutboundWriteFailure = a }
      | a <- shrink (abiOutboundWriteFailure abi) ]
      ++
      [ abi { abiSDUSize = a }
      | a <- shrink (abiSDUSize abi)
      ]


prop_shrinker_AbsBearerInfo :: Fixed AbsBearerInfo -> Bool
prop_shrinker_AbsBearerInfo (Fixed abi) =
    all (/= abi) (shrink abi)


newtype BearerInfoScript = BearerInfoScript (Script AbsBearerInfo)
  deriving       Show via (Script AbsBearerInfo)
  deriving stock Eq

fixupAbsBearerInfos :: [AbsBearerInfo] -> [AbsBearerInfo]
fixupAbsBearerInfos bis =
    if canFail (last bis)
      then bis ++ [abiNoAttenuation]
      else bis
  where
    abiNoAttenuation = AbsBearerInfo {
        abiConnectionDelay      = NormalDelay,
        abiInboundAttenuation   = NoAttenuation NormalSpeed,
        abiOutboundAttenuation  = NoAttenuation NormalSpeed,
        abiInboundWriteFailure  = Nothing,
        abiOutboundWriteFailure = Nothing,
        abiSDUSize              = NormalSDU
      }

instance Arbitrary BearerInfoScript where
  arbitrary = BearerInfoScript
            . Script
            . NonEmpty.fromList
            . fixupAbsBearerInfos
          <$> listOf1 arbitrary

  shrink (BearerInfoScript (Script script)) =
    [ BearerInfoScript (Script script')
    | script'
        <- map NonEmpty.fromList
         . map fixupAbsBearerInfos
         . filter (not . List.null)
         -- TODO: shrinking of 'AbsBearerInfo' needs to be more aggresive to use
         -- @shrinkList shrink@
         $ shrinkList (const []) (NonEmpty.toList script)
    , script' /= script
    ]

prop_shrinker_BearerInfoScript :: Fixed BearerInfoScript -> Bool
prop_shrinker_BearerInfoScript (Fixed bis) =
    all (\bis'@(BearerInfoScript (Script s)) ->
                  bis /= bis'
               && not (canFail (NonEmpty.last s))
        )
        (shrink bis)

newtype NonFailingBearerInfoScript = 
    NonFailingBearerInfoScript (Script AbsBearerInfo)
  deriving       Show via (Script AbsBearerInfo)
  deriving stock Eq

toNonFailingBearerInfoScript :: BearerInfoScript -> NonFailingBearerInfoScript
toNonFailingBearerInfoScript (BearerInfoScript script) =
    NonFailingBearerInfoScript $ fmap unfail script
  where
    unfail :: AbsBearerInfo -> AbsBearerInfo
    unfail bi = bi { abiInboundWriteFailure  = Nothing
                   , abiOutboundWriteFailure = Nothing
                   , abiInboundAttenuation   = unfailAtt $ abiInboundAttenuation bi
                   , abiOutboundAttenuation  = unfailAtt $ abiOutboundAttenuation bi
                   }

    unfailAtt (ErrorInterval    speed _ _) = NoAttenuation speed
    unfailAtt (SpeedAttenuation speed _ _) = NoAttenuation speed
    unfailAtt a = a

instance Arbitrary NonFailingBearerInfoScript where
  arbitrary = toNonFailingBearerInfoScript <$> arbitrary
  shrink (NonFailingBearerInfoScript script) = toNonFailingBearerInfoScript <$> shrink (BearerInfoScript script)

prop_generator_NonFailingBeararInfoScript :: NonFailingBearerInfoScript -> Bool
prop_generator_NonFailingBeararInfoScript (NonFailingBearerInfoScript s) = not (any canFail s)

--
-- Properties
--

prop_client_server :: [ByteString] -> BearerInfoScript -> Property
prop_client_server payloads (BearerInfoScript script) =
    let tr = runSimTrace $ clientServerSimulation script' payloads
    in -- Debug.traceShow script $
       case traceResult True tr of
         Left e         -> counterexample
                             (unlines
                               [ "=== Say Events ==="
                               , unlines (selectTraceEventsSay' tr)
                               , "=== Error ==="
                               , show e ++ "\n"
                               , "=== Trace Events ==="
                               , unlines (show `map` traceEvents tr)
                               ])
                             False
         Right Nothing  -> property False
         Right (Just b) -> property b
  where
    script' = toBearerInfo <$> script


--
-- Utils
--

traceTime :: MonadMonotonicTime m => Tracer m (Time, a) -> Tracer m a
traceTime = contramapM (\a -> (,a) <$> getMonotonicTime)
