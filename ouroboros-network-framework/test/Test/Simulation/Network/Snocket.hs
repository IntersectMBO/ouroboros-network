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

-- TODO: Create a 'snocket' package, in order to avoid having to have
-- ouroboros-network-testing as a dependency for this cabal library.
module Test.Simulation.Network.Snocket
  ( tests
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
import qualified Data.Map as Map
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
import           Ouroboros.Network.Testing.Data.AbsBearerInfo
import           Simulation.Network.Snocket

import           Network.Mux
import           Network.Mux.Types (SDUSize (..))
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec.CBOR
import           Network.TypedProtocol.ReqResp.Type
import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Server

import           Test.Ouroboros.Network.Orphans ()  -- ShowProxy ReqResp instance
-- ShowProxy ReqResp instance
import           Ouroboros.Network.Testing.Data.Script
                  (Script(..), singletonScript)

import           Test.QuickCheck hiding (Result (..))
import           Test.QuickCheck.Instances.ByteString
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup "Simulation.Network.Snocket"
    [ testProperty "client-server" prop_client_server
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
    => [payload]
    -> m (Maybe Bool)
clientServerSimulation payloads =
    withSnocket nullTracer noAttenuation Map.empty
    $ \snocket _ ->
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
-- Auxiliary
--

toBearerInfo :: AbsBearerInfo -> BearerInfo
toBearerInfo abi =
    BearerInfo {
        biConnectionDelay      = delay (abiConnectionDelay abi),
        biInboundAttenuation   = attenuation (abiInboundAttenuation abi),
        biOutboundAttenuation  = attenuation (abiOutboundAttenuation abi),
        biInboundWriteFailure  = abiInboundWriteFailure abi,
        biOutboundWriteFailure = abiOutboundWriteFailure abi,
        biAcceptFailures       = Nothing, -- TODO
        biSDUSize              = toSduSize (abiSDUSize abi)
      }

--
-- Properties
--

prop_client_server :: [ByteString] -> Property
prop_client_server payloads =
    let tr = runSimTrace $ clientServerSimulation payloads
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

--
-- Utils
--

traceTime :: MonadMonotonicTime m => Tracer m (Time, a) -> Tracer m a
traceTime = contramapM (\a -> (,a) <$> getMonotonicTime)
