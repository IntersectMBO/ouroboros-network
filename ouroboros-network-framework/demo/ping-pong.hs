{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void)
import           Data.Void (Void)

import Control.Concurrent.Async
import Control.Monad (when)
import Control.Tracer

import System.IO
import System.Directory
import System.Environment
import System.Exit

import Ouroboros.Network.Socket
import Ouroboros.Network.Snocket
import qualified Ouroboros.Network.Snocket as Snocket
import Ouroboros.Network.Mux
import Ouroboros.Network.ErrorPolicy
import Ouroboros.Network.IOManager
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Version

import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.PingPong.Type (PingPong)
import Network.TypedProtocol.PingPong.Client as PingPong
import Network.TypedProtocol.PingPong.Server as PingPong
import Network.TypedProtocol.PingPong.Codec.CBOR  as PingPong


main :: IO ()
main = do
    args <- getArgs
    case args of
      "pingpong":"client":[]           -> clientPingPong False
      "pingpong":"client-pipelined":[] -> clientPingPong True
      "pingpong":"server":[] -> do
        rmIfExists defaultLocalSocketAddrPath
        void serverPingPong

      "pingpong2":"client":[] -> clientPingPong2
      "pingpong2":"server":[] -> do
        rmIfExists defaultLocalSocketAddrPath
        void serverPingPong2

      _          -> usage

instance ShowProxy PingPong where
    showProxy _ = "PingPong"

usage :: IO ()
usage = do
    hPutStrLn stderr "usage: demo-ping-pong [pingpong|pingpong2] {client|server} [addr]"
    exitFailure

defaultLocalSocketAddrPath :: FilePath
defaultLocalSocketAddrPath =  "./demo-ping-pong.sock"

defaultLocalSocketAddr :: LocalAddress
defaultLocalSocketAddr = localAddressFromPath defaultLocalSocketAddrPath

rmIfExists :: FilePath -> IO ()
rmIfExists path = do
  b <- doesFileExist path
  when b (removeFile path)

-- TODO: provide sensible limits
-- https://github.com/input-output-hk/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }


--
-- Ping pong demo
--

demoProtocol0 :: RunMiniProtocol appType bytes m a b
              -> OuroborosApplication appType addr bytes m a b
demoProtocol0 pingPong =
    OuroborosApplication $ \_connectionId _controlMessageSTM -> [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = pingPong
      }
    ]


clientPingPong :: Bool -> IO ()
clientPingPong pipelined =
    withIOManager $ \iomgr ->
    connectToNode
      (Snocket.localSnocket iomgr)
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      nullNetworkConnectTracers
      acceptableVersion
      (unversionedProtocol app)
      Nothing
      defaultLocalSocketAddr
  where
    app :: OuroborosApplication InitiatorMode addr LBS.ByteString IO () Void
    app = demoProtocol0 pingPongInitiator

    pingPongInitiator | pipelined =
      InitiatorProtocolOnly $
      MuxPeerPipelined
        (contramap show stdoutTracer)
        codecPingPong
        (pingPongClientPeerPipelined (pingPongClientPipelinedMax 5))

      | otherwise =
      InitiatorProtocolOnly $
      MuxPeer
        (contramap show stdoutTracer)
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))


pingPongClientCount :: Applicative m => Int -> PingPongClient m ()
pingPongClientCount 0 = PingPong.SendMsgDone ()
pingPongClientCount n = SendMsgPing (pure (pingPongClientCount (n-1)))

serverPingPong :: IO Void
serverPingPong =
    withIOManager $ \iomgr -> do
    networkState <- newNetworkMutableState
    _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (Snocket.localSnocket iomgr)
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      defaultLocalSocketAddr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      acceptableVersion
      (unversionedProtocol (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    app :: OuroborosApplication ResponderMode addr LBS.ByteString IO Void ()
    app = demoProtocol0 pingPongResponder

    pingPongResponder =
      ResponderProtocolOnly $
      MuxPeer
        (contramap show stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

pingPongServerStandard
  :: Applicative m
  => PingPongServer m ()
pingPongServerStandard =
    PingPongServer {
      recvMsgPing = pure pingPongServerStandard,
      recvMsgDone = ()
    }


--
-- Ping pong demo2
--

demoProtocol1 :: RunMiniProtocol appType bytes m a b
              -> RunMiniProtocol appType bytes m a b
              -> OuroborosApplication appType addr bytes m a b
demoProtocol1 pingPong pingPong' =
    OuroborosApplication $ \_connectionId _controlMessageSTM -> [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = pingPong
      }
    , MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 3,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = pingPong'
      }
    ]


clientPingPong2 :: IO ()
clientPingPong2 =
    withIOManager $ \iomgr -> do
    connectToNode
      (Snocket.localSnocket iomgr)
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      nullNetworkConnectTracers
      acceptableVersion
      (unversionedProtocol app)
      Nothing
      defaultLocalSocketAddr
  where
    app :: OuroborosApplication InitiatorMode addr LBS.ByteString IO  () Void
    app = demoProtocol1 pingpong pingpong'

    pingpong =
      InitiatorProtocolOnly $
      MuxPeer
        (contramap (show . (,) (1 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))

    pingpong'=
      InitiatorProtocolOnly $
      MuxPeer
        (contramap (show . (,) (2 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))

pingPongClientPipelinedMax
  :: forall m. Monad m
  => Int
  -> PingPongClientPipelined m ()
pingPongClientPipelinedMax c =
    PingPongClientPipelined (go [] Zero 0)
  where
    go :: [Either Int Int] -> Nat o -> Int
       -> PingPongSender o Int m ()
    go acc o        n | n < c
                      = SendMsgPingPipelined
                          (return n)
                          (go (Left n : acc) (Succ o) (succ n))
    go _    Zero     _ = SendMsgDonePipelined ()
    go acc (Succ o) n = CollectPipelined
                          Nothing
                          (\n' -> go (Right n' : acc) o n)

serverPingPong2 :: IO Void
serverPingPong2 =
    withIOManager $ \iomgr -> do
    networkState <- newNetworkMutableState
    _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (Snocket.localSnocket iomgr)
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      defaultLocalSocketAddr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      acceptableVersion
      (unversionedProtocol (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    app :: OuroborosApplication ResponderMode addr LBS.ByteString IO Void ()
    app = demoProtocol1 pingpong pingpong'

    pingpong =
      ResponderProtocolOnly $
      MuxPeer
        (contramap (show . (,) (1 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

    pingpong' =
      ResponderProtocolOnly $
      MuxPeer
        (contramap (show . (,) (2 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

