{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Functor (void)
import           Data.Void (Void)

import Control.Concurrent.Async
import Control.Monad (when)
import Control.Tracer

import System.IO
import System.Directory
import System.Environment
import System.Exit

import Ouroboros.Network.Codec
import Ouroboros.Network.Socket
import Ouroboros.Network.Snocket
import Ouroboros.Network.Mux
import Ouroboros.Network.ErrorPolicy
import Ouroboros.Network.IOManager

import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version as Version
import qualified Codec.CBOR.Term as CBOR

import Network.TypedProtocol.Pipelined
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

--
-- Version negotation
--

data NullVersionData = NullVersionData
  deriving (Eq, Show)

instance Acceptable NullVersionData where
  acceptableVersion NullVersionData NullVersionData = Version.Accept

nullVersionDataCodecCBORTerm :: CodecCBORTerm Text NullVersionData
nullVersionDataCodecCBORTerm = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NullVersionData -> CBOR.Term
      encodeTerm NullVersionData = CBOR.TNull

      decodeTerm :: CBOR.Term -> Either Text NullVersionData
      decodeTerm CBOR.TNull = Right NullVersionData
      decodeTerm t          = Left $ Text.pack $ "unexpected term: " ++ show t


--
-- Ping pong demo
--

data DemoProtocol0 = PingPong0
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol0 where
  fromProtocolEnum PingPong0 = MiniProtocolNum 2

instance MiniProtocolLimits DemoProtocol0 where
  maximumMessageSize _ = maxBound
  maximumIngressQueue _ = maxBound


clientPingPong :: Bool -> IO ()
clientPingPong pipelined =
    withIOManager $ \iomgr ->
    connectToNode
      (localSnocket iomgr defaultLocalSocketAddrPath)
      cborTermVersionDataCodec
      nullNetworkConnectTracers
      (simpleSingletonVersions (0::Int)
                               NullVersionData
                               (DictVersion nullVersionDataCodecCBORTerm)
                               app)
      Nothing
      defaultLocalSocketAddr
  where
    app :: OuroborosApplication InitiatorApp
                                (ConnectionId LocalAddress)
                                DemoProtocol0
                                IO LBS.ByteString () Void
    app = simpleInitiatorApplication protocols

    protocols :: DemoProtocol0 -> MuxPeer DeserialiseFailure
                                          IO LBS.ByteString ()
    protocols PingPong0 | pipelined =
      MuxPeerPipelined
        (contramap show stdoutTracer)
        codecPingPong
        (pingPongClientPeerPipelined (pingPongClientPipelinedMax 5))

    protocols PingPong0 =
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
      (localSnocket iomgr defaultLocalSocketAddrPath)
      nullNetworkServerTracers
      networkState
      defaultLocalSocketAddr
      cborTermVersionDataCodec
      (\(DictVersion _) -> acceptableVersion)
      (simpleSingletonVersions (0::Int)
                               NullVersionData
                               (DictVersion nullVersionDataCodecCBORTerm)
                               (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    app :: OuroborosApplication ResponderApp
                                (ConnectionId LocalAddress)
                                DemoProtocol0
                                IO LBS.ByteString Void ()
    app = simpleResponderApplication protocols

    protocols :: DemoProtocol0 -> MuxPeer DeserialiseFailure
                                          IO LBS.ByteString ()
    protocols PingPong0 =
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

data DemoProtocol1 = PingPong1 | PingPong1'
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol1 where
  fromProtocolEnum PingPong1  = MiniProtocolNum 2
  fromProtocolEnum PingPong1' = MiniProtocolNum 3

instance MiniProtocolLimits DemoProtocol1 where
  maximumMessageSize _ = maxBound
  maximumIngressQueue _ = maxBound


clientPingPong2 :: IO ()
clientPingPong2 =
    withIOManager $ \iomgr ->
    connectToNode
      (localSnocket iomgr defaultLocalSocketAddrPath)
      cborTermVersionDataCodec
      nullNetworkConnectTracers
      (simpleSingletonVersions (0::Int)
                               NullVersionData
                               (DictVersion nullVersionDataCodecCBORTerm)
                               app)
      Nothing
      defaultLocalSocketAddr
  where
    app :: OuroborosApplication InitiatorApp
                                (ConnectionId LocalAddress)
                                DemoProtocol1
                                IO LBS.ByteString () Void
    app = simpleInitiatorApplication protocols

    protocols :: DemoProtocol1 -> MuxPeer DeserialiseFailure
                                          IO LBS.ByteString ()
    protocols PingPong1 =
      MuxPeer
        (contramap (show . (,) (1 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))

    protocols PingPong1' =
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
      (localSnocket iomgr defaultLocalSocketAddrPath)
      nullNetworkServerTracers
      networkState
      defaultLocalSocketAddr
      cborTermVersionDataCodec
      (\(DictVersion _) -> acceptableVersion)
      (simpleSingletonVersions (0::Int)
                               NullVersionData
                               (DictVersion nullVersionDataCodecCBORTerm)
                               (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    app :: OuroborosApplication ResponderApp
                                (ConnectionId LocalAddress)
                                DemoProtocol1
                                IO LBS.ByteString Void ()
    app = simpleResponderApplication protocols

    protocols :: DemoProtocol1 -> MuxPeer DeserialiseFailure
                                          IO LBS.ByteString ()
    protocols PingPong1 =
      MuxPeer
        (contramap (show . (,) (1 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

    protocols PingPong1' =
      MuxPeer
        (contramap (show . (,) (2 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

