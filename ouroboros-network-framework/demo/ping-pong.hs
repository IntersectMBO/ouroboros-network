{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Functor (void)
import Data.Void (Void)

import Control.Concurrent.Async
import Control.Monad (when)
import Control.Tracer

import GHC.Clock (getMonotonicTime)
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Printf (printf)

import Network.Mux qualified as Mx

import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux
import Ouroboros.Network.Snocket
import Ouroboros.Network.Snocket qualified as Snocket
import Ouroboros.Network.Socket
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

import Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Version
import Test.Ouroboros.Network.Server qualified as Test.Server

import Network.TypedProtocol.PingPong.Client as PingPong
import Network.TypedProtocol.PingPong.Codec.CBOR as PingPong
import Network.TypedProtocol.PingPong.Examples
import Network.TypedProtocol.PingPong.Server as PingPong
import Network.TypedProtocol.PingPong.Type (PingPong)


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
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }

tracer :: Show a => Tracer IO a
tracer = f `contramapM` stdoutTracer
  where
    f a = do
      t <- getMonotonicTime
      return (printf "%-20f %s" t (show a))

--
-- Ping pong demo
--

demoProtocol0 :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
              -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocol0 pingPong =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = pingPong
      }
    ]


clientPingPong :: Bool -> IO ()
clientPingPong pipelined =
    withIOManager $ \iomgr ->
    void $
    connectToNode
      (Snocket.localSnocket iomgr)
      makeLocalBearer
      ConnectToArgs {
        ctaHandshakeCodec      = unversionedHandshakeCodec,
        ctaHandshakeTimeLimits = noTimeLimitsHandshake,
        ctaVersionDataCodec    = unversionedProtocolDataCodec,
        ctaConnectTracers      = nullNetworkConnectTracers,
        ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
      }
      mempty
      (unversionedProtocol app)
      Nothing
      defaultLocalSocketAddr
  where
    app :: OuroborosApplicationWithMinimalCtx
             Mx.InitiatorMode LocalAddress LBS.ByteString IO () Void
    app = demoProtocol0 pingPongInitiator

    pingPongInitiator | pipelined =
      InitiatorProtocolOnly $
      mkMiniProtocolCbFromPeerPipelined $ \_ctx ->
        ( contramap show stdoutTracer
        , codecPingPong
        , void $ pingPongClientPeerPipelined (pingPongClientPipelinedMax 5)
        )

      | otherwise =
      InitiatorProtocolOnly $
      mkMiniProtocolCbFromPeer $ \_ctx ->
        ( contramap show stdoutTracer
        , codecPingPong
        , pingPongClientPeer (pingPongClientCount 5)
        )


serverPingPong :: IO Void
serverPingPong =
    withIOManager $ \iomgr -> do
    Test.Server.with
      (Snocket.localSnocket iomgr)
      makeLocalBearer
      mempty
      defaultLocalSocketAddr
      HandshakeArguments {
        haHandshakeTracer  = nullTracer,
        haHandshakeCodec   = unversionedHandshakeCodec,
        haVersionDataCodec = unversionedProtocolDataCodec,
        haAcceptVersion    = acceptableVersion,
        haQueryVersion     = queryVersion,
        haTimeLimits       = noTimeLimitsHandshake
      }
      (unversionedProtocol (SomeResponderApplication app))
      $ \_ serverAsync -> wait serverAsync -- block until server finishes
  where
    app :: OuroborosApplicationWithMinimalCtx
             Mx.ResponderMode LocalAddress LBS.ByteString IO Void ()
    app = demoProtocol0 pingPongResponder

    pingPongResponder =
      ResponderProtocolOnly $
      mkMiniProtocolCbFromPeer $ \_ctx ->
        ( tracer
        , codecPingPong
        , pingPongServerPeer pingPongServerStandard
        )

--
-- Ping pong demo2
--

demoProtocol1 :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
              -> RunMiniProtocolWithMinimalCtx appType addr bytes m a b
              -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocol1 pingPong pingPong' =
    OuroborosApplication [
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
    withIOManager $ \iomgr -> void $ do
    connectToNode
      (Snocket.localSnocket iomgr)
      makeLocalBearer
      ConnectToArgs {
        ctaHandshakeCodec      = unversionedHandshakeCodec,
        ctaHandshakeTimeLimits = noTimeLimitsHandshake,
        ctaVersionDataCodec    = unversionedProtocolDataCodec,
        ctaConnectTracers      = nullNetworkConnectTracers,
        ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
      }
      mempty
      (unversionedProtocol app)
      Nothing
      defaultLocalSocketAddr
  where
    app :: OuroborosApplicationWithMinimalCtx
             Mx.InitiatorMode addr LBS.ByteString IO  () Void
    app = demoProtocol1 pingpong pingpong'

    pingpong =
      InitiatorProtocolOnly $
      mkMiniProtocolCbFromPeer $ \_ctx ->
        ( contramap (show . (,) (1 :: Int)) tracer
        , codecPingPong
        , pingPongClientPeer (pingPongClientCount 5)
        )

    pingpong'=
      InitiatorProtocolOnly $
      mkMiniProtocolCbFromPeer $ \_ctx ->
        ( contramap (show . (,) (2 :: Int)) tracer
        , codecPingPong
        , pingPongClientPeer (pingPongClientCount 5)
        )


serverPingPong2 :: IO Void
serverPingPong2 =
    withIOManager $ \iomgr -> do
    Test.Server.with
      (Snocket.localSnocket iomgr)
      makeLocalBearer
      mempty
      defaultLocalSocketAddr
      HandshakeArguments {
        haHandshakeTracer  = nullTracer,
        haHandshakeCodec   = unversionedHandshakeCodec,
        haVersionDataCodec = unversionedProtocolDataCodec,
        haAcceptVersion    = acceptableVersion,
        haQueryVersion     = queryVersion,
        haTimeLimits       = noTimeLimitsHandshake
      }
      (unversionedProtocol (SomeResponderApplication app))
      $ \_ serverAsync -> wait serverAsync -- block until async exception
  where
    app :: OuroborosApplicationWithMinimalCtx
             Mx.ResponderMode addr LBS.ByteString IO Void ()
    app = demoProtocol1 pingpong pingpong'

    pingpong =
      ResponderProtocolOnly $
      mkMiniProtocolCbFromPeer $ \_ctx ->
        ( contramap (show . (,) (1 :: Int)) tracer
        , codecPingPong
        , pingPongServerPeer pingPongServerStandard
        )

    pingpong' =
      ResponderProtocolOnly $
      mkMiniProtocolCbFromPeer $ \_ctx ->
        ( contramap (show . (,) (2 :: Int)) tracer
        , codecPingPong
        , pingPongServerPeer pingPongServerStandard
        )


