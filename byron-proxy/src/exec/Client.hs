{-# LANGUAGE ScopedTypeVariables #-}

import qualified Codec.CBOR.Write as CBOR (toStrictByteString)
import Codec.Serialise (Serialise (..), deserialiseOrFail)
import Control.Concurrent.Async (race)
import Control.Exception (bracket)
import Control.Tracer (Tracer (..), contramap, nullTracer, stdoutTracer, traceWith)
import qualified Data.ByteString.Lazy as Lazy (ByteString, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as Lazy (pack)
import Data.Either (either)
import qualified Data.Map as Map

import Network.TypedProtocol.Driver (runPeer)
import qualified Network.Socket as Socket

import Ouroboros.Network.Channel (Channel, socketAsChannel)
import Ouroboros.Network.Server.Version (Application (..), Dict (..), Version (..), Versions (..), Sigma (..))
import Ouroboros.Network.Server.Version.Protocol (clientPeerFromVersions)
import qualified Ouroboros.Network.Server.Version.CBOR as Version

import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Byron.Proxy.ChainSync.Client as Client
import qualified Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync

import qualified Ouroboros.Byron.Proxy.DB as DB (blockEpochAndRelativeSlot, ebbEpoch)

-- | Echos rolls (forward or backward) using a trace.
chainSyncClient
  :: forall m x .
     ( Monad m )
  => Tracer m (Either ChainSync.Point ChainSync.Block, ChainSync.Point)
  -> ChainSync.ChainSyncClient ChainSync.Block ChainSync.Point m x
chainSyncClient trace = Client.chainSyncClient fold
  where
  fold :: Client.Fold m x
  fold = Client.Fold $ pure $ Client.Continue forward backward
  forward :: ChainSync.Block -> ChainSync.Point -> Client.Fold m x
  forward blk point = Client.Fold $ do
    traceWith trace (Right blk, point)
    Client.runFold fold
  backward :: ChainSync.Point -> ChainSync.Point -> Client.Fold m x
  backward point1 point2 = Client.Fold $ do
    traceWith trace (Left point1, point2)
    Client.runFold fold

versions
  :: Tracer IO String
  -> Versions Version.Number (Dict Serialise) (Channel IO Lazy.ByteString -> IO ())
versions tracer = Versions $ Map.fromList
  [ (0, version0 tracer)
  , (1, version1 tracer)
  ]

application0
  :: Tracer IO String
  -> Application (Channel IO Lazy.ByteString -> IO ()) Int
application0 tracer = Application $ \_localData remoteData _channel ->
  traceWith tracer $ mconcat
    [ "Version 0. Remote data is "
    , show remoteData
    ]

version0
  :: Tracer IO String
  -> Sigma (Version (Dict Serialise) (Channel IO Lazy.ByteString -> IO ()))
version0 tracer = Sigma (42 :: Int) $ Version
  { versionExtra = Dict
  , versionApplication = application0 tracer
  }

application1
  :: Tracer IO String
  -> Application (Channel IO Lazy.ByteString -> IO ()) Lazy.ByteString
application1 tracer = Application $ \_localData remoteData channel -> do
  traceWith tracer $ mconcat
    [ "Version 1. Remote data is "
    , show remoteData
    ]
  runPeer nullTracer ChainSync.codec channel peer
  where
  peer = ChainSync.chainSyncClientPeer (chainSyncClient (contramap chainSyncShow tracer))

version1
  :: Tracer IO String
  -> Sigma (Version (Dict Serialise) (Channel IO Lazy.ByteString -> IO ()))
version1 tracer = Sigma (Lazy.pack "this is the client version data") $ Version
  { versionExtra = Dict
  , versionApplication = application1 tracer
  }

encodeBlob :: Dict Serialise t -> t -> Version.Blob
encodeBlob Dict = CBOR.toStrictByteString . encode

decodeBlob :: Dict Serialise t -> Version.Blob -> Maybe t
decodeBlob Dict = either (const Nothing) Just . deserialiseOrFail . Lazy.fromStrict

-- | Connects to a server at 127.0.0.1:7777 and runs the version negotiation
-- protocol determined by `versions`.
runVersionedClient :: Tracer IO String -> IO ()
runVersionedClient tracer = bracket mkSocket Socket.close $ \socket -> do
  _ <- Socket.connect socket (Socket.SockAddrInet 7777 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  let channel = socketAsChannel socket
      versionClient = clientPeerFromVersions encodeBlob decodeBlob (versions tracer)
  -- Run the version negotiation client, and then whatever continuation it
  -- produces.
  mbVersion <- runPeer nullTracer Version.codec channel versionClient
  case mbVersion of
    -- TODO it should give an explanation.
    Nothing -> error "failed to negotiate version"
    Just k  -> k channel
  where
  mkSocket = do
    socket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.bind socket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (0, 0, 0, 0)))
    pure socket

chainSyncShow
  :: (Either ChainSync.Point ChainSync.Block, ChainSync.Point)
  -> String
chainSyncShow = \(roll, _tip) -> case roll of
  Left  back    -> mconcat
    [ "Roll back to "
    , show back
    ]
  Right forward -> mconcat
    [ "Roll forward to "
    , case ChainSync.getBlock forward of
        Left ebb  -> show $ DB.ebbEpoch ebb
        Right blk -> show $ DB.blockEpochAndRelativeSlot blk
    ]

main :: IO ()
main = do
  let userInterrupt = getChar
      mainThread = runVersionedClient stdoutTracer
  outcome <- race userInterrupt mainThread
  case outcome of
    Left  _ -> pure ()
    Right _ -> pure ()
