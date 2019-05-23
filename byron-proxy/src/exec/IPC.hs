{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IPC where

import qualified Codec.CBOR.Term as CBOR
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM)
import Control.Exception (SomeException, bracket, catch, throwIO)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Word (Word16)

import Network.TypedProtocol.Channel (hoistChannel)
import Network.TypedProtocol.Codec (hoistCodec)
import Network.TypedProtocol.Driver (runPeer)
import Network.Socket (Socket)
import qualified Network.Socket as Socket

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano

import Ouroboros.Network.Channel (Channel, socketAsChannel)
import qualified Ouroboros.Network.Server.Socket as Server
import Ouroboros.Network.Protocol.Handshake.Codec (codecHandshake)
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version

import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import qualified Ouroboros.Byron.Proxy.ChainSync.Client as Client
import qualified Ouroboros.Byron.Proxy.ChainSync.Server as Server
import qualified Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync

import qualified Ouroboros.Byron.Proxy.DB as DB

import qualified Control.Monad.Class.MonadThrow as NonStandard
import qualified Control.Monad.Catch as Standard

type VersionNumber = Word16

data CBORTerm t = CBORTerm
  { encodeTerm :: t -> CBOR.Term
  , decodeTerm :: CBOR.Term -> Either Text t
  }

unitCBORTerm :: CBORTerm ()
unitCBORTerm = CBORTer
  { encodeTerm = const CBOR.TNull
  , decodeTerm = \term -> case term of
      CBOR.TNull -> Right ()
      _          -> Left "expected TNull"
  }

clientVersions
  :: ( Monad m, MonadST m, NonStandard.MonadThrow m )
  => Tracer m String
  -> Cardano.EpochSlots
  -> DB.DB m
  -> Versions VersionNumber CBORTerm (Channel m Lazy.ByteString -> m ())
clientVersions tracer epochSlots db = Versions $ Map.fromList
  [ (0, sigmaClientVersion0 tracer epochSlots db)
  ]

serverVersions
  :: Tracer IO String
  -> Cardano.EpochSlots
  -> Int
  -> DB.DB IO
  -> Versions VersionNumber CBORTerm (Channel IO Lazy.ByteString -> IO ())
serverVersions tracer epochSlots usPoll db = Versions $ Map.fromList
  [ (0, sigmaServerVersion0 tracer epochSlots usPoll db)
  ]

sigmaClientVersion0
  :: ( Monad m, MonadST m, NonStandard.MonadThrow m )
  => Tracer m String
  -> Cardano.EpochSlots
  -> DB.DB m
  -> Sigma (Version CBORTerm (Channel m Lazy.ByteString -> m ()))
sigmaClientVersion0 tracer epochSlots db = Sigma () (clientVersion0 tracer epochSlots db)

sigmaServerVersion0
  :: Tracer IO String
  -> Cardano.EpochSlots
  -> Int
  -> DB.DB IO
  -> Sigma (Version CBORTerm (Channel IO Lazy.ByteString -> IO ()))
sigmaServerVersion0 tracer epochSlots usPoll db = Sigma () (serverVersion0 tracer epochSlots usPoll db)

clientVersion0
  :: ( Monad m, MonadST m, NonStandard.MonadThrow m )
  => Tracer m String
  -> Cardano.EpochSlots
  -> DB.DB m
  -> Version CBORTerm (Channel m Lazy.ByteString -> m ()) ()
clientVersion0 tracer epochSlots db = Version
  { versionApplication = clientApplication tracer epochSlots db
  , versionExtra = unitCBORTerm
  }

serverVersion0
  :: Tracer IO String
  -> Cardano.EpochSlots
  -> Int
  -> DB.DB IO  
  -> Version CBORTerm (Channel IO Lazy.ByteString -> IO ()) ()
serverVersion0 tracer epochSlots usPoll db = Version
  { versionApplication = serverApplication tracer epochSlots usPoll db
  , versionExtra = unitCBORTerm
  }

clientApplication
  :: ( Monad m, MonadST m, NonStandard.MonadThrow m )
  => Tracer m String
  -> Cardano.EpochSlots
  -> DB.DB m
  -> Application (Channel m Lazy.ByteString -> m ()) a
clientApplication tracer epochSlots db = Application $ \_ _ channel ->
  runPeer nullTracer (ChainSync.codec epochSlots) channel peer
  where
  peer = ChainSync.chainSyncClientPeer (chainSyncClient (contramap chainSyncShow tracer) epochSlots db)

serverApplication
  :: Tracer IO String
  -> Cardano.EpochSlots
  -> Int
  -> DB.DB IO
  -> Application (Channel IO Lazy.ByteString -> IO ()) a
serverApplication tracer epochSlots usPoll db = Application $ \_ _ channel -> do
  let peer = ChainSync.chainSyncServerPeer (chainSyncServer epochSlots usPoll db)
      -- `peer` is in ResourceT`, so we must hoist channel and codec into
      -- `ResourceT`
      inResourceT :: forall x . IO x -> ResourceT IO x
      inResourceT = liftIO
      codec' = hoistCodec inResourceT (ChainSync.codec epochSlots)
      channel' = hoistChannel inResourceT channel
  runResourceT $ runPeer nullTracer codec' channel' peer

-- | This chain sync client will first try to improve the read pointer to
-- the tip of the database, and then will roll forward forever, stopping
-- if there is a roll-back.
-- It makes sense given that we only have an immutable database and one
-- source for blocks: one read pointer improve is always enough.
chainSyncClient
  :: forall m x .
     ( Monad m )
  => Tracer m (Either ChainSync.Point ChainSync.Block, ChainSync.Point)
  -> Cardano.EpochSlots
  -> DB.DB m
  -> ChainSync.ChainSyncClient ChainSync.Block ChainSync.Point m x
chainSyncClient trace epochSlots db = Client.chainSyncClient fold
  where
  fold :: Client.Fold m x
  fold = Client.Fold $ do
    tip <- DB.readTip db
    mPoint <- case tip of
      -- DB is empty. Can go without improving read pointer.
      DB.TipGenesis -> pure Nothing
      -- EBB is nice because we already have the header hash.
      DB.TipEBB   slotNo hhash _     -> pure $ Just $ ChainSync.Point
        { ChainSync.pointSlot = slotNo
        , ChainSync.pointHash = hhash
        }
      DB.TipBlock slotNo       bytes -> pure $ Just $ ChainSync.Point
        { ChainSync.pointSlot = slotNo
        , ChainSync.pointHash = hhash
        }
        where
        hhash = case Binary.decodeFullAnnotatedBytes "Block or boundary" (Cardano.fromCBORABlockOrBoundary epochSlots) bytes of
          Left cborError -> error "failed to decode block"
          Right blk -> case blk of
            Cardano.ABOBBoundary _ -> error "Corrupt DB: expected block but got EBB"
            Cardano.ABOBBlock blk  -> Cardano.blockHashAnnotated blk
    case mPoint of
      Nothing -> Client.runFold roll
      -- We don't need to do anything with the result; the point is that
      -- the server now knows the proper read pointer.
      Just point -> pure $ Client.Improve [point] $ \_ _ -> roll
  roll :: Client.Fold m x
  roll = Client.Fold $ pure $ Client.Continue forward backward
  forward :: ChainSync.Block -> ChainSync.Point -> Client.Fold m x
  forward blk point = Client.Fold $ do
    traceWith trace (Right blk, point)
    -- FIXME
    -- Write one block at a time. CPS doesn't mix well with the typed
    -- protocol style.
    -- This will give terrible performance for the SQLite index as it is
    -- currently defined. As a workaround, the SQLite index is set to use
    -- non-synchronous writes (per connection).
    -- Possible solution: do the batching automatically, within the index
    -- itself?
    DB.appendBlocks db $ \dbAppend ->
      DB.appendBlock dbAppend (DB.CardanoBlockToWrite blk)
    Client.runFold roll
  backward :: ChainSync.Point -> ChainSync.Point -> Client.Fold m x
  backward point1 point2 = Client.Fold $ do
    traceWith trace (Left point1, point2)
    Client.runFold roll

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
    , case Binary.unAnnotated forward of
        Cardano.ABOBBoundary ebb -> show ebb
        -- TODO Cardano.renderBlock
        Cardano.ABOBBlock    blk -> show blk
    ]

-- a chain sync server that serves whole blocks.
-- The `ResourceT` is needed because we deal with DB iterators.
chainSyncServer
  :: Cardano.EpochSlots
  -> Int
  -> DB.DB IO
  -> ChainSync.ChainSyncServer ChainSync.Block ChainSync.Point (ResourceT IO) ()
chainSyncServer epochSlots usPoll = Server.chainSyncServer epochSlots err poll
  where
  err = throwIO
  poll :: Server.PollT IO
  poll p m = do
    s <- m
    mbT <- p s
    case mbT of
      Nothing -> lift (threadDelay usPoll) >> poll p m
      Just t  -> pure t

-- | Run a chain sync server over some socket.
--
-- You can give the `AddrInfo`, but you won't get good results if it's not
-- reliable ordered.
--
-- The `STM ()` is for normal shutdown. When it returns, the server stops.
-- So, for instance, use `STM.retry` to never stop (until killed).
runServer
  :: Socket.AddrInfo
  -> Tracer IO String
  -> Cardano.EpochSlots
  -> STM ()
  -> Int
  -> DB.DB IO
  -> IO ()
runServer addrInfo tracer epochSlots closeTx usPoll db = bracket mkSocket Socket.close $ \socket ->
  Server.run (fromSocket socket) throwIO accept complete (const closeTx) ()
  where
  -- New connections are always accepted. The channel is used to run the
  -- version negotiation protocol determined by `versions`. Some stdout
  -- printing is done just to help you see what's going on.
  accept sockAddr st = pure $ Server.Accept st $ \channel -> do
    traceWith tracer $ mconcat
      [ "Got connection from "
      , show sockAddr
      ]
    let versionPeer = handshakeServerPeer encodeTerm decodeTerm (\_ _ _ -> Accept) (serverVersions tracer epochSlots usPoll db)
    versionOutcome <- runPeer nullTracer codecHandshake channel versionPeer
    case versionOutcome of
      Left _ -> undefined
      Right app -> app channel
  -- When a connection completes, we do nothing. State is ().
  -- Crucially: we don't re-throw exceptions, because doing so would
  -- bring down the server.
  -- For the demo, the client will stop by closing the socket, which causes
  -- a deserialise failure (unexpected end of input) and we don't want that
  -- to bring down the proxy.
  complete outcome st = case outcome of
    Left  err -> pure st
    Right r   -> pure st
  mkSocket :: IO Socket
  mkSocket = do
    socket <- Socket.socket
      (Socket.addrFamily addrInfo)
      (Socket.addrSocketType addrInfo)
      (Socket.addrProtocol addrInfo)
    Socket.setSocketOption socket Socket.ReuseAddr 1
    Socket.bind socket (Socket.addrAddress addrInfo)
    Socket.listen socket 1
    pure socket
  -- Make a server-compatibile socket from a network socket.
  fromSocket :: Socket -> Server.Socket Socket.SockAddr (Channel IO Lazy.ByteString)
  fromSocket socket = Server.Socket
    { Server.acceptConnection = do
        (socket', addr) <- Socket.accept socket
        pure (addr, socketAsChannel socket', Socket.close socket')
    }

-- | Connect to a server at a given address and run the version negotiation
-- protocol determined by `clientVersions`.
--
-- You can give the `AddrInfo`, but you won't get good results if it's not
-- reliable ordered.
runClient
  :: Socket.AddrInfo -- ^ For the remote end.
  -> Tracer IO String
  -> Cardano.EpochSlots
  -> DB.DB IO
  -> IO ()
runClient addrInfo tracer epochSlots db = bracket mkSocket Socket.close $ \socket -> do
  _ <- Socket.connect socket (Socket.addrAddress addrInfo)
  let channel = socketAsChannel socket
      versionPeer = handshakeClientPeer encodeTerm decodeTerm (clientVersions tracer epochSlots db)
  versionOutcome <- runPeer nullTracer codecHandshake channel versionPeer
  case versionOutcome of
    Left _ -> undefined
    Right app -> app channel
  where
  mkSocket = do
    socket <- Socket.socket
      (Socket.addrFamily addrInfo)
      (Socket.addrSocketType addrInfo)
      (Socket.addrProtocol addrInfo)
    pure socket

-- Orphans, forced upon me because of the IO sim stuff.
-- Required because we use ResourceT in the chain sync server.

instance NonStandard.MonadThrow (ResourceT IO) where
  throwM = Standard.throwM

-- Non-standard MonadThrow includes bracket... we can get it for free if we
-- give a non-standard MonadCatch

instance NonStandard.MonadCatch (ResourceT IO) where
  catch = Standard.catch

instance NonStandard.MonadMask (ResourceT IO) where
  mask = Standard.mask
  uninterruptibleMask = Standard.uninterruptibleMask
