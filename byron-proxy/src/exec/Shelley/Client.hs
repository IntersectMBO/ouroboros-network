module Shelley.Client
  ( Options (..)
  , runClient
  ) where

import Codec.SerialiseTerm (encodeTerm, decodeTerm)
import qualified Network.Socket as Network
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Tracer (Tracer, traceWith)
import Data.Functor.Contravariant (contramap)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Lazy.Builder as Text

import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano
import qualified Cardano.Binary as Binary

import Ouroboros.Byron.Proxy.DB (DB)
import qualified Ouroboros.Byron.Proxy.DB as DB
import qualified Ouroboros.Byron.Proxy.ChainSync.Client as Client
import qualified Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync
import Ouroboros.Byron.Proxy.Network.Protocol (initiatorVersions)
import Ouroboros.Network.Socket (connectToNode)

import Orphans ()

data Options = Options
  { hostName          :: !Network.HostName
    -- ^ Of remote peer
  , serviceName       :: !Network.ServiceName
    -- ^ Of remote peer
  }

-- | Run a Shelley chain sync client which downloads, writes to a database,
-- and prints. It stops after any rollback, because the DB is at the moment
-- immutable (no rollbacks).
runClient
  :: Options
  -> Tracer IO Text.Builder
  -> Cardano.EpochSlots
  -> DB IO
  -> IO ()
runClient options tracer epochSlots db = do
  addrInfosLocal  <- Network.getAddrInfo (Just addrInfoHints) (Just "127.0.0.1") (Just "0")
  addrInfosRemote <- Network.getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  case (addrInfosLocal, addrInfosRemote) of
    (addrInfoLocal : _, addrInfoRemote : _) -> connectToNode
      encodeTerm
      decodeTerm
      -- TODO: this should be some proper type rather than a tuple
      (,)
      (initiatorVersions epochSlots chainSyncClient)
      (Just addrInfoLocal)
      addrInfoRemote
    _ -> error "no getAddrInfo"
  where
  host = hostName options
  port = serviceName options
  addrInfoHints = Network.defaultHints
  -- | This chain sync client will first try to improve the read pointer to
  -- the tip of the database, and then will roll forward forever, stopping
  -- if there is a roll-back.
  -- It makes sense given that we only have an immutable database and one
  -- source for blocks: one read pointer improve is always enough.
  chainSyncClient = Client.chainSyncClient fold
    where
    fold :: Client.Fold (ResourceT IO) ()
    fold = Client.Fold $ do
      tip <- lift $ DB.readTip db
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
          hhash = case Binary.decodeFullAnnotatedBytes (Text.pack "Block or boundary") (Cardano.fromCBORABlockOrBoundary epochSlots) bytes of
            Left cborError -> error "failed to decode block"
            Right blk -> case blk of
              Cardano.ABOBBoundary _ -> error "Corrupt DB: expected block but got EBB"
              Cardano.ABOBBlock blk  -> Cardano.blockHashAnnotated blk
      case mPoint of
        Nothing -> Client.runFold roll
        -- We don't need to do anything with the result; the point is that
        -- the server now knows the proper read pointer.
        Just point -> pure $ Client.Improve [point] $ \_ _ -> roll
    roll :: Client.Fold (ResourceT IO) ()
    roll = Client.Fold $ pure $ Client.Continue forward backward
    forward :: ChainSync.Block -> ChainSync.Point -> Client.Fold (ResourceT IO) ()
    forward blk point = Client.Fold $ do
      lift $ traceWith (contramap chainSyncShow tracer) (Right blk, point)
      -- FIXME
      -- Write one block at a time. CPS doesn't mix well with the typed
      -- protocol style.
      -- This will give terrible performance for the SQLite index as it is
      -- currently defined. As a workaround, the SQLite index is set to use
      -- non-synchronous writes (per connection).
      -- Possible solution: do the batching automatically, within the index
      -- itself?
      lift $ DB.appendBlocks db $ \dbAppend ->
        DB.appendBlock dbAppend (DB.CardanoBlockToWrite blk)
      Client.runFold roll
    backward :: ChainSync.Point -> ChainSync.Point -> Client.Fold (ResourceT IO) ()
    backward point1 point2 = Client.Fold $ do
      lift $ traceWith (contramap chainSyncShow tracer) (Left point1, point2)
      pure $ Client.Stop ()

  chainSyncShow
    :: (Either ChainSync.Point ChainSync.Block, ChainSync.Point)
    -> Text.Builder
  chainSyncShow = \(roll, _tip) -> case roll of
    Left  back    -> mconcat
      [ Text.fromString "Roll back to\n"
      , Text.fromString (show back)
      ]
    Right forward -> mconcat
      [ Text.fromString "Roll forward to\n"
      , case Binary.unAnnotated forward of
          Cardano.ABOBBoundary ebb -> Text.fromString (show ebb)
          Cardano.ABOBBlock    blk -> Cardano.renderHeader
            epochSlots
            (Cardano.blockHeader (fmap (const ()) blk))
      ]
