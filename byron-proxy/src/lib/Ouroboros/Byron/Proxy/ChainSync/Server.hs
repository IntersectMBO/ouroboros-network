{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Byron.Proxy.ChainSync.Server where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Resource (ResourceT, ReleaseKey, allocate, release)
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (foldlM)
import Data.List (sortBy)
import Data.Ord (Down (..))
import Data.Word (Word)

import qualified Pos.Binary.Class as CSL (decode)
import qualified Pos.Chain.Block as CSL

import Ouroboros.Byron.Proxy.DB (DB)
import qualified Ouroboros.Byron.Proxy.DB as DB
import Ouroboros.Storage.ImmutableDB.API (Epoch (..), Slot (..))

import Ouroboros.Network.Protocol.ChainSync.Server

data Point = Point
  { pointSlot :: !Slot
  , pointHash :: !CSL.HeaderHash
  }

-- | The declarative part of the server state, this is not the whole story:
-- the `IO`-backed `DB.Iterator` contains much implicit state.
--
-- - No slot, no iterator : Initial state. The client has not successfully
--     improved the read pointer, nor requested the next block.
-- - No slot, iterator    : The iterator's next block is the oldest block. The
--     client has not requested any blocks, nor successfully improved the read
--     pointer.
-- - Slot, no iterator : The client has the block at this slot. If it was served
--     to the client, then the iterator that served it has been closed. If not,
--     the client must have showed it in an improve read pointer request.
-- - Slot, iterator : The client has the block at this slot, and the iterator's
--     next block will be the child of the block at that slot (possibly the same
--     slot, in case of EBBs).
data ServerState = ServerState
  { -- | Newest slot that we know the client has: either because we sent a block
    -- at this slot to them, or they showed that they have it by improving the
    -- read pointer.
    ssNewestKnownSlot :: !(Maybe Slot)
    -- | The next block in the iterator is the next block to send to the client.
    -- The `ReleaseKey` is from the resourcet package, used to de-allocate,
    -- because we can't do typical bracketing.
  , ssIterator        :: !(Maybe (DB.Iterator IO, ReleaseKey))
  }

initialServerState :: ServerState
initialServerState = ServerState
  { ssNewestKnownSlot = Nothing
  , ssIterator        = Nothing
  }

nextSlot :: Maybe Slot -> Slot
nextSlot = maybe 0 ((+) 1)

-- | Run any `m` repeatedly until the condition is satisfied.
-- Since we currently don't have DB change notifications, we have to poll for
-- the "await reply" part of the chain sync server.
--
-- For example:
--
--   ioPoll :: Int -> Poll IO
--   ioPoll us k m = do
--     s <- m
--     case k s of
--       Nothing -> threadDelay us >> ioPoll us k m
--       Just t  -> pure t
--
type Poll m = forall s t . (s -> Maybe t) -> m s -> m t

-- | Since we'll use unfortunately have to use `ResourceT`, we'll need to be
-- able to take a `Poll (ResourceT IO s)`, but ideally the poll definition
-- would not be allowed to use any of the `ResourceT` stuff, so we'll use an
-- arbitrary `MonadTrans`.
type PollT m = forall t . MonadTrans t => Poll (t m)

-- | A ChainSync server of full cardano-sl `Block`s (that includes EBBs)
-- backed by a `DB`. Will never give a rollback because the DB (for now) is
-- immutable.
--
-- The whole thing must go in a `ResourceT`, because typed transitions protocol
-- applications are not in CPS and cannot do proper bracketing. This is the
-- same problem that conduit/pipes/streaming suffer from.
--
-- And then, since we use `ResourceT`, we're essentially forced to use `IO`,
-- by they type of `allocate`: `ResourceT` uses an `IORef`, so we're stuck.
-- It's tragic.
chainSyncServer
  :: (forall x . CBOR.DeserialiseFailure -> IO x)
  -> PollT IO
  -> DB IO
  -> ChainSyncServer CSL.Block Point (ResourceT IO) ()
chainSyncServer err poll db = chainSyncServerAt err poll db initialServerState

chainSyncServerAt
  :: (forall x . CBOR.DeserialiseFailure -> IO x)
  -> PollT IO
  -> DB IO
  -> ServerState
  -> ChainSyncServer CSL.Block Point (ResourceT IO) ()
chainSyncServerAt err poll db rp = ChainSyncServer $ pure $ chainSyncServerIdle err poll db rp

chainSyncServerIdle
  :: (forall x . CBOR.DeserialiseFailure -> IO x)
  -> PollT IO
  -> DB IO
  -> ServerState
  -> ServerStIdle CSL.Block Point (ResourceT IO) ()
chainSyncServerIdle err poll db ss = ServerStIdle
  { recvMsgDoneClient = ()

  , recvMsgFindIntersect = \points -> do
      -- Order the list by slot descending, and find the first entry which is
      -- in the database.
      let cmpSlots p1 p2 = Down (pointSlot p1) `compare` Down (pointSlot p2)
          orderedPoints = sortBy cmpSlots points
          -- For each point, take an iterator and the first point from it.
          -- If there is a first point, that's our new spot. We can de-allocate
          -- any existing iterator and use this new one.
          checkForPoint
            :: Maybe (Slot, CSL.HeaderHash, DB.Iterator IO, ReleaseKey)
            -> Point
            -> ResourceT IO (Maybe (Slot, CSL.HeaderHash, DB.Iterator IO, ReleaseKey))
          checkForPoint = \found point -> case found of
            Just _  -> pure found
            Nothing -> do
              (releaseKey, iteratorResource) <- allocate
                (DB.readFrom db (DB.BySlot (pointSlot point)))
                DB.closeIterator
              next <- lift $ DB.next (DB.iterator iteratorResource)
              case next of
                DB.Done -> do
                  release releaseKey
                  pure Nothing
                -- We have to decode the bytes and check the header hash against
                -- the given point.
                DB.More slot bytes iterator' -> do
                  hh <- case DB.decodeFull cslBlockDecoder (Lazy.fromStrict bytes) of
                    Left cborError -> lift $ err cborError
                    Right block    -> pure $ CSL.headerHash block
                  if hh == pointHash point
                  then pure $ Just (slot, hh, iterator', releaseKey)
                  else pure Nothing
      mFound <- foldlM checkForPoint Nothing orderedPoints
      -- No matter what, we have to give the current tip.
      -- FIXME why? Should only need to give it if there's a change.
      (tipSlot, tipBlock) <- lift $ DB.readTip db
      let tipHash  = CSL.headerHash tipBlock
          tipPoint = Point tipSlot tipHash
      -- If there's a new point, release any existing iterator and keep the
      -- one we just made. Since we already read a block from it that the
      -- client claims to have, the iterator is now at the appropriate point.
      case mFound of
        Nothing -> pure $ SendMsgIntersectUnchanged tipPoint (chainSyncServerAt err poll db ss)
        Just (newSlot, newHash, newIterator, newReleaseKey) -> do
          -- Release the old iterator, if any.
          maybe (pure ()) (release . snd) (ssIterator ss)
          -- The new iterator is used from now on.
          let newPoint = Point newSlot newHash
              ss' = ss { ssNewestKnownSlot = Just newSlot
                       , ssIterator = Just (newIterator, newReleaseKey)
                       }
          pure $ SendMsgIntersectImproved newPoint tipPoint (chainSyncServerAt err poll db ss')

  , recvMsgRequestNext = case ssIterator ss of
      -- There's no iterator. Bring one up beginning at least from the next
      -- slot, using `ResourceT` to ensure it gets de-allocated.
      Nothing -> do
        let point = DB.BySlot (nextSlot (ssNewestKnownSlot ss))
        (releaseKey, iteratorResource) <- allocate (DB.readFrom db point) DB.closeIterator
        -- now we can simply call back into this term with the new state.
        let ss' = ss { ssIterator = Just (DB.iterator iteratorResource, releaseKey) }
        recvMsgRequestNext (chainSyncServerIdle err poll db ss')
      Just (iterator, releaseKey) -> do
        next <- lift $ DB.next iterator
        case next of
          DB.Done -> pure $ Right $ do
            -- Release the iterator via `ResourceT`.
            release releaseKey
            -- Must block until there's a block with slot greater than or equal
            -- to the next slot. 
            let ss' = ss { ssIterator = Nothing }
                condition = \outcome -> case outcome of
                  Left stNext -> Just stNext
                  Right _     -> Nothing
            poll condition (recvMsgRequestNext (chainSyncServerIdle err poll db ss'))
          DB.More slot bytes iterator' -> do
            block <- case DB.decodeFull cslBlockDecoder (Lazy.fromStrict bytes) of
              Left cborError -> lift $ err cborError
              Right block    -> pure block
            -- We need to give the curren tip point with _every_ message.
            -- FIXME should only give it if it has changed.
            (tipSlot, tipBlock) <- lift $ DB.readTip db
            let hh     = CSL.headerHash block
                point' = Point slot hh
                tipHash  = CSL.headerHash tipBlock
                tipPoint = Point tipSlot tipHash
                ss'    = ss { ssNewestKnownSlot = Just slot
                            -- The releaseKey is for the whole iterator resource
                            -- so it stays the same.
                            , ssIterator        = Just (iterator', releaseKey)
                            }
            pure $ Left $ SendMsgRollForward block tipPoint (chainSyncServerAt err poll db ss')
  }

  where
  -- Grab the cardano-sl `Block` decoder from the cardano-sl `Bi` instance.
  -- The `Decoder` itself is unfortunately never exported, and accessible only
  -- by way of typeclass.
  cslBlockDecoder :: CBOR.Decoder s CSL.Block
  cslBlockDecoder = CSL.decode
