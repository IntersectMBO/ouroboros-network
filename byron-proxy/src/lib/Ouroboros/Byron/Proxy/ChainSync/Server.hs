{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Byron.Proxy.ChainSync.Server where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Resource (ResourceT, ReleaseKey, allocate, release)
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (foldlM)
import Data.List (sortBy)
import Data.Ord (Down (..))
import qualified Data.Text as Text (pack)

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano

import Ouroboros.Byron.Proxy.ChainSync.Types (Block, Point (..))
import Ouroboros.Byron.Proxy.DB (DB)
import qualified Ouroboros.Byron.Proxy.DB as DB
import Ouroboros.Network.Protocol.ChainSync.Server
import Ouroboros.Storage.ImmutableDB.API (SlotNo (..))


data ServerState where
  -- | Initial server state: we don't know the tip, either because we haven't
  -- tried to get it yet (no request) or because the database is empty.
  NoTip    :: ServerState
  -- | Some tip is known (was once the tip), and there could be a point that
  -- we know the client has, and there could be an iterator that is serving
  -- the client.
  KnownTip :: !Point
           -> !(Maybe Point)
           -> !(Maybe (DB.Iterator IO, ReleaseKey))
           -> ServerState

-- Problem: getting the next thing after a point.
-- If we know the client has an EBB, we want to get an iterator which takes
-- the first block of that epoch, if it's there.
-- Solution: just take an iterator from a block they definitely have, and
-- skip the first one.


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
type Poll m = forall s t . (s -> m (Maybe t)) -> m s -> m t

-- | Since we'll unfortunately have to use `ResourceT`, we'll need to be
-- able to take a `Poll (ResourceT IO s)`, but ideally the poll definition
-- would not be allowed to use any of the `ResourceT` stuff, so we'll use an
-- arbitrary `MonadTrans`.
type PollT m = forall f . (Monad (f m), MonadTrans f) => Poll (f m)

-- | A ChainSync server of full cardano-sl `Block`s (that includes EBBs)
-- backed by a `DB`. Will never give a rollback because the DB (for now) is
-- immutable.
--
-- The whole thing must go in a `ResourceT`, because typed transitions protocol
-- applications are not in CPS and cannot do proper bracketing. This is the
-- same problem that conduit/pipes/streaming suffer from.
--
-- And then, since we use `ResourceT`, we're essentially forced to use `IO`,
-- by the type of `allocate`: `ResourceT` uses an `IORef`, so we're stuck.
-- It's tragic.
chainSyncServer
  :: Cardano.EpochSlots
  -> (forall x . Binary.DecoderError -> IO x)
  -> PollT IO
  -> DB IO
  -> ChainSyncServer Block Point (ResourceT IO) ()
chainSyncServer epochSlots err poll db = chainSyncServerAt epochSlots err poll db NoTip

chainSyncServerAt
  :: Cardano.EpochSlots
  -> (forall x . Binary.DecoderError -> IO x)
  -> PollT IO
  -> DB IO
  -> ServerState
  -> ChainSyncServer Block Point (ResourceT IO) ()
chainSyncServerAt epochSlots err poll db ss = ChainSyncServer $
  pure $ chainSyncServerIdle epochSlots err poll db ss

-- | Periodically check the datbase until the tip is better than a given
-- point (use `Nothing` to wait until it's non-empty).
pollForTipChange
  :: forall m .
     ( Monad m )
  => Cardano.EpochSlots
  -> (forall x . Binary.DecoderError -> m x)
  -> PollT m
  -> DB m
  -> Maybe Point
  -> ResourceT m Point
pollForTipChange epochSlots err poll db mPoint = poll takePoint (lift (DB.readTip db))
  where
  takePoint :: DB.Tip -> ResourceT m (Maybe Point)
  takePoint tip = case mPoint of
    -- There's no existing point, so we'll take the tip unless the DB is empty.
    Nothing -> case tip of
      DB.TipGenesis -> pure Nothing
      DB.TipEBB slot hash _ -> pure $ Just (Point slot hash)
      DB.TipBlock slot bytes -> case decodeBlock epochSlots (Lazy.fromStrict bytes) of
        Left cborError -> lift $ err cborError
        Right ablk     -> case Binary.unAnnotated ablk of
          Cardano.ABOBBlock blk  -> pure $ Just $ Point slot (Cardano.blockHashAnnotated blk)
          -- FIXME deal with this better.
          Cardano.ABOBBoundary _ -> error "Corrupt DB: EBB where block expected"
    -- Must wait for a tip better than the given point.
    -- We assume (since there are no forks by assumption) that if the new
    -- tip is not equal to this point, then it's better.
    Just point -> do
      point' <- lift $ pickBetterTip epochSlots err point tip
      pure $ if point == point'
             then Nothing
             else Just point'

pickBetterTip
  :: ( Applicative m )
  => Cardano.EpochSlots
  -> (forall x . Binary.DecoderError -> m x)
  -> Point
  -> DB.Tip
  -> m Point
-- FIXME this case is actually an error. If we have a tip point but the
-- DB is empty then something went horribly wrong.
pickBetterTip _ _ point DB.TipGenesis = pure point
pickBetterTip _ _ point (DB.TipEBB slot hash _) =
  if pointSlot point < slot
  then pure $ Point slot hash
  else pure point
-- Careful: the `point` could be for an EBB, in which case the slots are the
-- same but we want to pick the block.
-- So just use a non-strict equality. Since we're only using immutable DB with
-- no forks, it is correct.
pickBetterTip epochSlots err point (DB.TipBlock slot bytes) =
  if pointSlot point <= slot
  then case decodeBlock epochSlots (Lazy.fromStrict bytes) of
    Left cborError -> err cborError
    Right ablk     -> case Binary.unAnnotated ablk of
      Cardano.ABOBBlock blk  -> pure $ Point slot (Cardano.blockHashAnnotated blk)
      -- FIXME deal with this better.
      Cardano.ABOBBoundary _ -> error "Corrupt DB: EBB where block expected"
  else pure point

chainSyncServerIdle
  :: Cardano.EpochSlots
  -> (forall x . Binary.DecoderError -> IO x)
  -> PollT IO
  -> DB IO
  -> ServerState
  -> ServerStIdle Block Point (ResourceT IO) ()
chainSyncServerIdle epochSlots err poll db ss = case ss of
  -- If there's no tip, poll for a tip change only when a request comes in.
  NoTip -> ServerStIdle
    { recvMsgDoneClient = pure ()
    , recvMsgFindIntersect = \points -> do
        tipPoint <- pollForTipChange epochSlots err poll db Nothing
        let ss' = KnownTip tipPoint Nothing Nothing
        recvMsgFindIntersect (chainSyncServerIdle epochSlots err poll db ss') points
    , recvMsgRequestNext = do
        tipPoint <- pollForTipChange epochSlots err poll db Nothing
        let ss' = KnownTip tipPoint Nothing Nothing
        recvMsgRequestNext (chainSyncServerIdle epochSlots err poll db ss')
    }

  KnownTip tipPoint mLastKnownPoint mIterator -> ServerStIdle
    { recvMsgDoneClient = case mIterator of
        Nothing -> pure ()
        Just (_, releaseKey) -> release releaseKey

    , recvMsgFindIntersect = \points -> do
        -- Order the list by slot descending, and find the first entry which is
        -- in the database.
        let cmpSlots p1 p2 = Down (pointSlot p1) `compare` Down (pointSlot p2)
            orderedPoints = sortBy cmpSlots points
            -- For each point, take an iterator and the first point from it.
            -- If there is a first point, that's our new spot. We can de-allocate
            -- any existing iterator and use this new one.
            checkForPoint
              :: Maybe (SlotNo, Cardano.HeaderHash, DB.Iterator IO, ReleaseKey)
              -> Point
              -> ResourceT IO (Maybe (SlotNo, Cardano.HeaderHash, DB.Iterator IO, ReleaseKey))
            checkForPoint = \found point -> case found of
              Just _  -> pure found
              Nothing -> do
                (releaseKey, iteratorResource) <- allocate
                  (DB.readFrom db (DB.FromPoint (pointSlot point) (pointHash point)))
                  DB.closeIterator
                next <- lift $ DB.next (DB.iterator iteratorResource)
                case next of
                  DB.Done -> do
                    release releaseKey
                    pure Nothing
                  -- EBBs come out of the DB with their hash so we can just
                  -- compare on that.
                  DB.NextEBB epoch hash bytes iterator' -> do
                    -- DB guarantees that `hash = pointHash point`.
                    pure $ Just (pointSlot point, hash, iterator', releaseKey)
                  -- The DB does not check that the hash matches, if the item is
                  -- not an EBB, so to figure out whether this is the block we
                  -- want, we have to get its header hash, which involves
                  -- deserialising and then reserialising (via headerHash).
                  DB.NextBlock slot bytes iterator' -> do
                    hash <- case decodeBlock epochSlots (Lazy.fromStrict bytes) of
                      Left cborError -> lift $ err cborError
                      Right ablk     -> case Binary.unAnnotated ablk of
                        Cardano.ABOBBlock blk  -> pure $ Cardano.blockHashAnnotated blk
                        Cardano.ABOBBoundary _ -> error "Corrupt DB: EBB where block expected"
                    if hash == pointHash point
                    then pure $ Just (slot, hash, iterator', releaseKey)
                    else pure Nothing
        mFound <- foldlM checkForPoint Nothing orderedPoints
        -- No matter what, we have to give the current tip.
        -- FIXME why? Should only need to give it if there's a change.
        dbTip <- lift $ DB.readTip db
        tipPoint' <- lift $ pickBetterTip epochSlots err tipPoint dbTip
        -- If there's a new point, release any existing iterator and keep the
        -- one we just made. Since we already read a block from it that the
        -- client claims to have, the iterator is now at the appropriate point.
        case mFound of
          Nothing -> do
            let ss' = KnownTip tipPoint' mLastKnownPoint mIterator
            pure $ SendMsgIntersectUnchanged tipPoint' (chainSyncServerAt epochSlots err poll db ss')
          Just (newSlot, newHash, newIterator, newReleaseKey) -> do
            -- Release the old iterator, if any.
            maybe (pure ()) (release . snd) mIterator
            -- The new iterator is used from now on.
            let newPoint = Point newSlot newHash
                ss' = KnownTip tipPoint' (Just newPoint) (Just (newIterator, newReleaseKey))
            pure $ SendMsgIntersectImproved newPoint tipPoint' (chainSyncServerAt epochSlots err poll db ss')

    , recvMsgRequestNext = case mIterator of
        -- There's no iterator. Bring one up beginning at least from the next
        -- slot, using `ResourceT` to ensure it gets de-allocated.
        Nothing -> case mLastKnownPoint of
          Nothing -> do
            (releaseKey, iteratorResource) <- allocate (DB.readFrom db DB.FromGenesis) DB.closeIterator
            -- Last known point remains Nothing because we haven't yet served
            -- a block.
            let ss' = KnownTip tipPoint Nothing (Just (DB.iterator iteratorResource, releaseKey))
            recvMsgRequestNext (chainSyncServerIdle epochSlots err poll db ss')
          Just point -> do
            (releaseKey, iteratorResource) <- allocate (DB.readFrom db (DB.FromPoint (pointSlot point) (pointHash point))) DB.closeIterator
            -- Iterator starts from that point, so we have to pass over it
            -- before recursing.
            next <- lift $ DB.next (DB.iterator iteratorResource)
            iterator' <- case next of
              -- We served them the block at this point. How could we all of
              -- a sudden not have it? DB is immutable.
              DB.Done -> error "this should not happen"
              DB.NextEBB _ _ _ iterator' -> pure iterator'
              DB.NextBlock _ _ iterator' -> pure iterator'
            let ss' = KnownTip tipPoint mLastKnownPoint (Just (iterator', releaseKey))
            recvMsgRequestNext (chainSyncServerIdle epochSlots err poll db ss')
        Just (iterator, releaseKey) -> do
          next <- lift $ DB.next iterator
          case next of
            -- If there's nothing, use the poller to repeatedly call back into
            -- this piece until it's `Left`.
            DB.Done -> pure $ Right $ do
              release releaseKey
              let ss' = KnownTip tipPoint mLastKnownPoint Nothing
                  condition term = pure $ case term of
                    Left serverNext -> Just serverNext
                    Right _ -> Nothing
              poll condition (recvMsgRequestNext (chainSyncServerIdle epochSlots err poll db ss'))
            DB.NextEBB slot hash bytes iterator' -> case decodeBlock epochSlots (Lazy.fromStrict bytes) of
              Left cborError -> lift $ err cborError
              Right ablk -> case Binary.unAnnotated ablk of
                Cardano.ABOBBlock _ -> error "Corrupt DB: block where EBB expected"
                Cardano.ABOBBoundary ebb -> do
                  dbTip <- lift $ DB.readTip db
                  tipPoint' <- lift $ pickBetterTip epochSlots err tipPoint dbTip
                  let ss' = KnownTip tipPoint' (Just (Point slot hash)) (Just (iterator', releaseKey))
                  pure $ Left $ SendMsgRollForward ablk tipPoint (chainSyncServerAt epochSlots err poll db ss')
            DB.NextBlock slot bytes iterator' -> case decodeBlock epochSlots (Lazy.fromStrict bytes) of
              Left cborError -> lift $ err cborError
              Right ablk -> case Binary.unAnnotated ablk of
                Cardano.ABOBBoundary _ -> error "Corrupt DB: EBB where block expected"
                Cardano.ABOBBlock blk -> do
                  dbTip <- lift $ DB.readTip db
                  tipPoint' <- lift $ pickBetterTip epochSlots err tipPoint dbTip
                  let hash = Cardano.blockHashAnnotated blk
                      ss' = KnownTip tipPoint' (Just (Point slot hash)) (Just (iterator', releaseKey))
                  pure $ Left $ SendMsgRollForward ablk tipPoint (chainSyncServerAt epochSlots err poll db ss')
    }

-- FIXME better/more concise/more efficient way to do this?
decodeBlock
  :: Cardano.EpochSlots
  -> Lazy.ByteString
  -> Either Binary.DecoderError Block
decodeBlock epochSlots lbs = fmap (flip Binary.Annotated (Lazy.toStrict lbs)) $
  Binary.decodeFullAnnotatedBytes
    (Text.pack "Block or boundary")
    (Cardano.fromCBORABlockOrBoundary epochSlots)
    lbs
