{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  deriving (Show, Eq)

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
chainSyncServer err poll db = chainSyncServerAt err poll db NoTip

chainSyncServerAt
  :: (forall x . CBOR.DeserialiseFailure -> IO x)
  -> PollT IO
  -> DB IO
  -> ServerState
  -> ChainSyncServer CSL.Block Point (ResourceT IO) ()
chainSyncServerAt err poll db ss = ChainSyncServer $
  pure $ chainSyncServerIdle err poll db ss

pollForTipChange
  :: forall m .
     ( Monad m )
  => (forall x . CBOR.DeserialiseFailure -> m x)
  -> PollT m
  -> DB m
  -> Maybe Point
  -> ResourceT m Point
pollForTipChange err poll db mPoint = poll takePoint (lift (DB.readTip db))
  where
  takePoint :: DB.Tip -> ResourceT m (Maybe Point)
  takePoint tip = case mPoint of
    -- There's no existing point, so we'll take the tip unless the DB is empty.
    Nothing -> case tip of
      DB.TipGenesis -> pure Nothing
      DB.TipEBB slot hash _ -> pure $ Just (Point slot hash)
      DB.TipBlock slot bytes -> case DB.decodeFull cslBlockDecoder (Lazy.fromStrict bytes) of
        Left cborError -> lift $ err cborError
        Right blk      -> pure $ Just $ Point slot (CSL.headerHash blk)
    -- Must wait for a tip better than the given point.
    -- We assume (since there are no forks by assumption) that if the new
    -- tip is not equal to this point, then it's better.
    Just point -> do
      point' <- lift $ pickBetterTip err point tip
      pure $ if point == point'
             then Nothing
             else Just point'

pickBetterTip
  :: ( Applicative m )
  => (forall x . CBOR.DeserialiseFailure -> m x)
  -> Point
  -> DB.Tip
  -> m Point
-- FIXME this case is actually an error. If we have a tip point but the
-- DB is empty then something went horribly wrong.
pickBetterTip _ point DB.TipGenesis = pure point
pickBetterTip _ point (DB.TipEBB slot hash _) =
  if pointSlot point < slot
  then pure $ Point slot hash
  else pure point
-- Careful: the `point` could be for an EBB, in which case the slots are the
-- same but we want to pick the block.
-- So just use a non-strict equality. Since we're only using immutable DB with
-- no forks, it is correct.
pickBetterTip err point (DB.TipBlock slot bytes) =
  if pointSlot point <= slot
  then case DB.decodeFull cslBlockDecoder (Lazy.fromStrict bytes) of
    Left cborError -> err cborError
    Right blk      -> pure $ Point slot (CSL.headerHash blk)
  else pure point

chainSyncServerIdle
  :: (forall x . CBOR.DeserialiseFailure -> IO x)
  -> PollT IO
  -> DB IO
  -> ServerState
  -> ServerStIdle CSL.Block Point (ResourceT IO) ()
chainSyncServerIdle err poll db ss = case ss of
  -- If there's no tip, poll for a tip change only when a request comes in.
  NoTip -> ServerStIdle
    { recvMsgDoneClient = pure ()
    , recvMsgFindIntersect = \points -> do
        tipPoint <- pollForTipChange err poll db Nothing
        let ss' = KnownTip tipPoint Nothing Nothing
        recvMsgFindIntersect (chainSyncServerIdle err poll db ss') points
    , recvMsgRequestNext = do
        tipPoint <- pollForTipChange err poll db Nothing
        let ss' = KnownTip tipPoint Nothing Nothing
        recvMsgRequestNext (chainSyncServerIdle err poll db ss')
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
              :: Maybe (Slot, CSL.HeaderHash, DB.Iterator IO, ReleaseKey)
              -> Point
              -> ResourceT IO (Maybe (Slot, CSL.HeaderHash, DB.Iterator IO, ReleaseKey))
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
                    hash <- case DB.decodeFull cslBlockDecoder (Lazy.fromStrict bytes) of
                      Left cborError -> lift $ err cborError
                      Right block    -> pure $ CSL.headerHash block
                    if hash == pointHash point
                    then pure $ Just (slot, hash, iterator', releaseKey)
                    else pure Nothing
        mFound <- foldlM checkForPoint Nothing orderedPoints
        -- No matter what, we have to give the current tip.
        -- FIXME why? Should only need to give it if there's a change.
        dbTip <- lift $ DB.readTip db
        tipPoint' <- lift $ pickBetterTip err tipPoint dbTip
        -- If there's a new point, release any existing iterator and keep the
        -- one we just made. Since we already read a block from it that the
        -- client claims to have, the iterator is now at the appropriate point.
        case mFound of
          Nothing -> do
            let ss' = KnownTip tipPoint' mLastKnownPoint mIterator
            pure $ SendMsgIntersectUnchanged tipPoint' (chainSyncServerAt err poll db ss')
          Just (newSlot, newHash, newIterator, newReleaseKey) -> do
            -- Release the old iterator, if any.
            maybe (pure ()) (release . snd) mIterator
            -- The new iterator is used from now on.
            let newPoint = Point newSlot newHash
                ss' = KnownTip tipPoint' (Just newPoint) (Just (newIterator, newReleaseKey))
            pure $ SendMsgIntersectImproved newPoint tipPoint' (chainSyncServerAt err poll db ss')

    , recvMsgRequestNext = case mIterator of
        -- There's no iterator. Bring one up beginning at least from the next
        -- slot, using `ResourceT` to ensure it gets de-allocated.
        Nothing -> case mLastKnownPoint of
          Nothing -> do
            (releaseKey, iteratorResource) <- allocate (DB.readFrom db DB.FromGenesis) DB.closeIterator
            -- Last known point remains Nothing because we haven't yet served
            -- a block.
            let ss' = KnownTip tipPoint Nothing (Just (DB.iterator iteratorResource, releaseKey))
            recvMsgRequestNext (chainSyncServerIdle err poll db ss')
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
            recvMsgRequestNext (chainSyncServerIdle err poll db ss')
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
              poll condition (recvMsgRequestNext (chainSyncServerIdle err poll db ss'))
            DB.NextEBB slot hash bytes iterator' -> case DB.decodeFull cslBlockDecoder (Lazy.fromStrict bytes) of
              Left cborError -> lift $ err cborError
              Right ebb -> do
                dbTip <- lift $ DB.readTip db
                tipPoint' <- lift $ pickBetterTip err tipPoint dbTip
                let ss' = KnownTip tipPoint' (Just (Point slot hash)) (Just (iterator', releaseKey))
                pure $ Left $ SendMsgRollForward ebb tipPoint (chainSyncServerAt err poll db ss')
            DB.NextBlock slot bytes iterator' -> case DB.decodeFull cslBlockDecoder (Lazy.fromStrict bytes) of
              Left cborError -> lift $ err cborError
              Right blk -> do
                dbTip <- lift $ DB.readTip db
                tipPoint' <- lift $ pickBetterTip err tipPoint dbTip
                let hash = CSL.headerHash blk
                    ss' = KnownTip tipPoint' (Just (Point slot hash)) (Just (iterator', releaseKey))
                pure $ Left $ SendMsgRollForward blk tipPoint (chainSyncServerAt err poll db ss')
    }

cslBlockDecoder :: CBOR.Decoder s CSL.Block
cslBlockDecoder = CSL.decode
