{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
-- | Model for the 'ImmutableDB' based on a chain.
--
-- The chain is just a list of slots that can be unfilled (@Nothing@) or
-- filled (@Just ByteString@).
module Test.Ouroboros.Storage.ImmutableDB.Model
  ( DBModel(..)
  , dbmBlobs
  , initDBModel
  , IteratorModel
  , openDBModel
  , lookupNextEpochSlot
  , getLastBlobLocation
  ) where

import           Control.Monad (when)
import           Control.Monad.State (MonadState, get, gets, put, modify)

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List (genericReplicate, dropWhileEnd)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe, fromMaybe, mapMaybe, isNothing)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, withFrozenCallStack)

import           Ouroboros.Consensus.Util (lastMaybe)

import           Ouroboros.Network.Block (Slot(..))

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Util hiding (lookupEpochSize)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes (CumulEpochSizes)
import qualified Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CES


data DBModel = DBModel
  { dbmChain           :: [Maybe ByteString]
    -- ^ 'Nothing' when a slot is empty
  , dbmNextSlot        :: Slot
    -- ^ The number of the next available slot
  , dbmCumulEpochSizes :: CumulEpochSizes
  , dbmIterators       :: Map IteratorID IteratorModel
  , dbmNextIterator    :: IteratorID
  } deriving (Show, Generic)

initDBModel :: EpochSize -- ^ The size of the first epoch
            -> DBModel
initDBModel firstEpochSize = DBModel
  { dbmChain           = []
  , dbmNextSlot        = 0
  , dbmCumulEpochSizes = CES.singleton firstEpochSize
  , dbmIterators       = Map.empty
  , dbmNextIterator    = initialIteratorId
  }

dbmBlobs :: DBModel -> Map EpochSlot ByteString
dbmBlobs dbm@DBModel {..} = foldr add Map.empty (zip (map Slot [0..]) dbmChain)
  where
    add (_,    Nothing)   = id
    add (slot, Just blob) = Map.insert (slotToEpochSlot dbm slot) blob


-- | Model for an 'Iterator'.
--
-- The first 'Slot' = return the next filled slot (doesn't have to exist) >=
-- this 'Slot'. The second 'Slot': last filled slot to return (inclusive).
--
-- An iterator is open iff its is present in 'dbmIterators'
newtype IteratorModel = IteratorModel (Slot, Slot)
  deriving (Show, Eq, Generic)




{------------------------------------------------------------------------------
  ImmutableDB API
------------------------------------------------------------------------------}

openDBModel :: MonadState DBModel m
            => ErrorHandling ImmutableDBError m
            -> EpochSize
            -> (DBModel, ImmutableDB m)
openDBModel err firstEpochSize = (dbModel, db)
  where
    dbModel = initDBModel firstEpochSize
    db = ImmutableDB
      { closeDB           = return ()
      , isOpen            = return True
      , reopen            = const reopenModel -- No recovery
      , getNextEpochSlot  = getNextEpochSlotModel
      , getBinaryBlob     = getBinaryBlobModel     err
      , appendBinaryBlob  = appendBinaryBlobModel  err
      , startNewEpoch     = startNewEpochModel
      , streamBinaryBlobs = streamBinaryBlobsModel err
      , immutableDBErr    = err
      }

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

impossible :: HasCallStack => String -> a
impossible msg = withFrozenCallStack (error ("Impossible: " ++ msg))

mustBeJust :: HasCallStack => String -> Maybe a -> a
mustBeJust msg = fromMaybe (impossible msg)

lookupNextEpochSlot :: HasCallStack => DBModel -> EpochSlot
lookupNextEpochSlot dbm@DBModel {..} = case slotToEpochSlot dbm dbmNextSlot of
    -- TODO imitates the Impl
    EpochSlot epoch 0 | epoch == currentEpoch + 1
      -> EpochSlot currentEpoch (fromIntegral epochSize)
    epochSlot -> epochSlot
  where
    currentEpoch = CES.lastEpoch dbmCumulEpochSizes
    epochSize    = lookupEpochSize dbm currentEpoch

lookupEpochSize :: HasCallStack => DBModel -> Epoch -> EpochSize
lookupEpochSize DBModel {..} epoch =
    mustBeJust msg $ CES.epochSize dbmCumulEpochSizes epoch
  where
    msg = "epoch (" <> show epoch <> ") size unknown (" <>
          show dbmCumulEpochSizes <> ")"

epochSlotToSlot :: HasCallStack => DBModel -> EpochSlot -> Slot
epochSlotToSlot DBModel {..} epochSlot =
    mustBeJust msg $ CES.epochSlotToSlot dbmCumulEpochSizes epochSlot
  where
    msg = "epochSlot (" <> show epochSlot <>
          ") could not be converted to a slot"

slotToEpochSlot :: HasCallStack => DBModel -> Slot -> EpochSlot
slotToEpochSlot DBModel {..} slot =
    mustBeJust msg $ CES.slotToEpochSlot dbmCumulEpochSizes slot
  where
    msg = "slot (" <> show slot <>
          ") could not be converted to an epochSlot (" <>
          show dbmCumulEpochSizes <> ")"

lookupBySlot :: HasCallStack => Slot -> [Maybe b] -> Maybe b
lookupBySlot (Slot i) = go i
  where
    go 0 (blob:_)  = blob
    go n (_:blobs) = go (n - 1) blobs
    go _ []        = error "lookupBySlot: index out of bounds"

getLastBlobLocation :: DBModel -> Maybe EpochSlot
getLastBlobLocation dbm@DBModel {..} =
    fmap (slotToEpochSlot dbm) $
    lastMaybe $ zipWith const [0..] $ dropWhileEnd isNothing dbmChain


{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

reopenModel :: MonadState DBModel m => m (Maybe EpochSlot)
reopenModel = gets getLastBlobLocation


getNextEpochSlotModel :: (HasCallStack, MonadState DBModel m)
                      => m EpochSlot
getNextEpochSlotModel = gets lookupNextEpochSlot

getBinaryBlobModel :: (HasCallStack, MonadState DBModel m)
                   => ErrorHandling ImmutableDBError m
                   -> EpochSlot
                   -> m (Maybe ByteString)
getBinaryBlobModel err readEpochSlot = do
    dbm@DBModel {..} <- get
    let nextEpochSlot = lookupNextEpochSlot dbm
    when (readEpochSlot >= nextEpochSlot) $ throwUserError err $
      ReadFutureSlotError readEpochSlot nextEpochSlot

    let EpochSlot epoch relativeSlot = readEpochSlot
        -- We already know that 'readEpochSlot' does not lie in the future, so
        -- the size of the corresponding epoch must be known.
        epochSize = lookupEpochSize dbm epoch
    when (getRelativeSlot relativeSlot >= epochSize) $ throwUserError err $
      SlotGreaterThanEpochSizeError relativeSlot epoch epochSize

    let slot = epochSlotToSlot dbm readEpochSlot

    return $ lookupBySlot slot dbmChain


appendBinaryBlobModel :: (HasCallStack, MonadState DBModel m)
                      => ErrorHandling ImmutableDBError m
                      -> RelativeSlot
                      -> Builder
                      -> m ()
appendBinaryBlobModel err relSlot bld = do
    dbm@DBModel {..} <- get
    let EpochSlot currentEpoch nextRelSlot = lookupNextEpochSlot dbm

    when (relSlot < nextRelSlot) $
      throwUserError err $ AppendToSlotInThePastError relSlot nextRelSlot

    let epochSize = lookupEpochSize dbm currentEpoch
    when (getRelativeSlot relSlot >= epochSize) $ throwUserError err $
      SlotGreaterThanEpochSizeError relSlot currentEpoch epochSize

    let appendEpochSlot = EpochSlot currentEpoch relSlot
        appendSlot = epochSlotToSlot dbm appendEpochSlot
        blob = BL.toStrict $ BS.toLazyByteString bld
        toPad = fromIntegral (appendSlot - dbmNextSlot)

    -- TODO snoc list?
    put dbm
      { dbmChain    = dbmChain ++ (replicate toPad Nothing ++ [Just blob])
      , dbmNextSlot = appendSlot + 1
      }

startNewEpochModel :: MonadState DBModel m
                   => EpochSize
                   -> m Epoch
startNewEpochModel nextEpochSize = do
    dbm@DBModel {..} <- get
    let toPad = CES.maxSlot dbmCumulEpochSizes + 1 - dbmNextSlot
        newEpoch = CES.lastEpoch dbmCumulEpochSizes + 1

    put dbm
      { dbmChain = dbmChain ++ genericReplicate toPad Nothing
        -- We use 'genericReplicate' instead of the regular 'replicate', which
        -- takes an 'Int', because when converting from a 'Slot' (~= 'Word')
        -- to an 'Int', it could underflow. While this scenario is not very
        -- realistic, it can turn up in the StateMachine tests, in which case
        -- we want it to fail with a better error than the error thrown by
        -- 'fromEnum'.
      , dbmNextSlot = dbmNextSlot + fromIntegral toPad
      , dbmCumulEpochSizes = CES.snoc dbmCumulEpochSizes nextEpochSize }
    return newEpoch

streamBinaryBlobsModel :: (HasCallStack, MonadState DBModel m)
                       => ErrorHandling ImmutableDBError m
                       -> Maybe EpochSlot
                       -> Maybe EpochSlot
                       -> m (Iterator m)
streamBinaryBlobsModel err mbStart mbEnd = do
    dbm@DBModel {..} <- get
    let nextEpochSlot = lookupNextEpochSlot dbm
        getEpochSize epoch = return $ lookupEpochSize dbm epoch
    validateIteratorRange err nextEpochSlot getEpochSize mbStart mbEnd

    case length $ dropWhileEnd isNothing dbmChain of
      -- Empty database, return an empty iterator
      0 -> do
        put dbm { dbmNextIterator = succ dbmNextIterator }
        return Iterator
          { iteratorNext  = return IteratorExhausted
          , iteratorClose = return ()
          , iteratorID    = dbmNextIterator
          }
      n -> do
        let currentSlot = fromIntegral n - 1
            start = maybe 0 (epochSlotToSlot dbm) mbStart
            end   = maybe currentSlot (epochSlotToSlot dbm) mbEnd
            itm   = IteratorModel (start, end)
            itID  = dbmNextIterator
        put dbm
          { dbmNextIterator = succ dbmNextIterator
          , dbmIterators    = Map.insert dbmNextIterator itm dbmIterators
          }
        return Iterator
          { iteratorNext  = iteratorNextModel  itID
          , iteratorClose = iteratorCloseModel itID
          , iteratorID    = itID
          }


iteratorNextModel :: (HasCallStack, MonadState DBModel m)
                  => IteratorID
                  -> m IteratorResult
iteratorNextModel itID = do
    dbm@DBModel {..} <- get
    case Map.lookup itID dbmIterators of
      Nothing -> return IteratorExhausted
      Just (IteratorModel (minNext, end)) -> do
        let mbBlob = listToMaybe
                   $ takeWhile ((<= end) . fst)
                   $ dropWhile ((< minNext) . fst)
                   $ mapMaybe (\(slot, mbBlob') -> (slot,) <$> mbBlob')
                   $ zip [0..] dbmChain
        case mbBlob of
          Nothing -> do
            iteratorCloseModel itID
            return IteratorExhausted
          Just (next, blob) -> do
            if next > end
              then iteratorCloseModel itID
              else
                let next' = succ next
                    itm'  = IteratorModel (next', end)
                in put dbm { dbmIterators = Map.insert itID itm' dbmIterators }
            return $ IteratorResult (slotToEpochSlot dbm next) blob

iteratorCloseModel :: MonadState DBModel m
                   => IteratorID -> m ()
iteratorCloseModel itID = modify $ \dbm@DBModel {..} ->
    dbm { dbmIterators = Map.delete itID dbmIterators }
