{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Ouroboros.Storage.ImmutableDB.Mock (openDBMock) where

import           Data.Bifunctor (first)
import           Data.Tuple (swap)

import           Ouroboros.Consensus.Util ((...:), (..:), (.:))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common (BlockComponent)
import           Ouroboros.Consensus.Storage.ImmutableDB.API
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks

import           Test.Ouroboros.Storage.ImmutableDB.Model

openDBMock  :: forall m hash. (IOLike m, Eq hash, Show hash)
            => ChunkInfo
            -> m (DBModel hash, ImmutableDB hash m)
openDBMock chunkInfo = do
    dbVar <- uncheckedNewTVarM dbModel
    return (dbModel, immDB dbVar)
  where
    dbModel = initDBModel chunkInfo

    immDB :: StrictTVar m (DBModel hash) -> ImmutableDB hash m
    immDB dbVar = ImmutableDB
        { closeDB_                = return ()
        , getTip_                 = query       $ getTipModel
        , getBlockComponent_      = queryE     .: getBlockComponentModel
        , getEBBComponent_        = queryE     .: getEBBComponentModel
        , getBlockOrEBBComponent_ = queryE    ..: getBlockOrEBBComponentModel
        , appendBlock_            = updateE_ ...: appendBlockModel
        , appendEBB_              = updateE_ ...: appendEBBModel
        , stream_                 = updateEE ...: \_rr bc s e -> fmap (fmap (first (iterator bc))) . streamModel s e
        }
      where
        iterator :: BlockComponent (ImmutableDB hash m) b
                 -> IteratorId
                 -> Iterator hash m b
        iterator blockComponent itId = Iterator
          { iteratorNext    = update  $ iteratorNextModel    itId blockComponent
          , iteratorHasNext = query   $ iteratorHasNextModel itId
          , iteratorClose   = update_ $ iteratorCloseModel   itId
          }

        update_ :: (DBModel hash -> DBModel hash) -> m ()
        update_ f = atomically $ modifyTVar dbVar f

        update :: (DBModel hash -> (a, DBModel hash)) -> m a
        update f = atomically $ updateTVar dbVar (swap . f)

        updateE_ :: (DBModel hash -> Either ImmutableDBError (DBModel hash)) -> m ()
        updateE_ f = atomically $ do
          db <- readTVar dbVar
          case f db of
            Left  e   -> throwM e
            Right db' -> writeTVar dbVar db'

        updateEE :: (DBModel hash -> Either ImmutableDBError (Either e (a, DBModel hash)))
                 -> m (Either e a)
        updateEE f = atomically $ do
          db <- readTVar dbVar
          case f db of
            Left  e                -> throwM e
            Right (Left e)         -> return (Left e)
            Right (Right (a, db')) -> writeTVar dbVar db' >> return (Right a)

        query :: (DBModel hash -> a) -> m a
        query f = fmap f $ atomically $ readTVar dbVar

        queryE :: (DBModel hash -> Either ImmutableDBError a) -> m a
        queryE f = query f >>= \case
          Left  e -> throwM e
          Right a -> return a
