{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Ouroboros.Storage.ImmutableDB.Mock (openDBMock) where

import           Data.Bifunctor (first)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util ((...:), (.:))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common (BlockComponent)
import           Ouroboros.Consensus.Storage.ImmutableDB.API
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.Serialisation

import           Test.Ouroboros.Storage.ImmutableDB.Model

openDBMock ::
     forall m blk.
     ( HasHeader blk
     , GetHeader blk
     , EncodeDisk blk blk
     , HasNestedContent Header blk
     , EncodeDiskDep (NestedCtxt Header) blk
     , IOLike m
     )
  => ChunkInfo
  -> CodecConfig blk
  -> m (DBModel blk, ImmutableDB m blk)
openDBMock chunkInfo ccfg = do
    dbVar <- uncheckedNewTVarM dbModel
    return (dbModel, immutableDB dbVar)
  where
    dbModel = initDBModel chunkInfo ccfg

    immutableDB :: StrictTVar m (DBModel blk) -> ImmutableDB m blk
    immutableDB dbVar = ImmutableDB {
          closeDB_                = return ()
        , getTip_                 = querySTM     $ getTipModel
        , getBlockComponent_      = query       .: getBlockComponentModel
        , appendBlock_            = updateE_     . appendBlockModel
        , stream_                 = updateEE  ...: \_rr bc s e -> fmap (fmap (first (iterator bc))) . streamModel s e
        }
      where
        iterator :: BlockComponent blk b -> IteratorId -> Iterator m blk b
        iterator blockComponent itId = Iterator
          { iteratorNext    = update   $ iteratorNextModel    itId blockComponent
          , iteratorHasNext = querySTM $ iteratorHasNextModel itId
          , iteratorClose   = update_  $ iteratorCloseModel   itId
          }

        update_ :: (DBModel blk -> DBModel blk) -> m ()
        update_ f = atomically $ modifyTVar dbVar f

        update :: (DBModel blk -> (a, DBModel blk)) -> m a
        update f = atomically $ stateTVar dbVar f

        updateE_ :: (DBModel blk -> Either (ImmutableDBError blk) (DBModel blk)) -> m ()
        updateE_ f = atomically $ do
          db <- readTVar dbVar
          case f db of
            Left  e   -> throwSTM e
            Right db' -> writeTVar dbVar db'

        updateEE ::
             (DBModel blk -> Either (ImmutableDBError blk) (Either e (a, DBModel blk)))
          -> m (Either e a)
        updateEE f = atomically $ do
          db <- readTVar dbVar
          case f db of
            Left  e                -> throwSTM e
            Right (Left e)         -> return (Left e)
            Right (Right (a, db')) -> writeTVar dbVar db' >> return (Right a)

        query :: (DBModel blk -> a) -> m a
        query f = fmap f $ atomically $ readTVar dbVar

        querySTM :: (DBModel blk -> a) -> STM m a
        querySTM f = fmap f $ readTVar dbVar
