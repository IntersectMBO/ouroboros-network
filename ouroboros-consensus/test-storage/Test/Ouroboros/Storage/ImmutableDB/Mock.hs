{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Ouroboros.Storage.ImmutableDB.Mock (openDBMock) where

import           Control.Monad (void)
import           Data.Bifunctor (first)
import           Data.Tuple (swap)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Util ((..:), (.:))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common (BlockComponent, EpochSize)
import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Ouroboros.Storage.ImmutableDB.Model


openDBMock  :: forall m hash. (IOLike m, Eq hash)
            => ErrorHandling ImmutableDBError m
            -> EpochSize
            -> m (DBModel hash, ImmutableDB hash m)
openDBMock err epochSize = do
    dbVar <- uncheckedNewTVarM dbModel
    return (dbModel, immDB dbVar)
  where
    dbModel = initDBModel epochSize

    immDB :: StrictTVar m (DBModel hash) -> ImmutableDB hash m
    immDB dbVar = ImmutableDB
        { closeDB                = return ()
        , isOpen                 = return True
        , reopen                 = \_valPol -> void $ update reopenModel
        , getTip                 = query      $ getTipModel
        , getBlockComponent      = queryE    .: getBlockComponentModel
        , getEBBComponent        = queryE    .: getEBBComponentModel
        , getBlockOrEBBComponent = queryE   ..: getBlockOrEBBComponentModel
        , appendBlock            = updateE_ ..: appendBlockModel
        , appendEBB              = updateE_ ..: appendEBBModel
        , stream                 = updateEE ..: \bc s e -> fmap (fmap (first (iterator bc))) . streamModel s e
        , immutableDBErr         = err
        }
      where
        iterator :: BlockComponent (ImmutableDB hash m) b
                 -> IteratorID
                 -> Iterator hash m b
        iterator blockComponent itID = Iterator
          { iteratorNext    = update  $ iteratorNextModel    itID blockComponent
          , iteratorPeek    = query   $ iteratorPeekModel    itID blockComponent
          , iteratorHasNext = query   $ iteratorHasNextModel itID
          , iteratorClose   = update_ $ iteratorCloseModel   itID
          , iteratorID      = itID
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
          Left  e -> EH.throwError err e
          Right a -> return a
