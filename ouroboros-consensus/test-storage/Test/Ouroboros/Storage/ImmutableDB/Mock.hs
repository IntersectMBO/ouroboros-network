{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Ouroboros.Storage.ImmutableDB.Mock (openDBMock) where

import           Control.Monad.Except (Except, runExcept)
import           Control.Monad.State (StateT, runStateT)
import           Data.Proxy

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Util ((..:), (.:))

import           Ouroboros.Storage.Common (EpochNo, EpochSize)
import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Ouroboros.Storage.ImmutableDB.Model

type MockM hash = StateT (DBModel hash) (Except ImmutableDBError)

openDBMock  :: forall m hash.
               (MonadSTM m, MonadThrow (STM m), Eq hash)
            => ErrorHandling ImmutableDBError m
            -> (EpochNo -> EpochSize)
            -> m (DBModel hash, ImmutableDB hash m)
openDBMock err epochSize = do
    dbVar <- atomically $ newTVar dbModel
    return (dbModel, immDB dbVar)
  where
    db :: ImmutableDB hash (MockM hash)
    (dbModel, db) = openDBModel err' epochSize

    err' :: ErrorHandling ImmutableDBError (MockM hash)
    err' = EH.liftErrState (Proxy @(DBModel hash)) EH.exceptT

    immDB :: TVar m (DBModel hash) -> ImmutableDB hash m
    immDB dbVar = ImmutableDB
        { closeDB           = wrap  $   closeDB          db
        , isOpen            = wrap  $   isOpen           db
        , reopen            = wrap  .   reopen           db
        , deleteAfter       = wrap  .   deleteAfter      db
        , getTip            = wrap  $   getTip           db
        , getBinaryBlob     = wrap  .   getBinaryBlob    db
        , getEBB            = wrap  .   getEBB           db
        , appendBinaryBlob  = wrap  .:  appendBinaryBlob db
        , appendEBB         = wrap  ..: appendEBB        db
        , streamBinaryBlobs = wrapI .: streamBinaryBlobs db
        , immutableDBErr    = err
        }
      where
        wrap  = wrapModel dbVar
        wrapI = wrapModel dbVar . fmap (wrapIter dbVar)

    wrapModel :: forall a. TVar m (DBModel hash)
              -> MockM hash a -> m a
    wrapModel dbVar m = atomically $ do
        st <- readTVar dbVar
        case runExcept (runStateT m st) of
          Left e           -> throwM e
          Right (res, st') -> do
            writeTVar dbVar st'
            return res

    wrapIter :: forall a.
                TVar m (DBModel hash)
             -> Iterator hash (MockM hash) a
             -> Iterator hash m a
    wrapIter dbVar it = Iterator
        { iteratorNext    = wrap $ iteratorNext    it
        , iteratorPeek    = wrap $ iteratorPeek    it
        , iteratorHasNext = wrap $ iteratorHasNext it
        , iteratorClose   = wrap $ iteratorClose   it
        , iteratorID      = iteratorID it
        }
      where
        wrap = wrapModel dbVar
