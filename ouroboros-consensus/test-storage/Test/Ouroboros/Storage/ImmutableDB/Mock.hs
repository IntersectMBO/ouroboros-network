{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Ouroboros.Storage.ImmutableDB.Mock (openDBMock) where

import           Control.Monad.Except (Except, runExcept)
import           Control.Monad.State (StateT, runStateT)
import           Data.Proxy

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Util ((..:), (.:))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common (EpochNo, EpochSize,
                     castBlockComponent)
import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Ouroboros.Storage.ImmutableDB.Model

type MockM hash = StateT (DBModel hash) (Except ImmutableDBError)

openDBMock  :: forall m hash. (IOLike m, Eq hash)
            => ErrorHandling ImmutableDBError m
            -> (EpochNo -> EpochSize)
            -> m (DBModel hash, ImmutableDB hash m)
openDBMock err epochSize = do
    dbVar <- uncheckedNewTVarM dbModel
    return (dbModel, immDB dbVar)
  where
    db :: ImmutableDB hash (MockM hash)
    (dbModel, db, _internal) = openDBModel err' epochSize

    err' :: ErrorHandling ImmutableDBError (MockM hash)
    err' = EH.liftErrState (Proxy @(DBModel hash)) EH.exceptT

    immDB :: StrictTVar m (DBModel hash) -> ImmutableDB hash m
    immDB dbVar = ImmutableDB
        { closeDB                = wrap  $    closeDB                db
        , isOpen                 = wrap  $    isOpen                 db
        , reopen                 = wrap  .    reopen                 db
        , getTip                 = wrap  $    getTip                 db
        , getBlockComponent      = wrap  .:  (getBlockComponent      db . castBlockComponent)
        , getEBBComponent        = wrap  .:  (getEBBComponent        db . castBlockComponent)
        , getBlockOrEBBComponent = wrap  ..: (getBlockOrEBBComponent db . castBlockComponent)
        , appendBlock            = wrap  ..:  appendBlock            db
        , appendEBB              = wrap  ..:  appendEBB              db
        , stream                 = wrapI ..: (stream                 db . castBlockComponent)
        , immutableDBErr         = err
        }
      where
        wrap  = wrapModel dbVar
        wrapI = wrapModel dbVar . fmap (wrapIter dbVar)

    wrapModel :: forall a. StrictTVar m (DBModel hash)
              -> MockM hash a -> m a
    wrapModel dbVar m = atomically $ do
        st <- readTVar dbVar
        case runExcept (runStateT m st) of
          Left e           -> throwM e
          Right (res, st') -> do
            writeTVar dbVar st'
            return res

    wrapIter :: forall a.
                StrictTVar m (DBModel hash)
             -> Either (WrongBoundError hash) (Iterator hash (MockM hash) a)
             -> Either (WrongBoundError hash) (Iterator hash m a)
    wrapIter _dbVar (Left e)   = Left e
    wrapIter  dbVar (Right it) = Right Iterator
        { iteratorNext    = wrap $ iteratorNext    it
        , iteratorPeek    = wrap $ iteratorPeek    it
        , iteratorHasNext = wrap $ iteratorHasNext it
        , iteratorClose   = wrap $ iteratorClose   it
        , iteratorID      = iteratorID it
        }
      where
        wrap = wrapModel dbVar
