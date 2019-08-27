{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Storage.ChainDB.Mock (
    openDB
  ) where

import           Data.Bifunctor (first)
import qualified Data.Map as Map

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (ChainUpdate)
import qualified Ouroboros.Network.MockChain.ProducerState as CPS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((..:))
import           Ouroboros.Consensus.Util.STM (blockUntilJust)

import           Ouroboros.Storage.ChainDB.API
import           Ouroboros.Storage.ChainDB.Model (Model)
import qualified Ouroboros.Storage.ChainDB.Model as Model

openDB :: forall m blk.
          ( MonadSTM   m
          , MonadThrow m
          , MonadThrow (STM m)
          , ProtocolLedgerView blk
          )
       => NodeConfig (BlockProtocol blk)
       -> ExtLedgerState blk
       -> m (ChainDB m blk)
openDB cfg initLedger = do
    db :: StrictTVar m (Model blk) <- atomically $ newTVar (Model.empty initLedger)

    let querySTM :: (Model blk -> a) -> STM m a
        querySTM f = readTVar db >>= \m ->
          if Model.isOpen m
            then return (f m)
            else throwM $ ClosedDBError @blk

        query :: (Model blk -> a) -> m a
        query = atomically . querySTM

        queryE :: (Model blk -> Either (ChainDbError blk) a) -> m a
        queryE f = query f >>= either throwM return

        updateSTM :: (Model blk -> (a, Model blk)) -> STM m a
        updateSTM f = do
          m <- readTVar db
          if Model.isOpen m
            then let (a, m') = f m in writeTVar db m' >> return a
            else
              throwM $ ClosedDBError @blk

        updateSTME :: (Model blk -> Either (ChainDbError blk) (a, Model blk))
                   -> STM m a
        updateSTME f = do
            m <- readTVar db
            if Model.isOpen m
              then case f m of
                Left e        -> throwM e
                Right (a, m') -> writeTVar db m' >> return a
              else
                throwM $ ClosedDBError @blk

        update :: (Model blk -> (a, Model blk)) -> m a
        update = atomically . updateSTM

        updateE :: (Model blk -> Either (ChainDbError blk) (a, Model blk))
                -> m a
        updateE = atomically . updateSTME

        update_ :: (Model blk -> Model blk) -> m ()
        update_ f = update (\m -> ((), f m))

        iterator :: IteratorId -> Iterator m blk
        iterator itrId = Iterator {
              iteratorNext  = update  $ Model.iteratorNext  itrId
            , iteratorClose = update_ $ Model.iteratorClose itrId
            , iteratorId    = itrId
            }

        reader :: CPS.ReaderId -> Reader m blk blk
        reader rdrId = Reader {
              readerInstruction =
                updateE readerInstruction'
            , readerInstructionBlocking = atomically $
                blockUntilJust $ updateSTME readerInstruction'
            , readerForward = \ps ->
                updateE $ Model.readerForward rdrId ps
            , readerClose =
                update_ $ Model.readerClose rdrId
            , readerId =
                rdrId
            }
          where
            readerInstruction' :: Model blk
                               -> Either (ChainDbError blk)
                                         (Maybe (ChainUpdate blk blk), Model blk)
            readerInstruction' = Model.readerInstruction rdrId

    return ChainDB {
        addBlock            = update_  . Model.addBlock cfg
      , getCurrentChain     = querySTM $ Model.lastK k getHeader
      , getCurrentLedger    = querySTM $ Model.currentLedger
      , getBlock            = queryE   . Model.getBlockByPoint
      , getTipBlock         = query    $ Model.tipBlock
      , getTipHeader        = query    $ (fmap getHeader . Model.tipBlock)
      , getTipPoint         = querySTM $ Model.tipPoint
      , getTipBlockNo       = querySTM $ Model.tipBlockNo
      , getIsFetched        = querySTM $ flip Model.hasBlockByPoint
      , getIsInvalidBlock   = querySTM $ (first (flip Map.member)) . Model.invalid
      , streamBlocks        = updateE  ..: const (fmap (first (fmap iterator)) ..: Model.streamBlocks k)
      , newBlockReader      = update   .   const (first reader . Model.readBlocks)
      , newHeaderReader     = update   .   const (first (fmap getHeader . reader) . Model.readBlocks)

      , closeDB             = atomically $ modifyTVar db Model.closeDB
      , isOpen              = Model.isOpen <$> readTVar db
      }
  where
    k = protocolSecurityParam cfg
