{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Ouroboros.Storage.ChainDB.Mock (
    openDB
  ) where

import           Control.Monad (void)
import           Data.Bifunctor (first)
import qualified Data.Map as Map
import           GHC.Stack (callStack)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (ChainUpdate)
import qualified Ouroboros.Network.MockChain.ProducerState as CPS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((..:))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (blockUntilJust)

import           Ouroboros.Storage.ChainDB.API

import           Test.Ouroboros.Storage.ChainDB.Model (Model)
import qualified Test.Ouroboros.Storage.ChainDB.Model as Model

openDB :: forall m blk. (
            IOLike m
          , ProtocolLedgerView blk
          , CanSelect (BlockProtocol blk) blk
          )
       => NodeConfig (BlockProtocol blk)
       -> ExtLedgerState blk
       -> BlockchainTime m
       -> m (ChainDB m blk)
openDB cfg initLedger btime = do
    curSlot <- atomically $ getCurrentSlot btime
    let initM = (Model.empty initLedger) { Model.currentSlot = curSlot }
    db :: StrictTVar m (Model blk) <- uncheckedNewTVarM initM

    let querySTM :: (Model blk -> a) -> STM m a
        querySTM f = readTVar db >>= \m ->
          if Model.isOpen m
            then return (f m)
            else throwM $ ClosedDBError callStack

        query :: (Model blk -> a) -> m a
        query = atomically . querySTM

        queryE :: (Model blk -> Either ChainDbError a) -> m a
        queryE f = query f >>= either throwM return

        updateSTM :: (Model blk -> (a, Model blk)) -> STM m a
        updateSTM f = do
          m <- readTVar db
          if Model.isOpen m
            then let (a, m') = f m in writeTVar db m' >> return a
            else
              throwM $ ClosedDBError callStack

        updateSTME :: (Model blk -> Either ChainDbError (a, Model blk))
                   -> STM m a
        updateSTME f = do
            m <- readTVar db
            if Model.isOpen m
              then case f m of
                Left e        -> throwM e
                Right (a, m') -> writeTVar db m' >> return a
              else
                throwM $ ClosedDBError callStack

        update :: (Model blk -> (a, Model blk)) -> m a
        update = atomically . updateSTM

        updateE :: (Model blk -> Either ChainDbError (a, Model blk))
                -> m a
        updateE = atomically . updateSTME

        update_ :: (Model blk -> Model blk) -> m ()
        update_ f = update (\m -> ((), f m))

        iterator :: IteratorId -> Iterator m (Deserialisable m blk blk)
        iterator itrId = Iterator {
              iteratorNext  = update  $ Model.iteratorNextDeserialised itrId
            , iteratorClose = update_ $ Model.iteratorClose            itrId
            , iteratorId    = itrId
            }

        reader :: forall b.
                  (blk -> b)
               -> CPS.ReaderId
               -> Reader m blk b
        reader toB rdrId = Reader {
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
            readerInstruction'
              :: Model blk
              -> Either ChainDbError
                        (Maybe (ChainUpdate blk b), Model blk)
            readerInstruction' = Model.readerInstruction toB rdrId

    void $ onSlotChange btime $ update_ . Model.advanceCurSlot cfg

    return ChainDB {
        addBlock            = update_  . Model.addBlock cfg
      , getCurrentChain     = querySTM $ Model.lastK k getHeader
      , getCurrentLedger    = querySTM $ Model.currentLedger
      , getPastLedger       = query    . Model.getPastLedger cfg
      , getBlock            = queryE   . Model.getBlockByPoint
      , getTipBlock         = query    $ Model.tipBlock
      , getTipHeader        = query    $ (fmap getHeader . Model.tipBlock)
      , getTipPoint         = querySTM $ Model.tipPoint
      , getTipBlockNo       = querySTM $ Model.tipBlockNo
      , getIsFetched        = querySTM $ flip Model.hasBlockByPoint
      , getIsInvalidBlock   = querySTM $ (fmap (fmap (fmap fst) . flip Map.lookup)) . Model.invalid
      , getMaxSlotNo        = querySTM $ Model.maxSlotNo
      , streamBlocks        = updateE  ..: const (fmap (first (fmap iterator)) ..: Model.streamBlocks k)
      , newBlockReader      = update   .   const (first (reader Model.toDeserialisable) . Model.readBlocks)
      , newHeaderReader     = update   .   const (first (reader (Model.toDeserialisable . getHeader)) . Model.readBlocks)

      , closeDB             = atomically $ modifyTVar db Model.closeDB
      , isOpen              = Model.isOpen <$> readTVar db
      }
  where
    k = protocolSecurityParam cfg
