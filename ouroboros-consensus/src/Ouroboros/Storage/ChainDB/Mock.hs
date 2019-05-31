{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Storage.ChainDB.Mock (
    openDB
  ) where

import           Data.Bifunctor (first)
import qualified Data.Set as Set

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash,
                     Point (..))
import qualified Ouroboros.Network.Block as Block
import qualified Ouroboros.Network.ChainProducerState as CPS

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((..:), (.:))
import           Ouroboros.Consensus.Util.STM (blockUntilJust)

import           Ouroboros.Storage.ChainDB.API
import           Ouroboros.Storage.ChainDB.Model (Model)
import qualified Ouroboros.Storage.ChainDB.Model as Model

openDB :: forall m blk hdr.
          ( MonadSTM m
          , HasHeader hdr
          , HeaderHash blk ~ HeaderHash hdr
          , ProtocolLedgerView blk
          , LedgerConfigView blk
          )
       => NodeConfig (BlockProtocol blk)
       -> ExtLedgerState blk
       -> (blk -> hdr)
       -> m (ChainDB m blk hdr)
openDB cfg initLedger blockHeader = do
    db :: TVar m (Model blk) <- atomically $ newTVar (Model.empty initLedger)

    let query :: (Model blk -> a) -> STM m a
        query f = f <$> readTVar db

        query' :: (Model blk -> a) -> m a
        query' = atomically . query

        updateSTM :: (Model blk -> (a, Model blk)) -> STM m a
        updateSTM f = do
            (a, m') <- f <$> readTVar db
            writeTVar db m'
            return a

        update :: (Model blk -> (a, Model blk)) -> m a
        update = atomically . updateSTM

        update_ :: (Model blk -> Model blk) -> m ()
        update_ f = update (\m -> ((), f m))

        iterator :: IteratorId -> Iterator m blk
        iterator itrId = Iterator {
              iteratorNext  = update $ Model.iteratorNext itrId
            , iteratorClose = return () -- TODO: Should we do something here?
            , iteratorId    = itrId
            }

        reader :: CPS.ReaderId -> Reader m blk blk
        reader rdrId = Reader {
              readerInstruction = atomically $
                updateSTM readerInstruction'
            , readerInstructionBlocking = atomically $
                blockUntilJust $ updateSTM readerInstruction'
            , readerForward = \ps -> atomically $
                updateSTM $ readerForward' ps
            , readerId =
                rdrId
            }
          where
            readerInstruction' :: Model blk
                               -> (Maybe (ChainUpdate blk blk), Model blk)
            readerInstruction' = Model.readerInstruction rdrId

            readerForward' :: [Point blk]
                           -> Model blk
                           -> (Maybe (Point blk), Model blk)
            readerForward' ps =
                  first (fmap Block.castPoint)
                . Model.readerForward rdrId (map Block.castPoint ps)

    return ChainDB {
        addBlock            = update_ . Model.addBlock cfg
      , getCurrentChain     = query   $ Model.lastK k blockHeader
      , getCurrentLedger    = query   $ Model.currentLedger
      , getBlock            = query'  . Model.getBlockByPoint
      , getTipBlock         = query'  $ Model.tipBlock
      , getTipHeader        = query'  $ (fmap blockHeader . Model.tipBlock)
      , getTipPoint         = query   $ Model.tipPoint
      , getIsFetched        = query   $ flip Model.hasBlockByPoint
      , knownInvalidBlocks  = query   $ const Set.empty -- TODO
      , streamBlocks        = update .: (first iterator ..: Model.streamBlocks)
      , readBlocks          = update  $ (first reader . Model.readBlocks)
      , readHeaders         = update  $ (first (fmap blockHeader . reader) . Model.readBlocks)
      , closeDB             = return () -- TODO
      }
  where
    k = protocolSecurityParam cfg
