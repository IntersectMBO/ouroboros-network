{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Analysis (
    AnalysisName (..)
  , runAnalysis
  ) where

import           Control.Monad.Except
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation (SizeInBytes)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (..),
                     ChainDB (..), StreamFrom (..), StreamTo (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB hiding
                     (iteratorNext)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmDB hiding
                     (stream)

import           HasAnalysis (HasAnalysis)
import qualified HasAnalysis

{-------------------------------------------------------------------------------
  Run the requested analysis
-------------------------------------------------------------------------------}

data AnalysisName =
    ShowSlotBlockNo
  | CountTxOutputs
  | ShowBlockHeaderSize
  | ShowBlockTxsSize
  | ShowEBBs
  | OnlyValidation
  deriving Show

type Analysis blk = TopLevelConfig blk
                 -> Either (ImmDB IO blk) (ChainDB IO blk)
                 -> ResourceRegistry IO
                 -> IO ()

emptyAnalysis :: Analysis blk
emptyAnalysis _ _ _ = return ()

runAnalysis :: (HasAnalysis blk, RunNode blk)
            => AnalysisName -> Analysis blk
runAnalysis ShowSlotBlockNo     = showSlotBlockNo
runAnalysis CountTxOutputs      = countTxOutputs
runAnalysis ShowBlockHeaderSize = showBlockHeaderSize
runAnalysis ShowBlockTxsSize    = showBlockTxsSize
runAnalysis ShowEBBs            = showEBBs
runAnalysis OnlyValidation      = emptyAnalysis

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. (HasHeader blk, ImmDbSerialiseConstraints blk)
                => Analysis blk
showSlotBlockNo _cfg immDB rr =
    processAll immDB rr go
  where
    go :: blk -> IO ()
    go blk = putStrLn $ intercalate "\t" [
        show (blockNo   blk)
      , show (blockSlot blk)
      ]

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs
  :: forall blk. (HasAnalysis blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
countTxOutputs _cfg immDB rr = do
    cumulative <- newIORef 0
    processAll immDB rr (go cumulative)
  where
    go :: IORef Int -> blk -> IO ()
    go cumulative blk = do
        countCum  <- atomicModifyIORef cumulative $ \c ->
                       let c' = c + count in (c', c')
        putStrLn $ intercalate "\t" [
            show slotNo
          , show count
          , show countCum
          ]
      where
        count = HasAnalysis.countTxOutputs blk
        slotNo = blockSlot blk

{-------------------------------------------------------------------------------
  Analysis: show the block header size in bytes for all blocks
-------------------------------------------------------------------------------}

showBlockHeaderSize
  :: forall blk. (HasAnalysis blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
showBlockHeaderSize _cfg immDB rr = do
    maxBlockHeaderSizeRef <- newIORef 0
    processAll immDB rr (go maxBlockHeaderSizeRef)
    maxBlockHeaderSize <- readIORef maxBlockHeaderSizeRef
    putStrLn ("Maximum encountered block header size = " <> show maxBlockHeaderSize)
  where
    go :: IORef SizeInBytes -> blk -> IO ()
    go maxBlockHeaderSizeRef blk = do
        void $ modifyIORef' maxBlockHeaderSizeRef (max blockHdrSz)
        putStrLn $ intercalate "\t" [
            show slotNo
          , "Block header size = " <> show blockHdrSz
          ]
      where
        slotNo = blockSlot blk
        blockHdrSz = HasAnalysis.blockHeaderSize blk

{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

showBlockTxsSize
  :: forall blk. (HasAnalysis blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
showBlockTxsSize _cfg immDB rr = processAll immDB rr process
  where
    process :: blk -> IO ()
    process blk = putStrLn $ intercalate "\t" [
          show slotNo
        , "Num txs in block = " <> show numBlockTxs
        , "Total size of txs in block = " <> show blockTxsSize
        ]
      where
        txSizes :: [SizeInBytes]
        txSizes = HasAnalysis.blockTxSizes blk

        numBlockTxs :: Int
        numBlockTxs = length txSizes

        blockTxsSize :: SizeInBytes
        blockTxsSize = sum txSizes

        slotNo = blockSlot blk

{-------------------------------------------------------------------------------
  Analysis: show EBBs and their predecessors
-------------------------------------------------------------------------------}

showEBBs
  :: forall blk. (HasAnalysis blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
showEBBs cfg immDB rr = do
    putStrLn "EBB\tPrev\tKnown"
    processAll immDB rr processIfEBB
  where
    processIfEBB :: blk -> IO ()
    processIfEBB blk =
        case blockIsEBB blk of
          Just _epoch ->
            putStrLn $ intercalate "\t" [
                show (blockHash blk)
              , show (blockPrevHash (configCodec cfg) blk)
              , show (    Map.lookup
                            (blockHash blk)
                            (HasAnalysis.knownEBBs (Proxy @blk))
                       == Just (blockPrevHash (configCodec cfg) blk)
                     )
              ]
          _otherwise ->
            return () -- Skip regular blocks

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the DB
-------------------------------------------------------------------------------}

processAll :: forall blk. (HasHeader blk, ImmDbSerialiseConstraints blk)
           => Either (ImmDB IO blk) (ChainDB IO blk)
           -> ResourceRegistry IO
           -> (blk -> IO ())
           -> IO ()
processAll db rr callback = case db of
  Left  immDB   -> processAllImmDB immDB rr callback
  Right chainDB -> processAllChainDB chainDB rr callback

processAllChainDB :: forall blk. StandardHash blk
                  => ChainDB IO blk
                  -> ResourceRegistry IO
                  -> (blk -> IO ())
                  -> IO ()
processAllChainDB chainDB rr callback = do
    tipPoint <- atomically $ ChainDB.getTipPoint chainDB
    case pointToWithOriginRealPoint tipPoint of
      Origin -> return ()
      At tip -> do
        Right itr <- ChainDB.stream chainDB rr GetBlock
          (StreamFromExclusive GenesisPoint)
          (StreamToInclusive tip)
        go itr
  where
    go :: ChainDB.Iterator IO blk (IO blk) -> IO ()
    go itr = do
      itrResult <- ChainDB.iteratorNext itr
      case itrResult of
        ChainDB.IteratorExhausted     -> return ()
        ChainDB.IteratorResult mblk   -> mblk >>= \blk -> callback blk >> go itr
        ChainDB.IteratorBlockGCed pnt -> error $ "block gced " ++ show pnt

processAllImmDB :: forall blk. (HasHeader blk, ImmDbSerialiseConstraints blk)
                => ImmDB IO blk
                -> ResourceRegistry IO
                -> (blk -> IO ())
                -> IO ()
processAllImmDB immDB rr callback = do
    tipPoint <- getPointAtTip immDB
    case pointToWithOriginRealPoint tipPoint of
      Origin -> return ()
      At tip -> do
        Right itr <- ImmDB.stream immDB rr GetBlock
          (StreamFromExclusive GenesisPoint)
          (StreamToInclusive tip)
        go itr
  where
    go :: Iterator (HeaderHash blk) IO (IO blk) -> IO ()
    go itr = do
        itrResult <- ImmDB.iteratorNext itr
        case itrResult of
          IteratorExhausted   -> return ()
          IteratorResult mblk -> mblk >>= \blk -> callback blk >> go itr
