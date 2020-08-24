{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Analysis (
    AnalysisName (..)
  , runAnalysis
  ) where

import           Control.Monad.Except
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.Serialisation (SizeInBytes)

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
                 -> Either (ImmutableDB IO blk) (ChainDB IO blk)
                 -> ResourceRegistry IO
                 -> IO ()

emptyAnalysis :: Analysis blk
emptyAnalysis _ _ _ = return ()

runAnalysis :: HasAnalysis blk => AnalysisName -> Analysis blk
runAnalysis ShowSlotBlockNo     = showSlotBlockNo
runAnalysis CountTxOutputs      = countTxOutputs
runAnalysis ShowBlockHeaderSize = showBlockHeaderSize
runAnalysis ShowBlockTxsSize    = showBlockTxsSize
runAnalysis ShowEBBs            = showEBBs
runAnalysis OnlyValidation      = emptyAnalysis

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasHeader blk => Analysis blk
showSlotBlockNo _cfg db rr = processAll db rr go
  where
    go :: blk -> IO ()
    go blk = putStrLn $ intercalate "\t" [
        show (blockNo   blk)
      , show (blockSlot blk)
      ]

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: forall blk. HasAnalysis blk => Analysis blk
countTxOutputs _cfg db rr = do
    cumulative <- newIORef 0
    processAll db rr (go cumulative)
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

showBlockHeaderSize :: forall blk. HasAnalysis blk => Analysis blk
showBlockHeaderSize _cfg db rr = do
    maxBlockHeaderSizeRef <- newIORef 0
    processAll db rr (go maxBlockHeaderSizeRef)
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

showBlockTxsSize :: forall blk. HasAnalysis blk => Analysis blk
showBlockTxsSize _cfg db rr = processAll db rr process
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

showEBBs :: forall blk. HasAnalysis blk => Analysis blk
showEBBs _cfg db rr = do
    putStrLn "EBB\tPrev\tKnown"
    processAll db rr processIfEBB
  where
    processIfEBB :: blk -> IO ()
    processIfEBB blk =
        case blockIsEBB blk of
          Just _epoch ->
            putStrLn $ intercalate "\t" [
                show (blockHash blk)
              , show (blockPrevHash blk)
              , show (    Map.lookup
                            (blockHash blk)
                            (HasAnalysis.knownEBBs (Proxy @blk))
                       == Just (blockPrevHash blk)
                     )
              ]
          _otherwise ->
            return () -- Skip regular blocks

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the DB
-------------------------------------------------------------------------------}

processAll ::
     forall blk. HasHeader blk
  => Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> (blk -> IO ())
  -> IO ()
processAll = either processAllImmutableDB processAllChainDB

processAllChainDB ::
     forall blk. HasHeader blk
  => ChainDB IO blk
  -> ResourceRegistry IO
  -> (blk -> IO ())
  -> IO ()
processAllChainDB chainDB rr callback =
    ChainDB.streamAll chainDB rr GetBlock >>= go
  where
    go :: ChainDB.Iterator IO blk blk -> IO ()
    go itr = do
        itrResult <- ChainDB.iteratorNext itr
        case itrResult of
          ChainDB.IteratorExhausted    -> return ()
          ChainDB.IteratorResult blk   -> callback blk >> go itr
          ChainDB.IteratorBlockGCed pt -> error $ "block GC'ed " ++ show pt

processAllImmutableDB ::
     forall blk. HasHeader blk
  => ImmutableDB IO blk
  -> ResourceRegistry IO
  -> (blk -> IO ())
  -> IO ()
processAllImmutableDB immutableDB rr callback = do
    ImmutableDB.streamAll immutableDB rr GetBlock >>= go
  where
    go :: ImmutableDB.Iterator IO blk blk -> IO ()
    go itr = do
        itrResult <- ImmutableDB.iteratorNext itr
        case itrResult of
          ImmutableDB.IteratorExhausted  -> return ()
          ImmutableDB.IteratorResult blk -> callback blk >> go itr
