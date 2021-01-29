{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Analysis (
    AnalysisName (..)
  , runAnalysis
  , AnalysisEnv (..)
  ) where

import           Control.Monad.Except
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Word (Word16)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Extended
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

runAnalysis :: HasAnalysis blk => AnalysisName -> Analysis blk
runAnalysis ShowSlotBlockNo     = showSlotBlockNo
runAnalysis CountTxOutputs      = countTxOutputs
runAnalysis ShowBlockHeaderSize = showHeaderSize
runAnalysis ShowBlockTxsSize    = showBlockTxsSize
runAnalysis ShowEBBs            = showEBBs
runAnalysis OnlyValidation      = \_ -> return ()

type Analysis blk = AnalysisEnv blk -> IO ()

data AnalysisEnv blk = AnalysisEnv {
      cfg        :: TopLevelConfig blk
    , initLedger :: ExtLedgerState blk
    , db         :: Either (ImmutableDB IO blk) (ChainDB IO blk)
    , registry   :: ResourceRegistry IO
    }

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasAnalysis blk => Analysis blk
showSlotBlockNo AnalysisEnv { db, registry } =
    processAll_ db registry GetHeader process
  where
    process :: Header blk -> IO ()
    process hdr = putStrLn $ intercalate "\t" [
        show (blockNo   hdr)
      , show (blockSlot hdr)
      ]

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: forall blk. HasAnalysis blk => Analysis blk
countTxOutputs AnalysisEnv { db, registry } = do
    void $ processAll db registry GetBlock 0 process
  where
    process :: Int -> blk -> IO Int
    process cumulative blk = do
        let cumulative' = cumulative + count
        putStrLn $ intercalate "\t" [
            show slotNo
          , show count
          , show cumulative'
          ]
        return cumulative'
      where
        count = HasAnalysis.countTxOutputs blk
        slotNo = blockSlot blk

{-------------------------------------------------------------------------------
  Analysis: show the header size in bytes for all blocks
-------------------------------------------------------------------------------}

showHeaderSize :: forall blk. HasAnalysis blk => Analysis blk
showHeaderSize AnalysisEnv { db, registry } = do
    maxHeaderSize <-
      processAll db registry ((,) <$> GetSlot <*> GetHeaderSize) 0 process
    putStrLn ("Maximum encountered header size = " <> show maxHeaderSize)
  where
    process :: Word16 -> (SlotNo, Word16) -> IO Word16
    process maxHeaderSize (slotNo, headerSize) = do
        putStrLn $ intercalate "\t" [
            show slotNo
          , "Header size = " <> show headerSize
          ]
        return $ maxHeaderSize `max` headerSize

{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

showBlockTxsSize :: forall blk. HasAnalysis blk => Analysis blk
showBlockTxsSize AnalysisEnv { db, registry } =
    processAll_ db registry GetBlock process
  where
    process :: blk -> IO ()
    process blk = putStrLn $ intercalate "\t" [
          show (blockSlot blk)
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

{-------------------------------------------------------------------------------
  Analysis: show EBBs and their predecessors
-------------------------------------------------------------------------------}

showEBBs :: forall blk. HasAnalysis blk => Analysis blk
showEBBs AnalysisEnv { db, registry } = do
    putStrLn "EBB\tPrev\tKnown"
    processAll_ db registry GetBlock process
  where
    process :: blk -> IO ()
    process blk =
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
     forall blk b st. HasHeader blk
  => Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> st
  -> (st -> b -> IO st)
  -> IO st
processAll = either processAllImmutableDB processAllChainDB

processAll_ ::
     forall blk b. HasHeader blk
  => Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> (b -> IO ())
  -> IO ()
processAll_ db rr blockComponent callback =
    processAll db rr blockComponent () (const callback)

processAllChainDB ::
     forall st blk b. HasHeader blk
  => ChainDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> st
  -> (st -> b -> IO st)
  -> IO st
processAllChainDB chainDB rr blockComponent initState callback = do
    itr <- ChainDB.streamAll chainDB rr blockComponent
    go itr initState
  where
    go :: ChainDB.Iterator IO blk b -> st -> IO st
    go itr !st = do
      itrResult <- ChainDB.iteratorNext itr
      case itrResult of
        ChainDB.IteratorExhausted    -> return st
        ChainDB.IteratorResult b     -> callback st b >>= go itr
        ChainDB.IteratorBlockGCed pt -> error $ "block GC'ed " <> show pt

processAllImmutableDB ::
     forall st blk b. HasHeader blk
  => ImmutableDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> st
  -> (st -> b -> IO st)
  -> IO st
processAllImmutableDB immutableDB rr blockComponent initState callback = do
    itr <- ImmutableDB.streamAll immutableDB rr blockComponent
    go itr initState
  where
    go :: ImmutableDB.Iterator IO blk b -> st -> IO st
    go itr !st = do
        itrResult <- ImmutableDB.iteratorNext itr
        case itrResult of
          ImmutableDB.IteratorExhausted -> return st
          ImmutableDB.IteratorResult b  -> callback st b >>= go itr
