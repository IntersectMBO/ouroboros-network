{-# LANGUAGE BangPatterns        #-}
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
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Word (Word16)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation (SizeInBytes)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (..),
                     ChainDB (..), StreamFrom (..), StreamTo (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB (ImmDB)
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

type Analysis blk =
     TopLevelConfig blk
  -> ExtLedgerState blk
  -> Either (ImmDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> IO ()

noAnalysis :: Analysis blk
noAnalysis _ _ _ _ = return ()

runAnalysis :: (HasAnalysis blk, RunNode blk)
            => AnalysisName -> Analysis blk
runAnalysis ShowSlotBlockNo     = showSlotBlockNo
runAnalysis CountTxOutputs      = countTxOutputs
runAnalysis ShowBlockHeaderSize = showBlockHeaderSize
runAnalysis ShowBlockTxsSize    = showBlockTxsSize
runAnalysis ShowEBBs            = showEBBs
runAnalysis OnlyValidation      = noAnalysis

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo ::
     forall blk.
     ( HasHeader blk
     , HasHeader (Header blk)
     , ImmDbSerialiseConstraints blk
     )
  => Analysis blk
showSlotBlockNo _cfg _initLedger db rr =
    processAll_ db rr GetHeader (>>= process)
  where
    process :: Header blk -> IO ()
    process hdr = putStrLn $ intercalate "\t" [
        show (blockNo   hdr)
      , show (blockSlot hdr)
      ]

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs
  :: forall blk. (HasAnalysis blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
countTxOutputs _cfg _initLedger db rr = do
    void $ processAll db rr GetBlock 0 ((=<<) . process)
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
  Analysis: show the block header size in bytes for all blocks
-------------------------------------------------------------------------------}

showBlockHeaderSize
  :: forall blk. (HasAnalysis blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
showBlockHeaderSize _cfg _initLedger db rr = do
    maxHeaderSize <-
      processAll db rr ((,) <$> GetSlot <*> GetHeaderSize) 0 process
    putStrLn ("Maximum encountered block header size = " <> show maxHeaderSize)
  where
    process :: Word16 -> (SlotNo, Word16) -> IO Word16
    process maxHeaderSize (slotNo, headerSize) = do
        putStrLn $ intercalate "\t" [
            show slotNo
          , "Block header size = " <> show headerSize
          ]
        return $ maxHeaderSize `max` headerSize

{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

showBlockTxsSize
  :: forall blk. (HasAnalysis blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
showBlockTxsSize _cfg _initLedger db rr =
    processAll_ db rr GetBlock (>>= process)
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
showEBBs cfg _initLedger db rr = do
    putStrLn "EBB\tPrev\tKnown"
    processAll_ db rr GetBlock (>>= process)
  where
    process :: blk -> IO ()
    process blk =
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

processAll ::
     forall blk b st. (HasHeader blk, ImmDbSerialiseConstraints blk)
  => Either (ImmDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent (ChainDB IO blk) b
  -> st
  -> (st -> b -> IO st)
  -> IO st
processAll = either processAllImmDB processAllChainDB

processAll_ ::
     forall blk b. (HasHeader blk, ImmDbSerialiseConstraints blk)
  => Either (ImmDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent (ChainDB IO blk) b
  -> (b -> IO ())
  -> IO ()
processAll_ db rr blockComponent callback =
    processAll db rr blockComponent () (const callback)

processAllChainDB ::
     forall st blk b. StandardHash blk
  => ChainDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent (ChainDB IO blk) b
  -> st
  -> (st -> b -> IO st)
  -> IO st
processAllChainDB chainDB rr blockComponent initState callback = do
    tipPoint <- atomically $ ChainDB.getTipPoint chainDB
    case pointToWithOriginRealPoint tipPoint of
      Origin        -> return initState
      NotOrigin tip -> do
        Right itr <-
          ChainDB.stream
            chainDB
            rr
            blockComponent
            (StreamFromExclusive GenesisPoint)
            (StreamToInclusive tip)
        go itr initState
  where
    go :: ChainDB.Iterator IO blk b -> st -> IO st
    go itr !st = do
      itrResult <- ChainDB.iteratorNext itr
      case itrResult of
        ChainDB.IteratorExhausted    -> return st
        ChainDB.IteratorResult b     -> callback st b >>= go itr
        ChainDB.IteratorBlockGCed pt -> error $ "block gced " ++ show pt

processAllImmDB ::
     forall st blk b. (HasHeader blk, ImmDbSerialiseConstraints blk)
  => ImmDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent (ChainDB IO blk) b
  -> st
  -> (st -> b -> IO st)
  -> IO st
processAllImmDB immDB rr blockComponent initState callback = do
    tipPoint <- ImmDB.getPointAtTip immDB
    case pointToWithOriginRealPoint tipPoint of
      Origin        -> return initState
      NotOrigin tip -> do
        Right itr <-
          ImmDB.stream
            immDB
            rr
            blockComponent
            (StreamFromExclusive GenesisPoint)
            (StreamToInclusive tip)
        go itr initState
  where
    go :: ImmDB.Iterator (HeaderHash blk) IO b -> st -> IO st
    go itr !st = do
        itrResult <- ImmDB.iteratorNext itr
        case itrResult of
          ImmDB.IteratorExhausted -> return st
          ImmDB.IteratorResult b  -> callback st b >>= go itr
