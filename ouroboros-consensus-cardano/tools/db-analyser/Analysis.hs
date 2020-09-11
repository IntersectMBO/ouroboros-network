{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Analysis (
    AnalysisName (..)
  , runAnalysis
  , AnalysisEnv (..)
  ) where

import qualified Control.Foldl as L
import           Control.Monad.Except
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Word (Word16)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract
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
  | MeasureLedgerValidation
  | OnlyValidation
  deriving Show

runAnalysis :: HasAnalysis blk => AnalysisName -> Analysis blk
runAnalysis = \case
    ShowSlotBlockNo         -> showSlotBlockNo
    CountTxOutputs          -> countTxOutputs
    ShowBlockHeaderSize     -> showHeaderSize
    ShowBlockTxsSize        -> showBlockTxsSize
    ShowEBBs                -> showEBBs
    MeasureLedgerValidation -> measureLedgerValidation
    OnlyValidation          -> \_ -> return ()

type Analysis blk = AnalysisEnv blk -> IO ()

data AnalysisEnv blk = AnalysisEnv {
      cfg        :: TopLevelConfig blk
    , initLedger :: ExtLedgerState blk
    , db         :: Either (ImmDB IO blk) (ChainDB IO blk)
    , registry   :: ResourceRegistry IO
    }

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasAnalysis blk => Analysis blk
showSlotBlockNo AnalysisEnv { db, registry } =
    processAll_ db registry GetHeader (>>= process)
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
    void $ processAll db registry GetBlock 0 ((=<<) . process)
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
    processAll_ db registry GetBlock (>>= process)
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
showEBBs AnalysisEnv { cfg, db, registry } = do
    putStrLn "EBB\tPrev\tKnown"
    processAll_ db registry GetBlock (>>= process)
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
  Analysis: measure ledger validation time
-------------------------------------------------------------------------------}

measureLedgerValidation :: forall blk. HasAnalysis blk => Analysis blk
measureLedgerValidation AnalysisEnv { cfg, initLedger, db, registry } = do
    -- putStrLn $ intercalate "," [
    --     "slot"
    --   , "applyChainTick"
    --   , "tickHeaderState"
    --   , "applyLedgerBlock"
    --   , "reapplyLedgerBlock"
    --   , "validateHeader"
    --   , "revalidateHeader"
    --   ]
    start <- getMonotonicTime
    (nbBlocks, _) <-
      processAll db registry GetBlock (0, initLedger) ((=<<) . process)
    stop  <- getMonotonicTime
    putStrLn $
      "Validated " <> show nbBlocks <>
      " blocks in " <> show (stop `diffTime` start)
  where
    lcfg :: LedgerConfig blk
    lcfg = configLedger cfg

    pcfg :: ConsensusConfig (BlockProtocol blk)
    pcfg = configConsensus cfg

    fbcfg :: FullBlockConfig (LedgerState blk) blk
    fbcfg = topLevelConfigBlock cfg

    process :: (Int, ExtLedgerState blk) -> blk -> IO (Int, ExtLedgerState blk)
    process !(!nbBlocks, !(ExtLedgerState !ledger !header)) !blk = do
        let !slot = blockSlot blk
            !hdr = getHeader blk

        (tickedLedgerState, applyChainTickTime) <-
          measure $ applyChainTick lcfg slot ledger

        (tickedLedgerView, _) <-
          measure $ protocolLedgerView lcfg tickedLedgerState

        (tickedHeaderState, tickHeaderStateTime) <-
          measure $ tickHeaderState pcfg tickedLedgerView slot header

        (ledger', applyLedgerBlockTime) <-
          measure $
            case runExcept (applyLedgerBlock fbcfg blk tickedLedgerState) of
              Right st -> st
              Left  e  ->
                error $
                  "applyLedgerBlock failed for " <> show slot <> ": " <> show e

        (_, reapplyLedgerBlockTime) <-
          measure $ reapplyLedgerBlock fbcfg blk tickedLedgerState

        (header', validateHeaderTime) <-
          measure $
            case runExcept (validateHeader cfg tickedLedgerView hdr tickedHeaderState) of
              Right st -> st
              Left  e  ->
                error $
                  "validateHeader failed for " <> show slot <> ": " <> show e

        (_, revalidateHeaderTime) <-
          measure $ revalidateHeader cfg tickedLedgerView hdr tickedHeaderState

        putStrLn $ intercalate "," $ show (unSlotNo slot) : map show [
            applyChainTickTime
          , tickHeaderStateTime
          , applyLedgerBlockTime
          , reapplyLedgerBlockTime
          , validateHeaderTime
          , revalidateHeaderTime
          ]

        return (succ nbBlocks, ExtLedgerState ledger' header')

    measure :: a -> IO (a, DiffTime)
    measure x = do
      start <- getMonotonicTime
      res   <- evaluate x
      stop  <- getMonotonicTime
      return (res, stop `diffTime` start)

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
