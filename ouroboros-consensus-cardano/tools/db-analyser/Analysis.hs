{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wwarn #-}
module Analysis (
    AnalysisName (..)
  , runAnalysis
  , AnalysisEnv (..)
  ) where

import qualified Control.Foldl as L
import           Control.Monad.Except
import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Profunctor (Profunctor (lmap))
import           Data.Word (Word16, Word64)

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
    -- TODO separate bench command
    -- TODO flag to spit out timings
  | MeasureLedgerValidation
  | OnlyValidation
    -- TODO separate export command?
    -- TODO flag to export ledger vs consensus format
    -- TODO argument: SlotNo
  | CreateLedgerSnapshot
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
    CreateLedgerSnapshot    -> createLedgerSnapshot

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

data Pair a b = Pair !a !b
  deriving (Show, Functor)

data Timings value = Timings {
      timingApplyChainTick     :: !value
    , timingTickHeaderState    :: !value
    , timingApplyLedgerBlock   :: !value
    , timingReapplyLedgerBlock :: !value
    , timingValidateHeader     :: !value
    , timingRevalidateHeader   :: !value
    }
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative Timings where
  pure a = Timings a a a a a a
  Timings f1 f2 f3 f4 f5 f6 <*> Timings a1 a2 a3 a4 a5 a6 =
    Timings (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6)

mainAnalysis ::
     L.Fold (SlotNo, Timings DiffTime)
            (Pair
              (Timings DiffTime)
              (Map Ordering (Timings DiffTime)))
mainAnalysis = Pair
    <$> (lmap snd (L.nest L.mean))
    <*> L.groupBy
          ((`compare` firstShelleySlot) . fst)
          (lmap snd (L.nest L.mean))

measureLedgerValidation :: forall blk. HasAnalysis blk => Analysis blk
measureLedgerValidation AnalysisEnv { cfg, initLedger, db, registry } = do
    tipBlockNo <- getTipBlockNo db
    let nbBlocks = withOrigin 0 (succ . unBlockNo) tipBlockNo
    putStrLn $
      "Started measuring the validation times of "
        <> show nbBlocks
        <> " blocks"

    timings <-
      processAllFoldM
        db
        registry
        GetBlock
        (L.premapM id
          (printProgress *> process))

    putStrLn "TODO"
  where
    progressEveryXBlocks :: Word64
    progressEveryXBlocks = 10000

    printProgress :: L.FoldM IO blk ()
    printProgress =
        L.prefilterM
          (\blk ->
              return $ unBlockNo (blockNo blk) `mod` progressEveryXBlocks == 0)
          (L.mapM_ $ \blk ->
              putStrLn $ "Applied " <> show (succ (unBlockNo (blockNo blk))) <> " blocks")

    process :: L.FoldM IO blk (Timings DiffTime)
    process =
        L.FoldM
          (\(!(_, !st)) blk -> measureTimings st blk)
          (return (pure 0, initLedger))
          (return . fst)

    lcfg :: LedgerConfig blk
    lcfg = configLedger cfg

    pcfg :: ConsensusConfig (BlockProtocol blk)
    pcfg = configConsensus cfg

    fbcfg :: FullBlockConfig (LedgerState blk) blk
    fbcfg = topLevelConfigBlock cfg

    time :: a -> IO (a, DiffTime)
    time x = do
        start <- getMonotonicTime
        res   <- evaluate x
        stop  <- getMonotonicTime
        return (res, stop `diffTime` start)

    measureTimings ::
         ExtLedgerState blk
      -> blk
      -> IO (Timings DiffTime, ExtLedgerState blk)
    measureTimings (ExtLedgerState ledger header) !blk = do
        let !slot = blockSlot blk
            !hdr = getHeader blk

        (tickedLedgerState, timingApplyChainTick) <-
          time $ applyChainTick lcfg slot ledger

        (tickedLedgerView, _) <-
          time $ protocolLedgerView lcfg tickedLedgerState

        (tickedHeaderState, timingTickHeaderState) <-
          time $ tickHeaderState pcfg tickedLedgerView slot header

        (ledger', timingApplyLedgerBlock) <-
          time $
            case runExcept (applyLedgerBlock fbcfg blk tickedLedgerState) of
              Right st -> st
              Left  e  ->
                error $
                  "applyLedgerBlock failed for " <> show slot <> ": " <> show e

        (_, timingReapplyLedgerBlock) <-
          time $ reapplyLedgerBlock fbcfg blk tickedLedgerState

        (header', timingValidateHeader) <-
          time $
            case runExcept (validateHeader cfg tickedLedgerView hdr tickedHeaderState) of
              Right st -> st
              Left  e  ->
                error $
                  "validateHeader failed for " <> show slot <> ": " <> show e

        (_, timingRevalidateHeader) <-
          time $ revalidateHeader cfg tickedLedgerView hdr tickedHeaderState

        let !timings = Timings {
                timingApplyChainTick
              , timingTickHeaderState
              , timingApplyLedgerBlock
              , timingReapplyLedgerBlock
              , timingValidateHeader
              , timingRevalidateHeader
              }

        return (timings, ExtLedgerState ledger' header')

{-------------------------------------------------------------------------------
  Analysis: create ledger snapshot
-------------------------------------------------------------------------------}

createLedgerSnapshot :: forall blk. HasAnalysis blk => Analysis blk
createLedgerSnapshot AnalysisEnv { cfg, initLedger, db, registry } =
    void $ processAll db registry GetBlock (ledgerState initLedger) ((=<<) . process)
  where
    slotNo = SlotNo 6393600

    process :: LedgerState blk -> blk -> IO (LedgerState blk)
    process st blk = do
        let !st' = tickThenReapply (topLevelConfigBlock cfg) blk st
        -- TODO yuck
        when (blockSlot blk == slotNo) $ do
          HasAnalysis.exportSnapshot st'
          throwM $ userError "DONE"
        return st'

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

processAllFoldM ::
     forall blk b res. (HasHeader blk, ImmDbSerialiseConstraints blk)
  => Either (ImmDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent (ChainDB IO blk) b
  -> L.FoldM IO b res
  -> IO res
processAllFoldM db rr blockComponent (L.FoldM step initial extract) = do
    initSt <- initial
    finalSt <- processAll db rr blockComponent initSt step
    extract finalSt

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

getTipBlockNo ::
     HasAnalysis blk
  => Either (ImmDB IO blk) (ChainDB IO blk)
  -> IO (WithOrigin BlockNo)
getTipBlockNo = either getImmDbTipBlockNo getChainDbTipBlockNo
  where
    getImmDbTipBlockNo =
          fmap (fmap (\(_, _, _, bno) -> bno))
        . ImmDB.getTipInfo

    getChainDbTipBlockNo =
          fmap (withOriginFromMaybe . fmap blockNo)
        . ChainDB.getTipHeader
