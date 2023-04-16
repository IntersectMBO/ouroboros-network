{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Cardano.Tools.DBAnalyser.Analysis (
    AnalysisEnv (..)
  , AnalysisName (..)
  , AnalysisResult (..)
  , Limit (..)
  , runAnalysis
  ) where

import qualified Cardano.Slotting.Slot as Slotting
import qualified Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint as DP
import           Cardano.Tools.DBAnalyser.HasAnalysis (HasAnalysis)
import qualified Cardano.Tools.DBAnalyser.HasAnalysis as HasAnalysis
import Cardano.Tools.DBAnalyser.LedgerEvents (convertAuxLedgerEvent, convertAuxLedgerEvent')
import Data.Maybe (mapMaybe)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text.IO
import           Data.Word (Word16, Word64)
import qualified Debug.Trace as Debug
import qualified GHC.Stats as GC
import           NoThunks.Class (noThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast (forecastFor)
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..),
                     HeaderState (..), annTipPoint, tickHeaderState,
                     validateHeader)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerCfg, LedgerConfig,
                     applyBlockLedgerResult, applyChainTick,
                     tickThenApplyLedgerResult, tickThenReapply)
import           Ouroboros.Consensus.Ledger.Basics (LedgerResult (..),
                     LedgerState, getTipSlot)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
                     (LedgerSupportsMempool)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol (..))
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Protocol.Abstract (LedgerView)
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB
                     (LgrDbSerialiseConstraints)
import           Ouroboros.Consensus.Storage.Common (BlockComponent (..),
                     StreamFrom (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB (DiskSnapshot (..),
                     writeSnapshot)
import           Ouroboros.Consensus.Storage.Serialisation (SizeInBytes,
                     encodeDisk)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           System.FS.API (SomeHasFS (..))
import qualified System.IO as IO
import qualified Text.Builder as Builder

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
  | StoreLedgerStateAt SlotNo
  | CountBlocks
  | CheckNoThunksEvery Word64
  | TraceLedgerProcessing
  | BenchmarkLedgerOps (Maybe FilePath)
  | ReproMempoolAndForge Int
  deriving Show

data AnalysisResult =
    ResultCountBlock Int
  | ResultMaxHeaderSize Word16
  deriving (Eq, Show)


runAnalysis ::
     forall blk .
     ( HasAnalysis blk
     , LedgerSupportsMempool.HasTxId (LedgerSupportsMempool.GenTx blk)
     , LedgerSupportsMempool.HasTxs blk
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , LgrDbSerialiseConstraints blk
     )
  => AnalysisName -> Analysis blk
runAnalysis analysisName env@(AnalysisEnv { tracer }) = do
    traceWith tracer (StartedEvent analysisName)
    result <- go analysisName
    traceWith tracer DoneEvent
    pure result
  where
    go ShowSlotBlockNo               = showSlotBlockNo env
    go CountTxOutputs                = countTxOutputs env
    go ShowBlockHeaderSize           = showHeaderSize env
    go ShowBlockTxsSize              = showBlockTxsSize env
    go ShowEBBs                      = showEBBs env
    go OnlyValidation                = pure Nothing
    go (StoreLedgerStateAt slotNo)   = storeLedgerStateAt slotNo env
    go CountBlocks                   = countBlocks env
    go (CheckNoThunksEvery nBks)     = checkNoThunksEvery nBks env
    go TraceLedgerProcessing         = traceLedgerProcessing env
    go (ReproMempoolAndForge nBks)   = reproMempoolForge nBks env
    go (BenchmarkLedgerOps mOutfile) = benchmarkLedgerOps mOutfile env

type Analysis blk = AnalysisEnv IO blk -> IO (Maybe AnalysisResult)

data AnalysisEnv m blk = AnalysisEnv {
      cfg        :: TopLevelConfig blk
    , initLedger :: ExtLedgerState blk
    , db         :: Either (ImmutableDB IO blk) (ChainDB IO blk)
    , registry   :: ResourceRegistry IO
    , ledgerDbFS :: SomeHasFS IO
    , limit      :: Limit
    , tracer     :: Tracer m (TraceEvent blk)
    }

data TraceEvent blk =
    StartedEvent AnalysisName
    -- ^ triggered when given analysis has started
  | DoneEvent
    -- ^ triggered when analysis has ended
  | BlockSlotEvent BlockNo SlotNo
    -- ^ triggered when block has been found, it holds:
    --   * block's number
    --   * slot number when the block was forged
  | CountTxOutputsEvent BlockNo SlotNo Int Int
    -- ^ triggered when block has been found, it holds:
    --   * block's number
    --   * slot number when the block was forged
    --   * cumulative tx output
    --   * count tx output
  | EbbEvent (HeaderHash blk) (ChainHash blk) Bool
    -- ^ triggered when EBB block has been found, it holds:
    --   * its hash,
    --   * hash of previous block
    --   * flag whether the EBB is known
  | CountedBlocksEvent Int
    -- ^ triggered once during CountBLocks analysis,
    --   when blocks were counted
  | HeaderSizeEvent BlockNo SlotNo Word16
    -- ^ triggered when header size has been measured
    --   * block's number
    --   * slot number when the block was forged
    --   * block's header size
  | MaxHeaderSizeEvent Word16
    -- ^ triggered once during ShowBlockTxsSize analysis,
    --   holding maximum encountered header size
  | SnapshotStoredEvent SlotNo
    -- ^ triggered when snapshot of ledger has been stored for SlotNo
  | SnapshotWarningEvent SlotNo SlotNo
    -- ^ triggered once during  StoreLedgerStateAt analysis,
    --   when snapshot was created in slot proceeding the
    --   requested one
  | BlockTxSizeEvent SlotNo Int SizeInBytes
    -- ^ triggered for all blocks during ShowBlockTxsSize analysis,
    --   it holds:
    --   * slot number when the block was forged
    --   * number of transactions in the block
    --   * total size of transactions in the block
  | BlockMempoolAndForgeRepro BlockNo SlotNo Int SizeInBytes IOLike.DiffTime IOLike.DiffTime
    -- ^ triggered for all blocks during ShowBlockTxsSize analysis,
    --   it holds:
    --   * block number
    --   * slot number when the block was forged
    --   * number of transactions in the block
    --   * total size of transactions in the block
    --   * time to tick ledger state
    --   * time to call 'Mempool.getSnapshotFor'

instance HasAnalysis blk => Show (TraceEvent blk) where
  show (StartedEvent analysisName)        = "Started " <> (show analysisName)
  show DoneEvent                          = "Done"
  show (BlockSlotEvent bn sn)             = intercalate "\t" $ [
      show bn
    , show sn
    ]
  show (CountTxOutputsEvent bn sn cumulative count) = intercalate "\t" $ [
      show bn
    , show sn
    , "cumulative: " <> show cumulative
    , "count: " <> show count
    ]
  show (EbbEvent ebb previous known)      = intercalate "\t" [
      "EBB: "   <> show ebb
    , "Prev: "  <> show previous
    , "Known: " <> show known
    ]
  show (CountedBlocksEvent counted)       = "Counted " <> show counted <> " blocks."
  show (HeaderSizeEvent bn sn headerSize) = intercalate "\t" $ [
      show bn
    , show sn
    , "header size: " <> show headerSize
    ]
  show (MaxHeaderSizeEvent size)          =
    "Maximum encountered header size = " <> show size
  show (SnapshotStoredEvent slot)         =
    "Snapshot stored at " <> show slot
  show (SnapshotWarningEvent requested actual) =
    "Snapshot was created at " <> show actual <> " " <>
    "because there was no block forged at requested " <> show requested
  show (BlockTxSizeEvent slot numBlocks txsSize) = intercalate "\t" [
      show slot
    , "Num txs in block = " <> show numBlocks
    , "Total size of txs in block = " <> show txsSize
    ]
  show (BlockMempoolAndForgeRepro bno slot txsCount txsSize durTick durSnap) = intercalate "\t" [
      show bno
    , show slot
    , "txsCount " <> show txsCount
    , "txsSize " <> show txsSize
    , "durTick " <> show durTick
    , "durSnap " <> show durSnap
    ]


{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasAnalysis blk => Analysis blk
showSlotBlockNo AnalysisEnv { db, registry, initLedger, limit, tracer } =
    processAll_ db registry GetHeader initLedger limit process
        >> pure Nothing
  where
    process :: Header blk -> IO ()
    process hdr = traceWith tracer $ BlockSlotEvent (blockNo hdr) (blockSlot hdr)

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: forall blk. HasAnalysis blk => Analysis blk
countTxOutputs AnalysisEnv { db, registry, initLedger, limit, tracer } = do
    void $ processAll db registry GetBlock initLedger limit 0 process
    pure Nothing
  where
    process :: Int -> blk -> IO Int
    process cumulative blk = do
        let cumulative' = cumulative + count
            event       = CountTxOutputsEvent (blockNo blk)
                                              (blockSlot blk)
                                              cumulative'
                                              count
        traceWith tracer event
        return cumulative'
      where
        count = HasAnalysis.countTxOutputs blk

{-------------------------------------------------------------------------------
  Analysis: show the header size in bytes for all blocks
-------------------------------------------------------------------------------}

showHeaderSize :: forall blk. HasAnalysis blk => Analysis blk
showHeaderSize AnalysisEnv { db, registry, initLedger, limit, tracer } = do
    maxHeaderSize <-
      processAll db registry ((,) <$> GetHeader <*> GetHeaderSize) initLedger limit 0 process
    traceWith tracer $ MaxHeaderSizeEvent maxHeaderSize
    pure $ Just $ ResultMaxHeaderSize maxHeaderSize
  where
    process :: Word16 -> (Header blk, Word16) -> IO Word16
    process maxHeaderSize (hdr, headerSize) = do
      let event = HeaderSizeEvent (blockNo hdr)
                                  (blockSlot hdr)
                                   headerSize
      traceWith tracer event
      return $ maxHeaderSize `max` headerSize

{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

showBlockTxsSize :: forall blk. HasAnalysis blk => Analysis blk
showBlockTxsSize AnalysisEnv { db, registry, initLedger, limit, tracer } = do
    processAll_ db registry GetBlock initLedger limit process
    pure Nothing
  where
    process :: blk -> IO ()
    process blk =
      traceWith tracer $ BlockTxSizeEvent (blockSlot blk) numBlockTxs blockTxsSize
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
showEBBs AnalysisEnv { db, registry, initLedger, limit, tracer } = do
    processAll_ db registry GetBlock initLedger limit process
    pure Nothing
  where
    process :: blk -> IO ()
    process blk =
        case blockIsEBB blk of
          Just _epoch -> do
            let known =  Map.lookup
                            (blockHash blk)
                            (HasAnalysis.knownEBBs (Proxy @blk))
                       == Just (blockPrevHash blk)
                event = EbbEvent (blockHash blk) (blockPrevHash blk) known
            traceWith tracer event
          _otherwise -> return () -- Skip regular blocks

{-------------------------------------------------------------------------------
  Analysis: store a ledger at specific slot
-------------------------------------------------------------------------------}

storeLedgerStateAt ::
     forall blk .
     ( LgrDbSerialiseConstraints blk
     , HasAnalysis blk
     , LedgerSupportsProtocol blk
     )
  => SlotNo -> Analysis blk
storeLedgerStateAt slotNo (AnalysisEnv { db, registry, initLedger, cfg, limit, ledgerDbFS, tracer }) = do
    void $ processAllUntil db registry GetBlock initLedger limit initLedger process
    pure Nothing
  where
    process :: ExtLedgerState blk -> blk -> IO (NextStep, ExtLedgerState blk)
    process oldLedger blk = do
      let ledgerCfg = ExtLedgerCfg cfg
          newLedger = lrResult $ tickThenReapply ledgerCfg blk oldLedger
      when (blockSlot blk >= slotNo) $ storeLedgerState blk newLedger
      when (blockSlot blk > slotNo) $ issueWarning blk
      when ((unBlockNo $ blockNo blk) `mod` 1000 == 0) $ reportProgress blk
      return (continue blk, newLedger)

    continue :: blk -> NextStep
    continue blk
      | blockSlot blk >= slotNo = Stop
      | otherwise               = Continue

    issueWarning blk   = let event = SnapshotWarningEvent slotNo (blockSlot blk)
                         in traceWith tracer event
    reportProgress blk = let event = BlockSlotEvent (blockNo blk) (blockSlot blk)
                         in traceWith tracer event

    storeLedgerState ::
         blk
      -> ExtLedgerState blk
      -> IO ()
    storeLedgerState blk ledgerState = do
      let snapshot = DiskSnapshot
                      (unSlotNo $ blockSlot blk)
                      (Just $ "db-analyser")
      writeSnapshot ledgerDbFS encLedger snapshot ledgerState
      traceWith tracer $ SnapshotStoredEvent (blockSlot blk)

    encLedger :: ExtLedgerState blk -> Encoding
    encLedger =
      let ccfg = configCodec cfg
      in encodeExtLedgerState
           (encodeDisk ccfg)
           (encodeDisk ccfg)
           (encodeDisk ccfg)

countBlocks ::
     forall blk .
     ( HasAnalysis blk
     )
  => Analysis blk
countBlocks (AnalysisEnv { db, registry, initLedger, limit, tracer }) = do
    counted <- processAll db registry (GetPure ()) initLedger limit 0 process
    traceWith tracer $ CountedBlocksEvent counted
    pure $ Just $ ResultCountBlock counted
  where
    process :: Int -> () -> IO Int
    process count _ = pure $ count + 1
{-------------------------------------------------------------------------------
  Analysis: check for ledger state thunks every n blocks
-------------------------------------------------------------------------------}

checkNoThunksEvery ::
  forall blk.
  ( HasAnalysis blk,
    LedgerSupportsProtocol blk
  ) =>
  Word64 ->
  Analysis blk
checkNoThunksEvery
  nBlocks
  (AnalysisEnv {db, registry, initLedger, cfg, limit}) = do
    putStrLn $
      "Checking for thunks in each block where blockNo === 0 (mod " <> show nBlocks <> ")."
    void $ processAll db registry GetBlock initLedger limit initLedger process
    pure Nothing
  where
    process :: ExtLedgerState blk -> blk -> IO (ExtLedgerState blk)
    process oldLedger blk = do
      let ledgerCfg     = ExtLedgerCfg cfg
          appliedResult = tickThenApplyLedgerResult ledgerCfg blk oldLedger
          newLedger     = either (error . show) lrResult $ runExcept $ appliedResult
          bn            = blockNo blk
      when (unBlockNo bn `mod` nBlocks == 0 ) $ IOLike.evaluate (ledgerState newLedger) >>= checkNoThunks bn
      return newLedger

    checkNoThunks :: BlockNo -> LedgerState blk -> IO ()
    checkNoThunks bn ls =
      noThunks ["--checkThunks"] ls >>= \case
        Nothing -> putStrLn $ show bn <> ": no thunks found."
        Just ti -> do
          putStrLn $ show bn <> ": thunks found."
          print ti

{-------------------------------------------------------------------------------
  Analysis: maintain a ledger state and issue trace markers at appropriate
  points in the epoch
-------------------------------------------------------------------------------}

traceLedgerProcessing ::
  forall blk.
  ( HasAnalysis blk,
    LedgerSupportsProtocol blk
  ) =>
  Analysis blk
traceLedgerProcessing
  (AnalysisEnv {db, registry, initLedger, cfg, limit}) = do
    void $ processAll db registry GetBlock initLedger limit initLedger process
    pure Nothing
  where
    process
      :: ExtLedgerState blk
      -> blk
      -> IO (ExtLedgerState blk)
    process oldLedger blk = do
      let ledgerCfg     = ExtLedgerCfg cfg
          appliedResult = runExcept $ tickThenApplyLedgerResult ledgerCfg blk oldLedger
          newLedger     = either (error . show) lrResult $ appliedResult
          events        = either (error . show) lrEvents $ appliedResult
          traces        =
            (HasAnalysis.emitTraces $
              HasAnalysis.WithLedgerState blk (ledgerState oldLedger) (ledgerState newLedger) events)
      mapM_ Debug.traceMarkerIO traces
      return $ newLedger

{-------------------------------------------------------------------------------
  Analysis: maintain a ledger state and time the five major ledger calculations
  for each block:

  0. Forecast.
  1. Header tick.
  2. Header application.
  3. Block tick.
  4. Block application.

  We focus on these 5 operations because they are involved in:

  - Chain syncing.
  - Block forging.
  - Block validation.

-------------------------------------------------------------------------------}
benchmarkLedgerOps ::
  forall blk.
     ( HasAnalysis blk
     , LedgerSupportsProtocol blk
     )
  => Maybe FilePath -> Analysis blk
benchmarkLedgerOps mOutfile AnalysisEnv {db, registry, initLedger, cfg, limit} =
    withFile mOutfile $ \outFileHandle -> do
      let line = Builder.run $ DP.showHeaders separator
                             <> separator
                             <> "...era-specific stats"
      Text.IO.hPutStrLn outFileHandle line

      void $ processAll db registry GetBlock initLedger limit initLedger (process outFileHandle)
      pure Nothing
  where
    withFile :: Maybe FilePath -> (IO.Handle -> IO r) -> IO r
    withFile (Just outfile) = IO.withFile outfile IO.WriteMode
    withFile Nothing        = \f -> f IO.stdout

    -- Separator for the data that is printed
    separator = "\t"

    ccfg = topLevelConfigProtocol cfg
    lcfg = topLevelConfigLedger   cfg

    process ::
         IO.Handle
      -> ExtLedgerState blk
      -> blk
      -> IO (ExtLedgerState blk)
    process outFileHandle prevLedgerState blk = do
        prevRtsStats <- GC.getRTSStats
        let
          -- Compute how many nanoseconds the mutator used from the last
          -- recorded 'elapsedTime' till the end of the execution of the given
          -- action. This function forces the evaluation of its argument's
          -- result.
          time act = do
              tPrev <- GC.mutator_elapsed_ns <$> GC.getRTSStats
              !r <- act
              tNow <- GC.mutator_elapsed_ns <$> GC.getRTSStats
              pure (r, tNow - tPrev)

        let slot = blockSlot      blk
        -- We do not use strictness annotation on the resulting tuples since
        -- 'time' takes care of forcing the evaluation of its argument's result.
        (tkLdgrView, tForecast) <- time $ forecast            slot prevLedgerState
        (tkHdrSt,    tHdrTick)  <- time $ tickTheHeaderState  slot prevLedgerState tkLdgrView
        (hdrSt',     tHdrApp)   <- time $ applyTheHeader                           tkLdgrView tkHdrSt
        (tkLdgrSt,   tBlkTick)  <- time $ tickTheLedgerState  slot prevLedgerState
        (ldgrSt',    tBlkApp)   <- time $ applyTheBlock                                       tkLdgrSt

        currentRtsStats <- GC.getRTSStats
        let
          currentMinusPrevious f = f currentRtsStats - f prevRtsStats
          slotDataPoint =
            DP.SlotDataPoint
            { slot            = realPointSlot rp
            , slotGap         = slot `slotCount` getTipSlot prevLedgerState
            , totalTime       = currentMinusPrevious GC.elapsed_ns          `div` 1000
            , mut             = currentMinusPrevious GC.mutator_elapsed_ns  `div` 1000
            , gc              = currentMinusPrevious GC.gc_elapsed_ns       `div` 1000
            , majGcCount      = currentMinusPrevious GC.major_gcs
            , mut_forecast    = tForecast `div` 1000
            , mut_headerTick  = tHdrTick  `div` 1000
            , mut_headerApply = tHdrApp   `div` 1000
            , mut_blockTick   = tBlkTick  `div` 1000
            , mut_blockApply  = tBlkApp   `div` 1000
            }

          slotCount (SlotNo i) = \case
            Slotting.Origin        -> i
            Slotting.At (SlotNo j) -> i - j

          line = Builder.run $  DP.showData slotDataPoint separator
                             <> separator
                             <> Builder.intercalate separator (HasAnalysis.blockStats blk)

        Text.IO.hPutStrLn outFileHandle line

        pure $ ExtLedgerState ldgrSt' hdrSt'
      where
        rp = blockRealPoint blk

        forecast ::
             SlotNo
          -> ExtLedgerState blk
          -> IO (Ticked (LedgerView (BlockProtocol blk)))
        forecast slot st = do
            let forecaster = ledgerViewForecastAt lcfg (ledgerState st)
            case runExcept $ forecastFor forecaster slot of
              Left err -> fail $ "benchmark doesn't support headers beyond the forecast limit: " <> show rp <> " " <> show err
              Right x  -> pure x

        tickTheHeaderState ::
             SlotNo
          -> ExtLedgerState blk
          -> Ticked (LedgerView (BlockProtocol blk))
          -> IO (Ticked (HeaderState blk))
        tickTheHeaderState slot st tickedLedgerView =
            pure $! tickHeaderState ccfg
                                    tickedLedgerView
                                    slot
                                    (headerState st)

        applyTheHeader ::
             Ticked (LedgerView (BlockProtocol blk))
          -> Ticked (HeaderState blk)
          -> IO (HeaderState blk)
        applyTheHeader tickedLedgerView tickedHeaderState = do
            case runExcept $ validateHeader cfg tickedLedgerView (getHeader blk) tickedHeaderState of
              Left err -> fail $ "benchmark doesn't support invalid headers: " <> show rp <> " " <> show err
              Right x -> pure x

        tickTheLedgerState ::
             SlotNo
          -> ExtLedgerState blk
          -> IO (Ticked (LedgerState blk))
        tickTheLedgerState slot st =
            pure $ applyChainTick lcfg slot (ledgerState st)

        applyTheBlock ::
             Ticked (LedgerState blk)
          -> IO (LedgerState blk)
        applyTheBlock tickedLedgerSt = do
            case runExcept (lrResult <$> applyBlockLedgerResult lcfg blk tickedLedgerSt) of
              Left err -> fail $ "benchmark doesn't support invalid blocks: " <> show rp <> " " <> show err
              Right x  -> pure x

{-------------------------------------------------------------------------------
  Analysis: reforge the blocks, via the mempool
-------------------------------------------------------------------------------}

data ReproMempoolForgeHowManyBlks = ReproMempoolForgeOneBlk | ReproMempoolForgeTwoBlks

reproMempoolForge ::
  forall blk.
  ( HasAnalysis blk
  , LedgerSupportsMempool.HasTxId (LedgerSupportsMempool.GenTx blk)
  , LedgerSupportsMempool.HasTxs blk
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  ) =>
  Int ->
  Analysis blk
reproMempoolForge numBlks env = do
    howManyBlocks <- case numBlks of
      1 -> pure ReproMempoolForgeOneBlk
      2 -> pure ReproMempoolForgeTwoBlks
      _ -> fail $ "--repro-mempool-and-forge only supports"
               <> "1 or 2 blocks at a time, not " <> show numBlks

    ref <- IOLike.newTVarIO initLedger
    mempool <- Mempool.openMempoolWithoutSyncThread
      Mempool.LedgerInterface {
        getCurrentLedgerState = ledgerState <$> IOLike.readTVar ref
      }
      lCfg
      -- one megabyte should generously accomodate two blocks' worth of txs
      (Mempool.MempoolCapacityBytesOverride $ Mempool.MempoolCapacityBytes $ 2^(20 :: Int))
      nullTracer
      LedgerSupportsMempool.txInBlockSize

    void $ processAll db registry GetBlock initLedger limit Nothing (process howManyBlocks ref mempool)
    pure Nothing
  where
    AnalysisEnv {
      cfg
    , initLedger
    , db
    , registry
    , limit
    , tracer
    } = env

    lCfg :: LedgerConfig blk
    lCfg = configLedger cfg

    elCfg :: LedgerCfg (ExtLedgerState blk)
    elCfg = ExtLedgerCfg cfg

    timed :: IO a -> IO (a, IOLike.DiffTime)
    timed m = do
      before <- IOLike.getMonotonicTime
      !x <- m
      after <- IOLike.getMonotonicTime
      pure (x, after `IOLike.diffTime` before)

    process
      :: ReproMempoolForgeHowManyBlks
      -> IOLike.StrictTVar IO (ExtLedgerState blk)
      -> Mempool.Mempool IO blk
      -> Maybe blk
      -> blk
      -> IO (Maybe blk)
    process howManyBlocks ref mempool mbBlk blk' = (\() -> Just blk') <$> do
      -- add this block's transactions to the mempool
      do
        results <- Mempool.addTxs mempool $ LedgerSupportsMempool.extractTxs blk'
        let rejs = [ rej | rej@Mempool.MempoolTxRejected{} <- results ]
        unless (null rejs) $ do
          fail $ "Mempool rejected some of the on-chain txs: " <> show rejs

      let scrutinee = case howManyBlocks of
            ReproMempoolForgeOneBlk  -> Just blk'
            ReproMempoolForgeTwoBlks -> mbBlk
      case scrutinee of
        Nothing  -> pure ()
        Just blk -> do
          st <- IOLike.readTVarIO ref

          -- time the suspected slow parts of the forge thread that created
          -- this block
          --
          -- Primary caveat: that thread's mempool may have had more transactions in it.
          do
            let slot = blockSlot blk
            (ticked, durTick) <- timed $ IOLike.evaluate $
              applyChainTick lCfg slot (ledgerState st)
            ((), durSnap) <- timed $ IOLike.atomically $ do
              snap <- Mempool.getSnapshotFor mempool $ Mempool.ForgeInKnownSlot slot ticked

              pure $ length (Mempool.snapshotTxs snap) `seq` Mempool.snapshotLedgerState snap `seq` ()

            let sizes = HasAnalysis.blockTxSizes blk
            traceWith tracer $
              BlockMempoolAndForgeRepro
                (blockNo blk)
                slot
                (length sizes)
                (sum sizes)
                durTick
                durSnap

          -- advance the ledger state to include this block
          --
          -- TODO We could inline/reuse parts of the IsLedger ExtLedgerState
          -- instance here as an optimization that avoids repeating the
          -- 'applyChainTick' call above. We want to leave that call alone, though,
          -- since it currently matches the call in the forging thread, which is
          -- the primary intention of this Analysis. Maybe GHC's CSE is already
          -- doing this sharing optimization?
          IOLike.atomically $ IOLike.writeTVar ref $! lrResult (tickThenReapply elCfg blk st)

          -- this flushes blk from the mempool, since every tx in it is now on the chain
          void $ Mempool.syncWithLedger mempool

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the DB
-------------------------------------------------------------------------------}

data Limit = Limit Int | Unlimited

decreaseLimit :: Limit -> Maybe Limit
decreaseLimit Unlimited = Just Unlimited
decreaseLimit (Limit 0) = Nothing
decreaseLimit (Limit n) = Just . Limit $ n - 1

data NextStep = Continue | Stop


processAllUntil ::
     forall blk b st. (HasHeader blk, HasAnnTip blk)
  => Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> ExtLedgerState blk
  -> Limit
  -> st
  -> (st -> b -> IO (NextStep, st))
  -> IO st
processAllUntil = either processAllImmutableDB processAllChainDB

processAll ::
     forall blk b st. (HasHeader blk, HasAnnTip blk)
  => Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> ExtLedgerState blk
  -> Limit
  -> st
  -> (st -> b -> IO st)
  -> IO st
processAll db rr blockComponent initLedger limit initSt cb =
  processAllUntil db rr blockComponent initLedger limit initSt callback
    where
      callback st b = (Continue, ) <$> cb st b

processAll_ ::
     forall blk b. (HasHeader blk, HasAnnTip blk)
  => Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> ExtLedgerState blk
  -> Limit
  -> (b -> IO ())
  -> IO ()
processAll_ db registry blockComponent initLedger limit callback =
    processAll db registry blockComponent initLedger limit () (const callback)

processAllChainDB ::
     forall st blk b. (HasHeader blk, HasAnnTip blk)
  => ChainDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> ExtLedgerState blk
  -> Limit
  -> st
  -> (st -> b -> IO (NextStep, st))
  -> IO st
processAllChainDB chainDB registry blockComponent ExtLedgerState{headerState} limit initState callback = do
    itr <- case headerStateTip headerState of
      Origin           -> ChainDB.streamAll
                             chainDB
                             registry
                             blockComponent
      NotOrigin annTip -> ChainDB.streamFrom
                             (StreamFromExclusive $ annTipPoint annTip)
                             chainDB
                             registry
                             blockComponent
    go itr limit initState
  where
    go :: ChainDB.Iterator IO blk b -> Limit -> st -> IO st
    go itr lt !st = case decreaseLimit lt of
      Nothing             -> return st
      Just decreasedLimit -> do
        itrResult <- ChainDB.iteratorNext itr
        case itrResult of
          ChainDB.IteratorExhausted    -> return st
          ChainDB.IteratorResult b     -> callback st b >>= \case
            (Continue, nst) -> go itr decreasedLimit nst
            (Stop, nst)     -> return nst
          ChainDB.IteratorBlockGCed pt -> error $ "block GC'ed " <> show pt

processAllImmutableDB ::
     forall st blk b. (HasHeader blk, HasAnnTip blk)
  => ImmutableDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> ExtLedgerState blk
  -> Limit
  -> st
  -> (st -> b -> IO (NextStep, st))
  -> IO st
processAllImmutableDB immutableDB registry blockComponent ExtLedgerState{headerState} limit initState callback = do
    itr <- case headerStateTip headerState of
      Origin           -> ImmutableDB.streamAll
                             immutableDB
                             registry
                             blockComponent
      NotOrigin annTip -> ImmutableDB.streamAfterKnownPoint
                             immutableDB
                             registry
                             blockComponent
                            (annTipPoint annTip)
    go itr limit initState
  where
    go :: ImmutableDB.Iterator IO blk b -> Limit -> st -> IO st
    go itr lt !st = case decreaseLimit lt of
      Nothing               -> return st
      Just decreasedLimit   -> do
        itrResult <- ImmutableDB.iteratorNext itr
        case itrResult of
          ImmutableDB.IteratorExhausted -> return st
          ImmutableDB.IteratorResult b  -> callback st b >>= \case
            (Continue, nst) -> go itr decreasedLimit nst
            (Stop, nst)     -> return nst
