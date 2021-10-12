{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Analysis (
    AnalysisEnv (..)
  , AnalysisName (..)
  , Limit (..)
  , runAnalysis
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Control.Tracer (Tracer (..), traceWith)
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import           Data.Word (Word16)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..),
                     HeaderState (..), annTipPoint)
import           Ouroboros.Consensus.Ledger.Abstract (tickThenApplyLedgerResult)
import           Ouroboros.Consensus.Ledger.Basics (LedgerResult (..))
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol (..))
import           Ouroboros.Consensus.Storage.Common (StreamFrom (..))
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (..))
import           Ouroboros.Consensus.Util.ResourceRegistry


import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB
                     (LgrDbSerialiseConstraints)
import           Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (DiskSnapshot (..),
                     writeSnapshot)
import           Ouroboros.Consensus.Storage.Serialisation (SizeInBytes,
                     encodeDisk)

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
  | StoreLedgerStateAt SlotNo
  | CountBlocks
  deriving Show

runAnalysis ::
     forall blk .
     ( LgrDbSerialiseConstraints blk
     , HasAnalysis blk
     , LedgerSupportsProtocol blk
     )
  => AnalysisName -> Analysis blk
runAnalysis analysisName env@(AnalysisEnv { tracer }) = do
  traceWith tracer (StartedEvent analysisName)
  go analysisName
  traceWith tracer DoneEvent
  where
    go ShowSlotBlockNo             = showSlotBlockNo env
    go CountTxOutputs              = countTxOutputs env
    go ShowBlockHeaderSize         = showHeaderSize env
    go ShowBlockTxsSize            = showBlockTxsSize env
    go ShowEBBs                    = showEBBs env
    go OnlyValidation              = return ()
    go (StoreLedgerStateAt slotNo) = (storeLedgerStateAt slotNo) env
    go CountBlocks                 = countBlocks env

type Analysis blk = AnalysisEnv IO blk -> IO ()

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
  | DoneEvent
  | BlockSlotEvent BlockNo SlotNo (Maybe String)
  | EbbEvent (HeaderHash blk) (ChainHash blk) Bool
  | CountedBlocksEvent Int
  | MaxHeaderSizeEvent Word16
  | SnapshotWarning SlotNo SlotNo
  | BlockTxSizeEvent SlotNo Int SizeInBytes

instance HasAnalysis blk => Show (TraceEvent blk) where
  show (StartedEvent analysisName)        = "Started " <> (show analysisName)
  show DoneEvent                          = "Done"
  show (BlockSlotEvent bn sn cmt)         = intercalate "\t" $ [
      show bn
    , show sn
    ] <> (maybeToList cmt)
  show (EbbEvent ebb previous known)      = intercalate "\t" [
      "EBB: "   <> show ebb
    , "Prev: "  <> show previous
    , "Known: " <> show known
    ]
  show (CountedBlocksEvent counted)       = "Counted " <> show counted <> " blocks."
  show (MaxHeaderSizeEvent size)          =
    "Maximum encountered header size = " <> show size
  show (SnapshotWarning requested actual) =
    "Snapshot was created at " <> show actual <> " " <>
    "because there was no block forged at requested " <> show requested
  show (BlockTxSizeEvent slot numBlocks txsSize) = intercalate "\t" [
      show slot
    , "Num txs in block = " <> show numBlocks
    , "Total size of txs in block = " <> show txsSize
    ]


{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasAnalysis blk => Analysis blk
showSlotBlockNo AnalysisEnv { db, registry, initLedger, limit, tracer } =
    processAll_ db registry GetHeader initLedger limit process
  where
    process :: Header blk -> IO ()
    process hdr = traceWith tracer $ BlockSlotEvent (blockNo hdr) (blockSlot hdr) Nothing

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: forall blk. HasAnalysis blk => Analysis blk
countTxOutputs AnalysisEnv { db, registry, initLedger, limit, tracer } = do
    void $ processAll db registry GetBlock initLedger limit 0 process
  where
    process :: Int -> blk -> IO Int
    process cumulative blk = do
        let cumulative' = cumulative + count
            event       = BlockSlotEvent (blockNo blk)
                                         (blockSlot blk)
                                         (Just $ "cumulative: " <> show cumulative')
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
      processAll db registry ((,) <$> GetBlock <*> GetHeaderSize) initLedger limit 0 process
    traceWith tracer $ MaxHeaderSizeEvent maxHeaderSize
  where
    process :: Word16 -> (blk, Word16) -> IO Word16
    process maxHeaderSize (blk, headerSize) = do
      let event = BlockSlotEvent (blockNo blk)
                                 (blockSlot blk)
                                 (Just $ "header size: " <> show headerSize)
      traceWith tracer event
      return $ maxHeaderSize `max` headerSize

{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

showBlockTxsSize :: forall blk. HasAnalysis blk => Analysis blk
showBlockTxsSize AnalysisEnv { db, registry, initLedger, limit, tracer } =
    processAll_ db registry GetBlock initLedger limit process
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
showEBBs AnalysisEnv { db, registry, initLedger, limit, tracer } =
    processAll_ db registry GetBlock initLedger limit process
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
storeLedgerStateAt slotNo (AnalysisEnv { db, registry, initLedger, cfg, limit, ledgerDbFS, tracer }) =
    void $ processAllUntil db registry GetBlock initLedger limit initLedger process
  where
    process :: ExtLedgerState blk -> blk -> IO (NextStep, ExtLedgerState blk)
    process oldLedger blk = do
      let ledgerCfg     = ExtLedgerCfg cfg
          appliedResult = tickThenApplyLedgerResult ledgerCfg blk oldLedger
          newLedger     = either (error . show) lrResult $ runExcept $ appliedResult
      when (blockSlot blk >= slotNo) $ storeLedgerState blk newLedger
      when (blockSlot blk > slotNo) $ issueWarning blk
      when ((unBlockNo $ blockNo blk) `mod` 1000 == 0) $ reportProgress blk
      return (continue blk, newLedger)

    continue :: blk -> NextStep
    continue blk
      | blockSlot blk >= slotNo = Stop
      | otherwise               = Continue

    issueWarning blk   = let event = SnapshotWarning slotNo (blockSlot blk)
                         in traceWith tracer event
    reportProgress blk = let event = BlockSlotEvent (blockNo blk) (blockSlot blk) (Just "reached")
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
      traceWith tracer $ BlockSlotEvent (blockNo blk) (blockSlot blk) (Just "snapshot stored")

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
  where
    process :: Int -> () -> IO Int
    process count _ = pure $ count + 1
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
processAllChainDB chainDB registry blockComponent (ExtLedgerState {..}) limit initState callback = do
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
processAllImmutableDB immutableDB registry blockComponent (ExtLedgerState {..}) limit initState callback = do
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
