{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Analysis (
    AnalysisEnv (..)
  , AnalysisName (..)
  , runAnalysis
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise
import           Control.Monad.Except
import           Control.Tracer (Tracer (..), traceWith)
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Word (Word16, Word64)
import qualified Debug.Trace as Debug

import           Ouroboros.Network.Point (WithOrigin (At))

import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Block.RealPoint
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB hiding
                     (TraceEvent)
import           Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk hiding (TraceEvent)
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as OnDisk
import           Ouroboros.Consensus.Storage.Serialisation

import           HasAnalysis

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
  deriving Show

runAnalysis ::
     forall blk .
     ( LgrDbSerialiseConstraints blk
     , HasAnalysis blk)
  => AnalysisName -> Analysis blk
runAnalysis analysisName env@AnalysisEnv{ tracer } = do
  traceWith tracer (StartedEvent analysisName)
  go analysisName
  traceWith tracer DoneEvent
  where
    go ShowSlotBlockNo             = showSlotBlockNo env
    go CountTxOutputs              = Analysis.countTxOutputs env
    go ShowBlockHeaderSize         = showHeaderSize env
    go ShowBlockTxsSize            = showBlockTxsSize env
    go ShowEBBs                    = showEBBs env
    go OnlyValidation              = return ()
    go (StoreLedgerStateAt slotNo) = storeLedgerStateAt slotNo env
    go CountBlocks                 = countBlocks env
    go (CheckNoThunksEvery nBks)   = checkNoThunksEvery nBks env
    go TraceLedgerProcessing       = traceLedgerProcessing env

type Analysis blk = AnalysisEnv IO blk -> IO ()

data AnalysisEnv m blk = AnalysisEnv {
      cfg        :: TopLevelConfig blk
    , initLedger :: Either DiskSnapshot (ExtLedgerState blk ValuesMK)
      -- ^ Whether we are initializing from a snapshot or directly from a
      -- genesis ledger state
    , db         :: Either (ImmutableDB IO blk) (ChainDB IO blk)
    , ledgerDbFS :: SomeHasFS IO
    , backing    :: BackingStoreSelector IO
    , limit      :: Maybe Int
      -- ^ Maybe stop when a certain number of blocks have been processed, or
      -- process the whole Chain/ImmutableDB
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

instance HasAnalysis blk => Show (TraceEvent blk) where
  show (StartedEvent analysisName)        = "Started " <> show analysisName
  show DoneEvent                          = "Done"
  show (BlockSlotEvent bn sn)             = intercalate "\t" [
      "Number of processed block: " <> show (unBlockNo bn)
    , show sn
    ]
  show (CountTxOutputsEvent bn sn cumulative count) = intercalate "\t" [
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
  show (HeaderSizeEvent bn sn headerSize) = intercalate "\t" [
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


{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasAnalysis blk => Analysis blk
showSlotBlockNo env = do
  doProcess env () process GetBlock
  where
    process :: blk -> () -> IO ()
    process blk () = traceWith (tracer env) $ BlockSlotEvent (blockNo blk) (blockSlot blk)

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: forall blk. HasAnalysis blk => Analysis blk
countTxOutputs env = void $ doProcess env 0 process GetBlock
  where
    process :: blk -> Int ->  IO Int
    process blk cumulative = do
        let cumulative' = cumulative + count
            event       = CountTxOutputsEvent (blockNo blk)
                                              (blockSlot blk)
                                              cumulative'
                                              count
        traceWith (tracer env) event
        return cumulative'
      where
        count = HasAnalysis.countTxOutputs blk

{-------------------------------------------------------------------------------
  Analysis: show the header size in bytes for all blocks
-------------------------------------------------------------------------------}

showHeaderSize :: forall blk. HasAnalysis blk => Analysis blk
showHeaderSize env = do
  maxHeaderSize <- doProcess env 0 process ((,) <$> GetHeader <*> GetHeaderSize)
  traceWith (tracer env) $ MaxHeaderSizeEvent maxHeaderSize
  where
    process :: (Header blk, Word16) -> Word16 -> IO Word16
    process (hdr, headerSize) maxHeaderSize = do
      let event = HeaderSizeEvent (blockNo hdr)
                                  (blockSlot hdr)
                                   headerSize
      traceWith (tracer env) event
      return $ maxHeaderSize `max` headerSize

{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

showBlockTxsSize :: forall blk. HasAnalysis blk => Analysis blk
showBlockTxsSize env = void $ doProcess env () process GetBlock
  where
    process :: blk -> () -> IO ()
    process blk () =
      traceWith (tracer env) $ BlockTxSizeEvent
                                 (blockSlot blk)
                                 numBlockTxs
                                 blockTxsSize
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
showEBBs  env = void $ doProcess env () process GetBlock
  where
    process :: blk -> () -> IO ()
    process blk () =
        case blockIsEBB blk of
          Just _epoch -> do
            let known =  Map.lookup
                            (blockHash blk)
                            (HasAnalysis.knownEBBs (Proxy @blk))
                       == Just (blockPrevHash blk)
                event = EbbEvent (blockHash blk) (blockPrevHash blk) known
            traceWith (tracer env) event
          _otherwise -> return () -- Skip regular blocks

{-------------------------------------------------------------------------------
  Analysis: store a ledger at specific slot
-------------------------------------------------------------------------------}

storeLedgerStateAt ::
     forall blk .
     ( LgrDbSerialiseConstraints blk
     , HasAnalysis blk
     )
  => SlotNo
  -> Analysis blk
storeLedgerStateAt slotNo AnalysisEnv { db, backing, initLedger, cfg, ledgerDbFS, tracer } = do

    tp <- atomically $ either ImmutableDB.getTipPoint ChainDB.getTipPoint db
    when (pointSlot tp < At slotNo) $ warning (pointSlot tp)

    initLedger' <- initialiseLedger ledgerDbFS cfg initLedger

    tracer' <- decorateReplayTracerWithStart (castPoint . getTip $ initLedger')
             . mkTracer
           <$> newIORef (0 :: Int)

    (initDb, backingStore) <- ledgerDbAndBackingStore
                                backing
                                ledgerDbFS
                              $ case initLedger of
                                  Left s  -> Left (initLedger', s)
                                  Right v -> Right v

    eDB <- runExceptT
         $ replayStartingWith
             tracer'
             configLedgerDb
             backingStore
             (mkStream (\b -> if blockSlot b >= slotNo
                              then return NoMoreBlocks
                              else return $ NextBlock b) GetBlock db)
             initDb

    case eDB of
      Left e -> error $ show e
      Right (ldb, _) -> do
        -- The replay performed above leaves the DbChangelog at @tip - k@
        -- blocks. We use this flush to push all the remaining differences into
        -- the backing store.
        OnDisk.flush backingStore $ ledgerDbChangelog ldb

        writeSnapshot
          ledgerDbFS
          backingStore
          encLedger
          (snapshot (pointSlot $ Ouroboros.Consensus.Ledger.Abstract.getTip $ ledgerDbCurrent ldb))
          (ledgerDbCurrent ldb)

    traceWith tracer $ SnapshotStoredEvent slotNo
  where
    warning s = putStrLn $ "WARNING: The tip of your chain is behind the requested slot number: " <> show s <> " < " <> show slotNo

    mkTracer traceIORef = Tracer $ \f ->
          case f (ReplayGoal GenesisPoint) of
            ReplayedBlock (RealPoint s _) _ _ _ -> do
              c <- readIORef traceIORef
              modifyIORef traceIORef (+ 1)
              when (c `mod` 10000 == 0) $ traceWith tracer (BlockSlotEvent (fromIntegral c) s)
            _ -> return ()

    ccfg = configCodec cfg

    encLedger :: ExtLedgerState blk EmptyMK -> Encoding
    encLedger = encodeExtLedgerState
                 (encodeDisk ccfg)
                 (encodeDisk ccfg)
                 (encodeDisk ccfg)

    snapshot woSlotNo = DiskSnapshot
                        (withOrigin 0 unSlotNo woSlotNo)
                        (Just "db-analyser")

    configLedgerDb = LedgerDbCfg {
        ledgerDbCfgSecParam = configSecurityParam cfg
      , ledgerDbCfg         = ExtLedgerCfg cfg
      }

{-------------------------------------------------------------------------------
  Analysis: count how many blocks are processed
-------------------------------------------------------------------------------}

countBlocks ::
     forall blk .
     ( HasAnalysis blk
     )
  => Analysis blk
countBlocks env = do
  counted <- doProcess env 0 process (GetPure ())
  traceWith (tracer env) $ CountedBlocksEvent counted
  where
    process :: () -> Int -> IO Int
    process () count = pure $ count + 1

{-------------------------------------------------------------------------------
  Analysis: check for ledger state thunks every n blocks
-------------------------------------------------------------------------------}

checkNoThunksEvery ::
  forall blk.
     HasAnalysis blk
  => Word64
  -> Analysis blk
checkNoThunksEvery
  nBlocks
  AnalysisEnv {db, initLedger, cfg, ledgerDbFS, limit, backing} = void $ do
    putStrLn $
      "Checking for thunks in each block where blockNo === 0 (mod " <> show nBlocks <> ")."
    doCheck <- onlyCheckNumBlocks limit

    initLedger' <- initialiseLedger ledgerDbFS cfg initLedger

    (ldb, bs) <- ledgerDbAndBackingStore backing ledgerDbFS $ case initLedger of
                                                                Left s -> Left (initLedger', s)
                                                                Right v -> Right v

    runExceptT (consumeStream (mkStream doCheck GetBlock db) (castPoint . getTip $ initLedger') (ldb, 0, 0) (push configLedgerDb f bs))
  where
    configLedgerDb = LedgerDbCfg {
        ledgerDbCfgSecParam = configSecurityParam cfg
      , ledgerDbCfg         = ExtLedgerCfg cfg
      }

    f blk _ newLedger = when (unBlockNo (blockNo blk) `mod` nBlocks == 0 ) $ checkNoThunks (blockNo blk) newLedger

    checkNoThunks :: BlockNo -> ExtLedgerState blk EmptyMK -> IO ()
    checkNoThunks bn ls =
      noThunks [] (ledgerState ls) >>= \case
        Nothing -> putStrLn $ "BlockNo " <> show bn <> ": no thunks found."
        Just ti -> do
          putStrLn $ "BlockNo " <> show bn <> ": thunks found."
          print ti

{-------------------------------------------------------------------------------
  Analysis: maintain a ledger state and issue trace markers at appropriate
  points in the epoch
-------------------------------------------------------------------------------}

traceLedgerProcessing ::
  forall blk.
  HasAnalysis blk =>
  Analysis blk
traceLedgerProcessing
  AnalysisEnv {db, initLedger, cfg, limit, backing, ledgerDbFS} = void $ do
    doCheck <- onlyCheckNumBlocks limit

    initLedger' <- initialiseLedger ledgerDbFS cfg initLedger
    (ldb, bs) <- ledgerDbAndBackingStore backing ledgerDbFS $ case initLedger of
                                                                Left s -> Left (initLedger', s)
                                                                Right v -> Right v

    runExceptT (consumeStream (mkStream doCheck GetBlock db) (castPoint . getTip $ initLedger') (ldb, 0, 0) (push configLedgerDb f bs))
  where
    configLedgerDb = LedgerDbCfg {
        ledgerDbCfgSecParam = configSecurityParam cfg
      , ledgerDbCfg         = ExtLedgerCfg cfg
      }

    f blk oldLedger newLedger = do
      let traces = HasAnalysis.emitTraces $
                   HasAnalysis.WithLedgerState blk (ledgerState oldLedger) (ledgerState newLedger)
      mapM_ Debug.traceMarkerIO traces

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the DB
-------------------------------------------------------------------------------}

-- | Perform a processing that just requires a stream of blocks and not a
-- LedgerDB
doProcess :: HasAnalysis blk
          => AnalysisEnv m blk
          -> st -- ^ Initial state
          -> (a -> st -> IO st) -- ^ Transition function
          -> BlockComponent blk a
          -> IO st
doProcess AnalysisEnv { db, initLedger, limit, ledgerDbFS, cfg } st process bc = do
  doCheck <- onlyCheckNumBlocks limit
  initLedger' <- initialiseLedger ledgerDbFS cfg initLedger
  either (error . show) id
      <$> runExceptT (consumeStream (mkStream doCheck bc db) (castPoint . getTip $ initLedger') st process)

-- | Load an EmptyMK ledger state either from a snapshot or from the genesis
-- ledger state.
initialiseLedger ::
  ( LedgerSupportsProtocol blk
  , Serialise (HeaderHash blk)
  , HasAnalysis blk
  )
  => SomeHasFS IO
  -> TopLevelConfig blk
  -> Either DiskSnapshot (ExtLedgerState blk ValuesMK)
  -> IO (ExtLedgerState blk EmptyMK)
initialiseLedger fs cfg e = do
  r <- either
    (\s -> either (error . show) id
           <$> runExceptT (readSnapshot fs (decodeExtLedgerState' cfg) decode s))
    (return . forgetLedgerTables) e
  -- This marker divides the "loading" phase of the program, where the
  -- system is principally occupied with reading snapshot data from
  -- disk, from the "processing" phase, where we are streaming blocks
  -- and running the ledger processing on them.
  Debug.traceMarkerIO "SNAPSHOT_LOADED"
  pure r

-- | Uniformly get a initial ledger DB and a Backing store either from genesis
-- or from a snapshot.
ledgerDbAndBackingStore ::
     ( LedgerSupportsProtocol blk
     , SufficientSerializationForAnyBackingStore (LedgerState blk)
     )
  => BackingStoreSelector IO
  -> SomeHasFS IO
  -> Either (ExtLedgerState blk EmptyMK, DiskSnapshot) (ExtLedgerState blk ValuesMK)
  -> IO (LedgerDB' blk, LedgerBackingStore IO (ExtLedgerState blk))
ledgerDbAndBackingStore bss fs  = \case
  Left (initLedger', s) -> do
        backingStore <- restoreBackingStore fs s bss
        return (ledgerDbWithAnchor RunOnlyNew initLedger'
               , backingStore)
  Right genesisLedger -> do
        backingStore <- newBackingStore mempty bss fs (projectLedgerTables genesisLedger)
        return (ledgerDbWithAnchor RunOnlyNew (forgetLedgerTables genesisLedger)
               , backingStore)

decodeExtLedgerState' :: forall s blk .
     HasAnalysis blk
  => TopLevelConfig blk
  -> Decoder s (ExtLedgerState blk EmptyMK)
decodeExtLedgerState' cfg =
  let ccfg = configCodec cfg
  in decodeExtLedgerState
           (decodeDisk ccfg)
           (decodeDisk ccfg)
           (decodeDisk ccfg)

-- | This is quite similar to Ou*.OnDisk.replayStartingWith.push but there we
-- cannot inject the check we are doing here with the callback. Also it performs
-- @ApplyVal@ instead of @ReapplyVal@.
push :: forall blk. LedgerSupportsProtocol blk
     => LedgerDbCfg (ExtLedgerState blk)
     -> (   blk
         -> ExtLedgerState blk EmptyMK
         -> ExtLedgerState blk EmptyMK
         -> IO ()
        ) -- ^ A callback that will be executed on every call to push with the
          -- block, old ledger state and new ledger state
     -> LedgerBackingStore IO (ExtLedgerState blk)
     -> blk
     -> (LedgerDB' blk, Word64, Word64)
     -> IO (LedgerDB' blk, Word64, Word64)
push configLedgerDb callback backingStore blk (!ldb, !replayed, !sinceLast) = do
        !edb' <-
            defaultReadKeySets (readKeySets backingStore) $
                  runExceptT $ ledgerDbPush configLedgerDb (ApplyVal blk) ldb
        let _ = edb' :: Either (AnnLedgerError (ExtLedgerState blk) blk) (LedgerDB' blk)
            db' = case edb' of
              Left e -> error $ "Applying block " <> show (blockHash blk) <> " failed with " <> show (annLedgerErr e)
              Right ld -> ld

        callback blk (ledgerDbCurrent ldb) (ledgerDbCurrent db')
        -- TODO flush policy: flush less often?

        -- It's OK to flush without a lock here, since the `LgrDB` has not
        -- finishined initializing: only this thread has access to the backing
        -- store.
        (db'', sinceLast') <-
          if sinceLast == 100
          then do
            let (toFlush, toKeep) =
                  ledgerDbFlush DbChangelogFlushAllImmutable db'
            OnDisk.flush backingStore toFlush
            pure (toKeep, 0)
          else pure (db', sinceLast + 1)

        -- TODO snapshot policy: create snapshots during replay?

        let replayed' :: Word64
            !replayed' = replayed + 1

        return (db'', replayed', sinceLast')

-- | A check that just counts how many blocks have been applied.
onlyCheckNumBlocks :: Maybe Int -> IO (b -> IO (NextBlock b))
onlyCheckNumBlocks = \case
    Nothing -> return $ return . NextBlock
    Just l -> do
      countIORef <- newIORef 0
      return $ \b -> do
        c <- readIORef countIORef
        modifyIORef countIORef (+ 1)
        if c >= l
          then return (NextBlock b)
          else return NoMoreBlocks

consumeStream ::
     StreamAPI IO blk a
  -> Point blk -- ^ the starting point
  -> st -- ^ initial state
  -> (a -> st -> IO st) -- ^ transition function
  -> ExceptT (InitFailure blk) IO st
consumeStream s point = streamAll s point InitFailureTooRecent

mkStream ::
     HasHeader blk
  => (a -> IO (NextBlock a)) -- ^ short circuiting monadic condition
  -> BlockComponent blk a
  -> Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> StreamAPI IO blk a
mkStream c bc = either (streamAPI' c bc) (chainDBStreamAPI' c bc)

-- | A StreamAPI usable by 'streamAll' but traversing the whole ChainDB.
--
-- Pretty much similar to `LgrDB.streamAPI`.
chainDBStreamAPI' ::
     forall blk a.
     (HasHeader blk)
  => (a -> IO (NextBlock a))
  -> BlockComponent blk a
  -> ChainDB IO blk
  -> StreamAPI IO blk a
chainDBStreamAPI' shouldStop blockComponent chainDB = StreamAPI streamAfter
  where
    streamAfter :: Point blk
                -> (Either (RealPoint blk) (IO (NextBlock a)) -> IO b)
                -> IO b
    streamAfter tip k = withRegistry $ \registry -> do
        itr <- ChainDB.streamFrom
                 (StreamFromExclusive tip)
                 chainDB
                 registry
                 blockComponent
        k $ Right $ streamUsing itr

    streamUsing :: ChainDB.Iterator IO blk a
                -> IO (NextBlock a)
    streamUsing itr = do
        itrResult <- ChainDB.iteratorNext itr
        case itrResult of
          ChainDB.IteratorExhausted    -> return NoMoreBlocks
          ChainDB.IteratorResult b     -> shouldStop b
          ChainDB.IteratorBlockGCed point -> error $ "block GC'ed " <> show point
