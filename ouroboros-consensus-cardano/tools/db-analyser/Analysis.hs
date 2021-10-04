{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Word (Word16)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract (tickThenApplyLedgerResult)
import           Ouroboros.Consensus.Ledger.Basics (LedgerResult (..))
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol (..))
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
  deriving Show

runAnalysis ::
     forall blk .
     ( LgrDbSerialiseConstraints blk
     , HasAnalysis blk
     , LedgerSupportsProtocol blk
     )
  => AnalysisName -> Analysis blk
runAnalysis ShowSlotBlockNo             = showSlotBlockNo
runAnalysis CountTxOutputs              = countTxOutputs
runAnalysis ShowBlockHeaderSize         = showHeaderSize
runAnalysis ShowBlockTxsSize            = showBlockTxsSize
runAnalysis ShowEBBs                    = showEBBs
runAnalysis OnlyValidation              = \_ -> return ()
runAnalysis (StoreLedgerStateAt slotNo) = storeLedgerStateAt slotNo

type Analysis blk = AnalysisEnv blk -> IO ()

data AnalysisEnv blk = AnalysisEnv {
      cfg        :: TopLevelConfig blk
    , initLedger :: ExtLedgerState blk
    , db         :: Either (ImmutableDB IO blk) (ChainDB IO blk)
    , registry   :: ResourceRegistry IO
    , ledgerDbFS :: SomeHasFS IO
    , limit      :: Limit
    }

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasAnalysis blk => Analysis blk
showSlotBlockNo AnalysisEnv { db, registry, limit } =
    processAll_ db registry GetHeader limit process
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
countTxOutputs AnalysisEnv { db, registry, limit } = do
    void $ processAll db registry GetBlock limit 0 process
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
showHeaderSize AnalysisEnv { db, registry, limit } = do
    maxHeaderSize <-
      processAll db registry ((,) <$> GetSlot <*> GetHeaderSize) limit 0 process
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
showBlockTxsSize AnalysisEnv { db, registry, limit } =
    processAll_ db registry GetBlock limit process
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
showEBBs AnalysisEnv { db, registry, limit } = do
    putStrLn "EBB\tPrev\tKnown"
    processAll_ db registry GetBlock limit process
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
  Analysis: store a ledger at specific slot
-------------------------------------------------------------------------------}

storeLedgerStateAt ::
     forall blk .
     ( LgrDbSerialiseConstraints blk
     , HasAnalysis blk
     , LedgerSupportsProtocol blk
     )
  => SlotNo -> Analysis blk
storeLedgerStateAt slotNo (AnalysisEnv { db, registry, initLedger, cfg, limit, ledgerDbFS }) = do
    putStrLn $ "About to store snapshot of a ledger at " <>
               show slotNo <> " " <>
               "this might take a while..."
    void $ processAllUntil db registry GetBlock limit initLedger process
  where
    process :: ExtLedgerState blk -> blk -> IO (NextStep, ExtLedgerState blk)
    process oldLedger blk = do
      let ledgerCfg     = ExtLedgerCfg cfg
          appliedResult = tickThenApplyLedgerResult ledgerCfg blk oldLedger
          newLedger     = either (error . show) lrResult $ runExcept $ appliedResult
      when (blockSlot blk == slotNo) $ storeLedgerState blk newLedger
      return (continue blk, newLedger)

    continue :: blk -> NextStep
    continue blk
      | blockSlot blk >= slotNo = Stop
      | otherwise               = Continue

    storeLedgerState ::
         blk
      -> ExtLedgerState blk
      -> IO ()
    storeLedgerState blk ledgerState = do
      let snapshot = DiskSnapshot
                      (unSlotNo $ blockSlot blk)
                      (Just $ "db-analyser")
      writeSnapshot ledgerDbFS encLedger snapshot ledgerState
      putStrLn $ "storing state at " <> intercalate "\t" [
          show (blockNo   blk)
        , show (blockSlot blk)
        , show (blockHash blk)
        ]

    encLedger :: ExtLedgerState blk -> Encoding
    encLedger =
      let ccfg = configCodec cfg
      in encodeExtLedgerState
           (encodeDisk ccfg)
           (encodeDisk ccfg)
           (encodeDisk ccfg)

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
     forall blk b st. HasHeader blk
  => Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> Limit
  -> st
  -> (st -> b -> IO (NextStep, st))
  -> IO st
processAllUntil = either processAllImmutableDB processAllChainDB

processAll ::
     forall blk b st. HasHeader blk
  => Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> Limit
  -> st
  -> (st -> b -> IO st)
  -> IO st
processAll db rr blockComponent limit initSt cb =
  processAllUntil db rr blockComponent limit initSt callback
    where
      callback st b = (Continue, ) <$> cb st b

processAll_ ::
     forall blk b. HasHeader blk
  => Either (ImmutableDB IO blk) (ChainDB IO blk)
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> Limit
  -> (b -> IO ())
  -> IO ()
processAll_ db rr blockComponent limit callback =
    processAll db rr blockComponent limit () (const callback)

processAllChainDB ::
     forall st blk b. HasHeader blk
  => ChainDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> Limit
  -> st
  -> (st -> b -> IO (NextStep, st))
  -> IO st
processAllChainDB chainDB rr blockComponent limit initState callback = do
    itr <- ChainDB.streamAll chainDB rr blockComponent
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
     forall st blk b. HasHeader blk
  => ImmutableDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> Limit
  -> st
  -> (st -> b -> IO (NextStep, st))
  -> IO st
processAllImmutableDB immutableDB rr blockComponent limit initState callback = do
    itr <- ImmutableDB.streamAll immutableDB rr blockComponent
    go itr limit initState
  where
    go :: ImmutableDB.Iterator IO blk b -> Limit -> st -> IO st
    go itr lt !st = case decreaseLimit lt of
      Nothing               -> return st
      Just decreasedLimit -> do
        itrResult <- ImmutableDB.iteratorNext itr
        case itrResult of
          ImmutableDB.IteratorExhausted -> return st
          ImmutableDB.IteratorResult b  -> callback st b >>= \case
            (Continue, nst) -> go itr decreasedLimit nst
            (Stop, nst)     -> return nst
