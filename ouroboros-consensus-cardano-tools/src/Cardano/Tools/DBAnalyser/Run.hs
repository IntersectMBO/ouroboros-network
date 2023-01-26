{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBAnalyser.Run (analyse) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.Serialise (Serialise (decode))
import           Control.Monad.Except (runExceptT)
import qualified Debug.Trace as Debug
import           System.IO

import           Control.Tracer (Tracer (..), nullTracer)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
                     (HasTxs)
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (fromChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (SnapshotInterval (..), defaultDiskPolicy)
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (readSnapshot)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

import           Cardano.Tools.DBAnalyser.Analysis
import           Cardano.Tools.DBAnalyser.HasAnalysis
import           Cardano.Tools.DBAnalyser.Types


{-------------------------------------------------------------------------------
  Analyse
-------------------------------------------------------------------------------}

analyse ::
     forall blk .
     ( Node.RunNode blk
     , Show (Header blk)
     , HasAnalysis blk
     , HasProtocolInfo blk
     , LedgerSupportsMempool.HasTxs blk
     )
  => DBAnalyserConfig
  -> Args blk
  -> IO (Maybe AnalysisResult)
analyse DBAnalyserConfig{analysis, confLimit, dbDir, selectDB, validation, verbose} args =
    withRegistry $ \registry -> do

      chainDBTracer  <- mkTracer verbose
      analysisTracer <- mkTracer True
      ProtocolInfo { pInfoInitLedger = genesisLedger, pInfoConfig = cfg } <-
        mkProtocolInfo args
      let chunkInfo  = Node.nodeImmutableDbChunkInfo (configStorage cfg)
          k          = configSecurityParam cfg
          diskPolicy = defaultDiskPolicy k DefaultSnapshotInterval
          args' =
            Node.mkChainDbArgs
              registry InFuture.dontCheck cfg genesisLedger chunkInfo $
            ChainDB.defaultArgs (Node.stdMkChainDbHasFS dbDir) diskPolicy
          chainDbArgs = args' {
              ChainDB.cdbImmutableDbValidation = immValidationPolicy
            , ChainDB.cdbVolatileDbValidation  = volValidationPolicy
            , ChainDB.cdbTracer                = chainDBTracer
            }
          (immutableDbArgs, _, _, _) = fromChainDbArgs chainDbArgs
          ledgerDbFS = ChainDB.cdbHasFSLgrDB chainDbArgs

      case selectDB of
        SelectImmutableDB initializeFrom -> do
          -- TODO we need to check if the snapshot exists. If not, print an
          -- error and ask the user if she wanted to create a snapshot first and
          -- how to do it.
          initLedgerErr <- runExceptT $ case initializeFrom of
            Nothing       -> pure genesisLedger
            Just snapshot -> readSnapshot ledgerDbFS (decodeExtLedgerState' cfg) decode snapshot
              -- TODO @readSnapshot@ has type @ExceptT ReadIncrementalErr m
              -- (ExtLedgerState blk)@ but it also throws exceptions! This makes
              -- error handling more challenging than it ought to be. Maybe we
              -- can enrich the error that @readSnapthot@ return, so that it can
              -- contain the @HasFS@ errors as well.
          initLedger <- either (error . show) pure initLedgerErr
          -- This marker divides the "loading" phase of the program, where the
          -- system is principally occupied with reading snapshot data from
          -- disk, from the "processing" phase, where we are streaming blocks
          -- and running the ledger processing on them.
          Debug.traceMarkerIO "SNAPSHOT_LOADED"
          ImmutableDB.withDB (ImmutableDB.openDB immutableDbArgs runWithTempRegistry) $ \immutableDB -> do
            result <- runAnalysis analysis $ AnalysisEnv {
                cfg
              , initLedger
              , db = Left immutableDB
              , registry
              , ledgerDbFS = ledgerDbFS
              , limit = confLimit
              , tracer = analysisTracer
              }
            tipPoint <- atomically $ ImmutableDB.getTipPoint immutableDB
            putStrLn $ "ImmutableDB tip: " ++ show tipPoint
            pure result
        SelectChainDB ->
          ChainDB.withDB chainDbArgs $ \chainDB -> do
            result <- runAnalysis analysis $ AnalysisEnv {
                cfg
              , initLedger = genesisLedger
              , db = Right chainDB
              , registry
              , ledgerDbFS = ledgerDbFS
              , limit = confLimit
              , tracer = analysisTracer
              }
            tipPoint <- atomically $ ChainDB.getTipPoint chainDB
            putStrLn $ "ChainDB tip: " ++ show tipPoint
            pure result
  where
    mkTracer False = return nullTracer
    mkTracer True  = do
      startTime <- getMonotonicTime
      return $ Tracer $ \ev -> do
        traceTime <- getMonotonicTime
        let diff = diffTime traceTime startTime
        hPutStrLn stderr $ concat ["[", show diff, "] ", show ev]
        hFlush stderr

    immValidationPolicy = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> ImmutableDB.ValidateAllChunks
      (_, Just MinimumBlockValidation) -> ImmutableDB.ValidateMostRecentChunk
      (OnlyValidation, _ )             -> ImmutableDB.ValidateAllChunks
      _                                -> ImmutableDB.ValidateMostRecentChunk

    volValidationPolicy = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> VolatileDB.ValidateAll
      (_, Just MinimumBlockValidation) -> VolatileDB.NoValidation
      (OnlyValidation, _ )             -> VolatileDB.ValidateAll
      _                                -> VolatileDB.NoValidation

    decodeExtLedgerState' :: forall s . TopLevelConfig blk -> Decoder s (ExtLedgerState blk Canonical)
    decodeExtLedgerState' cfg =
      let ccfg = configCodec cfg
      in decodeExtLedgerState
           (decodeDisk ccfg)
           (decodeDisk ccfg)
           (decodeDisk ccfg)
