{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-
  Database conversion tool.
-}
module Main where

import qualified Cardano.Binary as CB
import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Epoch.File as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Chain.Update as CC.Update
import           Cardano.Crypto (Hash, RequiresNetworkMagic (..),
                     decodeAbstractHash, getAProtocolMagicId)
import           Control.Exception (Exception, bracket, throwIO)
import           Control.Monad.Except (liftIO, runExceptT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Tracer (contramap, debugTracer, nullTracer)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.List (sort)
import           Data.Reflection (give)
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import qualified Options.Applicative as Options
import           Options.Generic
import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB)
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import           Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig (..))
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract (pInfoConfig,
                     pInfoInitLedger)
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import qualified Ouroboros.Storage.ChainDB as ChainDB
import qualified Ouroboros.Storage.ChainDB.Impl.Args as Args
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Storage.Common (EpochSize (..))
import           Ouroboros.Storage.EpochInfo (fixedSizeEpochInfo)
import qualified Ouroboros.Storage.LedgerDB.DiskPolicy as LgrDB
import qualified Ouroboros.Storage.LedgerDB.InMemory as LgrDB
import           Path
import           Path.IO (createDirIfMissing, listDir)
import qualified Streaming.Prelude as S
import           System.Directory (canonicalizePath)
import qualified System.IO as IO

instance ParseField UTCTime

instance ParseFields UTCTime

instance ParseRecord UTCTime where

  parseRecord = fmap getOnly parseRecord

instance ParseField (Hash CB.Raw) where

  readField = Options.eitherReader (first Text.unpack . decodeAbstractHash . Text.pack)

instance ParseFields (Hash CB.Raw)

instance ParseRecord (Hash CB.Raw) where

  parseRecord = fmap getOnly parseRecord

data Args w
  = Convert
      { epochDir :: w ::: FilePath <?> "Path to the directory containing old epoch files"
      , dbDir :: w ::: FilePath <?> "Path to the new database directory"
      , epochSlots :: w ::: Word64 <?> "Slots per epoch"
      }
  | Validate
      { dbDir :: w ::: FilePath <?> "Path to the new database directory"
      , configFile :: w ::: FilePath <?> "Configuration file (e.g. mainnet-genesis.json)"
      , systemStart :: w ::: Maybe UTCTime <?> "System start time"
      , requiresNetworkMagic :: w ::: Bool <?> "Expecto patronum?"
      , genesisHash :: w ::: Hash CB.Raw <?> "Expected genesis hash"
      , verbose :: w ::: Bool <?> "Enable verbose logging"
      , onlyImmDB :: w ::: Bool <?> "Validate only the immutable DB (e.g. do not do ledger validation)"
      }
  deriving (Generic)

instance ParseRecord (Args Wrapped)

deriving instance Show (Args Unwrapped)

data ValidationError
  = MkConfigError CC.Genesis.ConfigurationError
  deriving (Show, Typeable)

instance Exception ValidationError

main :: IO ()
main = do
  (cmd :: Args Unwrapped) <- unwrapRecord "Byron DB converter"
  case cmd of
    Convert {epochDir, dbDir, epochSlots} -> do
      dbDir' <- parseAbsDir =<< canonicalizePath dbDir
      (_, files) <- listDir =<< parseAbsDir =<< canonicalizePath epochDir
      let epochFiles = filter (\f -> fileExtension f == ".epoch") files
      putStrLn $ "Writing to " ++ show dbDir
      for_ (sort epochFiles) $ \f -> do
        putStrLn $ "Converting file " ++ show f
        convertEpochFile (EpochSlots epochSlots) f dbDir'
    Validate
      { dbDir
      , configFile
      , requiresNetworkMagic
      , genesisHash
      , verbose
      , onlyImmDB
      } -> do
        dbDir' <- parseAbsDir =<< canonicalizePath dbDir
        econfig <-
          runExceptT $
            CC.Genesis.mkConfigFromFile
              (if requiresNetworkMagic then RequiresMagic else RequiresNoMagic)
              configFile
              genesisHash
        case econfig of
          Left err     -> throwIO $ MkConfigError err
          Right config -> validateChainDb dbDir' config onlyImmDB verbose

convertEpochFile
  :: EpochSlots
  -> Path Abs File -- ^ Input
  -> Path Abs Dir -- ^ Ouput directory
  -> IO (Either CC.ParseError ())
convertEpochFile es inFile outDir =
  let inStream = CC.parseEpochFileWithBoundary es (toFilePath inFile)
      dbDir = outDir </> [reldir|immutable|]
      encode = CB.serializeEncoding' . Byron.encodeByronBlock . Byron.ByronBlockOrEBB
   in do
        createDirIfMissing True dbDir
        -- Old filename format is XXXXX.dat, new is epoch-XXX.dat
        outFileName <-
          parseRelFile $ "epoch-" <>
            drop 2 (toFilePath (filename inFile))
        outFile <- (dbDir </> outFileName) -<.> "dat"
        IO.withFile (toFilePath outFile) IO.WriteMode $ \h ->
          runResourceT $ runExceptT $ S.mapM_ (liftIO . BS.hPut h) . S.map encode $ inStream

validateChainDb
  :: forall blk. (blk ~ ByronBlockOrEBB ByronConfig)
  => Path Abs Dir -- ^ DB directory
  -> CC.Genesis.Config
  -> Bool -- Immutable DB only?
  -> Bool -- Verbose
  -> IO ()
validateChainDb dbDir cfg onlyImmDB verbose =
  withRegistry $ \registry -> give protocolMagicId $ give epochSlots $
    if onlyImmDB
    then
      let (immDBArgs, _, _, _) = Args.fromChainDbArgs $ args registry
      in bracket
        (ImmDB.openDB immDBArgs)
        ImmDB.closeDB
        (\immdb -> do
          immDbTipBlock <- ImmDB.getBlockAtTip immdb
          putStrLn $ "DB tip: " ++ condense immDbTipBlock
        )
    else bracket
      (ChainDB.openDB $ args registry)
        ChainDB.closeDB
      (\chaindb -> do
        blk <- ChainDB.getTipBlock chaindb
        putStrLn $ "DB tip: " ++ condense blk
      )
  where
    byronProtocolInfo =
      protocolInfoByron
        cfg
        (Just $ PBftSignatureThreshold 0.22) -- PBFT signature threshold
        (CC.Update.ProtocolVersion 1 0 0)
        ( CC.Update.SoftwareVersion
          (CC.Update.ApplicationName "Cardano SL")
          2
        )
        Nothing
    protocolMagicId = CB.unAnnotated . getAProtocolMagicId $ CC.Genesis.configProtocolMagic cfg
    epochSlots = CC.Genesis.configEpochSlots cfg
    securityParam = SecurityParam $ CC.unBlockCount k
    k = CC.Genesis.configK cfg
    args registry =
      (ChainDB.defaultArgs @blk (toFilePath dbDir))
        { ChainDB.cdbGenesis = return $ pInfoInitLedger byronProtocolInfo
        , ChainDB.cdbDecodeBlock = Byron.decodeByronBlock epochSlots
        , ChainDB.cdbDecodeChainState = Byron.decodeByronChainState
        , ChainDB.cdbDecodeHash = Byron.decodeByronHeaderHash
        , ChainDB.cdbDecodeLedger = Byron.decodeByronLedgerState
        , ChainDB.cdbEncodeBlock = Byron.encodeByronBlock
        , ChainDB.cdbEncodeChainState = Byron.encodeByronChainState
        , ChainDB.cdbEncodeHash = Byron.encodeByronHeaderHash
        , ChainDB.cdbEncodeLedger = Byron.encodeByronLedgerState
          -- Policy
        , ChainDB.cdbValidation = ImmDB.ValidateAllEpochs
        , ChainDB.cdbBlocksPerFile = 10
        , ChainDB.cdbParamsLgrDB = LgrDB.ledgerDbDefaultParams securityParam
        , ChainDB.cdbDiskPolicy = LgrDB.defaultDiskPolicy securityParam 20000
          -- Integration
        , ChainDB.cdbNodeConfig = pInfoConfig byronProtocolInfo
        , ChainDB.cdbEpochInfo = fixedSizeEpochInfo . EpochSize . unEpochSlots $ epochSlots
        , ChainDB.cdbIsEBB = \blk -> case Byron.unByronBlockOrEBB blk of
          CC.ABOBBlock _      -> Nothing
          CC.ABOBBoundary ebb -> Just (Byron.ByronHash (CC.boundaryHashAnnotated ebb))
          -- Misc
        , ChainDB.cdbTracer = if verbose
            then
              contramap
                ( give protocolMagicId $
                  give epochSlots $
                  show
                )
                debugTracer
            else nullTracer
        , ChainDB.cdbRegistry = registry
        , ChainDB.cdbGcDelay = 0
        }
