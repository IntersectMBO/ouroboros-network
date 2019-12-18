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
import qualified Cardano.Chain.Epoch.File as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Chain.Update as CC.Update
import           Cardano.Crypto (Hash, RequiresNetworkMagic (..),
                     decodeAbstractHash)
import           Control.Exception (Exception, bracket, throwIO)
import           Control.Monad.Except (liftIO, runExceptT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Tracer (contramap, debugTracer, nullTracer)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.List (sort)
import           Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import qualified Options.Applicative as Options
import           Options.Generic
import           Ouroboros.Consensus.BlockchainTime (realBlockchainTime,
                     slotLengthFromMillisec)
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock)
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import qualified Ouroboros.Consensus.Node as Node
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
                     (ProtocolInfo (..))
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike (atomically)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.ChainDB.Impl.Args (fromChainDbArgs)
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Storage.Common (EpochSize (..))
import           Ouroboros.Storage.EpochInfo (fixedSizeEpochInfo)
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
      encode = CB.serializeEncoding' . Byron.encodeByronBlock . Byron.mkByronBlock es
   in do
        createDirIfMissing True dbDir
        -- Old filename format is XXXXX.dat, new is XXXXX.epoch
        outFileName <- parseRelFile (toFilePath (filename inFile))
        outFile <- (dbDir </> outFileName) -<.> "epoch"
        IO.withFile (toFilePath outFile) IO.WriteMode $ \h ->
          runResourceT $ runExceptT $ S.mapM_ (liftIO . BS.hPut h) . S.map encode $ inStream

validateChainDb
  :: Path Abs Dir -- ^ DB directory
  -> CC.Genesis.Config
  -> Bool -- Immutable DB only?
  -> Bool -- Verbose
  -> IO ()
validateChainDb dbDir genesisConfig onlyImmDB verbose =
    withRegistry $ \registry -> do
      btime <- realBlockchainTime
        registry
        nullTracer
        slotLength
        (nodeStartTime (Proxy @ByronBlock) cfg)
      let chainDbArgs = mkChainDbArgs registry btime

      if onlyImmDB then
        bracket
          (let (immDbArgs, _, _, _) = fromChainDbArgs chainDbArgs
           in ImmDB.openDB immDbArgs)
          ImmDB.closeDB
          (\immdb -> do
            immDbTipPoint <- ImmDB.getPointAtTip immdb
            putStrLn $ "DB tip: " ++ condense immDbTipPoint
          )
      else
        bracket
          (ChainDB.openDB chainDbArgs)
          ChainDB.closeDB
          (\chaindb -> do
            chainDbTipPoint <- atomically $ ChainDB.getTipPoint chaindb
            putStrLn $ "DB tip: " ++ condense chainDbTipPoint
          )
  where
    slotLength = slotLengthFromMillisec (20 * 1000)
    ProtocolInfo { pInfoInitLedger = initLedger, pInfoConfig = cfg } =
      protocolInfoByron
        genesisConfig
        (Just $ PBftSignatureThreshold 0.22) -- PBFT signature threshold
        (CC.Update.ProtocolVersion 1 0 0)
        (CC.Update.SoftwareVersion (CC.Update.ApplicationName "Cardano SL") 2)
        Nothing

    tracer
      | verbose   = contramap show debugTracer
      | otherwise = nullTracer

    epochSlots = CC.Genesis.configEpochSlots genesisConfig
    epochInfo = fixedSizeEpochInfo . EpochSize . unEpochSlots $ epochSlots

    mkChainDbArgs registry btime =
      let args = Node.mkChainDbArgs tracer registry btime
            (toFilePath dbDir) cfg initLedger slotLength epochInfo
      in args {
          ChainDB.cdbValidation = ImmDB.ValidateAllEpochs
        }
