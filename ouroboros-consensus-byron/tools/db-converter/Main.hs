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

{-# OPTIONS_GHC -Wno-orphans -Wno-partial-fields #-}
{-
  Database conversion tool.
-}
module Main (main) where

import           Control.Exception (Exception, throwIO)
import           Control.Monad.Except (liftIO, runExceptT)
import           Control.Monad.Trans.Resource (runResourceT)
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
import           Path
import           Path.IO (createDirIfMissing, listDir)
import qualified Streaming.Prelude as S
import           System.Directory (canonicalizePath)
import qualified System.IO as IO

import           Control.Tracer (contramap, debugTracer, nullTracer)

import qualified Cardano.Binary as CB
import qualified Cardano.Chain.Epoch.File as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Chain.Update as CC.Update
import           Cardano.Crypto (Hash, RequiresNetworkMagic (..),
                     decodeAbstractHash)
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.Node as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike (atomically)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (fromChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Node

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
    { epochDir             :: w ::: FilePath      <?> "Path to the directory containing old epoch files"
    , dbDir                :: w ::: FilePath      <?> "Path to the new database directory"
    , epochSlots           :: w ::: Word64        <?> "Slots per epoch"
    }
  | Validate
    { dbDir                :: w ::: FilePath      <?> "Path to the new database directory"
    , configFile           :: w ::: FilePath      <?> "Configuration file (e.g. mainnet-genesis.json)"
    , systemStart          :: w ::: Maybe UTCTime <?> "System start time"
    , requiresNetworkMagic :: w ::: Bool          <?> "Expecto patronum?"
    , genesisHash          :: w ::: Hash CB.Raw   <?> "Expected genesis hash"
    , verbose              :: w ::: Bool          <?> "Enable verbose logging"
    , onlyImmDB            :: w ::: Bool          <?> "Validate only the immutable DB (e.g. do not do ledger validation)"
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
        convertChunkFile (EpochSlots epochSlots) f dbDir'
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

convertChunkFile
  :: EpochSlots
  -> Path Abs File -- ^ Input
  -> Path Abs Dir -- ^ Ouput directory
  -> IO (Either CC.ParseError ())
convertChunkFile es inFile outDir = do
    createDirIfMissing True dbDir
    -- Old filename format is XXXXX.epoch, new is XXXXX.chunk
    outFileName <- parseRelFile (toFilePath (filename inFile))
    outFile <- (dbDir </> outFileName) -<.> "chunk"
    IO.withFile (toFilePath outFile) IO.WriteMode $ \h ->
      runResourceT $ runExceptT $ S.mapM_ (liftIO . BS.hPut h) . S.map encode $ inStream
  where
    inStream = CC.parseEpochFileWithBoundary es (toFilePath inFile)
    dbDir = outDir </> [reldir|immutable|]
    encode =
        CB.serializeEncoding'
      . Byron.encodeByronBlock
      . Byron.mkByronBlock es

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
        (nodeStartTime (Proxy @ByronBlock) cfg)
        (focusSlotLengths $ singletonSlotLengths slotLength)
      let chainDbArgs = mkChainDbArgs registry btime
          (immDbArgs, _, _, _) = fromChainDbArgs chainDbArgs
      if onlyImmDB then
        ImmDB.withImmDB immDbArgs $ \immDB -> do
          immDbTipPoint <- ImmDB.getPointAtTip immDB
          putStrLn $ "DB tip: " ++ condense immDbTipPoint
      else
        ChainDB.withDB chainDbArgs $ \chainDB -> do
          chainDbTipPoint <- atomically $ ChainDB.getTipPoint chainDB
          putStrLn $ "DB tip: " ++ condense chainDbTipPoint
  where
    -- This converts the old chain, which has a 20s slot length.
    slotLength = slotLengthFromSec 20
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
    chunkInfo  = simpleChunkInfo . EpochSize . unEpochSlots $ epochSlots

    mkChainDbArgs registry btime =
      let args = Node.mkChainDbArgs tracer registry btime
            (toFilePath dbDir) cfg initLedger chunkInfo
      in args {
          ChainDB.cdbImmValidation = ImmDB.ValidateAllChunks
        }
