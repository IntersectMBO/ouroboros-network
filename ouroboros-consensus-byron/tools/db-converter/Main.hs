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

-- | Database conversion tool.
module Main (main) where

import           Control.Monad.Except (liftIO, runExceptT)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.List (sort)
import           Data.Word (Word64)
import           Options.Generic
import           Path
import           Path.IO (createDirIfMissing, listDir)
import qualified Streaming.Prelude as S
import           System.Directory (canonicalizePath)
import qualified System.IO as IO

import qualified Cardano.Binary as CB
import qualified Cardano.Chain.Epoch.File as CC
import           Cardano.Chain.Slotting (EpochSlots (..))

import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Consensus.Byron.Ledger as Byron

data Args w = Args
    { epochDir   :: w ::: FilePath <?> "Path to the directory containing old epoch files"
    , dbDir      :: w ::: FilePath <?> "Path to the new database directory"
    , epochSlots :: w ::: Word64   <?> "Slots per epoch"
    }
  deriving (Generic)

instance ParseRecord (Args Wrapped)

deriving instance Show (Args Unwrapped)

main :: IO ()
main = do
    Args {epochDir, dbDir, epochSlots} <- unwrapRecord "Byron DB converter"
    dbDir' <- parseAbsDir =<< canonicalizePath dbDir
    (_, files) <- listDir =<< parseAbsDir =<< canonicalizePath epochDir
    let epochFiles = filter (\f -> fileExtension f == ".epoch") files
    putStrLn $ "Writing to " ++ show dbDir
    for_ (sort epochFiles) $ \f -> do
      putStrLn $ "Converting file " ++ show f
      convertChunkFile (EpochSlots epochSlots) f dbDir'

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
