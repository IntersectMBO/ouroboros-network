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
import qualified Data.Text as T
import           Data.Word (Word32, Word64)
import           Options.Generic
import           Path
import           Path.IO (createDirIfMissing, listDir)
import qualified Streaming.Prelude as S
import           System.Directory (canonicalizePath)
import qualified System.IO as IO

import qualified Cardano.Binary as CB
import qualified Cardano.Chain.Epoch.File as CC
import           Cardano.Chain.Slotting (EpochSlots (..))

import           Ouroboros.Network.Magic

import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Consensus.Byron.Ledger as Byron

data Args w = Args
    { epochDir        :: w ::: FilePath     <?> "Path to the directory containing old epoch files"
    , dbDir           :: w ::: FilePath     <?> "Path to the new database directory"
    , epochSlots      :: w ::: Word64       <?> "Slots per epoch"
    , networkMagic    :: w ::: Maybe Word32 <?> "Magic Id of the network"
    }
  deriving (Generic)

instance ParseRecord (Args Wrapped)

deriving instance Show (Args Unwrapped)

main :: IO ()
main = do
    Args {epochDir, dbDir, epochSlots, networkMagic} <- unwrapRecord "Byron DB converter"
    dbDir' <- parseAbsDir =<< canonicalizePath dbDir
    (_, files) <- listDir =<< parseAbsDir =<< canonicalizePath epochDir
    let epochFiles = filter (\f -> fileExtension f == ".epoch") files
    putStrLn $ "Writing to " ++ show dbDir
    for_ (sort epochFiles) $ \f -> do
      putStrLn $ "Converting file " ++ show f
      convertChunkFile (EpochSlots epochSlots) f dbDir' (mkNetworkMagic networkMagic)

-- | If 'NetworkMagic' is not specified, we assume by default it's mainnet.
mkNetworkMagic :: Maybe Word32 -> NetworkMagic
mkNetworkMagic Nothing  = NetworkMagic 764824073
mkNetworkMagic (Just w) = NetworkMagic w

convertChunkFile
  :: EpochSlots
  -> Path Abs File -- ^ Input
  -> Path Abs Dir -- ^ Ouput directory
  -> NetworkMagic
  -> IO (Either CC.ParseError ())
convertChunkFile es inFile outDir magicId = do
    createDirIfMissing True dbDir
    dbMarkerFile' <- parseRelFile $ T.unpack dbMarkerFile
    BS.writeFile (toFilePath $ outDir </> dbMarkerFile') $
      dbMarkerContents magicId
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
