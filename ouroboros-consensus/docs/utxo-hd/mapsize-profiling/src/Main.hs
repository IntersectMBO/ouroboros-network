module Main (
    main
  , sizeBytesWord64
  ) where

import           Control.Monad
import           Data.Word
import           System.Directory
import           System.Environment

-- import qualified Database.LMDB.Raw as LMDB.Raw
import qualified Database.LMDB.Simple as LMDB.Simple
import qualified Database.LMDB.Simple.Extra as LMDB.Extra
import qualified Database.LMDB.Simple.Internal as LMDB.Internal

-- | Simple program that reads some commands from a file or cli and
-- performs them on an @LMDB@ database. Used for profiling
-- @LMDB@ performance/memory usage/storage usage under different
-- @`LMDB.Simple.mapSize`@ parameters and workloads.
main :: IO ()
main = do
  putStrLn "> Starting..."

  -- @arg1@: The database filepath.
  -- @arg2@: The factorised mapsize (factorised for ease of writing).
  -- @arg3@: Whether to read input commands from file or the CLI.
  args0@(arg1 : arg2 : arg3 : args) <- getArgs

  putStrLn $ "> Arguments: " ++ show args0

  let
    dbFilePath :: String
    dbFilePath = arg1
    mapSizeFactors :: [Int]
    mapSizeFactors = read arg2
    limits :: LMDB.Simple.Limits
    limits = LMDB.Simple.defaultLimits {
        LMDB.Simple.mapSize = product mapSizeFactors
      }

  -- Read commands either from an input file, or from the CLI.
  cmds <- case arg3 of
    "file" -> do
      [arg4] <- pure args
      let
        cmdsFilePath :: FilePath
        cmdsFilePath = arg4

      b <- doesFileExist cmdsFilePath
      unless b $ error "Input commands filepath does not exist!"
      fileContents <- readFile cmdsFilePath

      pure $ readCommands fileContents

    "cli" -> do
      [arg4] <- pure args
      pure $ read arg4

    _ -> error $ "Third argument sould be \"file\" or \"cli\": " <> arg3

  -- (Re-)create the database directory, since the @LMDB@ package expects
  -- the database directory to exist already.
  b <- doesDirectoryExist dbFilePath
  when b $ removeDirectoryRecursive dbFilePath
  createDirectoryIfMissing True dbFilePath

  dbEnv <-
    LMDB.Simple.openReadWriteEnvironment
    dbFilePath
    limits
  db <-
    LMDB.Simple.readOnlyTransaction dbEnv $
      LMDB.Simple.getDatabase Nothing :: IO (LMDB.Internal.Database Word64 Word64)

  mapM_ (cmdDo dbEnv db) cmds

  LMDB.Simple.closeEnvironment dbEnv

  putStrLn "> Stopping..."
  putStrLn ""

{-------------------------------------------------------------------------------
  Input commands
-------------------------------------------------------------------------------}

data Cmd
  = Puts !Word64 !Word64
  | Gets !Word64 !Word64
  | GetSize
  deriving (Show, Read)

readCommands :: String -> [Cmd]
readCommands = map read . lines

cmdDo ::
     LMDB.Internal.Environment LMDB.Internal.ReadWrite
  -> LMDB.Internal.Database Word64 Word64
  -> Cmd
  -> IO ()
cmdDo dbEnv db cmd = do
  putStrLn $ "> " ++ show cmd

  case cmd of
    Puts k1 k2 -> cmdPuts dbEnv db k1 k2
    Gets k1 k2 -> cmdGets dbEnv db k1 k2
    GetSize    -> cmdGetSize dbEnv db

cmdPuts ::
     LMDB.Internal.Environment LMDB.Internal.ReadWrite
  -> LMDB.Internal.Database Word64 Word64
  -> Word64
  -> Word64
  -> IO ()
cmdPuts dbEnv db k1 k2 = do
  LMDB.Simple.readWriteTransaction dbEnv $
    puts db [(k, Just v) | k <- [k1 .. k2], v <- [k]]

cmdGets ::
     LMDB.Internal.Environment LMDB.Internal.ReadWrite
  -> LMDB.Internal.Database Word64 Word64
  -> Word64
  -> Word64
  -> IO ()
cmdGets dbEnv db k1 k2 = do
  _vs <- LMDB.Simple.readOnlyTransaction dbEnv $
    gets db [k | k <- [k1 .. k2]]
  pure ()

cmdGetSize ::
     LMDB.Internal.Environment LMDB.Internal.ReadWrite
  -> LMDB.Internal.Database Word64 Word64
  -> IO ()
cmdGetSize dbEnv db = do
  dbSize <- LMDB.Simple.readOnlyTransaction dbEnv $
    LMDB.Extra.size db

  putStrLn $ "# Database size: " ++ show dbSize

{-------------------------------------------------------------------------------
  Sequences of puts/gets
-------------------------------------------------------------------------------}

puts ::
     (LMDB.Internal.Serialise k, LMDB.Internal.Serialise v)
  => LMDB.Internal.Database k v
  -> [(k, Maybe v)]
  -> LMDB.Internal.Transaction LMDB.Internal.ReadWrite ()
puts db kvs = forM_ kvs $ uncurry (LMDB.Simple.put db)

gets ::
     (LMDB.Internal.Serialise k, LMDB.Internal.Serialise v)
  => LMDB.Internal.Database k v
  -> [k]
  -> LMDB.Internal.Transaction LMDB.Internal.ReadOnly [Maybe v]
gets db ks = forM ks $ LMDB.Simple.get db

{-------------------------------------------------------------------------------
  Sequences of puts/gets
-------------------------------------------------------------------------------}


-- | Hard-coded size of a @Word64@ in bytes: @size == 64 / 8@.
sizeBytesWord64 :: Int
sizeBytesWord64 = 8
