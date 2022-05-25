{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | This tiny executable loads an in-mem backing store and an lmdb backing store and checks:
--
-- 1. Slot number is the same
-- 2. The values are the same
--
-- It only works for CardanoBlock for the time being.

module Main (main) where

import qualified Codec.CBOR.Read as CBOR
import           Control.Monad (unless)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           Options.Applicative as O
import           Unsafe.Coerce (unsafeCoerce)

import           Cardano.Binary as CBOR
import           Cardano.Ledger.Crypto
import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))

import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Storage.FS.API as FS
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS
import qualified Ouroboros.Consensus.Storage.FS.IO as FS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB as LMDB

import qualified Database.LMDB.Simple as LMDB
import qualified Database.LMDB.Simple.Extra as LMDB

data Options = Options { inmemFile :: FilePath, lmdbFile :: FilePath } deriving Show

optsParser :: Parser Options
optsParser = Options
  <$> O.argument O.str (metavar "INMEM path")
  <*> O.argument O.str (metavar "LMDB path")

opts :: ParserInfo Options
opts = info (optsParser <**> helper)
      ( fullDesc
     <> progDesc "Check databases"
     <> O.header "db-checker" )

main :: IO ()
main = do
  op <- execParser opts
  (s1, v1) <- getMemDb $ inmemFile op
  putStrLn "INMEM done"
  (s2, v2) <- getLMDB $ lmdbFile op
  putStrLn "LMDB done"
  if s1 == s2
    then putStrLn "SAME SLOT"
    else putStrLn $ "DIFFERENT SLOT" <> show (s1, s2)

  if v1 == v2
    then putStrLn "SAME VALUES"
    else putStrLn "DIFFERENT VALUES"

getMemDb :: FilePath -> IO (WithOrigin SlotNo, LedgerTables (ExtLedgerState (CardanoBlock StandardCrypto)) ValuesMK)
getMemDb f = do
  let fs = FS.ioHasFS (FS.MountPoint f)
  FS.withFile fs (FS.fsPathFromList [fromString "tvar"]) FS.ReadMode $ \h -> do
    bs <- FS.hGetAll fs h
    case CBOR.deserialiseFromBytes ((,) <$> CBOR.fromCBOR <*> valuesMKDecoder) bs of
      Left  err        -> error $ show err
      Right (extra, x) -> do
        unless (BSL.null extra) $ error $ show "Leftover bytes"
        pure x

getLMDB :: FilePath -> IO (WithOrigin SlotNo, LedgerTables (ExtLedgerState (CardanoBlock StandardCrypto)) ValuesMK)
getLMDB dbFilePath = do
  dbEnv <- LMDB.openEnvironment dbFilePath LMDB.defaultLMDBLimits
  Just dbSettings <- LMDB.readWriteTransaction dbEnv $ (LMDB.getDatabase (Just "_dbstate") :: LMDB.Transaction LMDB.ReadWrite (LMDB.Database () LMDB.DbState)) >>= flip LMDB.get ()
  dbBackingTables <- LMDB.readWriteTransaction dbEnv $ traverseLedgerTables (\(NameMK name) -> LMDBMK name <$> LMDB.getDatabase (Just name)) namesLedgerTables
  (LMDB.dbsSeq dbSettings,) <$> (LMDB.readWriteTransaction dbEnv (traverseLedgerTables2 f dbBackingTables codecLedgerTables) :: IO (LedgerTables (ExtLedgerState (CardanoBlock StandardCrypto)) ValuesMK))
  where
    f :: Ord k => LMDB.LMDBMK k v -> CodecMK k v -> LMDB.Transaction mode (ValuesMK k v)
    f (LMDB.LMDBMK _ db) cdc = ApplyValuesMK
                        . HD.UtxoValues
                       <$> LMDB.foldrWithKey
                            Map.insert
                            Map.empty
                            cdc
                            db
