module DB
  ( DBConfig (..)
  , withDB
  ) where

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR

import Control.Exception (throwIO)
import qualified System.Directory (createDirectoryIfMissing)

import qualified Cardano.Binary as Binary (fromCBOR, toCBOR)
import qualified Cardano.Chain.Block as Cardano (HeaderHash)
import qualified Cardano.Chain.Slotting as Cardano (EpochSlots (..))

import Ouroboros.Byron.Proxy.DB (DB)
import qualified Ouroboros.Byron.Proxy.DB as DB
import qualified Ouroboros.Byron.Proxy.Index.Sqlite as Index
import qualified Ouroboros.Storage.Common as Immutable
import qualified Ouroboros.Storage.EpochInfo as Immutable
import qualified Ouroboros.Storage.ImmutableDB.API as Immutable
import qualified Ouroboros.Storage.ImmutableDB.Impl as Immutable
import Ouroboros.Storage.FS.API.Types (MountPoint (..))
import Ouroboros.Storage.FS.API (HasFS)
import Ouroboros.Storage.FS.IO (HandleIO, ioHasFS)
import qualified Ouroboros.Storage.Util.ErrorHandling as FS (exceptions)

-- This module makes it convenient to set up a DB (ImmutableDB, HasFS, and
-- SQLite index) in IO, handling cardano-sl block types.

data DBConfig = DBConfig
  { dbFilePath    :: !FilePath
    -- ^ Directory to house the `ImmutableDB`.
  , indexFilePath :: !FilePath
    -- ^ Path to the SQLite index.
  , slotsPerEpoch :: !Cardano.EpochSlots
    -- ^ Number of slots per epoch. Cannot handle a chain for which this is
    -- not constant.
  }

-- | Set up and use a DB.
--
-- The directory at `dbFilePath` will be created if it does not exist.
withDB :: DBConfig -> (DB IO -> IO t) -> IO t
withDB dbOptions k = do
  -- The ImmutableDB/Storage layer will not create a directory for us, we have
  -- to ensure it exists.
  System.Directory.createDirectoryIfMissing True (dbFilePath dbOptions)
  let fsMountPoint :: MountPoint
      fsMountPoint = MountPoint (dbFilePath dbOptions)
      fs :: HasFS IO HandleIO
      fs = ioHasFS fsMountPoint
      getEpochSize epoch = pure $ Immutable.EpochSize $
        fromIntegral (Cardano.unEpochSlots (slotsPerEpoch dbOptions))
  epochInfo <- Immutable.newEpochInfo getEpochSize
  let openImmutableDB = Immutable.openDB
        decodeHeaderHash
        encodeHeaderHash
        fs
        FS.exceptions
        epochInfo
        Immutable.ValidateMostRecentEpoch
        (DB.epochFileParser (slotsPerEpoch dbOptions) fs)
  Index.withDB_ (indexFilePath dbOptions) $ \idx ->
    Immutable.withDB openImmutableDB $ \idb ->
      k (DB.mkDB throwIO (slotsPerEpoch dbOptions) idx idb)

encodeHeaderHash :: Cardano.HeaderHash -> CBOR.Encoding
encodeHeaderHash = Binary.toCBOR

decodeHeaderHash :: CBOR.Decoder s Cardano.HeaderHash
decodeHeaderHash = Binary.fromCBOR
