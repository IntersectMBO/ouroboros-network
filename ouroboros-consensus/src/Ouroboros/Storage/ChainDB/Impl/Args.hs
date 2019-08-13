{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Storage.ChainDB.Impl.Args
  ( ChainDbArgs (..)
  , ChainDbSpecificArgs (..)
  , defaultArgs
    -- * Internal
  , fromChainDbArgs
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Time.Clock (DiffTime, secondsToDiffTime)

import           Control.Monad.Class.MonadSTM

import           Control.Tracer (Tracer, contramap)

import           Ouroboros.Network.Block (HeaderHash, StandardHash)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.ThreadRegistry (ThreadRegistry)

import           Ouroboros.Storage.EpochInfo (EpochInfo)
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling,
                     ThrowCantCatch)

import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Storage.ChainDB.Impl.Types (TraceEvent (..))
import qualified Ouroboros.Storage.ChainDB.Impl.VolDB as VolDB

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data ChainDbArgs m blk = forall h1 h2 h3. ChainDbArgs {

      -- Decoders
      cdbDecodeHash       :: forall s. Decoder s (HeaderHash blk)
    , cdbDecodeBlock      :: forall s. Decoder s (Lazy.ByteString -> blk)
    , cdbDecodeLedger     :: forall s. Decoder s (LedgerState blk)
    , cdbDecodeChainState :: forall s. Decoder s (ChainState (BlockProtocol blk))

      -- Encoders
    , cdbEncodeBlock      :: blk -> Encoding
    , cdbEncodeHash       :: HeaderHash blk -> Encoding
    , cdbEncodeLedger     :: LedgerState blk -> Encoding
    , cdbEncodeChainState :: ChainState (BlockProtocol blk) -> Encoding

      -- Error handling
    , cdbErrImmDb         :: ErrorHandling ImmDB.ImmutableDBError m
    , cdbErrVolDb         :: ErrorHandling (VolDB.VolatileDBError (HeaderHash blk)) m
    , cdbErrVolDbSTM      :: ThrowCantCatch (VolDB.VolatileDBError (HeaderHash blk)) (STM m)

      -- HasFS instances
    , cdbHasFSImmDb       :: HasFS m h1
    , cdbHasFSVolDb       :: HasFS m h2
    , cdbHasFSLgrDB       :: HasFS m h3

      -- Policy
    , cdbValidation       :: ImmDB.ValidationPolicy
    , cdbBlocksPerFile    :: Int
    , cdbMemPolicy        :: LgrDB.MemPolicy
    , cdbDiskPolicy       :: LgrDB.DiskPolicy m

      -- Integration
    , cdbNodeConfig       :: NodeConfig (BlockProtocol blk)
    , cdbEpochInfo        :: EpochInfo m
    , cdbIsEBB            :: blk -> Maybe (HeaderHash blk)
    , cdbGenesis          :: m (ExtLedgerState blk)

      -- Misc
    , cdbTracer         :: Tracer m (TraceEvent blk)
    , cdbThreadRegistry :: ThreadRegistry m
    , cdbGcDelay        :: DiffTime
    }

-- | Arguments specific to the ChainDB, not to the ImmutableDB, VolatileDB, or
-- LedgerDB.
data ChainDbSpecificArgs m blk = ChainDbSpecificArgs {
      cdbsTracer         :: Tracer m (TraceEvent blk)
    , cdbsThreadRegistry :: ThreadRegistry m
    , cdbsGcDelay        :: DiffTime
    }

-- | Default arguments
--
-- The following fields must still be defined:
--
-- * 'cdbsTracer'
-- * 'cdbsThreadRegistry'
defaultSpecificArgs :: ChainDbSpecificArgs m blk
defaultSpecificArgs = ChainDbSpecificArgs{
      cdbsGcDelay        = oneHour
      -- Fields without a default
    , cdbsTracer         = error "no default for cdbsTracer"
    , cdbsThreadRegistry = error "no default for cdbsThreadRegistry"
    }
  where
    oneHour = secondsToDiffTime 60 * 60

-- | Default arguments for use within IO
--
-- See 'ImmDB.defaultArgs', 'VolDB.defaultArgs', 'LgrDB.defaultArgs', and
-- 'defaultSpecificArgs' for a list of which fields are not given a default
-- and must therefore be set explicitly.
defaultArgs :: StandardHash blk
            => FilePath
            -> ChainDbArgs IO blk
defaultArgs fp = toChainDbArgs (ImmDB.defaultArgs fp)
                               (VolDB.defaultArgs fp)
                               (LgrDB.defaultArgs fp)
                               defaultSpecificArgs


-- | Internal: split 'ChainDbArgs' into 'ImmDbArgs', 'VolDbArgs, 'LgrDbArgs',
-- and 'ChainDbSpecificArgs'.
fromChainDbArgs :: ChainDbArgs m blk
                -> ( ImmDB.ImmDbArgs     m blk
                   , VolDB.VolDbArgs     m blk
                   , LgrDB.LgrDbArgs     m blk
                   , ChainDbSpecificArgs m blk
                   )
fromChainDbArgs ChainDbArgs{..} = (
      ImmDB.ImmDbArgs {
          immDecodeHash       = cdbDecodeHash
        , immDecodeBlock      = cdbDecodeBlock
        , immEncodeHash       = cdbEncodeHash
        , immEncodeBlock      = cdbEncodeBlock
        , immErr              = cdbErrImmDb
        , immEpochInfo        = cdbEpochInfo
        , immValidation       = cdbValidation
        , immIsEBB            = cdbIsEBB
        , immHasFS            = cdbHasFSImmDb
        , immTracer           = contramap TraceImmDBEvent cdbTracer
        }
    , VolDB.VolDbArgs {
          volHasFS            = cdbHasFSVolDb
        , volErr              = cdbErrVolDb
        , volErrSTM           = cdbErrVolDbSTM
        , volBlocksPerFile    = cdbBlocksPerFile
        , volEncodeBlock      = cdbEncodeBlock
        , volDecodeBlock      = cdbDecodeBlock
        }
    , LgrDB.LgrDbArgs {
          lgrNodeConfig       = cdbNodeConfig
        , lgrHasFS            = cdbHasFSLgrDB
        , lgrDecodeLedger     = cdbDecodeLedger
        , lgrDecodeChainState = cdbDecodeChainState
        , lgrDecodeHash       = cdbDecodeHash
        , lgrEncodeLedger     = cdbEncodeLedger
        , lgrEncodeChainState = cdbEncodeChainState
        , lgrEncodeHash       = cdbEncodeHash
        , lgrMemPolicy        = cdbMemPolicy
        , lgrDiskPolicy       = cdbDiskPolicy
        , lgrGenesis          = cdbGenesis
        , lgrTracer           = contramap TraceLedgerEvent cdbTracer
        }
    , ChainDbSpecificArgs {
          cdbsTracer          = cdbTracer
        , cdbsThreadRegistry  = cdbThreadRegistry
        , cdbsGcDelay         = cdbGcDelay
        }
    )

-- | Internal: construct 'ChainDbArgs' from 'ImmDbArgs', 'VolDbArgs,
-- 'LgrDbArgs', and 'ChainDbSpecificArgs'.
--
-- Useful in 'defaultArgs'
toChainDbArgs :: ImmDB.ImmDbArgs     m blk
              -> VolDB.VolDbArgs     m blk
              -> LgrDB.LgrDbArgs     m blk
              -> ChainDbSpecificArgs m blk
              -> ChainDbArgs         m blk
toChainDbArgs ImmDB.ImmDbArgs{..}
              VolDB.VolDbArgs{..}
              LgrDB.LgrDbArgs{..}
              ChainDbSpecificArgs{..} = ChainDbArgs{
      -- Decoders
      cdbDecodeHash       = immDecodeHash
    , cdbDecodeBlock      = immDecodeBlock
    , cdbDecodeLedger     = lgrDecodeLedger
    , cdbDecodeChainState = lgrDecodeChainState
      -- Encoders
    , cdbEncodeBlock      = immEncodeBlock
    , cdbEncodeHash       = immEncodeHash
    , cdbEncodeLedger     = lgrEncodeLedger
    , cdbEncodeChainState = lgrEncodeChainState
      -- Error handling
    , cdbErrImmDb         = immErr
    , cdbErrVolDb         = volErr
    , cdbErrVolDbSTM      = volErrSTM
      -- HasFS instances
    , cdbHasFSImmDb       = immHasFS
    , cdbHasFSVolDb       = volHasFS
    , cdbHasFSLgrDB       = lgrHasFS
      -- Policy
    , cdbValidation       = immValidation
    , cdbBlocksPerFile    = volBlocksPerFile
    , cdbMemPolicy        = lgrMemPolicy
    , cdbDiskPolicy       = lgrDiskPolicy
      -- Integration
    , cdbNodeConfig       = lgrNodeConfig
    , cdbEpochInfo        = immEpochInfo
    , cdbIsEBB            = immIsEBB
    , cdbGenesis          = lgrGenesis
      -- Misc
    , cdbTracer           = cdbsTracer
    , cdbThreadRegistry   = cdbsThreadRegistry
    , cdbGcDelay          = cdbsGcDelay
    }
