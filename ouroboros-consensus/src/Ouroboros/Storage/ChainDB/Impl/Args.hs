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

import           Control.Tracer (Tracer, contramap)

import           Ouroboros.Network.Block (HeaderHash)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (BlockchainTime)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Storage.Common (EpochNo)
import           Ouroboros.Storage.EpochInfo (EpochInfo)
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling,
                     ThrowCantCatch)

import           Ouroboros.Storage.ChainDB.Impl.ImmDB (BinaryInfo (..),
                     HashInfo (..))
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
    , cdbEncodeBlock      :: blk -> BinaryInfo Encoding
    , cdbEncodeHash       :: HeaderHash blk -> Encoding
    , cdbEncodeLedger     :: LedgerState blk -> Encoding
    , cdbEncodeChainState :: ChainState (BlockProtocol blk) -> Encoding

      -- Error handling
    , cdbErrImmDb         :: ErrorHandling ImmDB.ImmutableDBError m
    , cdbErrVolDb         :: ErrorHandling VolDB.VolatileDBError m
    , cdbErrVolDbSTM      :: ThrowCantCatch VolDB.VolatileDBError (STM m)

      -- HasFS instances
    , cdbHasFSImmDb       :: HasFS m h1
    , cdbHasFSVolDb       :: HasFS m h2
    , cdbHasFSLgrDB       :: HasFS m h3

      -- Policy
    , cdbValidation       :: ImmDB.ValidationPolicy
    , cdbBlocksPerFile    :: Int
    , cdbParamsLgrDB      :: LgrDB.LedgerDbParams
    , cdbDiskPolicy       :: LgrDB.DiskPolicy m

      -- Integration
    , cdbNodeConfig       :: NodeConfig (BlockProtocol blk)
    , cdbEpochInfo        :: EpochInfo m
    , cdbHashInfo         :: HashInfo (HeaderHash blk)
    , cdbIsEBB            :: blk -> Maybe EpochNo
    , cdbCheckIntegrity   :: blk -> Bool
    , cdbGenesis          :: m (ExtLedgerState blk)
    , cdbBlockchainTime   :: BlockchainTime m

      -- Misc
    , cdbTracer           :: Tracer m (TraceEvent blk)
    , cdbTraceLedger      :: Tracer m (LgrDB.LedgerDB blk)
    , cdbRegistry         :: ResourceRegistry m
    , cdbGcDelay          :: DiffTime
    }

-- | Arguments specific to the ChainDB, not to the ImmutableDB, VolatileDB, or
-- LedgerDB.
data ChainDbSpecificArgs m blk = ChainDbSpecificArgs {
      cdbsTracer         :: Tracer m (TraceEvent blk)
    , cdbsRegistry       :: ResourceRegistry m
    , cdbsGcDelay        :: DiffTime
    , cdbsBlockchainTime :: BlockchainTime m
    }

-- | Default arguments
--
-- The following fields must still be defined:
--
-- * 'cdbsTracer'
-- * 'cdbsRegistry'
defaultSpecificArgs :: ChainDbSpecificArgs m blk
defaultSpecificArgs = ChainDbSpecificArgs{
      cdbsGcDelay        = oneHour
      -- Fields without a default
    , cdbsTracer         = error "no default for cdbsTracer"
    , cdbsRegistry       = error "no default for cdbsRegistry"
    , cdbsBlockchainTime = error "no default for cdbsBlockchainTime"
    }
  where
    oneHour = secondsToDiffTime 60 * 60

-- | Default arguments for use within IO
--
-- See 'ImmDB.defaultArgs', 'VolDB.defaultArgs', 'LgrDB.defaultArgs', and
-- 'defaultSpecificArgs' for a list of which fields are not given a default
-- and must therefore be set explicitly.
defaultArgs :: FilePath -> ChainDbArgs IO blk
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
        , immHashInfo         = cdbHashInfo
        , immValidation       = cdbValidation
        , immIsEBB            = cdbIsEBB
        , immCheckIntegrity   = cdbCheckIntegrity
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
        , volIsEBB            = \blk -> case cdbIsEBB blk of
                                          Nothing -> IsNotEBB
                                          Just _  -> IsEBB
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
        , lgrParams           = cdbParamsLgrDB
        , lgrDiskPolicy       = cdbDiskPolicy
        , lgrGenesis          = cdbGenesis
        , lgrTracer           = contramap TraceLedgerEvent cdbTracer
        , lgrTraceLedger      = cdbTraceLedger
        }
    , ChainDbSpecificArgs {
          cdbsTracer          = cdbTracer
        , cdbsRegistry        = cdbRegistry
        , cdbsGcDelay         = cdbGcDelay
        , cdbsBlockchainTime  = cdbBlockchainTime
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
    , cdbParamsLgrDB      = lgrParams
    , cdbDiskPolicy       = lgrDiskPolicy
      -- Integration
    , cdbNodeConfig       = lgrNodeConfig
    , cdbEpochInfo        = immEpochInfo
    , cdbHashInfo         = immHashInfo
    , cdbIsEBB            = immIsEBB
    , cdbCheckIntegrity   = immCheckIntegrity
    , cdbGenesis          = lgrGenesis
    , cdbBlockchainTime   = cdbsBlockchainTime
      -- Misc
    , cdbTracer           = cdbsTracer
    , cdbTraceLedger      = lgrTraceLedger
    , cdbRegistry         = cdbsRegistry
    , cdbGcDelay          = cdbsGcDelay
    }
