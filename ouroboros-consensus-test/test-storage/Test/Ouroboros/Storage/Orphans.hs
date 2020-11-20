{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Ouroboros.Storage.Orphans () where

import           Data.Maybe (isJust)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.CallStack

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbError,
                     ChainDbFailure)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, sameFsError)
import           Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDBError)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import           Ouroboros.Consensus.Storage.VolatileDB.API (VolatileDBError)
import qualified Ouroboros.Consensus.Storage.VolatileDB.API as VolatileDB

{-------------------------------------------------------------------------------
  PrettyCallStack
-------------------------------------------------------------------------------}

-- | NOTE: all 'PrettyCallStack' are equal to each other.
--
-- This is useful for testing, when comparing error types that embed a
-- 'PrettyCallStack'. The call stack will differ in practice, i.e., model vs
-- implementation.
instance Eq PrettyCallStack where
  _ == _ = True

{-------------------------------------------------------------------------------
  FS
-------------------------------------------------------------------------------}

instance Eq FsError where
  (==) = sameFsError

{-------------------------------------------------------------------------------
  VolatileDB
-------------------------------------------------------------------------------}

instance Eq VolatileDB.ApiMisuse where
  VolatileDB.ClosedDBError mbEx1 == VolatileDB.ClosedDBError mbEx2 =
      -- The exceptions can differ, we only care about the presence of one.
      isJust mbEx1 == isJust mbEx2

deriving instance StandardHash blk => Eq (VolatileDB.UnexpectedFailure blk)

deriving instance StandardHash blk => Eq (VolatileDBError blk)

{-------------------------------------------------------------------------------
  ImmutableDB
-------------------------------------------------------------------------------}

deriving instance StandardHash blk => Eq (ImmutableDBError blk)

deriving instance StandardHash blk => Eq (ImmutableDB.ApiMisuse blk)

deriving instance StandardHash blk => Eq (ImmutableDB.UnexpectedFailure blk)

{-------------------------------------------------------------------------------
  ChainDB
-------------------------------------------------------------------------------}

deriving instance (StandardHash blk) => Eq (ChainDbFailure blk)

deriving instance (StandardHash blk) => Eq (ChainDbError blk)
