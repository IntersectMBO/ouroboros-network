{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Chain state for the hard fork combinator
--
-- Intended for qualified import.
--
-- > import Ouroboros.Consensus.Protocol.HardFork.ChainState (AfterForkChainState(..))
-- > import qualified Ouroboros.Consensus.Protocol.HardFork.ChainState as AFCS
module Ouroboros.Consensus.Protocol.HardFork.ChainState (
    AfterForkChainState(..)
  , init
  , update
  , dropSnapshotIfRedundant
    -- * Serialisation
  , encodeAfterForkChainState
  , decodeAfterForkChainState
  ) where

import           Prelude hiding (init)

import           Codec.Serialise (Serialise (..))
import           Control.Monad (guard)
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Binary
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.HardFork.CanHardFork
import           Ouroboros.Consensus.Protocol.HardFork.Config

{-------------------------------------------------------------------------------
  ChainState for after the fork
-------------------------------------------------------------------------------}

-- | After the fork we need to keep track of when we forked and keep a snapshot
--   of the old era 'ChainState' for k blocks
data AfterForkChainState p1 p2 =
    AfterForkChainState {
        -- | The slot number of the block that initiated the hard fork
        afcsSlotNo   :: SlotNo

        -- | The block number of the block that initiated the hard fork
      , afcsBlockNo  :: BlockNo

        -- | Snapshot of the chain state before the hard fork
        --
        -- Will be set to 'Nothing' once this is no longer required (when we are
        -- sure we cannot roll back anymore to before the hard fork).
      , afcsSnapshot :: Maybe (ChainState p1)

        -- | Current chain state
      , afcsState    :: ChainState p2
      }
  deriving (Generic)

deriving instance (OuroborosTag p1, OuroborosTag p2)
               => Show (AfterForkChainState p1 p2)

deriving instance (OuroborosTag p1, OuroborosTag p2)
               => NoUnexpectedThunks (AfterForkChainState p1 p2)

-- | Initial chain state (immediately after the fork)
init :: (HasHeader hdr, CanHardFork p1 p2)
     => NodeConfig (p1 `HardForksTo` p2)
     -> hdr
     -> ChainState p1
     -> AfterForkChainState p1 p2
init cfg hdr oldState = AfterForkChainState {
      afcsSlotNo   = blockSlot hdr
    , afcsBlockNo  = blockNo hdr
    , afcsSnapshot = Just oldState
    , afcsState    = chainStateAfterFork cfg oldState
    }

dropSnapshotIfRedundant :: (HasHeader hdr, OuroborosTag p2)
                        => NodeConfig p2
                        -> hdr            -- ^ Most recent applied header
                        -> AfterForkChainState p1 p2
                        -> AfterForkChainState p1 p2
dropSnapshotIfRedundant cfg hdr afcs = afcs {
      afcsSnapshot = guard (distance < k) >> afcsSnapshot afcs
    }
  where
    k, atFork, now, distance :: Word64
    k        = maxRollbacks (protocolSecurityParam cfg)
    atFork   = unBlockNo (afcsBlockNo afcs)
    now      = unBlockNo (blockNo hdr)
    distance = if now >= atFork
                 then now - atFork
                 else error "apply: block from the past"

-- | Update the chain state
update :: Monad m
       => AfterForkChainState p1 p2
       -> (ChainState p2 -> m (ChainState p2))
       -> m (AfterForkChainState p1 p2)
update afcs f = do
    newState <- f (afcsState afcs)
    return $ afcs {
        afcsState = newState
      }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeAfterForkChainState
  :: (ChainState p1 -> Encoding)
  -> (ChainState p2 -> Encoding)
  -> AfterForkChainState p1 p2
  -> Encoding
encodeAfterForkChainState encodeBeforeFork
                          encodeAfterFork
                          AfterForkChainState{..} =
    mconcat [
        encodeListLen 4
      , encode afcsSlotNo
      , encode afcsBlockNo
      , toCBORMaybe encodeBeforeFork afcsSnapshot
      , encodeAfterFork afcsState
      ]

decodeAfterForkChainState
  :: Decoder s (ChainState p1)
  -> Decoder s (ChainState p2)
  -> Decoder s (AfterForkChainState p1 p2)
decodeAfterForkChainState decodeBeforeFork decodeAfterFork = do
    decodeListLenOf 4
    afcsSlotNo   <- decode
    afcsBlockNo  <- decode
    afcsSnapshot <- fromCBORMaybe decodeBeforeFork
    afcsState    <- decodeAfterFork
    return AfterForkChainState{..}
