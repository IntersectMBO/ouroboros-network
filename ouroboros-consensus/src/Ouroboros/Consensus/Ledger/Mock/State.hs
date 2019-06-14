{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.Mock.State (
    -- * State of the mock ledger
    MockState(..)
  , MockError(..)
  , updateMockState
  , updateMockTip
    -- * Genesis state
  , genesisMockState
  ) where

import           Control.Monad.Except
import           Data.Set (Set)
import qualified Data.Set as Set

import           Cardano.Crypto.Hash

import           Ouroboros.Network.Block (ChainHash (..), HasHeader, HeaderHash,
                     Point, StandardHash, fromTPoint, pointHash)
import           Ouroboros.Network.Chain (genesisPoint)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Mock.Address
import           Ouroboros.Consensus.Ledger.Mock.UTxO

{-------------------------------------------------------------------------------
  State of the mock ledger
-------------------------------------------------------------------------------}

data MockState blk = MockState {
      mockUtxo      :: Utxo
    , mockConfirmed :: Set TxId
    , mockTip       :: Point blk
    }

deriving instance Show (HeaderHash blk) => Show (MockState blk)

data MockError blk =
    MockInvalidInputs InvalidInputs
  | MockInvalidHash (ChainHash blk) (ChainHash blk)

deriving instance StandardHash blk => Show (MockError blk)

updateMockState :: (Monad m, HasUtxo a)
                => a
                -> MockState blk
                -> ExceptT (MockError blk) m (MockState blk)
updateMockState b (MockState u c t) = do
    u' <- withExceptT MockInvalidInputs $ updateUtxo b u
    return $ MockState u' (c `Set.union` confirmed b) t

updateMockTip :: (Monad m, HasHeader (Header blk), StandardHash blk)
              => Header blk
              -> MockState blk
              -> ExceptT (MockError blk) m (MockState blk)
updateMockTip hdr (MockState u c t) = ExceptT $ return $
    if headerPrevHash hdr == tHash
      then Right $ MockState u c (headerPoint hdr)
      else Left  $ MockInvalidHash (headerPrevHash hdr) tHash
  where
  tHash = fromTPoint GenesisHash (BlockHash . pointHash) t

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

genesisMockState :: AddrDist -> MockState blk
genesisMockState addrDist = MockState {
      mockUtxo      = genesisUtxo addrDist
    , mockConfirmed = Set.singleton (hash (genesisTx addrDist))
    , mockTip       = genesisPoint
    }
