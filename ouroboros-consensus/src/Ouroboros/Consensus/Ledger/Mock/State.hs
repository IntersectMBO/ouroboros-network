module Ouroboros.Consensus.Ledger.Mock.State (
    -- * State of the mock ledger
    MockState(..)
  , updateMockState
    -- * Genesis state
  , genesisMockState
  ) where

import           Control.Monad.Except
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Network.Block (Point)
import           Ouroboros.Network.Chain (genesisPoint)

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Ledger.Mock.Address
import           Ouroboros.Consensus.Ledger.Mock.UTxO

{-------------------------------------------------------------------------------
  State of the mock ledger
-------------------------------------------------------------------------------}

data MockState b = MockState {
      mockUtxo      :: Utxo
    , mockConfirmed :: Set (Hash ShortHash Tx)
    , mockTip       :: Point b
    }
  deriving (Show)

updateMockState :: (Monad m, HasUtxo a)
                => a
                -> MockState b
                -> ExceptT InvalidInputs m (MockState b)
updateMockState b (MockState u c t) = do
    u' <- updateUtxo b u
    return $ MockState u' (c `Set.union` confirmed b) t

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

genesisMockState :: AddrDist -> MockState b
genesisMockState addrDist = MockState {
      mockUtxo      = genesisUtxo addrDist
    , mockConfirmed = Set.singleton (hash (genesisTx addrDist))
    , mockTip       = genesisPoint
    }
