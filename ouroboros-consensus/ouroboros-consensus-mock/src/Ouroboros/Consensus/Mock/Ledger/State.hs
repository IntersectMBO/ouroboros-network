{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Mock.Ledger.State (
    -- * State of the mock ledger
    MockState(..)
  , MockError(..)
  , updateMockState
  , updateMockTip
  , updateMockUTxO
    -- * Genesis state
  , genesisMockState
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Cardano.Crypto.Hash
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (ChainHash, HasHeader, HeaderHash,
                     Point, StandardHash, genesisPoint, pointHash)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.UTxO

{-------------------------------------------------------------------------------
  State of the mock ledger
-------------------------------------------------------------------------------}

data MockState blk = MockState {
      mockUtxo      :: !Utxo
    , mockConfirmed :: !(Set TxId)
    , mockTip       :: !(Point blk)
    }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

deriving instance Serialise (HeaderHash blk) => Serialise (MockState blk)

data MockError blk =
    MockUtxoError UtxoError
  | MockInvalidHash (ChainHash blk) (ChainHash blk)
  deriving (Generic, NoUnexpectedThunks)

deriving instance StandardHash blk => Show (MockError blk)
deriving instance StandardHash blk => Eq   (MockError blk)
deriving instance Serialise (HeaderHash blk) => Serialise (MockError blk)

updateMockState :: ( GetHeader blk
                   , HasHeader (Header blk)
                   , StandardHash blk
                   , HasUtxo blk
                   )
                => blk
                -> MockState blk
                -> Except (MockError blk) (MockState blk)
updateMockState b st = do
    st' <- updateMockTip (getHeader b) st
    updateMockUTxO b st'

updateMockTip :: (HasHeader (Header blk), StandardHash blk)
              => Header blk
              -> MockState blk
              -> Except (MockError blk) (MockState blk)
updateMockTip hdr (MockState u c t)
    | headerPrevHash hdr == pointHash t
    = return $ MockState u c (headerPoint hdr)
    | otherwise
    = throwError $ MockInvalidHash (headerPrevHash hdr) (pointHash t)

updateMockUTxO :: HasUtxo a
               => a
               -> MockState blk
               -> Except (MockError blk) (MockState blk)
updateMockUTxO b (MockState u c t) = do
    u' <- withExcept MockUtxoError $ updateUtxo b u
    return $ MockState u' (c `Set.union` confirmed b) t

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

genesisMockState :: AddrDist -> MockState blk
genesisMockState addrDist = MockState {
      mockUtxo      = genesisUtxo addrDist
    , mockConfirmed = Set.singleton (hash (genesisTx addrDist))
    , mockTip       = genesisPoint
    }
