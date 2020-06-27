{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.UTxO
import           Ouroboros.Consensus.Util (repeatedlyM)

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
    MockExpired !SlotNo !SlotNo
    -- ^ The transaction expired in the first 'SlotNo', and it failed to
    -- validate in the second 'SlotNo'.
  | MockUtxoError UtxoError
  | MockInvalidHash (ChainHash blk) (ChainHash blk)
  deriving (Generic, NoUnexpectedThunks)

deriving instance StandardHash blk => Show (MockError blk)
deriving instance StandardHash blk => Eq   (MockError blk)
deriving instance Serialise (HeaderHash blk) => Serialise (MockError blk)

updateMockState :: (GetPrevHash blk, HasMockTxs blk)
                => CodecConfig blk
                -> blk
                -> MockState blk
                -> Except (MockError blk) (MockState blk)
updateMockState cfg blk st = do
    let hdr = getHeader blk
    st' <- updateMockTip cfg hdr st
    updateMockUTxO (blockSlot hdr) blk st'

updateMockTip :: GetPrevHash blk
              => CodecConfig blk
              -> Header blk
              -> MockState blk
              -> Except (MockError blk) (MockState blk)
updateMockTip cfg hdr (MockState u c t)
    | headerPrevHash cfg hdr == pointHash t
    = return $ MockState u c (headerPoint hdr)
    | otherwise
    = throwError $ MockInvalidHash (headerPrevHash cfg hdr) (pointHash t)

updateMockUTxO :: HasMockTxs a
               => SlotNo
               -> a
               -> MockState blk
               -> Except (MockError blk) (MockState blk)
updateMockUTxO now = repeatedlyM (updateMockUTxO1 now) . getMockTxs

updateMockUTxO1 :: forall blk.
                   SlotNo
                -> Tx
                -> MockState blk
                -> Except (MockError blk) (MockState blk)
updateMockUTxO1 now tx (MockState u c t) = case hasExpired of
    Just e  -> throwError e
    Nothing -> do
      u' <- withExcept MockUtxoError $ updateUtxo tx u
      return $ MockState u' (c `Set.union` confirmed tx) t
  where
      Tx expiry _ins _outs = tx

      hasExpired :: Maybe (MockError blk)
      hasExpired = case expiry of
          DoNotExpire       -> Nothing
          ExpireAtOnsetOf s -> do
            guard $ s <= now
            Just $ MockExpired s now

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

genesisMockState :: AddrDist -> MockState blk
genesisMockState addrDist = MockState {
      mockUtxo      = genesisUtxo addrDist
    , mockConfirmed = Set.singleton (hash (genesisTx addrDist))
    , mockTip       = GenesisPoint
    }
