{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Mock.Ledger.State (
    -- * State of the mock ledger
    MockError (..)
  , MockState (..)
  , updateMockState
  , updateMockTip
  , updateMockUTxO
    -- * Genesis state
  , genesisMockState
  ) where

import           Cardano.Binary (toCBOR)
import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Crypto.Hash

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.UTxO
import           Ouroboros.Consensus.Util (ShowProxy (..), repeatedlyM)

{-------------------------------------------------------------------------------
  State of the mock ledger
-------------------------------------------------------------------------------}

data MockState blk = MockState {
      mockUtxo      :: !Utxo
    , mockConfirmed :: !(Set TxId)
    , mockTip       :: !(Point blk)
    }
  deriving (Show, Eq, Generic, NoThunks)

deriving instance Serialise (HeaderHash blk) => Serialise (MockState blk)

data MockError blk =
    MockExpired !SlotNo !SlotNo
    -- ^ The transaction expired in the first 'SlotNo', and it failed to
    -- validate in the second 'SlotNo'.
  | MockUtxoError UtxoError
  | MockInvalidHash (ChainHash blk) (ChainHash blk)
  deriving (Generic, NoThunks)

deriving instance StandardHash blk => Show (MockError blk)
deriving instance StandardHash blk => Eq   (MockError blk)
deriving instance Serialise (HeaderHash blk) => Serialise (MockError blk)

instance Typeable blk => ShowProxy (MockError blk) where

updateMockState :: (GetPrevHash blk, HasMockTxs blk)
                => blk
                -> MockState blk
                -> Except (MockError blk) (MockState blk)
updateMockState blk st = do
    let hdr = getHeader blk
    st' <- updateMockTip hdr st
    updateMockUTxO (blockSlot hdr) blk st'

updateMockTip :: GetPrevHash blk
              => Header blk
              -> MockState blk
              -> Except (MockError blk) (MockState blk)
updateMockTip hdr (MockState u c t)
    | headerPrevHash hdr == pointHash t
    = return $ MockState u c (headerPoint hdr)
    | otherwise
    = throwError $ MockInvalidHash (headerPrevHash hdr) (pointHash t)

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
    , mockConfirmed = Set.singleton (hashWithSerialiser toCBOR (genesisTx addrDist))
    , mockTip       = GenesisPoint
    }
