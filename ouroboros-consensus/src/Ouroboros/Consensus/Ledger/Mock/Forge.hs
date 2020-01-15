{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Consensus.Ledger.Mock.Forge (forgeSimple) where

import           Codec.Serialise (Serialise (..), serialise)
import           Crypto.Random (MonadRandom)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Typeable (Typeable)
import           Data.Word

import           Cardano.Crypto.Hash

import           Ouroboros.Network.Block (BlockNo, SlotNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Run
import           Ouroboros.Consensus.Protocol.Abstract

forgeSimple :: forall p c m ext.
               ( HasNodeState p m
               , MonadRandom m
               , SimpleCrypto c
               , RunMockBlock p c ext
               , SupportedBlock (SimpleBlock c ext)
               , Typeable ext
               )
            => NodeConfig p
            -> SlotNo                              -- ^ Current slot
            -> BlockNo                             -- ^ Current block number
            -> ExtLedgerState (SimpleBlock c ext)  -- ^ Previous hash
            -> [GenTx (SimpleBlock c ext)]         -- ^ Txs to add in the block
            -> IsLeader p                          -- ^ Proof we are slot leader
            -> m (SimpleBlock c ext)
forgeSimple cfg curSlot curBlock extLedger txs proof = do
    forgeExt cfg proof $ SimpleBlock {
        simpleHeader = mkSimpleHeader encode stdHeader ()
      , simpleBody   = body
      }
  where
    body :: SimpleBody
    body = SimpleBody { simpleTxs = map simpleGenTx txs }

    stdHeader :: SimpleStdHeader c ext
    stdHeader = SimpleStdHeader {
          simplePrev      = ledgerTipHash (ledgerState extLedger)
        , simpleSlotNo    = curSlot
        , simpleBlockNo   = curBlock
        , simpleBodyHash  = hash body
        , simpleBlockSize = bodySize
        }

    -- We use the size of the body, not of the whole block (= header + body),
    -- since the header size is fixed and this size is only used for
    -- prioritisation.
    bodySize :: Word64
    bodySize = fromIntegral $ Lazy.length $ serialise body
