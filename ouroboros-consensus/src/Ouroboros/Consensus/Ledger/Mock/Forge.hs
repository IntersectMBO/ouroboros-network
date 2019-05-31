{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Consensus.Ledger.Mock.Forge (
    forgeSimple
  , ForgeExt(..)
  ) where

import           Codec.Serialise (Serialise (..), serialise)
import           Crypto.Random (MonadRandom)
import qualified Data.ByteString.Lazy as Lazy

import           Ouroboros.Network.Block (BlockNo, ChainHash, SlotNo)

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Protocol.Abstract

class ForgeExt p c ext where
  -- | Construct the protocol specific part of the block
  forgeExt :: (HasNodeState p m, MonadRandom m)
           => NodeConfig p
           -> IsLeader p
           -> SimpleBlock' c ext ()
           -> m (SimpleBlock c ext)

forgeSimple :: forall p c m ext.
               ( HasNodeState p m
               , MonadRandom m
               , SimpleCrypto c
               , ForgeExt p c ext
               )
            => NodeConfig p
            -> SlotNo                         -- ^ Current slot
            -> BlockNo                        -- ^ Current block number
            -> ChainHash (SimpleHeader c ext) -- ^ Previous hash
            -> [GenTx (SimpleBlock c ext)]    -- ^ Txs to add in the block
            -> IsLeader p                     -- ^ Proof we are slot leader
            -> m (SimpleBlock c ext)
forgeSimple cfg curSlot curBlock prevHash txs proof = do
    forgeExt cfg proof $ SimpleBlock {
        simpleHeader = mkSimpleHeader encode stdHeader ()
      , simpleBody   = body
      }
  where
    body :: SimpleBody
    body = SimpleBody { simpleTxs = map simpleGenTx txs }

    stdHeader :: SimpleStdHeader c ext
    stdHeader = SimpleStdHeader {
          simplePrev      = prevHash
        , simpleSlotNo    = curSlot
        , simpleBlockNo   = curBlock
        , simpleBodyHash  = hash body
        , simpleBlockSize = bodySize
        }

    -- We use the size of the body, not of the whole block (= header + body),
    -- since the header size is fixed and this size is only used for
    -- prioritisation.
    bodySize :: Word
    bodySize = fromIntegral $ Lazy.length $ serialise body
