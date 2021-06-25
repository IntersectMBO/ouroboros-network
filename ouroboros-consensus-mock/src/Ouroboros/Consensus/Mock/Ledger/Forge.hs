{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.Mock.Ledger.Forge (
    ForgeExt (..)
  , forgeSimple
  ) where

import           Cardano.Binary (toCBOR)
import           Codec.Serialise (Serialise (..), serialise)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word

import           Cardano.Crypto.Hash (hashWithSerialiser)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Protocol.Abstract

-- | Construct the protocol specific part of the block
--
-- This is used in 'forgeSimple', which takes care of the generic part of the
-- mock block.
--
-- Note: this is a newtype and not a type class to allow for things in the
-- closure. For example, if Praos had to use a stateful KES key, it could
-- refer to it in its closure.
newtype ForgeExt c ext = ForgeExt {
      forgeExt :: TopLevelConfig                (SimpleBlock c ext)
               -> IsLeader       (BlockProtocol (SimpleBlock c ext))
               -> SimpleBlock' c ext ()
               -> SimpleBlock c ext
    }

forgeSimple :: forall c ext.
               ( SimpleCrypto c
               , MockProtocolSpecific c ext
               )
            => ForgeExt c ext
            -> TopLevelConfig (SimpleBlock c ext)
            -> BlockNo                               -- ^ Current block number
            -> SlotNo                                -- ^ Current slot number
            -> TickedLedgerState (SimpleBlock c ext) -- ^ Current ledger
            -> MaxTxCapacityOverride
            -> [GenTx (SimpleBlock c ext)]           -- ^ Txs to include
            -> IsLeader (BlockProtocol (SimpleBlock c ext))
            -> SimpleBlock c ext
forgeSimple ForgeExt { forgeExt } cfg curBlock curSlot tickedLedger _mx txs proof =
    forgeExt cfg proof $ SimpleBlock {
        simpleHeader = mkSimpleHeader encode stdHeader ()
      , simpleBody   = body
      }
  where
    body :: SimpleBody
    body = SimpleBody { simpleTxs = map simpleGenTx txs }

    stdHeader :: SimpleStdHeader c ext
    stdHeader = SimpleStdHeader {
          simplePrev     = castHash $ getTipHash tickedLedger
        , simpleSlotNo   = curSlot
        , simpleBlockNo  = curBlock
        , simpleBodyHash = hashWithSerialiser toCBOR body
        , simpleBodySize = bodySize
        }

    bodySize :: Word32
    bodySize = fromIntegral $ Lazy.length $ serialise body
