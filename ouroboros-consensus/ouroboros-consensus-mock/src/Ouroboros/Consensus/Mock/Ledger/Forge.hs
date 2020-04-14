{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.Mock.Ledger.Forge (forgeSimple) where

import           Codec.Serialise (Serialise (..), serialise)
import           Crypto.Random (MonadRandom)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word

import           Cardano.Crypto.Hash

import           Ouroboros.Network.Block (BlockNo, SlotNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Protocol.Abstract

forgeSimple :: forall c m ext. (MonadRandom m, RunMockBlock c ext)
            => TopLevelConfig (SimpleBlock c ext)
            -> Update m (NodeState (SimpleBlock c ext))
            -> BlockNo                               -- ^ Current block number
            -> TickedLedgerState (SimpleBlock c ext) -- ^ Current ledger
            -> [GenTx (SimpleBlock c ext)]           -- ^ Txs to include
            -> IsLeader (BlockProtocol (SimpleBlock c ext))
            -> m (SimpleBlock c ext)
forgeSimple cfg updateState curBlock tickedLedger txs proof = do
    forgeExt cfg updateState proof $ SimpleBlock {
        simpleHeader = mkSimpleHeader encode stdHeader ()
      , simpleBody   = body
      }
  where
    curSlot :: SlotNo
    curSlot = tickedSlotNo tickedLedger

    body :: SimpleBody
    body = SimpleBody { simpleTxs = map simpleGenTx txs }

    stdHeader :: SimpleStdHeader c ext
    stdHeader = SimpleStdHeader {
          simplePrev      = ledgerTipHash (tickedLedgerState tickedLedger)
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
