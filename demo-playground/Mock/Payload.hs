{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
module Mock.Payload (
      SimpleUtxoBlock(..) -- re-export
    , fixupBlock
    , chainFrom
    , toChain
    , addTxs
    ) where

import           Data.Semigroup ((<>))
import           Data.Set (Set)

import           Ouroboros.Consensus.Block.SimpleUTxO
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as C
import           Ouroboros.Network.Testing.ConcreteBlock hiding (fixupBlock)

fixupBlock :: Chain SimpleUtxoBlock -> SimpleUtxoBlock -> SimpleUtxoBlock
fixupBlock c (SimpleUtxoBlock header body) =
    SimpleUtxoBlock (fixupBlockHeader c (simpleUtxoBodyHash body) header) body

toChain :: [Int] -> Chain SimpleUtxoBlock
toChain = C.fromNewestFirst
        . reverse
        . chainWithSlots (castHash (C.headHash genesis)) (C.headBlockNo genesis)
        . map (Slot . fromIntegral)
  where
    genesis :: Chain SimpleUtxoBlock
    genesis = Genesis

-- | Dummy chain with empty bodies in the specified slots
chainWithSlots :: Hash BlockHeader -> BlockNo -> [Slot] -> [SimpleUtxoBlock]
chainWithSlots _        _      []     = []
chainWithSlots prevHash prevNo (s:ss) =
    let block = mkBlock s
    in block : chainWithSlots (BlockHash (blockHash block)) (blockNo block) ss
  where
    mkBlock :: Slot -> SimpleUtxoBlock
    mkBlock slot = SimpleUtxoBlock {
          simpleUtxoHeader = mkBlockHeader slot (simpleUtxoBodyHash body)
        , simpleUtxoBody   = body
        }
      where
        body = mempty

    mkBlockHeader :: Slot -> BodyHash -> BlockHeader
    mkBlockHeader slot bHash =
        let hdr = BlockHeader {
             headerHash     = hashHeader hdr
          ,  headerPrevHash = prevHash
          ,  headerSlot     = slot
          ,  headerBlockNo  = succ prevNo
          ,  headerSigner   = BlockSigner 0
          ,  headerBodyHash = bHash
          } in hdr

chainFrom :: Chain SimpleUtxoBlock
          -> Int
          -> [SimpleUtxoBlock]
chainFrom c n =
    chainWithSlots (castHash (C.headHash c))
                   (C.headBlockNo c)
                   [Slot (headSlot + s) | s <- [1 .. fromIntegral n]]
  where
    Slot headSlot = C.headSlot c

addTxs :: Set Mock.Tx
       -> SimpleUtxoBlock
       -> SimpleUtxoBlock
addTxs txs b = b { simpleUtxoBody = txs <> simpleUtxoBody b }
