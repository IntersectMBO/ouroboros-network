{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
module MockPayload (MockBlock, fixupBlock, chainFrom) where

import           Block      hiding (BlockBody)
import           Block.Mock (BlockBody (..))
import           Chain      (Chain (..))
import qualified Chain      as C
import           Ouroboros

type MockBlock p = Block 'MockLedgerDomain p

fixupBlock :: Chain (MockBlock p) -> MockBlock p -> MockBlock p
fixupBlock = C.fixupBlock

chainFrom :: forall p. KnownOuroborosProtocol p
          => Chain (MockBlock p)
          -> Int
          -> Chain (MockBlock p)
chainFrom initialChain n = go initialChain 0
  where
    go :: KnownOuroborosProtocol p => Chain (MockBlock p) -> Int -> Chain (MockBlock p)
    go acc i | i == n = acc
    go acc i =
      let newHead = mkBlock i (C.head acc)
      in go (acc :> newHead) (i + 1)

    mkBlock :: Int -> Maybe (MockBlock p) -> MockBlock p
    mkBlock currentInt mbHead =
      let body = BlockBody mempty
      in Block {
           blockHeader = mkBlockHeader currentInt mbHead (hashBody body)
         , blockBody   = body
         }
    mkBlockHeader :: Int -> Maybe (MockBlock p) -> BodyHash -> BlockHeader p
    mkBlockHeader slotNo mbHead bHash =
        let hdr = BlockHeader {
             headerHash     = hashHeader hdr
          ,  headerPrevHash = maybe C.genesisHash (hashHeader . blockHeader) mbHead
          ,  headerSlot     = Slot (fromIntegral $! slotNo + 1)
          ,  headerBlockNo  = case mbHead of
                                   Nothing           -> BlockNo 1
                                   Just (Block bh _) -> succ (headerBlockNo bh)
          ,  headerSigner   = BlockSigner 0
          ,  headerBodyHash = bHash
          } in hdr
