{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
module MockPayload (
      MockBlock
    , fixupBlock
    , chainFrom
    , toChain
    ) where

import           Block hiding (BlockBody)
import           Block.Mock (BlockBody (..))
import           Chain (Chain (..))
import qualified Chain as C
import           Ouroboros

type MockBlock p = Block 'MockLedgerDomain p

fixupBlock :: Chain (MockBlock p) -> MockBlock p -> MockBlock p
fixupBlock = C.fixupBlock

toChain :: KnownOuroborosProtocol p => [Int] -> Chain (MockBlock p)
toChain = undefined -- foldl' (\c i -> chainFrom c i) Genesis

chainFrom :: forall p. KnownOuroborosProtocol p
          => Chain (MockBlock p)
          -> Int
          -> [MockBlock p]
chainFrom = \startingChain n ->
    go (C.headHash    startingChain)
       (C.headBlockNo startingChain)
       (  headSlot    startingChain)
       n
  where
    go :: HeaderHash -> BlockNo -> Slot -> Int -> [MockBlock p]
    go _       _       _        0    = []
    go prevHash prevNo prevSlot todo =
          block
        : go (blockHash block) (blockNo block) (blockSlot block) (todo - 1)
      where
        block = mkBlock prevHash prevNo prevSlot

    mkBlock :: HeaderHash -> BlockNo -> Slot -> MockBlock p
    mkBlock prevHash prevNo prevSlot = Block {
          blockHeader = mkBlockHeader prevHash prevNo prevSlot (hashBody body)
        , blockBody   = body
        }
      where
        body = BlockBody mempty

    mkBlockHeader :: HeaderHash -> BlockNo -> Slot -> BodyHash -> BlockHeader p
    mkBlockHeader prevHash prevNo prevSlot bHash =
        let hdr = BlockHeader {
             headerHash     = hashHeader hdr
          ,  headerPrevHash = prevHash
          ,  headerSlot     = succ prevSlot
          ,  headerBlockNo  = succ prevNo
          ,  headerSigner   = BlockSigner 0
          ,  headerBodyHash = bHash
          } in hdr

headSlot :: HasHeader block => Chain block -> Slot
headSlot Genesis  = Slot 0
headSlot (_ :> b) = blockSlot b
