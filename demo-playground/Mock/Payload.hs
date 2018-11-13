{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Mock.Payload (
      SimpleBlock(..) -- re-export
    , fixupBlock
    , chainFrom
    , toChain
    , addTxs
    ) where

import           Data.Set (Set)

import qualified Ouroboros.Consensus.Crypto.Hash as H
import           Ouroboros.Consensus.Ledger.Mock (SimpleBlock (..),
                     SimpleBlockHash (..), SimpleBody (..), SimpleHeader (..),
                     SimplePreHeader (..), Tx)
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Network.Block (BlockNo (..), Slot (..), castHash)
import qualified Ouroboros.Network.Block as Network
import           Ouroboros.Network.Chain (Chain (..), headBlockNo, headHash)
import qualified Ouroboros.Network.Chain as C
import           Ouroboros.Network.Serialise

fixupBlock :: forall p c.
           ( SimpleBlockCrypto c
           , Serialise (OuroborosPayload p (SimplePreHeader p c))
           )
           => Chain (SimpleBlock p c)
           -> SimpleBlock p c
           -> SimpleBlock p c
fixupBlock c (SimpleBlock header body) =
    let bodyHash  = H.hash body
        preHeader = headerPreHeader header
    in SimpleBlock {
        simpleHeader = header { headerPreHeader = fixupPreHeader bodyHash preHeader }
      , simpleBody   = body
      }
  where
    fixupPreHeader :: ( SimpleBlockCrypto c
                      , Serialise (OuroborosPayload p (SimplePreHeader p c))
                      )
                   => H.Hash (SimpleBlockHash c) SimpleBody
                   -> SimplePreHeader p c
                   -> SimplePreHeader p c
    fixupPreHeader h b = b'
      where
        b' = SimplePreHeader {
          headerPrev     = castHash (headHash c)
        , headerSlot     = headerSlot b
        , headerBlockNo  = succ $ headBlockNo c
        , headerBodyHash = h
        }


toChain :: ( SimpleBlockCrypto c
           , Serialise (OuroborosPayload p (SimplePreHeader p c))
           )
         => [Int] -> Chain (SimpleBlock p c)
toChain = C.fromNewestFirst
        . reverse
        . chainWithSlots (C.headHash Genesis) C.genesisBlockNo
        . map (Slot . fromIntegral)

-- | Dummy chain with empty bodies in the specified slots
chainWithSlots :: forall p c.
               ( SimpleBlockCrypto c
               , Serialise (OuroborosPayload p (SimplePreHeader p c))
               )
               => Network.Hash (SimpleHeader p c)
               -> BlockNo
               -> [Slot]
               -> [SimpleBlock p c]
chainWithSlots _        _      []     = []
chainWithSlots prevHash prevNo (s:ss) =
    let block = mkBlock s
    in block : chainWithSlots (Network.BlockHash (C.blockHash block)) (C.blockNo block) ss
  where
    mkBlock :: SimpleBlockCrypto c => Slot -> SimpleBlock p c
    mkBlock slot = SimpleBlock {
          simpleHeader = mkBlockHeader slot (H.hash body)
        , simpleBody   = body
        }
      where
        body = SimpleBody mempty

    mkBlockHeader :: SimpleBlockCrypto c
                  => Slot
                  -> H.Hash (SimpleBlockHash c) SimpleBody
                  -> SimpleHeader p c
    mkBlockHeader slot bHash =
        let ph = SimplePreHeader {
                 headerPrev     = prevHash
               , headerSlot     = slot
               , headerBlockNo  = succ prevNo
               , headerBodyHash = bHash
               }
        in SimpleHeader {
             headerPreHeader = ph
           , headerOuroboros = undefined -- mkOuroborosPayload proofLeader ph
           }

chainFrom :: ( SimpleBlockCrypto c
             , Serialise (OuroborosPayload p (SimplePreHeader p c))
             )
          => Chain (SimpleBlock p c)
          -> Int
          -> [SimpleBlock p c]
chainFrom c n =
    chainWithSlots (castHash . C.headHash $ c)
                   (C.headBlockNo c)
                   [Slot (headSlot + s) | s <- [1 .. fromIntegral n]]
  where
    Slot headSlot = C.headSlot c

addTxs :: Set Tx
       -> (SimpleBlock p c)
       -> (SimpleBlock p c)
addTxs txs b@(SimpleBlock _ (SimpleBody t)) =
    b { simpleBody = SimpleBody (txs <> t) }
