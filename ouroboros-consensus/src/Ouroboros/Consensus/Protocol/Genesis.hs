{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.Protocol.Genesis (
    Genesis
  ) where

import           Numeric.Natural (Natural)
import           Data.Word (Word64)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.ModChainSel
import           Ouroboros.Consensus.Util.Chain (forksAtMostKBlocks,
                     intersectionSlot, upToSlot)
import           Ouroboros.Network.Block (SlotNo (..))
import qualified Ouroboros.Network.Chain as Chain

type Genesis p = ModChainSel (ExtNodeConfig Natural p) (GenesisChainSelection p)

data GenesisChainSelection p

instance OuroborosTag p => ChainSelection (ExtNodeConfig Natural p) (GenesisChainSelection p) where

    preferCandidate' _ EncNodeConfig{..} slot ours theirs
        | forksAtMostKBlocks k ours' theirs' = preferCandidate encNodeConfigP slot ours theirs
        | otherwise                          =
            let sl    = case intersectionSlot ours' theirs' of
                            Nothing -> SlotNo s
                            Just j  -> SlotNo $ s + unSlotNo j
                ours''   = upToSlot sl ours'
                theirs'' = upToSlot sl theirs'
            in if Chain.length theirs'' > Chain.length ours''
                 then Just theirs''
                 else Nothing
      where
        clip = upToSlot slot

        ours'   = clip ours
        theirs' = clip theirs

        k :: Word64
        k = maxRollbacks $ protocolSecurityParam encNodeConfigP

        s :: Word64
        s = fromIntegral encNodeConfigExt

    compareCandidates' = error "TODO"
