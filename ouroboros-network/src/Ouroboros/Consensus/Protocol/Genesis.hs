{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.Protocol.Genesis (
    Genesis
  ) where

import           Data.List (foldl')
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.ModChainSel
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util.Chain (forksAtMostKBlocks, intersectionSlot, upToSlot)
import           Ouroboros.Network.Block (Slot (..))
import qualified Ouroboros.Network.Chain as Chain

type Genesis c = ExtNodeConfig Natural (Praos c)

data GenesisChainSelection c

instance PraosCrypto c => ChainSelection (GenesisChainSelection c) where

    type Protocol (GenesisChainSelection c) = Genesis c

    selectChain' _ EncNodeConfig{..} slot ours = foldl' f ours . map (upToSlot slot)
      where
        f cmax c
            | forksAtMostKBlocks k cmax c = if Chain.length c > Chain.length cmax
                                                then c
                                                else cmax
            | otherwise                   =
                let sl = case intersectionSlot cmax c of
                            Nothing -> Slot s
                            Just j  -> Slot $ s + getSlot j
                    cmax' = upToSlot sl cmax
                    c'    = upToSlot sl c
                in  if Chain.length c' > Chain.length cmax'
                        then c
                        else cmax

        k :: Int
        k = fromIntegral $ praosK encNodeConfigP

        s :: Word
        s = fromIntegral encNodeConfigExt
