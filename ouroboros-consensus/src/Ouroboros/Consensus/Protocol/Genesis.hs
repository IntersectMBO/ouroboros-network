{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.Protocol.Genesis (
    HasK (..)
  , Genesis
  ) where

import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.ModChainSel
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util.Chain (forksAtMostKBlocks,
                     intersectionSlot, upToSlot)
import           Ouroboros.Network.Block (Slot (..))
import qualified Ouroboros.Network.Chain as Chain

class OuroborosTag p => HasK p where
    getK :: NodeConfig p -> Word

type Genesis p = ModChainSel (ExtNodeConfig Natural p) (GenesisChainSelection p)

data GenesisChainSelection p

instance HasK p => ChainSelection (ExtNodeConfig Natural p) (GenesisChainSelection p) where

    compareChain' _ EncNodeConfig{..} slot ours theirs
        | forksAtMostKBlocks k ours' theirs' = compareChain encNodeConfigP slot ours theirs
        | otherwise                          =
            let sl    = case intersectionSlot ours' theirs' of
                            Nothing -> Slot s
                            Just j  -> Slot $ s + getSlot j
                ours''   = upToSlot sl ours'
                theirs'' = upToSlot sl theirs'
            in  if Chain.length theirs'' > Chain.length ours''
                    then Theirs
                    else Ours
      where
        clip = upToSlot slot

        ours'   = clip ours
        theirs' = clip theirs

        k :: Int
        k = fromIntegral $ getK encNodeConfigP

        s :: Word
        s = fromIntegral encNodeConfigExt

instance PraosCrypto c => HasK (Praos c) where
    getK = praosK . praosParams
