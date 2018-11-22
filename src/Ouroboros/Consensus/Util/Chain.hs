-- | Utility functions on chains
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Util.Chain as Chain
module Ouroboros.Consensus.Util.Chain (
    lastSlot
  , commonPrefix
  , upToSlot
  ) where

import           Data.Foldable (foldl')
import           Data.Sequence (Seq (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain

{-------------------------------------------------------------------------------
  Utility functions on chains
-------------------------------------------------------------------------------}

lastSlot :: HasHeader b => Chain b -> Maybe Slot
lastSlot Genesis  = Nothing
lastSlot (_ :> b) = Just $ blockSlot b

commonPrefix :: Eq b => Chain b -> Chain b -> Chain b
commonPrefix c d = chainFromSeq $ go (chainToSeq c) (chainToSeq d)
  where
    go :: Eq b => Seq b -> Seq b -> Seq b
    go Empty      _          = Empty
    go _          Empty      = Empty
    go (x :<| xs) (y :<| ys)
        | x == y             = x :<| go xs ys
        | otherwise          = Empty

upToSlot :: HasHeader b => Slot -> Chain b -> Chain b
upToSlot slot = go
  where
    go Genesis = Genesis
    go bs@(cs :> b)
        | blockSlot b <= slot = bs
        | otherwise           = go cs

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

chainFromSeq :: Seq b -> Chain b
chainFromSeq = foldl' (:>) Genesis

chainToSeq :: Chain b -> Seq b
chainToSeq = foldChain (:|>) Empty
