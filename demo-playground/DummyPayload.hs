{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}

module DummyPayload (
    DummyPayload(..)
  , fixupBlock
  , chainFrom
  , toChain
  ) where

import           Block
import           Chain      (Chain (..))
import           Infra.Util
import           Ouroboros
import           Serialise

newtype DummyPayload (p :: OuroborosProtocol) = DummyPayload Int deriving Condense

instance Show (DummyPayload p) where
    show (DummyPayload x) = show x

deriving instance Eq (DummyPayload p)

instance Serialise (DummyPayload p) where
    encode  (DummyPayload x) = encodeInt x
    decode  = DummyPayload <$> decodeInt

instance HasHeader DummyPayload where
    blockHash      (DummyPayload x) = HeaderHash x
    blockPrevHash  (DummyPayload x) = HeaderHash (pred x)
    blockSlot      (DummyPayload x) = Slot (toEnum x)
    blockNo        (DummyPayload x) = BlockNo (toEnum x)
    blockSigner    (DummyPayload _) = BlockSigner 0
    blockBodyHash  (DummyPayload x) = BodyHash x
    blockInvariant _ = True

fixupBlock :: Chain (DummyPayload p) -> DummyPayload p -> DummyPayload p
fixupBlock Genesis               _ = DummyPayload 1
fixupBlock (_ :> DummyPayload x) _ = DummyPayload $! x + 1

toChain :: KnownOuroborosProtocol p => [Int] -> Chain (DummyPayload p)
toChain = go Genesis
  where
      go :: Chain (DummyPayload p) -> [Int] -> Chain (DummyPayload p)
      go acc []     = acc
      go acc (x:xs) = go (acc :> (DummyPayload x)) xs

chainFrom :: Chain (DummyPayload p) -> Int -> [DummyPayload p]
chainFrom Genesis               n = [DummyPayload i | i <- [1..n]]
chainFrom (_ :> DummyPayload x) n = [DummyPayload i | i <- [x+1..x+n]]
