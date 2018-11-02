{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module DummyPayload (
    DummyPayload(..)
  , fixupBlock
  , chainFrom
  , toChain
  ) where

import           Block
import           Block.Concrete hiding (fixupBlock)
import           Chain (Chain (..))
import           Infra.Util
import           Serialise

newtype DummyPayload = DummyPayload Int deriving Condense

instance Show DummyPayload where
    show (DummyPayload x) = show x

deriving instance Eq DummyPayload

instance Serialise DummyPayload where
    encode  (DummyPayload x) = encodeInt x
    decode  = DummyPayload <$> decodeInt

-- TODO: For now this uses the representation from Block.Concrete
-- There is no need for this.
instance HasHeader DummyPayload where
    type HeaderHash DummyPayload = ConcreteHeaderHash

    blockHash      (DummyPayload x) = HeaderHash x
    blockPrevHash  (DummyPayload x) = HeaderHash (pred x)
    blockSlot      (DummyPayload x) = Slot (toEnum x)
    blockNo        (DummyPayload x) = BlockNo (toEnum x)
    blockInvariant _ = True

    genesisHash _ = HeaderHash 0

fixupBlock :: Chain DummyPayload -> DummyPayload -> DummyPayload
fixupBlock Genesis               _ = DummyPayload 1
fixupBlock (_ :> DummyPayload x) _ = DummyPayload $! x + 1

toChain :: [Int] -> Chain (DummyPayload)
toChain = go Genesis
  where
      go :: Chain DummyPayload -> [Int] -> Chain (DummyPayload)
      go acc []     = acc
      go acc (x:xs) = go (acc :> (DummyPayload x)) xs

chainFrom :: Chain DummyPayload -> Int -> [DummyPayload]
chainFrom Genesis               n = [DummyPayload i | i <- [1..n]]
chainFrom (_ :> DummyPayload x) n = [DummyPayload i | i <- [x+1..x+n]]
