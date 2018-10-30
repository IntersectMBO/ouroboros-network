{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}

module DummyPayload (
    DummyPayload(..)
  , fixupBlock
  , chainFrom
  ) where

import           Block
import           Chain      (Chain (..), addBlock)
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

chainFrom :: Chain (DummyPayload p) -> Int -> Chain (DummyPayload p)
chainFrom Genesis n =
    foldl (\acc b -> addBlock (DummyPayload b) acc) Genesis [1..n]
chainFrom c@(_ :> DummyPayload x) n =
    foldl (\acc b -> addBlock (DummyPayload b) acc) c [x+1..x+n]
