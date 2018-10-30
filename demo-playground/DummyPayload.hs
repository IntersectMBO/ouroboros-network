{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module DummyPayload (
    DummyPayload(..)
  , fixupBlock
  ) where

import           Block
import           Chain     (Chain (..))
import           Ouroboros
import           Serialise

data DummyPayload (p :: OuroborosProtocol) = DummyPayload Int

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
