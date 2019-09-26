{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Shared by the ChainSync server and client
module Ouroboros.Consensus.ChainSync
  ( Tip (..)
  , encodeTip
  , decodeTip
  ) where

import           Codec.CBOR.Decoding (Decoder, decodeListLenOf)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise (decode, encode)

import           GHC.Generics (Generic)

import           Ouroboros.Network.Block (BlockNo, HeaderHash, Point,
                     decodePoint, encodePoint)

-- | Used to advertise the tip of the server
data Tip b = Tip
  { tipPoint   :: !(Point b)
  , tipBlockNo :: !BlockNo
  } deriving (Eq, Show, Generic)

encodeTip :: (HeaderHash blk -> Encoding)
          -> (Tip        blk -> Encoding)
encodeTip encodeHeaderHash Tip { tipPoint, tipBlockNo } = mconcat
    [ encodeListLen 2
    , encodePoint encodeHeaderHash tipPoint
    , encode                       tipBlockNo
    ]

decodeTip :: (forall s. Decoder s (HeaderHash blk))
          -> (forall s. Decoder s (Tip        blk))
decodeTip decodeHeaderHash = do
  decodeListLenOf 2
  tipPoint    <- decodePoint decodeHeaderHash
  tipBlockNo  <- decode
  return Tip { tipPoint, tipBlockNo }
