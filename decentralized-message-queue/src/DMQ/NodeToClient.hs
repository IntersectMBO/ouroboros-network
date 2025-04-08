{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module DMQ.NodeToClient where

import Codec.CBOR.Term qualified as CBOR
import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Data.Bits (Bits (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Control.Monad.Class.MonadST (MonadST)
import Control.Tracer (Tracer)
import Ouroboros.Network.CodecCBORTerm (CodecCBORTerm (..))
import Ouroboros.Network.ConnectionId (ConnectionId)
import Ouroboros.Network.Handshake.Acceptable (Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.NodeToClient (HandshakeTr)
import Ouroboros.Network.Protocol.Handshake (Accept (..),
           HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
           codecHandshake, noTimeLimitsHandshake)

data NodeToClientVersion =
  NodeToClientV_1
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, NFData)

nodeToClientVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToClientVersion
nodeToClientVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
    where
      encodeTerm = \case
          NodeToClientV_1 -> enc 1
        where
          enc :: Int -> CBOR.Term
          enc = CBOR.TInt . (`setBit` nodeToClientVersionBit)

      decodeTerm =
          dec >=> \case
            1 -> Right NodeToClientV_1
            n  -> Left (unknownTag n)
        where
          dec :: CBOR.Term -> Either (Text, Maybe Int) Int
          dec (CBOR.TInt x) | x `testBit` nodeToClientVersionBit
                            = Right (x `clearBit` nodeToClientVersionBit)
                            | otherwise
                            = Left (unknownTag x)
          dec _             = Left ( T.pack "decode NodeToClientVersion: unexpected term"
                                   , Nothing
                                   )

          unknownTag x = ( T.pack "decode NodeToClientVersion: unknown tag: " <> T.pack (show x), Just x)

      -- The 16th bit to distinguish `NodeToNodeVersion` and `NodeToClientVersion`.
      -- This is different than the one defined in ouroboros-network.
      nodeToClientVersionBit :: Int
      nodeToClientVersionBit = 12

-- | Version data for NodeToClient protocol v1
--
-- This data type is inpired by the one defined in 'ouroboros-network-api',
-- however, it is redefined here to tie it to our custom `NodeToClientVersion`
-- and to avoid divergences.
--
data NodeToClientVersionData = NodeToClientVersionData
  { networkMagic :: !NetworkMagic
  , query        :: !Bool
  }
  deriving (Eq, Show)

instance Acceptable NodeToClientVersionData where
    acceptableVersion local remote
      | networkMagic local == networkMagic remote
      = Accept NodeToClientVersionData
          { networkMagic  = networkMagic local
          , query         = query local || query remote
          }
      | otherwise =  Refuse $ T.pack $ "version data mismatch: "
                                    ++ show local
                                    ++ " /= " ++ show remote

instance Queryable NodeToClientVersionData where
    queryVersion = query

nodeToClientCodecCBORTerm :: NodeToClientVersion -> CodecCBORTerm Text NodeToClientVersionData
nodeToClientCodecCBORTerm _v = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToClientVersionData -> CBOR.Term
      encodeTerm NodeToClientVersionData { networkMagic, query }
        = CBOR.TList [CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic), CBOR.TBool query]

      decodeTerm :: CBOR.Term -> Either Text NodeToClientVersionData
      decodeTerm (CBOR.TList [CBOR.TInt x, CBOR.TBool query])
        = decoder x query
      decodeTerm t
        = Left $ T.pack $ "unknown encoding: " ++ show t

      decoder :: Int -> Bool -> Either Text NodeToClientVersionData
      decoder x query | x >= 0 && x <= 0xffffffff = Right (NodeToClientVersionData (NetworkMagic $ fromIntegral x) query)
                      | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x

data Protocols =
  Protocols {
  }

ntcHandshakeArguments
  :: MonadST m
  => Tracer m (HandshakeTr ntcAddr NodeToClientVersion)
  -> HandshakeArguments
      (ConnectionId ntcAddr)
      NodeToClientVersion
      NodeToClientVersionData
      m
ntcHandshakeArguments tracer =
  HandshakeArguments {
    haHandshakeTracer  = tracer
  , haHandshakeCodec   = codecHandshake nodeToClientVersionCodec
  , haVersionDataCodec =
      cborTermVersionDataCodec
        nodeToClientCodecCBORTerm
  , haAcceptVersion = acceptableVersion
  , haQueryVersion  = queryVersion
  , haTimeLimits    = noTimeLimitsHandshake
  }

stdVersionDataNTC :: NetworkMagic -> NodeToClientVersionData
stdVersionDataNTC networkMagic =
  NodeToClientVersionData
    { networkMagic
    , query        = False
    }
