{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.NodeToNode.Version
  ( NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , nodeToNodeVersionCodec
  , nodeToNodeCodecCBORTerm
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import qualified Codec.CBOR.Term as CBOR

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Protocol.Handshake.Version (Acceptable (..), Accept (..))


-- | Enumeration of node to node protocol versions.
--
data NodeToNodeVersion
    = NodeToNodeV_1
    | NodeToNodeV_2
    -- ^ Changes:
    --
    -- * Enable block size hints for Byron headers in ChainSync
    --
    -- * Enable @CardanoNodeToNodeVersion2@
    | NodeToNodeV_3
    -- ^ Changes:
    --
    -- * Enable KeepAlive miniprotocol
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

nodeToNodeVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToNodeVersion
nodeToNodeVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm NodeToNodeV_1  = CBOR.TInt 1
    encodeTerm NodeToNodeV_2  = CBOR.TInt 2
    encodeTerm NodeToNodeV_3  = CBOR.TInt 3

    decodeTerm (CBOR.TInt 1) = Right NodeToNodeV_1
    decodeTerm (CBOR.TInt 2) = Right NodeToNodeV_2
    decodeTerm (CBOR.TInt 3) = Right NodeToNodeV_3
    decodeTerm (CBOR.TInt n) = Left ( T.pack "decode NodeToNodeVersion: unknonw tag: "
                                        <> T.pack (show n)
                                    , Just n
                                    )
    decodeTerm _ = Left ( T.pack "decode NodeToNodeVersion: unexpected term"
                        , Nothing)


-- | Version data for NodeToNode protocol v1
--
newtype NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic :: NetworkMagic }
  deriving (Eq, Show, Typeable)

instance Acceptable NodeToNodeVersionData where
    acceptableVersion local remote
      | local == remote
      = Accept
      | otherwise
      =  Refuse $ T.pack $ "version data mismatch: "
                        ++ show local
                        ++ " /= " ++ show remote

nodeToNodeCodecCBORTerm :: CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToNodeVersionData -> CBOR.Term
      encodeTerm NodeToNodeVersionData { networkMagic } =
        CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

      decodeTerm :: CBOR.Term -> Either Text NodeToNodeVersionData
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (NodeToNodeVersionData $ NetworkMagic $ fromIntegral x)
                               | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t
