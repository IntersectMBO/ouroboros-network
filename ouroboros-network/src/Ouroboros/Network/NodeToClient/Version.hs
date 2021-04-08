
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.NodeToClient.Version
  ( NodeToClientVersion (..)
  , NodeToClientVersionData (..)
  , nodeToClientCodecCBORTerm
  , nodeToClientVersionCodec
  ) where

import           Data.Bits (clearBit, setBit, testBit)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import qualified Codec.CBOR.Term as CBOR

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Protocol.Handshake.Version (Accept (..),
                     Acceptable (..))


-- | Enumeration of node to client protocol versions.
--
data NodeToClientVersion
    = NodeToClientV_1
    | NodeToClientV_2
    -- ^ added local-query mini-protocol
    | NodeToClientV_3
    -- ^ enabled @CardanoNodeToClientVersion2@
    | NodeToClientV_4
    -- ^ enabled @CardanoNodeToClientVersion3@, adding more queries
    | NodeToClientV_5
    -- ^ enabled @CardanoNodeToClientVersion4@, i.e., Allegra
    | NodeToClientV_6
    -- ^ enabled @CardanoNodeToClientVersion5@, i.e., Mary
    | NodeToClientV_7
    -- ^ enabled @CardanoNodeToClientVersion6@, adding a query
    | NodeToClientV_8
    -- ^ 'LocalStateQuery' protocol codec change, allows to acquire tip point.
    | NodeToClientV_9
    -- ^ 'LocalStateQuery' protocol codec change, queries are now
    -- wrapped in a top level 'Query blk' Type.
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

-- | We set 16ths bit to distinguish `NodeToNodeVersion` and
-- `NodeToClientVersion`.  This way connectin wrong protocol suite will fail
-- during `Handshake` negotation
--
-- This is done in backward compatible way, so `NodeToClientV_1` encoding is not
-- changed.
--
nodeToClientVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToClientVersion
nodeToClientVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
    where
      encodeTerm NodeToClientV_1 = CBOR.TInt 1
      encodeTerm NodeToClientV_2 = CBOR.TInt (2 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_3 = CBOR.TInt (3 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_4 = CBOR.TInt (4 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_5 = CBOR.TInt (5 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_6 = CBOR.TInt (6 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_7 = CBOR.TInt (7 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_8 = CBOR.TInt (8 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_9 = CBOR.TInt (9 `setBit` nodeToClientVersionBit)

      decodeTerm (CBOR.TInt tag) =
       case ( tag `clearBit` nodeToClientVersionBit
            , tag `testBit`  nodeToClientVersionBit
            ) of
        (1, False) -> Right NodeToClientV_1
        (2, True)  -> Right NodeToClientV_2
        (3, True)  -> Right NodeToClientV_3
        (4, True)  -> Right NodeToClientV_4
        (5, True)  -> Right NodeToClientV_5
        (6, True)  -> Right NodeToClientV_6
        (7, True)  -> Right NodeToClientV_7
        (8, True)  -> Right NodeToClientV_8
        (9, True)  -> Right NodeToClientV_9
        (n, _)     -> Left ( T.pack "decode NodeToClientVersion: unknown tag: " <> T.pack (show tag)
                            , Just n)
      decodeTerm _  = Left ( T.pack "decode NodeToClientVersion: unexpected term"
                           , Nothing)


nodeToClientVersionBit :: Int
nodeToClientVersionBit = 15


-- | Version data for NodeToClient protocol v1
--
newtype NodeToClientVersionData = NodeToClientVersionData
  { networkMagic :: NetworkMagic }
  deriving (Eq, Show, Typeable)

instance Acceptable NodeToClientVersionData where
    acceptableVersion local remote
      | local == remote
      = Accept local
      | otherwise =  Refuse $ T.pack $ "version data mismatch: "
                                    ++ show local
                                    ++ " /= " ++ show remote

nodeToClientCodecCBORTerm :: NodeToClientVersion -> CodecCBORTerm Text NodeToClientVersionData
nodeToClientCodecCBORTerm _ = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToClientVersionData -> CBOR.Term
      encodeTerm NodeToClientVersionData { networkMagic } =
        CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

      decodeTerm :: CBOR.Term -> Either Text NodeToClientVersionData
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (NodeToClientVersionData $ NetworkMagic $ fromIntegral x)
                               | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t
