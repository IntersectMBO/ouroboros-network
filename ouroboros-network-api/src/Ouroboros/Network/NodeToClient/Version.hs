{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Ouroboros.Network.NodeToClient.Version
  ( NodeToClientVersion (..)
  , NodeToClientVersionData (..)
  , nodeToClientVersionCodec
  , nodeToClientCodecCBORTerm
  ) where

import           Data.Bits (clearBit, setBit, testBit)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Enum.Min

import           GHC.Generics
import           Generics.SOP.TH

import qualified Codec.CBOR.Term as CBOR

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Handshake.Acceptable (Accept (..),
                     Acceptable (..))
import           Ouroboros.Network.Magic


-- | Enumeration of node to client protocol versions.
--
data NodeToClientVersion
    = NodeToClientV_9
    -- ^ enabled @CardanoNodeToClientVersion7@, i.e., Alonzo
    | NodeToClientV_10
    -- ^ added 'GetChainBlockNo' and 'GetChainPoint' queries
    | NodeToClientV_11
    -- ^ added 'GetRewardInfoPools` Block query
    | NodeToClientV_12
    -- ^ added 'LocalTxMonitor' mini-protocol
    | NodeToClientV_13
    -- ^ enabled @CardanoNodeToClientVersion9@, i.e., Babbage
    | NodeToClientV_14
    -- ^ added @GetPoolDistr, @GetPoolState, @GetSnapshots
  deriving (Eq, Ord, Bounded, Show, Typeable, Generic)

deriveGeneric ''NodeToClientVersion

deriving via MinEnum 9 NodeToClientVersion instance Enum NodeToClientVersion

-- | We set 16ths bit to distinguish `NodeToNodeVersion` and
-- `NodeToClientVersion`.  This way connecting wrong protocol suite will fail
-- during `Handshake` negotiation
--
-- This is done in backward compatible way, so `NodeToClientV_1` encoding is not
-- changed.
--
nodeToClientVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToClientVersion
nodeToClientVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
    where
      encodeTerm NodeToClientV_9  = CBOR.TInt (9  `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_10 = CBOR.TInt (10 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_11 = CBOR.TInt (11 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_12 = CBOR.TInt (12 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_13 = CBOR.TInt (13 `setBit` nodeToClientVersionBit)
      encodeTerm NodeToClientV_14 = CBOR.TInt (14 `setBit` nodeToClientVersionBit)

      decodeTerm (CBOR.TInt tag) =
       case ( tag `clearBit` nodeToClientVersionBit
            , tag `testBit`  nodeToClientVersionBit
            ) of
        (9, True)  -> Right NodeToClientV_9
        (10, True) -> Right NodeToClientV_10
        (11, True) -> Right NodeToClientV_11
        (12, True) -> Right NodeToClientV_12
        (13, True) -> Right NodeToClientV_13
        (14, True) -> Right NodeToClientV_14
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
