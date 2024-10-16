{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.NodeToClient.Version
  ( Version (..)
  , VersionData (..)
  , codecCBORTerm
  , versionCodec
  ) where

import Codec.CBOR.Term qualified as CBOR
import Control.DeepSeq
import Data.Bits (clearBit, setBit, testBit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import GHC.Generics
import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Handshake.Acceptable (Accept (..), Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.Magic


-- | Enumeration of node to client protocol versions.
--
data Version
    = V_9
    -- ^ enabled @CardanoNodeToClientVersion7@, i.e., Alonzo
    | V_10
    -- ^ added 'GetChainBlockNo' and 'GetChainPoint' queries
    | V_11
    -- ^ added 'GetRewardInfoPools` Block query
    | V_12
    -- ^ added 'LocalTxMonitor' mini-protocol
    | V_13
    -- ^ enabled @CardanoNodeToClientVersion9@, i.e., Babbage
    | V_14
    -- ^ added @GetPoolDistr@, @GetPoolState@, @GetSnapshots@
    | V_15
    -- ^ added `query` to NodeToClientVersionData
    | V_16
    -- ^ added @ImmutableTip@ to @LocalStateQuery@, enabled
    -- @CardanoNodeToClientVersion11@, i.e., Conway and
    -- @GetStakeDelegDeposits@.
    | V_17
    -- ^ added @GetProposals@ and @GetRatifyState@ queries
    | V_18
    -- ^ added @GetFuturePParams@ query
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Generic, NFData)

-- | We set 16ths bit to distinguish `NodeToNodeVersion` and
-- `NodeToClientVersion`.  This way connecting wrong protocol suite will fail
-- during `Handshake` negotiation
--
-- This is done in backward compatible way, so `NodeToClientV_1` encoding is not
-- changed.
--
versionCodec :: CodecCBORTerm (Text, Maybe Int) Version
versionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
    where
      encodeTerm V_9  = CBOR.TInt (9  `setBit` nodeToClientVersionBit)
      encodeTerm V_10 = CBOR.TInt (10 `setBit` nodeToClientVersionBit)
      encodeTerm V_11 = CBOR.TInt (11 `setBit` nodeToClientVersionBit)
      encodeTerm V_12 = CBOR.TInt (12 `setBit` nodeToClientVersionBit)
      encodeTerm V_13 = CBOR.TInt (13 `setBit` nodeToClientVersionBit)
      encodeTerm V_14 = CBOR.TInt (14 `setBit` nodeToClientVersionBit)
      encodeTerm V_15 = CBOR.TInt (15 `setBit` nodeToClientVersionBit)
      encodeTerm V_16 = CBOR.TInt (16 `setBit` nodeToClientVersionBit)
      encodeTerm V_17 = CBOR.TInt (17 `setBit` nodeToClientVersionBit)
      encodeTerm V_18 = CBOR.TInt (18 `setBit` nodeToClientVersionBit)

      decodeTerm (CBOR.TInt tag) =
       case ( tag `clearBit` nodeToClientVersionBit
            , tag `testBit`  nodeToClientVersionBit
            ) of
        (9, True)  -> Right V_9
        (10, True) -> Right V_10
        (11, True) -> Right V_11
        (12, True) -> Right V_12
        (13, True) -> Right V_13
        (14, True) -> Right V_14
        (15, True) -> Right V_15
        (16, True) -> Right V_16
        (17, True) -> Right V_17
        (18, True) -> Right V_18
        (n, _)     -> Left ( T.pack "decode NodeToClientVersion: unknown tag: " <> T.pack (show tag)
                            , Just n)
      decodeTerm _  = Left ( T.pack "decode NodeToClientVersion: unexpected term"
                           , Nothing)

      -- The 16th bit to distinguish `NodeToNodeVersion` and `NodeToClientVersion`.
      nodeToClientVersionBit :: Int
      nodeToClientVersionBit = 15


-- | Version data for NodeToClient protocol v1
--
data VersionData = VersionData
  { networkMagic :: !NetworkMagic
  , query        :: !Bool
  }
  deriving (Eq, Show, Typeable)

instance Acceptable VersionData where
    acceptableVersion local remote
      | networkMagic local == networkMagic remote
      = Accept VersionData
          { networkMagic  = networkMagic local
          , query         = query local || query remote
          }
      | otherwise =  Refuse $ T.pack $ "version data mismatch: "
                                    ++ show local
                                    ++ " /= " ++ show remote

instance Queryable VersionData where
    queryVersion = query

codecCBORTerm :: Version -> CodecCBORTerm Text VersionData
codecCBORTerm v = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: VersionData -> CBOR.Term
      encodeTerm VersionData { networkMagic, query }
        | v >= V_15
        = CBOR.TList [CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic), CBOR.TBool query]
        | otherwise
        = CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

      decodeTerm :: CBOR.Term -> Either Text VersionData
      decodeTerm (CBOR.TList [CBOR.TInt x, CBOR.TBool query])
        | v >= V_15
        = decoder x query
      decodeTerm (CBOR.TInt x)
        | v < V_15
        = decoder x False
      decodeTerm t
        = Left $ T.pack $ "unknown encoding: " ++ show t

      decoder :: Int -> Bool -> Either Text VersionData
      decoder x query | x >= 0 && x <= 0xffffffff = Right (VersionData (NetworkMagic $ fromIntegral x) query)
                      | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
