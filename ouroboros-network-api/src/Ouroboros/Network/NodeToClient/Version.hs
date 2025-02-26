{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.NodeToClient.Version
  ( NodeToClientVersion (..)
  , NodeToClientVersionData (..)
  , nodeToClientCodecCBORTerm
  , nodeToClientVersionCodec
  ) where

import Codec.CBOR.Term qualified as CBOR
import Control.DeepSeq
import Control.Monad ((>=>))
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
data NodeToClientVersion
    -- = NodeToClientV_9
    -- -- ^ enabled @CardanoNodeToClientVersion7@, i.e., Alonzo
    -- | NodeToClientV_10
    -- -- ^ added 'GetChainBlockNo' and 'GetChainPoint' queries
    -- | NodeToClientV_11
    -- -- ^ added 'GetRewardInfoPools` Block query
    -- | NodeToClientV_12
    -- -- ^ added 'LocalTxMonitor' mini-protocol
    -- | NodeToClientV_13
    -- -- ^ enabled @CardanoNodeToClientVersion9@, i.e., Babbage
    -- | NodeToClientV_14
    -- -- ^ added @GetPoolDistr@, @GetPoolState@, @GetSnapshots@
    -- | NodeToClientV_15
    -- -- ^ added `query` to NodeToClientVersionData
    = NodeToClientV_16
    -- ^ added @ImmutableTip@ to @LocalStateQuery@, enabled
    -- @CardanoNodeToClientVersion11@, i.e., Conway and
    -- @GetStakeDelegDeposits@.
    | NodeToClientV_17
    -- ^ added @GetProposals@ and @GetRatifyState@ queries
    | NodeToClientV_18
    -- ^ added @GetFuturePParams@ query
    | NodeToClientV_19
    -- ^ added @GetLedgerPeerSnapshot@
    | NodeToClientV_20
    -- ^ added @QueryStakePoolDefaultVote@,
    -- added @MsgGetMeasures@ / @MsgReplyGetMeasures@ to @LocalTxMonitor@
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Generic, NFData)

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
      encodeTerm = \case
          NodeToClientV_16 -> enc 16
          NodeToClientV_17 -> enc 17
          NodeToClientV_18 -> enc 18
          NodeToClientV_19 -> enc 19
          NodeToClientV_20 -> enc 20
        where
          enc :: Int -> CBOR.Term
          enc = CBOR.TInt . (`setBit` nodeToClientVersionBit)

      decodeTerm =
          dec >=> \case
            16 -> Right NodeToClientV_16
            17 -> Right NodeToClientV_17
            18 -> Right NodeToClientV_18
            19 -> Right NodeToClientV_19
            20 -> Right NodeToClientV_20
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
      nodeToClientVersionBit :: Int
      nodeToClientVersionBit = 15


-- | Version data for NodeToClient protocol v1
--
data NodeToClientVersionData = NodeToClientVersionData
  { networkMagic :: !NetworkMagic
  , query        :: !Bool
  }
  deriving (Eq, Show, Typeable)

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
