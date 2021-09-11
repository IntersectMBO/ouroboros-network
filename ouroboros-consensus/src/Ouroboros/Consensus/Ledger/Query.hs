{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Ouroboros.Consensus.Ledger.Query (
    BlockQuery
  , ConfigSupportsNode (..)
  , HeaderStateTip (..)
  , Query (..)
  , QueryLedger (..)
  , QueryVersion (..)
  , ShowQuery (..)
  , answerQuery
  , nodeToClientVersionToQueryVersion
  , queryDecodeNodeToClient
  , queryEncodeNodeToClient
  ) where

import           GHC.Generics (Generic)

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))

import           Control.Exception (Exception, throw)
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Data.Typeable (Typeable)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.Serialise (Serialise (..))

import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))

import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query.Version
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..))
import           Ouroboros.Consensus.Util (ShowProxy (..), SomeSecond (..))
import           Ouroboros.Consensus.Util.DepPair

import           Ouroboros.Consensus.HeaderValidation (AnnTip (..),
                     HasAnnTip (..), HeaderState (..))

import           Ouroboros.Network.Block (HeaderHash)


{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Different queries supported by the ledger for all block types, indexed
-- by the result type.
--
-- Additions to the set of queries is versioned by 'QueryVersion'
data Query blk result where
  -- | This constructor is supported by all @QueryVersion@s. The @BlockQuery@
  -- argument is versioned by the @BlockNodeToClientVersion blk@.
  BlockQuery :: BlockQuery blk result -> Query blk result

  -- | Get the 'SystemStart' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion1'.
  GetSystemStart :: Query blk SystemStart

  -- | Get the 'GetHeaderStateTip' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetHeaderStateTip :: Query blk (WithOrigin (HeaderStateTip blk))


instance (ShowProxy (BlockQuery blk)) => ShowProxy (Query blk) where
  showProxy (Proxy :: Proxy (Query blk)) = "Query (" ++ showProxy (Proxy @(BlockQuery blk)) ++ ")"

instance (ShowQuery (BlockQuery blk), Show (HeaderHash blk)) => ShowQuery (Query blk) where
  showResult (BlockQuery blockQuery) = showResult blockQuery
  showResult GetSystemStart          = show
  showResult GetHeaderStateTip       = show

instance Eq (SomeSecond BlockQuery blk) => Eq (SomeSecond Query blk) where
  SomeSecond (BlockQuery blockQueryA) == SomeSecond (BlockQuery blockQueryB)
    = SomeSecond blockQueryA == SomeSecond blockQueryB
  SomeSecond (BlockQuery _) == _ = False

  SomeSecond GetSystemStart == SomeSecond GetSystemStart = True
  SomeSecond GetSystemStart == _                         = False

  SomeSecond GetHeaderStateTip == SomeSecond GetHeaderStateTip = True
  SomeSecond GetHeaderStateTip == _                            = False

instance Show (SomeSecond BlockQuery blk) => Show (SomeSecond Query blk) where
  show (SomeSecond (BlockQuery blockQueryA)) = "Query " ++ show (SomeSecond blockQueryA)
  show (SomeSecond GetSystemStart) = "Query GetSystemStart"
  show (SomeSecond GetHeaderStateTip) = "Query GetHeaderStateTip"


-- | Exception thrown in the encoders
data QueryEncoderException blk =
    -- | A query was submitted that is not supported by the given 'QueryVersion'
    QueryEncoderUnsupportedQuery
         (SomeSecond Query blk)
         QueryVersion

deriving instance Show (SomeSecond BlockQuery blk) => Show (QueryEncoderException blk)
instance (Typeable blk, Show (SomeSecond BlockQuery blk)) => Exception (QueryEncoderException blk)


queryEncodeNodeToClient ::
     forall blk.
     Typeable blk
  => Show (SomeSecond BlockQuery blk)
  => SerialiseNodeToClient blk (SomeSecond BlockQuery blk)
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> SomeSecond Query blk
  -> Encoding
queryEncodeNodeToClient codecConfig queryVersion blockVersion (SomeSecond query)
  = case queryVersion of

    -- In "version 0" we only support BlockQuery and add no extra wrapping so
    -- that it's backwards compatible with when there were no top level queries.
    TopLevelQueryDisabled ->
      case query of
        BlockQuery blockQuery ->
          encodeBlockQuery blockQuery
        _ ->
          throw $ QueryEncoderUnsupportedQuery (SomeSecond query) queryVersion

    -- From version 1 onwards, we use normal constructor tags
    _ ->
      case query of
        BlockQuery blockQuery ->
          requireVersion QueryVersion1 queryVersion $ mconcat
            [ encodeListLen 2
            , encodeWord8 0
            , encodeBlockQuery blockQuery
            ]

        GetSystemStart ->
          requireVersion QueryVersion1 queryVersion $ mconcat
            [ encodeListLen 1
            , encodeWord8 1
            ]

        GetHeaderStateTip ->
          requireVersion QueryVersion2 queryVersion $ mconcat
            [ encodeListLen 1
            , encodeWord8 2
            ]

  where
    requireVersion :: QueryVersion -> QueryVersion -> a -> a
    requireVersion expectedVersion actualVersion a =
      if actualVersion >= expectedVersion
        then a
        else throw $ QueryEncoderUnsupportedQuery (SomeSecond query) actualVersion

    encodeBlockQuery blockQuery =
      encodeNodeToClient
        @blk
        @(SomeSecond BlockQuery blk)
        codecConfig
        blockVersion
        (SomeSecond blockQuery)

queryDecodeNodeToClient ::
     forall blk.
     SerialiseNodeToClient blk (SomeSecond BlockQuery blk)
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> forall s. Decoder s (SomeSecond Query blk)
queryDecodeNodeToClient codecConfig queryVersion blockVersion
  = case queryVersion of
      TopLevelQueryDisabled -> decodeBlockQuery
      QueryVersion1         -> handleTopLevelQuery
      QueryVersion2         -> handleTopLevelQuery
  where
    handleTopLevelQuery :: Decoder s (SomeSecond Query blk)
    handleTopLevelQuery = do
        size <- decodeListLen
        tag  <- decodeWord8
        case (size, tag) of
          (2, 0) -> requireVersion "BlockQuery"        QueryVersion1   decodeBlockQuery
          (1, 1) -> requireVersion "GetSystemStart"    QueryVersion1 $ return (SomeSecond GetSystemStart)
          (1, 2) -> requireVersion "GetHeaderStateTip" QueryVersion2 $ return (SomeSecond GetHeaderStateTip)
          _      -> fail $ "Query: invalid size and tag" <> show (size, tag)

    requireVersion :: String -> QueryVersion -> Decoder s (SomeSecond Query blk) -> Decoder s (SomeSecond Query blk)
    requireVersion name expectedVersion f =
      if queryVersion >= expectedVersion
        then f
        else fail $ "Query: " <> name <> " requires at least " <> show expectedVersion

    decodeBlockQuery :: Decoder s (SomeSecond Query blk)
    decodeBlockQuery = do
      SomeSecond blockQuery <- decodeNodeToClient
        @blk
        @(SomeSecond BlockQuery blk)
        codecConfig
        blockVersion
      return (SomeSecond (BlockQuery blockQuery))

instance Serialise (HeaderHash blk) => Serialise (HeaderStateTip blk) where

instance (SerialiseResult blk (BlockQuery blk), Serialise (HeaderHash blk), Typeable blk) => SerialiseResult blk (Query blk) where
  encodeResult codecConfig blockVersion (BlockQuery blockQuery) result
    = encodeResult codecConfig blockVersion blockQuery result
  encodeResult _ _ GetSystemStart result
    = toCBOR result
  encodeResult _ _ GetHeaderStateTip result
    = toCBOR result

  decodeResult codecConfig blockVersion (BlockQuery query)
    = decodeResult codecConfig blockVersion query
  decodeResult _ _ GetSystemStart
    = fromCBOR
  decodeResult _ _ GetHeaderStateTip
    = fromCBOR

instance SameDepIndex (BlockQuery blk) => SameDepIndex (Query blk) where
  sameDepIndex (BlockQuery blockQueryA) (BlockQuery blockQueryB)
    = sameDepIndex blockQueryA blockQueryB
  sameDepIndex (BlockQuery _) _
    = Nothing
  sameDepIndex GetSystemStart GetSystemStart
    = Just Refl
  sameDepIndex GetSystemStart _
    = Nothing
  sameDepIndex GetHeaderStateTip GetHeaderStateTip
    = Just Refl
  sameDepIndex GetHeaderStateTip _
    = Nothing

deriving instance Show (BlockQuery blk result) => Show (Query blk result)

data HeaderStateTip blk = HeaderStateTip {
      queriedTipSlotNo  :: !SlotNo
    , queriedTipBlockNo :: !BlockNo
    , queriedTipInfo    :: !(HeaderHash blk)
    }
  deriving (Generic)

deriving instance Show (HeaderHash blk) => Show (HeaderStateTip blk)

-- | Answer the given query about the extended ledger state.
answerQuery :: forall blk result.
     (QueryLedger blk, ConfigSupportsNode blk, HasAnnTip blk)
  => ExtLedgerCfg blk
  -> Query blk result
  -> ExtLedgerState blk
  -> result
answerQuery cfg query st = case query of
  BlockQuery blockQuery -> answerBlockQuery cfg blockQuery st
  GetSystemStart -> getSystemStart (topLevelConfigBlock (getExtLedgerCfg cfg))
  GetHeaderStateTip -> case headerStateTip (headerState st) of
    Origin -> Origin
    At hst -> At HeaderStateTip
      { queriedTipSlotNo  = annTipSlotNo hst
      , queriedTipBlockNo = annTipBlockNo hst
      , queriedTipInfo    = tipInfoHash (Proxy @blk) (annTipInfo hst)
      }

-- | Different queries supported by the ledger, indexed by the result type.
data family BlockQuery blk :: Type -> Type

-- | Query the ledger extended state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the extended
-- ledger state.
class (ShowQuery (BlockQuery blk), SameDepIndex (BlockQuery blk)) => QueryLedger blk where

  -- | Answer the given query about the extended ledger state.
  answerBlockQuery :: ExtLedgerCfg blk -> BlockQuery blk result -> ExtLedgerState blk -> result

instance SameDepIndex (BlockQuery blk) => Eq (SomeSecond BlockQuery blk) where
  SomeSecond qry == SomeSecond qry' = isJust (sameDepIndex qry qry')

deriving instance (forall result. Show (BlockQuery blk result)) => Show (SomeSecond BlockQuery blk)
