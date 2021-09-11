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
  , Query (..)
  , QueryLedger (..)
  , QueryVersion (..)
  , ShowQuery (..)
  , answerQuery
  , nodeToClientVersionToQueryVersion
  , queryDecodeNodeToClient
  , queryEncodeNodeToClient
  ) where

import           Control.Exception (Exception, throw)
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Data.Typeable (Typeable)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (WithOrigin (..))

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.Serialise (Serialise)
import           Codec.Serialise.Class (decode, encode)

import           Ouroboros.Network.Block (HeaderHash, Point (..), StandardHash,
                     decodePoint, encodePoint)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))

import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..),
                     headerStateBlockNo, headerStatePoint)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query.Version
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..))
import           Ouroboros.Consensus.Util (ShowProxy (..), SomeSecond (..))
import           Ouroboros.Consensus.Util.DepPair

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

queryName :: Query blk result -> String
queryName query = case query of
  BlockQuery _    -> "BlockQuery"
  GetSystemStart  -> "GetSystemStart"
  GetChainBlockNo -> "GetChainBlockNo"
  GetChainPoint   -> "GetChainPoint"

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

  -- | Get the 'GetChainBlockNo' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainBlockNo :: Query blk (WithOrigin BlockNo)

  -- | Get the 'GetChainPoint' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainPoint :: Query blk (Point blk)

instance (ShowProxy (BlockQuery blk)) => ShowProxy (Query blk) where
  showProxy (Proxy :: Proxy (Query blk)) = "Query (" ++ showProxy (Proxy @(BlockQuery blk)) ++ ")"

instance (ShowQuery (BlockQuery blk), StandardHash blk) => ShowQuery (Query blk) where
  showResult (BlockQuery blockQuery) = showResult blockQuery
  showResult GetSystemStart          = show
  showResult GetChainBlockNo         = show
  showResult GetChainPoint           = show

instance Eq (SomeSecond BlockQuery blk) => Eq (SomeSecond Query blk) where
  SomeSecond (BlockQuery blockQueryA) == SomeSecond (BlockQuery blockQueryB)
    = SomeSecond blockQueryA == SomeSecond blockQueryB
  SomeSecond (BlockQuery _) == _ = False

  SomeSecond GetSystemStart == SomeSecond GetSystemStart = True
  SomeSecond GetSystemStart == _                         = False

  SomeSecond GetChainBlockNo == SomeSecond GetChainBlockNo  = True
  SomeSecond GetChainBlockNo == _                           = False

  SomeSecond GetChainPoint == SomeSecond GetChainPoint  = True
  SomeSecond GetChainPoint == _                         = False

instance Show (SomeSecond BlockQuery blk) => Show (SomeSecond Query blk) where
  show (SomeSecond (BlockQuery blockQueryA))  = "Query " ++ show (SomeSecond blockQueryA)
  show (SomeSecond GetSystemStart)            = "Query GetSystemStart"
  show (SomeSecond GetChainBlockNo)           = "Query GetChainBlockNo"
  show (SomeSecond GetChainPoint)             = "Query GetChainPoint"


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
          requireVersion QueryVersion1 $ mconcat
            [ encodeListLen 2
            , encodeWord8 0
            , encodeBlockQuery blockQuery
            ]

        GetSystemStart ->
          requireVersion QueryVersion1 $ mconcat
            [ encodeListLen 1
            , encodeWord8 1
            ]

        GetChainBlockNo ->
          requireVersion QueryVersion2 $ mconcat
            [ encodeListLen 1
            , encodeWord8 2
            ]

        GetChainPoint ->
          requireVersion QueryVersion2 $ mconcat
            [ encodeListLen 1
            , encodeWord8 3
            ]

  where
    requireVersion :: QueryVersion -> a -> a
    requireVersion expectedVersion a =
      if queryVersion >= expectedVersion
        then a
        else throw $ QueryEncoderUnsupportedQuery (SomeSecond query) queryVersion

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
          (2, 0) -> requireVersion QueryVersion1 =<< decodeBlockQuery
          (1, 1) -> requireVersion QueryVersion1 $ SomeSecond GetSystemStart
          (1, 2) -> requireVersion QueryVersion2 $ SomeSecond GetChainBlockNo
          (1, 3) -> requireVersion QueryVersion2 $ SomeSecond GetChainPoint
          _      -> fail $ "Query: invalid size and tag" <> show (size, tag)

    requireVersion :: QueryVersion -> SomeSecond Query blk -> Decoder s (SomeSecond Query blk)
    requireVersion expectedVersion someSecondQuery =
      if queryVersion >= expectedVersion
        then return someSecondQuery
        else case someSecondQuery of
          SomeSecond query -> fail $ "Query: " <> queryName query <> " requires at least " <> show expectedVersion

    decodeBlockQuery :: Decoder s (SomeSecond Query blk)
    decodeBlockQuery = do
      SomeSecond blockQuery <- decodeNodeToClient
        @blk
        @(SomeSecond BlockQuery blk)
        codecConfig
        blockVersion
      return (SomeSecond (BlockQuery blockQuery))

instance ( SerialiseResult blk (BlockQuery blk)
         , Serialise (HeaderHash blk)
         ) => SerialiseResult blk (Query blk) where
  encodeResult codecConfig blockVersion (BlockQuery blockQuery) result
    = encodeResult codecConfig blockVersion blockQuery result
  encodeResult _ _ GetSystemStart result
    = toCBOR result
  encodeResult _ _ GetChainBlockNo result
    = toCBOR result
  encodeResult _ _ GetChainPoint result
    = encodePoint encode result

  decodeResult codecConfig blockVersion (BlockQuery query)
    = decodeResult codecConfig blockVersion query
  decodeResult _ _ GetSystemStart
    = fromCBOR
  decodeResult _ _ GetChainBlockNo
    = fromCBOR
  decodeResult _ _ GetChainPoint
    = decodePoint decode

instance SameDepIndex (BlockQuery blk) => SameDepIndex (Query blk) where
  sameDepIndex (BlockQuery blockQueryA) (BlockQuery blockQueryB)
    = sameDepIndex blockQueryA blockQueryB
  sameDepIndex (BlockQuery _) _
    = Nothing
  sameDepIndex GetSystemStart GetSystemStart
    = Just Refl
  sameDepIndex GetSystemStart _
    = Nothing
  sameDepIndex GetChainBlockNo GetChainBlockNo
    = Just Refl
  sameDepIndex GetChainBlockNo _
    = Nothing
  sameDepIndex GetChainPoint GetChainPoint
    = Just Refl
  sameDepIndex GetChainPoint _
    = Nothing

deriving instance Show (BlockQuery blk result) => Show (Query blk result)

-- | Answer the given query about the extended ledger state.
answerQuery ::
     (QueryLedger blk, ConfigSupportsNode blk, HasAnnTip blk)
  => ExtLedgerCfg blk
  -> Query blk result
  -> ExtLedgerState blk
  -> result
answerQuery cfg query st = case query of
  BlockQuery blockQuery -> answerBlockQuery cfg blockQuery st
  GetSystemStart -> getSystemStart (topLevelConfigBlock (getExtLedgerCfg cfg))
  GetChainBlockNo -> headerStateBlockNo (headerState st)
  GetChainPoint -> headerStatePoint (headerState st)

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
