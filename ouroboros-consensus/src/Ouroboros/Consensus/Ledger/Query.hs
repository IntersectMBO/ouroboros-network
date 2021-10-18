{-# LANGUAGE DataKinds             #-}
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
  , answerQuery
  , nodeToClientVersionToQueryVersion
  , queryDecodeNodeToClient
  , queryEncodeNodeToClient
  , SmallQuery (..)
  , withSmallQueryProof
    -- * Re-exports
  , FootprintL (..)
  , QueryWithSomeFootprintL (..)
  , QueryWithSomeResult (..)
  , EqQuery (..)
  , ShowQuery (..)
  , SomeQuery (..)
  ) where

import           Control.Exception (Exception, throw)
import           Data.Kind (Type)
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

import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..),
                     headerStateBlockNo, headerStatePoint)
import           Ouroboros.Consensus.Ledger.Basics (DiskLedgerView)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query.Version
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..))
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.DepPair

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

queryName :: Query blk fp result -> String
queryName query = case query of
  BlockQuery _    -> "BlockQuery"
  GetSystemStart  -> "GetSystemStart"
  GetChainBlockNo -> "GetChainBlockNo"
  GetChainPoint   -> "GetChainPoint"

-- | Different queries supported by the ledger for all block types, indexed
-- by the result type.
--
-- Additions to the set of queries is versioned by 'QueryVersion'
data Query blk (fp :: FootprintL) result where
  -- | This constructor is supported by all @QueryVersion@s. The @BlockQuery@
  -- argument is versioned by the @BlockNodeToClientVersion blk@.
  BlockQuery :: BlockQuery blk fp result -> Query blk fp result

  -- | Get the 'SystemStart' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion1'.
  GetSystemStart :: Query blk SmallL SystemStart

  -- | Get the 'GetChainBlockNo' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainBlockNo :: Query blk SmallL (WithOrigin BlockNo)

  -- | Get the 'GetChainPoint' time.
  --
  -- Supported by 'QueryVersion' >= 'QueryVersion2'.
  GetChainPoint :: Query blk SmallL (Point blk)

instance (ShowProxy (BlockQuery blk)) => ShowProxy (Query blk) where
  showProxy (Proxy :: Proxy (Query blk)) = "Query (" ++ showProxy (Proxy @(BlockQuery blk)) ++ ")"

instance (ShowQuery (BlockQuery blk), StandardHash blk) => ShowQuery (Query blk) where
  showResult (BlockQuery blockQuery) = showResult blockQuery
  showResult GetSystemStart          = show
  showResult GetChainBlockNo         = show
  showResult GetChainPoint           = show

-- | Exception thrown in the encoders
data QueryEncoderException blk =
    -- | A query was submitted that is not supported by the given 'QueryVersion'
    QueryEncoderUnsupportedQuery
         (SomeQuery (Query blk))
         QueryVersion

deriving instance (forall fp result. Show (BlockQuery blk fp result)) => Show (QueryEncoderException blk)
instance (Typeable blk, forall fp result. Show (BlockQuery blk fp result)) => Exception (QueryEncoderException blk)

queryEncodeNodeToClient ::
     forall blk.
     Typeable blk
  => (forall fp result. Show (BlockQuery blk fp result))
  => SerialiseNodeToClient blk (SomeQuery (BlockQuery blk))
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> SomeQuery (Query blk)
  -> Encoding
queryEncodeNodeToClient codecConfig queryVersion blockVersion (SomeQuery query)
  = case queryVersion of

    -- In "version 0" we only support BlockQuery and add no extra wrapping so
    -- that it's backwards compatible with when there were no top level queries.
    TopLevelQueryDisabled ->
      case query of
        BlockQuery blockQuery ->
          encodeBlockQuery blockQuery
        _ ->
          throw $ QueryEncoderUnsupportedQuery (SomeQuery query) queryVersion

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
        else throw $ QueryEncoderUnsupportedQuery (SomeQuery query) queryVersion

    encodeBlockQuery blockQuery =
      encodeNodeToClient
        @blk
        @(SomeQuery (BlockQuery blk))
        codecConfig
        blockVersion
        (SomeQuery blockQuery)

queryDecodeNodeToClient ::
     forall blk.
     SerialiseNodeToClient blk (SomeQuery (BlockQuery blk))
  => CodecConfig blk
  -> QueryVersion
  -> BlockNodeToClientVersion blk
  -> forall s. Decoder s (SomeQuery (Query blk))
queryDecodeNodeToClient codecConfig queryVersion blockVersion
  = case queryVersion of
      TopLevelQueryDisabled -> decodeBlockQuery
      QueryVersion1         -> handleTopLevelQuery
      QueryVersion2         -> handleTopLevelQuery
  where
    handleTopLevelQuery :: Decoder s (SomeQuery (Query blk))
    handleTopLevelQuery = do
        size <- decodeListLen
        tag  <- decodeWord8
        case (size, tag) of
          (2, 0) -> requireVersion QueryVersion1 =<< decodeBlockQuery
          (1, 1) -> requireVersion QueryVersion1 $ SomeQuery GetSystemStart
          (1, 2) -> requireVersion QueryVersion2 $ SomeQuery GetChainBlockNo
          (1, 3) -> requireVersion QueryVersion2 $ SomeQuery GetChainPoint
          _      -> fail $ "Query: invalid size and tag" <> show (size, tag)

    requireVersion :: QueryVersion -> SomeQuery (Query blk) -> Decoder s (SomeQuery (Query blk))
    requireVersion expectedVersion some2Query =
      if queryVersion >= expectedVersion
        then return some2Query
        else case some2Query of
          SomeQuery query -> fail $ "Query: " <> queryName query <> " requires at least " <> show expectedVersion

    decodeBlockQuery :: Decoder s (SomeQuery (Query blk))
    decodeBlockQuery = do
      SomeQuery blockQuery <- decodeNodeToClient
        @blk
        @(SomeQuery (BlockQuery blk))
        codecConfig
        blockVersion
      return (SomeQuery (BlockQuery blockQuery))

instance ( SerialiseResult blk (BlockQuery blk)
         , Serialise (HeaderHash blk)
         ) => SerialiseResult blk (Query blk) where
  encodeResult codecConfig blockVersion qry result = case qry of
    BlockQuery blockQuery -> encodeResult codecConfig blockVersion blockQuery result
    GetSystemStart        -> toCBOR result
    GetChainBlockNo       -> toCBOR result
    GetChainPoint         -> encodePoint encode result

  decodeResult codecConfig blockVersion qry = case qry of
    BlockQuery blockQuery -> decodeResult codecConfig blockVersion blockQuery
    GetSystemStart        -> fromCBOR
    GetChainBlockNo       -> fromCBOR
    GetChainPoint         -> decodePoint decode

instance EqQuery (BlockQuery blk) => EqQuery (Query blk) where
  eqQuery (BlockQuery blockQueryA) (BlockQuery blockQueryB)
    = eqQuery blockQueryA blockQueryB
  eqQuery (BlockQuery _) _
    = Nothing
  eqQuery GetSystemStart GetSystemStart
    = Just Refl
  eqQuery GetSystemStart _
    = Nothing
  eqQuery GetChainBlockNo GetChainBlockNo
    = Just Refl
  eqQuery GetChainBlockNo _
    = Nothing
  eqQuery GetChainPoint GetChainPoint
    = Just Refl
  eqQuery GetChainPoint _
    = Nothing

deriving instance Show (BlockQuery blk fp result) => Show (Query blk fp result)

-- | A variant of 'answerQuery' that is restricted to 'SmallL'.
answerSmallQuery ::
     (QueryLedger blk, ConfigSupportsNode blk, HasAnnTip blk)
  => ExtLedgerCfg blk
  -> Query          blk SmallL result
  -> ExtLedgerState SmallL blk
  -> result
answerSmallQuery cfg query st = case query of
  BlockQuery blockQuery -> answerBlockSmallQuery cfg blockQuery st
  GetSystemStart -> getSystemStart (topLevelConfigBlock (getExtLedgerCfg cfg))
  GetChainBlockNo -> headerStateBlockNo (headerState st)
  GetChainPoint -> headerStatePoint (headerState st)

-- | Answer the given query about the extended ledger state.
answerQuery :: forall blk m fp result.
     (QueryLedger blk, ConfigSupportsNode blk, HasAnnTip blk, Monad m)
  => ExtLedgerCfg blk
  -> DiskLedgerView blk m
  -> Query          blk fp result
  -> ExtLedgerState fp blk
  -> m result
answerQuery cfg dlv query st = case query of
    BlockQuery blockQuery -> answerBlockQuery cfg dlv blockQuery st
    GetSystemStart        -> small
    GetChainBlockNo       -> small
    GetChainPoint         -> small
  where
    small :: (fp ~ SmallL) => m result
    small = pure $ answerSmallQuery cfg query st

-- | Different queries supported by the ledger, indexed by the result type.
data family BlockQuery blk :: FootprintL -> Type -> Type

-- | Query the ledger extended state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the extended
-- ledger state.
class ( ShowQuery (BlockQuery blk)
      , EqQuery (BlockQuery blk)
      ) => QueryLedger blk where

  -- | A variant of 'answerBlockQuery' that is restricted to 'SmallL'.
  answerBlockSmallQuery :: ExtLedgerCfg blk -> BlockQuery blk SmallL result -> ExtLedgerState SmallL blk -> result

  -- | Answer the given query about the extended ledger state.
  --
  -- TODO de-duplicate with answerBlockSmallQuery; eg a GADT that maps @fp ~
  -- 'SmallL'@ to @m ~ 'Identity'@ and 'DiskLedgerView' to @NothingIf@ but maps
  -- @fp ~ 'LargeL'@ to 'DiskLedgerView' to @JustIf@?
  --
  -- TODO I'm having second thoughts. Should we, at this level here, explicitly
  -- factor the task of " use disk to answer the query " into the same two-step
  -- process we're using for block and tx validation? IE Ask the ledger what
  -- KeySet the query needs, fetch those, build the " complete enough " ledger
  -- state, and then use it to answer the query? I think it's possible that the
  -- " one shot " approach currently expressed here may be simpler for some
  -- queries. It might even be necessary, if we ever want to eg answer a debug
  -- query that must be streamed because the whole result would not fit in
  -- memory at once (but that'd probably require enriching LocalStateQuery to
  -- support multi-message results... perhaps we'd instead introduce a set of
  -- queries that amount to range queries...). Open CAD-3559 for this.
  answerBlockQuery      ::
       Monad m
    => ExtLedgerCfg blk
    -> DiskLedgerView blk m
    -> BlockQuery     blk fp result
    -> ExtLedgerState fp blk
    -> m result

{-------------------------------------------------------------------------------
  Queries that are small
-------------------------------------------------------------------------------}

class SmallQuery query where
  proveSmallQuery :: ((fp ~ SmallL) => a) -> query fp result -> a

withSmallQueryProof :: SmallQuery query => query fp result -> ((fp ~ SmallL) => a) -> a
withSmallQueryProof q k = proveSmallQuery k q
