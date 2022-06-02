{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Ouroboros.Consensus.Ledger.Query (
    BlockQuery
  , ConfigSupportsNode (..)
  , FootprintQ (..)
  , IncrementalQueryHandler (..)
  , Query (..)
  , QueryLedger (..)
  , QuerySat
  , QueryVersion (..)
  , SmallQuery (..)
  , answerQuery
  , handleLargeQuery
  , handleQuery
  , nodeToClientVersionToQueryVersion
  , prepareQuery
  , proveNotLargeQuery
  , proveNotWholeQuery
  , queryDecodeNodeToClient
  , queryEncodeNodeToClient
  , withSmallQueryProof
    -- * Re-exports
  , EqQuery (..)
  , FootprintL (..)
  , IsQuery (..)
  , QueryWithSomeFootprintL (..)
  , QueryWithSomeResult (..)
  , ShowQuery (..)
  , SomeQuery (..)
  ) where

import           Control.Exception (Exception, throw)
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Data.Monoid (All (..))
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
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query.Version
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import           Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore
                     (RangeQuery (..))
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
  = case query of
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
      QueryVersion1 -> handleTopLevelQuery
      QueryVersion2 -> handleTopLevelQuery
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

-- | Answer the given query about the extended ledger state.
answerQuery ::
     (QueryLedger blk, ConfigSupportsNode blk, HasAnnTip blk, QuerySat mk fp)
  => ExtLedgerCfg blk
  -> Query          blk fp result
  -> ExtLedgerState blk mk
  -> result
answerQuery cfg query st = case query of
  BlockQuery blockQuery -> answerBlockQuery cfg blockQuery st
  GetSystemStart -> getSystemStart (topLevelConfigBlock (getExtLedgerCfg cfg))
  GetChainBlockNo -> headerStateBlockNo (headerState st)
  GetChainPoint -> headerStatePoint (headerState st)

prepareQuery :: QueryLedger blk => Query blk LargeL result -> LedgerTables (LedgerState blk) KeysMK
prepareQuery (BlockQuery query) = prepareBlockQuery query

class QuerySat (mk :: MapKind) (fp :: FootprintL)

instance QuerySat mk SmallL

instance QuerySat ValuesMK LargeL

-- | Different queries supported by the ledger, indexed by the result type.
data family BlockQuery blk :: FootprintL -> Type -> Type

-- | Query the ledger extended state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the extended
-- ledger state.
class IsQuery (BlockQuery blk) => QueryLedger blk where

  -- | Answer the given query about the extended ledger state.
  answerBlockQuery :: QuerySat mk fp => ExtLedgerCfg blk -> BlockQuery blk fp result -> ExtLedgerState blk mk -> result

  prepareBlockQuery :: BlockQuery blk LargeL result -> LedgerTables (LedgerState blk) KeysMK

  answerWholeBlockQuery :: BlockQuery blk WholeL result -> IncrementalQueryHandler blk result

  -- This method need not be defined for a @'BlockQuery' blk@ that only contains
  -- 'SmallL' queries.
  default prepareBlockQuery :: SmallQuery (BlockQuery blk) => BlockQuery blk LargeL result -> LedgerTables (LedgerState blk) KeysMK
  prepareBlockQuery query = proveNotLargeQuery query

  -- This method need not be defined for a @'BlockQuery' blk@ that only contains
  -- 'SmallL' queries.
  default answerWholeBlockQuery :: SmallQuery (BlockQuery blk) => BlockQuery blk WholeL result -> IncrementalQueryHandler blk result
  answerWholeBlockQuery query = proveNotWholeQuery query

data IncrementalQueryHandler :: Type -> Type -> Type where
  IncrementalQueryHandler ::
       (LedgerState blk ValuesMK -> st)
    -> st
    -> (st -> st -> st)
    -> (st -> result)
    -> IncrementalQueryHandler blk result

-- | Same as 'handleLargeQuery', but can also handle small queries.
handleQuery ::
     forall blk m fp result.
     ( ConfigSupportsNode blk
     , HasAnnTip blk
     , QueryLedger blk
     , Monad m
     , LedgerSupportsProtocol blk
     )
  => ExtLedgerCfg blk
  -> DiskLedgerView m (ExtLedgerState blk)
  -> Query blk fp result
  -> m result
handleQuery cfg dlv query = case classifyQuery query of
    SmallQ -> pure $ answerQuery cfg query st
    LargeQ -> handleLargeQuery cfg dlv query
    WholeQ -> handleWholeQuery dlv query
  where
    DiskLedgerView st _dbRead _dbRangeRead _dbClose = dlv

handleLargeQuery ::
     forall blk m result.
     ( ConfigSupportsNode blk
     , QueryLedger blk
     , Monad m
     , LedgerSupportsProtocol blk
     )
  => ExtLedgerCfg blk
  -> DiskLedgerView m (ExtLedgerState blk)
  -> Query blk LargeL result
  -> m result
handleLargeQuery cfg dlv query = do
    let DiskLedgerView st dbRead _dbReadRange _dbClose = dlv
        keys                                           = prepareQuery query
    values <- dbRead (ExtLedgerStateTables keys)
    pure $ answerQuery cfg query (st `withLedgerTables` values)

handleWholeQuery ::
     forall blk m result.
     ( QueryLedger blk
     , Monad m
     , LedgerSupportsProtocol blk
     )
  => DiskLedgerView m (ExtLedgerState blk)
  -> Query blk WholeL result
  -> m result
handleWholeQuery dlv query = do
    case let BlockQuery bq = query in answerWholeBlockQuery bq of
      IncrementalQueryHandler
        partial
        (empty :: st)
        comb
        post ->
          let
            loop ::
                 Maybe (LedgerTables (ExtLedgerState blk) KeysMK)
              -> st
              -> m st
            loop !prev !acc = do
              extValues@(ExtLedgerStateTables values) <-
                dbReadRange RangeQuery{rqPrev = prev, rqCount = batchSize}
              if getAll $ foldLedgerTables (All . f) values then pure acc else do
                loop
                  (Just $ mapLedgerTables toKeys extValues)
                  (comb acc $ partial $ ledgerState st `withLedgerTables` values)
          in post <$> loop Nothing empty
  where

    DiskLedgerView st _dbRead dbReadRange _dbClose = dlv

    f :: ApplyMapKind ValuesMK k v -> Bool
    f (ApplyValuesMK (HD.UtxoValues vs)) = Map.null vs

    toKeys :: ApplyMapKind ValuesMK k v -> ApplyMapKind KeysMK k v
    toKeys (ApplyValuesMK (HD.UtxoValues vs)) = ApplyKeysMK $ HD.UtxoKeys $ Map.keysSet vs

    batchSize = 100000   -- TODO tune, expose as config, etc

{-------------------------------------------------------------------------------
  Queries that are small
-------------------------------------------------------------------------------}

class SmallQuery query where
  proveSmallQuery :: ((fp ~ SmallL) => a) -> query fp result -> a

withSmallQueryProof :: SmallQuery query => query fp result -> ((fp ~ SmallL) => a) -> a
withSmallQueryProof q k = proveSmallQuery k q

proveNotLargeQuery :: forall query result a. SmallQuery query => query LargeL result -> a
proveNotLargeQuery q = case mk q of {}
  where
    mk :: forall fp. query fp result -> fp :~: SmallL
    mk q' = withSmallQueryProof q' Refl

proveNotWholeQuery :: forall query result a. SmallQuery query => query WholeL result -> a
proveNotWholeQuery q = case mk q of {}
  where
    mk :: forall fp. query fp result -> fp :~: SmallL
    mk q' = withSmallQueryProof q' Refl

data FootprintQ :: FootprintL -> Type where
  SmallQ :: FootprintQ SmallL
  LargeQ :: FootprintQ LargeL
  WholeQ :: FootprintQ WholeL

class ( EqQuery query
      , ShowQuery query
      ) => IsQuery query where
  classifyQuery :: query fp result -> FootprintQ fp

  -- This method need not be defined for a @query@ that only contains 'SmallL'
  -- queries.
  default classifyQuery :: SmallQuery query => query fp result -> FootprintQ fp
  classifyQuery query = withSmallQueryProof query SmallQ

instance (IsQuery (BlockQuery blk), StandardHash blk) => IsQuery (Query blk) where
  classifyQuery = \case
    BlockQuery query -> classifyQuery query
    GetSystemStart   -> SmallQ
    GetChainBlockNo  -> SmallQ
    GetChainPoint    -> SmallQ
