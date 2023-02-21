{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
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
    -- * Table queries
  , DiskLedgerView (..)
  , TraversingQueryHandler (..)
  , handleQueryWithStowedKeySets
  , handleTraversingQuery
  , mkDiskLedgerView
  ) where

import           Control.Exception (Exception, throw)
import           Data.Kind (Type)
import qualified Data.Map.Diff.Strict.Internal as DS
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Set as Set
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
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query.Version
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..))
import           Ouroboros.Consensus.Storage.LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog hiding (empty)
import qualified Ouroboros.Consensus.Storage.LedgerDB.DiffSeq as DS
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
import           Ouroboros.Consensus.Util (ShowProxy (..), SomeSecond (..))
import           Ouroboros.Consensus.Util.DepPair
import           Ouroboros.Consensus.Util.IOLike (IOLike)

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
      QueryVersion1 -> handleTopLevelQuery
      QueryVersion2 -> handleTopLevelQuery
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
     (QueryLedger blk, ConfigSupportsNode blk, HasAnnTip blk, Monad m)
  => ExtLedgerCfg blk
  -> DiskLedgerView m (ExtLedgerState blk)
  -> Query blk result
  -> m result
answerQuery cfg dlv query = case query of
    BlockQuery blockQuery -> answerBlockQuery cfg blockQuery dlv
    GetSystemStart -> pure $ getSystemStart (topLevelConfigBlock (getExtLedgerCfg cfg))
    GetChainBlockNo -> pure $ headerStateBlockNo (headerState st)
    GetChainPoint -> pure $ headerStatePoint (headerState st)
  where
    DiskLedgerView st _ _ _ = dlv


-- | Different queries supported by the ledger, indexed by the result type.
data family BlockQuery blk :: Type -> Type

-- | Query the ledger extended state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the extended
-- ledger state.
class (ShowQuery (BlockQuery blk), SameDepIndex (BlockQuery blk)) => QueryLedger blk where

  -- | Answer the given query about the extended ledger state.
  answerBlockQuery :: Monad m => ExtLedgerCfg blk -> BlockQuery blk result -> DiskLedgerView m (ExtLedgerState blk) -> m result

  getQueryKeySets :: BlockQuery blk result -> LedgerTables (LedgerState blk) KeysMK

  tableTraversingQuery :: BlockQuery blk result -> Maybe (TraversingQueryHandler blk result)

instance SameDepIndex (BlockQuery blk) => Eq (SomeSecond BlockQuery blk) where
  SomeSecond qry == SomeSecond qry' = isJust (sameDepIndex qry qry')

deriving instance (forall result. Show (BlockQuery blk result)) => Show (SomeSecond BlockQuery blk)

{-------------------------------------------------------------------------------
  Ledger Tables queries
-------------------------------------------------------------------------------}

data DiskLedgerView m l =
    DiskLedgerView
      !(l EmptyMK)
      (LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
      (RangeQuery (LedgerTables l KeysMK) -> m (LedgerTables l ValuesMK))
      (m ())

mkDiskLedgerView ::
     (GetTip l, IOLike m, HasLedgerTables l)
  => (LedgerBackingStoreValueHandle m l, LedgerDB l, m ())
  -> DiskLedgerView m l
mkDiskLedgerView (LedgerBackingStoreValueHandle seqNo vh, ldb, close) =
    DiskLedgerView
      (current ldb)
      (\ks -> do
          let chlog = ledgerDbChangelog ldb
              rew   = rewindTableKeySets chlog ks
          unfwd <- readKeySetsWith
                     (fmap (seqNo,) . BackingStore.bsvhRead vh)
                     rew
          case forwardTableKeySets chlog unfwd of
              Left _err -> error "impossible!"
              Right vs  -> pure vs
      )
      (\rq -> do
          let chlog = ledgerDbChangelog ldb
              -- Get the differences without the keys that are greater or equal
              -- than the maximum previously seen key.
              diffs =
                maybe
                  id
                  (zipLedgerTables doDropLTE)
                  (BackingStore.rqPrev rq)
                  $ mapLedgerTables prj
                  $ changelogDiffs chlog
              -- (1) Ensure that we never delete everything read from disk (ie
              --     if our result is non-empty then it contains something read
              --     from disk).
              --
              -- (2) Also, read one additional key, which we will not include in
              --     the result but need in order to know which in-memory
              --     insertions to include.
              maxDeletes = maybe 0 getMax
                         $ foldLedgerTables (Just . Max . numDeletesDiffMK) diffs
              nrequested = 1 + max (BackingStore.rqCount rq) (1 + maxDeletes)

          values <- BackingStore.bsvhRangeRead vh (rq{BackingStore.rqCount = nrequested})
          pure $ zipLedgerTables (doFixupReadResult nrequested) diffs values
      )
      close
  where
    prj ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK k v
    prj (SeqDiffMK sq) = DiffMK (DS.cumulativeDiff sq)

    -- Remove all diff elements that are <= to the greatest given key
    doDropLTE ::
         Ord k
      => KeysMK k v
      -> DiffMK k v
      -> DiffMK k v
    doDropLTE (KeysMK ks) (DiffMK ds) =
        DiffMK
      $ case Set.lookupMax ks of
          Nothing -> ds
          Just k  -> DS.filterOnlyKey (\dk -> dk > k) ds

    -- NOTE: this is counting the deletions wrt disk.
    numDeletesDiffMK :: DiffMK k v -> Int
    numDeletesDiffMK (DiffMK d) =
      getSum $ DS.foldMapDiffEntry (Sum . oneIfDel) d
      where
        oneIfDel x = case x of
          DS.Delete _           -> 1
          DS.Insert _           -> 0
          DS.UnsafeAntiDelete _ -> 0
          DS.UnsafeAntiInsert _ -> 0


    -- INVARIANT: nrequested > 0
    --
    -- (1) if we reached the end of the store, then simply yield the given diff
    --     applied to the given values
    -- (2) otherwise, the readset must be non-empty, since 'rqCount' is positive
    -- (3) remove the greatest read key
    -- (4) remove all diff elements that are >= the greatest read key
    -- (5) apply the remaining diff
    -- (6) (the greatest read key will be the first fetched if the yield of this
    --     result is next passed as 'rqPrev')
    --
    -- Note that if the in-memory changelog contains the greatest key, then
    -- we'll return that in step (1) above, in which case the next passed
    -- 'rqPrev' will contain it, which will cause 'doDropLTE' to result in an
    -- empty diff, which will result in an entirely empty range query result,
    -- which is the termination case.
    doFixupReadResult ::
         Ord k
      => Int
      -- ^ Number of requested keys from the backing store.
      -> DiffMK   k v
      -- ^ Differences that will be applied to the values read from the backing
      -- store.
      -> ValuesMK k v
      -- ^ Values read from the backing store. The number of values read should
      -- be at most @nrequested@.
      -> ValuesMK k v
    doFixupReadResult
      nrequested
      (DiffMK ds)
      (ValuesMK vs) =
        let includingAllKeys        =
              DS.unsafeApplyDiff vs ds
            definitelyNoMoreToFetch = Map.size vs < nrequested
        in
        ValuesMK
      $ case Map.maxViewWithKey vs of
          Nothing             ->
              if definitelyNoMoreToFetch
              then includingAllKeys
              else error $ "Size of values " <> show (Map.size vs) <> ", nrequested " <> show nrequested
          Just ((k, _v), vs') ->
            if definitelyNoMoreToFetch then includingAllKeys else
            DS.unsafeApplyDiff
              vs'
              (DS.filterOnlyKey (\dk -> dk < k) ds)


{-------------------------------------------------------------------------------
  Handle non in-mem queries
-------------------------------------------------------------------------------}

handleQueryWithStowedKeySets ::
     forall blk m result.
     ( QueryLedger blk
     , Monad m
     , LedgerSupportsProtocol blk
     )
  => DiskLedgerView m (ExtLedgerState blk)
  -> BlockQuery blk result
  -> (ExtLedgerState blk EmptyMK -> result)
  -> m result
handleQueryWithStowedKeySets dlv query f = do
    let DiskLedgerView st dbRead _dbReadRange _dbClose = dlv
        keys                                           = getQueryKeySets query
    values <- dbRead (ExtLedgerStateTables keys)
    pure $ f (stowLedgerTables $ st `withLedgerTables` values)

data TraversingQueryHandler blk result where
  TraversingQueryHandler :: (ExtLedgerState blk EmptyMK -> st)
                         -> st
                         -> (st -> st -> st)
                         -> (st -> result)
                         -> TraversingQueryHandler blk result

handleTraversingQuery ::
     forall blk m result.
     ( QueryLedger blk
     , Monad m
     , LedgerSupportsProtocol blk
     )
  => DiskLedgerView m (ExtLedgerState blk)
  -> BlockQuery blk result
  -> m result
handleTraversingQuery dlv query =
  case tableTraversingQuery query of
    Nothing -> error "Tried to perform a traversing query on a query that doesn't need to traverse the Ledger tables!"
    Just (TraversingQueryHandler partial empty comb post) ->
      let
        loop !prev !acc = do
          extValues <-
            dbReadRange RangeQuery{rqPrev = prev, rqCount = batchSize}
          if getAll $ foldLedgerTables (All . f) extValues
          then pure acc
          else loop
                (Just $ mapLedgerTables toKeys extValues)
                (comb acc $ partial (stowLedgerTables (st `withLedgerTables` extValues) `withLedgerTables` emptyLedgerTables))
       in
        post <$> loop Nothing empty
  where
    DiskLedgerView st _dbRead dbReadRange _dbClose = dlv

    f :: ValuesMK k v -> Bool
    f (ValuesMK vs) = Map.null vs

    toKeys :: ValuesMK k v -> KeysMK k v
    toKeys (ValuesMK vs) = KeysMK $ Map.keysSet vs

    batchSize = 100000   -- TODO: #4401 tune, expose as config, etc
