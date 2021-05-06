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
  , Query (..)
  , QueryLedger (..)
  , ShowQuery (..)
  , answerQuery
  , queryDecodeNodeToClient
  , queryDecodeResult
  , queryEncodeNodeToClient
  , queryEncodeResult
  ) where

import           Data.Kind (Type)
import           Data.Maybe (isJust)

import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))

import           Cardano.Binary
import           Ouroboros.Consensus.Config (topLevelConfigLedger)
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (HasNetworkProtocolVersion (..), NodeToClientVersion (..))
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..))
import           Ouroboros.Consensus.Util (ShowProxy (..), SomeSecond (..))
import           Ouroboros.Consensus.Util.DepPair

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Different queries supported by the ledger for all block types, indexed
-- by the result type.
data Query blk result where
  BlockQuery :: BlockQuery blk result -> Query blk result
  GetPartialLedgerConfig :: Query blk (PartialLedgerConfig blk)

instance (ShowProxy (BlockQuery blk)) => ShowProxy (Query blk) where
  showProxy (Proxy :: Proxy (Query blk)) = "Query (" ++ showProxy (Proxy @(BlockQuery blk)) ++ ")"

instance (ShowQuery (BlockQuery blk)) => ShowQuery (Query blk) where
  showResult query result = case query of
    BlockQuery blockQuery  -> showResult blockQuery result
    GetPartialLedgerConfig -> "PartialLedgerConfig {..}"

instance Eq (SomeSecond BlockQuery blk) => Eq (SomeSecond Query blk) where
  SomeSecond (BlockQuery blockQueryA) == SomeSecond (BlockQuery blockQueryB)
    = SomeSecond blockQueryA == SomeSecond blockQueryB
  SomeSecond (BlockQuery _) == SomeSecond _
    = False
  SomeSecond GetPartialLedgerConfig == SomeSecond GetPartialLedgerConfig
    = True
  SomeSecond GetPartialLedgerConfig == SomeSecond _
    = False

instance Show (SomeSecond BlockQuery blk) => Show (SomeSecond Query blk) where
  show (SomeSecond query) = case query of
    BlockQuery blockQueryA -> "Query " ++ show (SomeSecond blockQueryA)
    GetPartialLedgerConfig -> "GetPartialLedgerConfig"

instance SerialiseNodeToClient blk (SomeSecond BlockQuery blk) => SerialiseNodeToClient blk (SomeSecond Query blk) where
  encodeNodeToClient codecConfig blockVersion (SomeSecond query)
    = case query of
      BlockQuery blockQuery ->
        encodeTag 0 <> encodeNodeToClient
                        @blk
                        @(SomeSecond BlockQuery blk)
                        codecConfig
                        blockVersion
                        (SomeSecond blockQuery)
      GetPartialLedgerConfig ->
        encodeTag 1

  decodeNodeToClient codecConfig blockVersion = do
    tag <- decodeTag
    case tag of
      0 -> do
        SomeSecond x <- decodeNodeToClient
            @blk
            @(SomeSecond BlockQuery blk)
            codecConfig
            blockVersion
        return (SomeSecond (BlockQuery x))
      1 -> return (SomeSecond GetPartialLedgerConfig)
      _ -> fail $ "SomeSecond Query blk: unknown tag " ++ show tag

instance ( SerialiseResult blk (BlockQuery blk)
         , SerialiseNodeToClient blk (PartialLedgerConfig blk)
         ) => SerialiseResult blk (Query blk) where
  encodeResult codecConfig blockVersion query result = case query of
    BlockQuery blockQuery -> encodeResult codecConfig blockVersion blockQuery result
    GetPartialLedgerConfig -> encodeNodeToClient codecConfig blockVersion result

  decodeResult codecConfig blockVersion query = case query of
    BlockQuery blockQuery -> decodeResult codecConfig blockVersion blockQuery
    GetPartialLedgerConfig -> decodeNodeToClient codecConfig blockVersion

queryEncodeNodeToClient ::
    forall blk. (
      SerialiseNodeToClient blk (SomeSecond BlockQuery blk)
    )
  => CodecConfig blk
  -> NodeToClientVersion
  -> BlockNodeToClientVersion blk
  -> (SomeSecond Query blk)
  -> Encoding
queryEncodeNodeToClient codecConfig version blockVersion (SomeSecond query)
  | not (version >= NodeToClientV_9)
  = error "encode failure: Serializing Query is not supported"
  | otherwise
  = case query of
    BlockQuery blockQuery ->
      encodeTag 0 <> encodeNodeToClient
                      @blk
                      @(SomeSecond BlockQuery blk)
                      codecConfig
                      blockVersion
                      (SomeSecond blockQuery)

queryDecodeNodeToClient ::
    forall blk. (
      SerialiseNodeToClient blk (SomeSecond BlockQuery blk)
    )
  => CodecConfig blk
  -> NodeToClientVersion
  -> BlockNodeToClientVersion blk
  -> forall s. Decoder s (SomeSecond Query blk)
queryDecodeNodeToClient codecConfig version blockVersion
  | not (version >= NodeToClientV_9)
  = fail $ "decode failure: Deserializing Query is not supported (on node to client version: " ++ show version ++ ")"
  | otherwise
  = do
    tag <- decodeTag
    case tag of
      0 -> do
        SomeSecond x <- decodeNodeToClient
            @blk
            @(SomeSecond BlockQuery blk)
            codecConfig
            blockVersion
        return (SomeSecond (BlockQuery x))
      _ -> fail $ "SomeSecond Query blk: unknown tag " ++ show tag

queryEncodeResult ::
     forall blk result. SerialiseResult blk (BlockQuery blk)
  => CodecConfig blk
  -> NodeToClientVersion
  -> BlockNodeToClientVersion blk
  -> Query blk result
  -> result
  -> Encoding
queryEncodeResult codecConfig version blockNodeToClientVersion query result
  | not (version >= NodeToClientV_9)
  = error $ "encode failure: Serializing Query result is not supported (on node to client version: " ++ show version ++ ")"
  | otherwise
  = case query of
      BlockQuery blockQuery ->
        encodeTag 0
        <> encodeResult codecConfig blockNodeToClientVersion blockQuery result

queryDecodeResult ::
     forall blk result. SerialiseResult blk (BlockQuery blk)
  => CodecConfig blk
  -> NodeToClientVersion
  -> BlockNodeToClientVersion blk
  -> Query blk result
  -> forall s. Decoder s result
queryDecodeResult codecConfig version blockNodeToClientVersion query
  | not (version >= NodeToClientV_9)
  = error $ "encode failure: Deserializing Query result is not supported (on node to client version: " ++ show version ++ ")"
  | otherwise
  = do
    tag <- decodeTag
    case query of
      BlockQuery blockQuery
        | tag /= 0 -> fail $ "Query blk: Expected tag 0 but got " ++ show tag
        | otherwise -> decodeResult codecConfig blockNodeToClientVersion blockQuery

instance SameDepIndex (BlockQuery blk) => SameDepIndex (Query blk) where
  sameDepIndex (BlockQuery blockQueryA) (BlockQuery blockQueryB)
    = sameDepIndex blockQueryA blockQueryB
  sameDepIndex (BlockQuery _) _
    = Nothing
  sameDepIndex GetPartialLedgerConfig GetPartialLedgerConfig
    = Just Refl
  sameDepIndex GetPartialLedgerConfig _
    = Nothing

deriving instance Show (BlockQuery blk result) => Show (Query blk result)

-- | Answer the given query about the extended ledger state.
answerQuery :: forall blk result. (QueryLedger blk, HasPartialLedgerConfig blk) => ExtLedgerCfg blk -> Query blk result -> ExtLedgerState blk -> result
answerQuery cfg query st = case query of
  BlockQuery blockQuery -> answerBlockQuery cfg blockQuery st
  GetPartialLedgerConfig -> toPartialLedgerConfig (Proxy @blk) $ topLevelConfigLedger $ getExtLedgerCfg cfg

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
