{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
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
  , queryEncodeNodeToClient
  , queryDecodeNodeToClient
  , queryEncodeResult
  , queryDecodeResult
  ) where

import           Data.Kind (Type)
import           Data.Maybe (isJust)

import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))

import           Cardano.Binary
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (NodeToClientVersion(..), HasNetworkProtocolVersion (..))
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..))
import           Ouroboros.Consensus.Util (ShowProxy (..), SomeSecond (..))
import           Ouroboros.Consensus.Util.DepPair
import Ouroboros.Consensus.Block.Abstract (CodecConfig)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Different queries supported by the ledger for all block types, indexed
-- by the result type.
data Query blk result = BlockQuery (BlockQuery blk result)

instance (ShowProxy (BlockQuery blk)) => ShowProxy (Query blk) where
  showProxy (Proxy :: Proxy (Query blk)) = "Query (" ++ showProxy (Proxy @(BlockQuery blk)) ++ ")"

instance (ShowQuery (BlockQuery blk)) => ShowQuery (Query blk) where
  showResult (BlockQuery blockQuery) = showResult blockQuery

instance Eq (SomeSecond BlockQuery blk) => Eq (SomeSecond Query blk) where
  SomeSecond (BlockQuery blockQueryA) == SomeSecond (BlockQuery blockQueryB)
    = SomeSecond blockQueryA == SomeSecond blockQueryB

instance Show (SomeSecond BlockQuery blk) => Show (SomeSecond Query blk) where
  show (SomeSecond (BlockQuery blockQueryA)) = "Query " ++ show (SomeSecond blockQueryA)

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

deriving instance Show (BlockQuery blk result) => Show (Query blk result)

-- | Answer the given query about the extended ledger state.
answerQuery :: QueryLedger blk => ExtLedgerCfg blk -> Query blk result -> ExtLedgerState blk -> result
answerQuery cfg query st = case query of
  BlockQuery blockQuery -> answerBlockQuery cfg blockQuery st

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
