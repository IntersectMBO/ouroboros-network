{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Ouroboros.Consensus.Ledger.Query (
    BlockQuery
  , QueryLedger (..)
  , ShowQuery (..)
  ) where

import           Data.Kind (Type)
import           Data.Maybe (isJust)

import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Util (SomeSecond (..))
import           Ouroboros.Consensus.Util.DepPair

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Different queries supported by the ledger, indexed by the result type.
data family BlockQuery blk :: Type -> Type

-- | Query the ledger extended state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the extended
-- ledger state.
class (ShowQuery (BlockQuery blk), SameDepIndex (BlockQuery blk)) => QueryLedger blk where

  -- | Answer the given query about the extended ledger state.
  answerQuery :: ExtLedgerCfg blk -> BlockQuery blk result -> ExtLedgerState blk -> result

instance SameDepIndex (BlockQuery blk) => Eq (SomeSecond BlockQuery blk) where
  SomeSecond qry == SomeSecond qry' = isJust (sameDepIndex qry qry')

deriving instance (forall result. Show (BlockQuery blk result)) => Show (SomeSecond BlockQuery blk)
