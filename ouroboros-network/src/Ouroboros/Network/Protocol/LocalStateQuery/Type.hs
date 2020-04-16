{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The type of the local ledger state query protocol.
--
-- This is used by local clients (like wallets and CLI tools) to query the
-- ledger state of a local node.
--
module Ouroboros.Network.Protocol.LocalStateQuery.Type where

import           Data.Kind (Type)
import           Network.TypedProtocol.Core
import           Ouroboros.Network.Block (Point, StandardHash)


-- | The kind of the local state query protocol, and the types of
-- the states in the protocol state machine.
--
-- It is parametrised over the type of block (for points), the type of queries
-- and query results.
--
data LocalStateQuery block (query :: Type -> Type) where

  -- | The client has agency. It can ask to acquire a state or terminate.
  --
  -- There is no timeout in this state.
  --
  StIdle :: LocalStateQuery block query

  -- | The server has agency. it must acquire the state at the requested point
  -- or report a failure.
  --
  -- There is a timeout in this state.
  --
  StAcquiring :: LocalStateQuery block query

  -- | The client has agency. It can request queries against the current state,
  -- or it can release the state.
  --
  StAcquired :: LocalStateQuery block query

  -- | The server has agency. It must respond with the query result.
  --
  StQuerying :: result -> LocalStateQuery block query

  -- | Nobody has agency. The terminal state.
  --
  StDone :: LocalStateQuery block query

instance Protocol (LocalStateQuery block query) where

  -- | The messages in the state query protocol.
  --
  -- The pattern of use is to
  --
  data Message (LocalStateQuery block query) from to where

    -- | The client requests that the state as of a particular recent point on
    -- the server's chain (within K of the tip) be made available to query,
    -- and waits for confirmation or failure.
    --
    MsgAcquire
      :: Point block
      -> Message (LocalStateQuery block query) StIdle StAcquiring

    -- | The server can confirm that it has the state at the requested point.
    --
    MsgAcquired
      :: Message (LocalStateQuery block query) StAcquiring StAcquired

    -- | The server can report that it cannot obtain the state for the
    -- requested point.
    --
    MsgFailure
      :: AcquireFailure
      -> Message (LocalStateQuery block query) StAcquiring StIdle

    -- | The client can perform queries on the current acquired state.
    --
    MsgQuery
      :: query result
      -> Message (LocalStateQuery block query) StAcquired (StQuerying result)

    -- | The server must reply with the queries.
    --
    MsgResult
      :: query result
         -- ^ The query will not be sent across the network, it is solely used
         -- as evidence that @result@ is a valid type index of @query@.
      -> result
      -> Message (LocalStateQuery block query) (StQuerying result) StAcquired

    -- | The client can instruct the server to release the state. This lets
    -- the server free resources.
    --
    MsgRelease
      :: Message (LocalStateQuery block query) StAcquired StIdle

    -- | This is like 'MsgAcquire' but for when the client already has a
    -- state. By moveing to another state directly without a 'MsgRelease' it
    -- enables optimisations on the server side (e.g. moving to the state for
    -- the immediate next block).
    --
    -- Note that failure to re-acquire is equivalent to 'MsgRelease',
    -- rather than keeping the exiting acquired state.
    --
    MsgReAcquire
      :: Point block
      -> Message (LocalStateQuery block query) StAcquired StAcquiring

    -- | The client can terminate the protocol.
    --
    MsgDone
      :: Message (LocalStateQuery block query) StIdle StDone


  data ClientHasAgency st where
    TokIdle      :: ClientHasAgency StIdle
    TokAcquired  :: ClientHasAgency StAcquired

  data ServerHasAgency st where
    TokAcquiring  :: ServerHasAgency StAcquiring
    TokQuerying   :: query result
                  -> ServerHasAgency (StQuerying result :: LocalStateQuery block query)

  data NobodyHasAgency st where
    TokDone  :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle     tok = case tok of {}
  exclusionLemma_ClientAndServerHaveAgency TokAcquired tok = case tok of {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


data AcquireFailure = AcquireFailurePointTooOld
                    | AcquireFailurePointNotOnChain
  deriving (Eq, Enum, Show)

instance Show (ClientHasAgency (st :: LocalStateQuery block query)) where
  show TokIdle     = "TokIdle"
  show TokAcquired = "TokAcquired"

instance Show (ServerHasAgency (st :: LocalStateQuery block query)) where
  show TokAcquiring    = "TokAcquiring"
  show (TokQuerying _) = "TokQuerying"

-- | To implement 'Show' for:
--
-- > ('Message' ('LocalStateQuery' block query) st st')
--
-- we need a way to print the @query@ GADT and its type index, @result@. This
-- class contain the method we need to provide this 'Show' instance.
--
-- We use a type class for this, as this 'Show' constraint propagates to a lot
-- of places.
class (forall result. Show (query result)) => ShowQuery query where
    showResult :: forall result. query result -> result -> String

instance (ShowQuery query, StandardHash block)
      => Show (Message (LocalStateQuery block query) st st') where
  showsPrec p msg = case msg of
      MsgAcquire pt -> showParen (p >= 11) $
        showString "MsgAcquire " .
        showsPrec 11 pt
      MsgAcquired ->
        showString "MsgAcquired"
      MsgFailure failure -> showParen (p >= 11) $
        showString "MsgFailure " .
        showsPrec 11 failure
      MsgQuery query -> showParen (p >= 11) $
        showString "MsgQuery " .
        showsPrec 11 query
      MsgResult query result -> showParen (p >= 11) $
        showString "MsgResult " .
        showParen True (showString (showResult query result))
      MsgRelease ->
        showString "MsgRelease"
      MsgReAcquire pt -> showParen (p >= 11) $
        showString "MsgReAcquire " .
        showsPrec 11 pt
      MsgDone ->
        showString "MsgDone"
