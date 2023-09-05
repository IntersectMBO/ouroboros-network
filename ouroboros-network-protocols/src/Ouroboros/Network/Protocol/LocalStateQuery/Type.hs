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
{-# LANGUAGE StandaloneKindSignatures #-}

-- | The type of the local ledger state query protocol.
--
-- This is used by local clients (like wallets and CLI tools) to query the
-- ledger state of a local node.
--
module Ouroboros.Network.Protocol.LocalStateQuery.Type where

import           Data.Kind (Type)
import           Data.Proxy (Proxy (..))

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

-- | Query with existential quantification on the result and the fingerprint
--
-- Used mainly for query decoders.
type SomeQuery :: (QueryFootprint -> Type -> Type) -> Type
data SomeQuery q =
  forall fp result. SingI fp => SomeQuery (q fp result)

-- | The kind of the local state query protocol, and the types of
-- the states in the protocol state machine.
--
-- It is parametrised over the type of block (for points), the type of queries
-- and query results.
--
data LocalStateQuery block point (query :: QueryFootprint -> Type -> Type) where

  -- | The client has agency. It can ask to acquire a state or terminate.
  --
  -- There is no timeout in this state.
  --
  StIdle :: LocalStateQuery block point query

  -- | The server has agency. it must acquire the state at the requested point
  -- or report a failure.
  --
  -- There is a timeout in this state.
  --
  StAcquiring :: LocalStateQuery block point query

  -- | The client has agency. It can request queries against the current state,
  -- or it can release the state.
  --
  StAcquired :: LocalStateQuery block point query

  -- | The server has agency. It must respond with the query result.
  --
  StQuerying :: result -> LocalStateQuery block point query

  -- | Nobody has agency. The terminal state.
  --
  StDone :: LocalStateQuery block point query

instance ( ShowProxy block
         , ShowProxy query
         ) => ShowProxy (LocalStateQuery block point query) where
    showProxy _ = concat
      [ "LocalStateQuery "
      , showProxy (Proxy :: Proxy block)
      , " "
      , showProxy (Proxy :: Proxy query)
      ]

instance Protocol (LocalStateQuery block point query) where

  -- | The messages in the state query protocol.
  --
  -- The pattern of use is to
  --
  data Message (LocalStateQuery block point query) from to where

    -- | The client requests that the state as of a particular recent point on
    -- the server's chain (within K of the tip) be made available to query,
    -- and waits for confirmation or failure.
    --
    -- From 'NodeToClient_V8' onwards if the point is not specified, current tip
    -- will be acquired.  For previous versions of the protocol 'point' must be
    -- given.
    --
    MsgAcquire
      :: Maybe point
      -> Message (LocalStateQuery block point query) StIdle StAcquiring

    -- | The server can confirm that it has the state at the requested point.
    --
    MsgAcquired
      :: Message (LocalStateQuery block point query) StAcquiring StAcquired

    -- | The server can report that it cannot obtain the state for the
    -- requested point.
    --
    MsgFailure
      :: AcquireFailure
      -> Message (LocalStateQuery block point query) StAcquiring StIdle

    -- | The client can perform queries on the current acquired state.
    --
    MsgQuery
      :: SingI fp
      => query fp result
      -> Message (LocalStateQuery block point query) StAcquired (StQuerying result)

    -- | The server must reply with the queries.
    --
    MsgResult
      :: query fp result
         -- ^ The query will not be sent across the network, it is solely used
         -- as evidence that @result@ is a valid type index of @query@.
      -> result
      -> Message (LocalStateQuery block point query) (StQuerying result) StAcquired

    -- | The client can instruct the server to release the state. This lets
    -- the server free resources.
    --
    MsgRelease
      :: Message (LocalStateQuery block point query) StAcquired StIdle

    -- | This is like 'MsgAcquire' but for when the client already has a
    -- state. By moveing to another state directly without a 'MsgRelease' it
    -- enables optimisations on the server side (e.g. moving to the state for
    -- the immediate next block).
    --
    -- Note that failure to re-acquire is equivalent to 'MsgRelease',
    -- rather than keeping the exiting acquired state.
    --
    -- From 'NodeToClient_V8' onwards if the point is not specified, current tip
    -- will be acquired.  For previous versions of the protocol 'point' must be
    -- given.
    --
    MsgReAcquire
      :: Maybe point
      -> Message (LocalStateQuery block point query) StAcquired StAcquiring

    -- | The client can terminate the protocol.
    --
    MsgDone
      :: Message (LocalStateQuery block point query) StIdle StDone


  data ClientHasAgency st where
    TokIdle      :: ClientHasAgency StIdle
    TokAcquired  :: ClientHasAgency StAcquired

  data ServerHasAgency st where
    TokAcquiring  :: ServerHasAgency StAcquiring
    TokQuerying   :: query fp result
                  -> ServerHasAgency (StQuerying result :: LocalStateQuery block point query)

  data NobodyHasAgency st where
    TokDone  :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle     tok = case tok of {}
  exclusionLemma_ClientAndServerHaveAgency TokAcquired tok = case tok of {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


data AcquireFailure = AcquireFailurePointTooOld
                    | AcquireFailurePointNotOnChain
  deriving (Eq, Enum, Show)

instance Show (ClientHasAgency (st :: LocalStateQuery block point query)) where
  show TokIdle     = "TokIdle"
  show TokAcquired = "TokAcquired"

instance (forall fp result. Show (query fp result))
    => Show (ServerHasAgency (st :: LocalStateQuery block point query)) where
  show TokAcquiring        = "TokAcquiring"
  show (TokQuerying query) = "TokQuerying " ++ show query

-- | To implement 'Show' for:
--
-- > ('Message' ('LocalStateQuery' block query) st st')
--
-- we need a way to print the @query@ GADT and its type index, @result@. This
-- class contain the method we need to provide this 'Show' instance.
--
-- We use a type class for this, as this 'Show' constraint propagates to a lot
-- of places.
class (forall fp result. Show (query fp result)) => ShowQuery query where
    showResult :: forall fp result. query fp result -> result -> String

-- | Some queries will not have a footprint parameter.
class (forall result. Show (query result)) => ShowQueryNone query where
    showResultNone :: forall result. query result -> result -> String

instance (ShowQuery query, Show point)
      => Show (Message (LocalStateQuery block point query) st st') where
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

{-------------------------------------------------------------------------------
  Footprints
-------------------------------------------------------------------------------}

-- | A singleton-like data family
data family Sing (a :: k)

-- | A singleton-like class
class SingI (a :: k) where
  sing :: Sing a

-- | Queries on the local state might use ledger tables. This datatype (which
-- will sometimes be concretized via @sing@) allows Consensus to categorize the
-- queries.
data QueryFootprint =
    -- | The query doesn't need ledger tables, thus can be answered only with
    -- the ledger state.
    QFNone
    -- | The query needs some tables, but doesn't need to traverse the whole
    -- UTxO set.
  | QFOne
    -- | The query needs to traverse the whole UTxO set.
  | QFAll

data instance Sing (qf :: QueryFootprint) where
  SQFNone :: Sing QFNone
  SQFOne :: Sing QFOne
  SQFAll :: Sing QFAll

instance SingI QFNone where
  sing = SQFNone
instance SingI QFOne where
  sing = SQFOne
instance SingI QFAll where
  sing = SQFAll
