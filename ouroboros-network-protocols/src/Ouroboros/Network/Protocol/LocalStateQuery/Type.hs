{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | The type of the local ledger state query protocol.
--
-- This is used by local clients (like wallets and CLI tools) to query the
-- ledger state of a local node.
--
module Ouroboros.Network.Protocol.LocalStateQuery.Type where

import Data.Kind (Type)
import Data.Singletons

import Network.TypedProtocol.Core

import Control.DeepSeq
import GHC.Generics
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))


-- | The kind of the local state query protocol, and the types of
-- the states in the protocol state machine.
--
-- It is parametrised over the type of block (for points), the type of queries
-- and query results.
--
data LocalStateQuery block point (query :: Type -> Type) where

  -- | The client has agency. It can ask to acquire a state or terminate.
  --
  -- There is no timeout in this state.
  --
  StIdle :: LocalStateQuery block point query

  -- | The server has agency. It must acquire the state at the requested point
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


-- | Singletons for 'LocalStateQuery' state types.
--
type SingLocalStateQuery :: LocalStateQuery block point query
                         -> Type
data SingLocalStateQuery k where
    SingIdle      :: SingLocalStateQuery StIdle
    SingAcquiring :: SingLocalStateQuery StAcquiring
    SingAcquired  :: SingLocalStateQuery StAcquired
    SingQuerying  :: SingLocalStateQuery (StQuerying result
                                            :: LocalStateQuery block point query)
    SingDone      :: SingLocalStateQuery StDone

instance StateTokenI StIdle              where stateToken = SingIdle
instance StateTokenI StAcquiring         where stateToken = SingAcquiring
instance StateTokenI StAcquired          where stateToken = SingAcquired
instance StateTokenI (StQuerying result) where stateToken = SingQuerying
instance StateTokenI StDone              where stateToken = SingDone


instance (forall result. Show (query result))
      => Show (SingLocalStateQuery (k :: LocalStateQuery block point query)) where
    show SingIdle      = "SingIdle"
    show SingAcquiring = "SingAcuiring"
    show SingAcquired  = "SingAcquired"
    show SingQuerying  = "SingQuerying"
    show SingDone      = "SingDone"


data Target point = -- | The tip of the volatile chain
                    --
                    -- Cannot fail to be acquired.
                    VolatileTip
                  | -- | A specified point
                    --
                    -- Fails to be acquired if the point is not between
                    -- 'VolatileTip' and 'ImmutableTip' (inclusive).
                    SpecificPoint point
                    -- | The tip of the immutable chain
                    --
                    -- Cannot fail to be acquired.
                    --
                    -- Requires at least 'NodeToClientV_16'.
                  | ImmutableTip
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable, NFData)

instance Protocol (LocalStateQuery block point query) where

  -- | The messages in the state query protocol.
  --
  -- The pattern of use is to
  --
  data Message (LocalStateQuery block point query) from to where

    -- | The client requests that the 'Target' ledger state on the server's
    -- chain be made available to query, and waits for confirmation or failure.
    --
    MsgAcquire
      :: Target point
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
      :: query result
      -> Message (LocalStateQuery block point query) StAcquired (StQuerying result)

    -- | The server must reply with the queries.
    --
    MsgResult
      :: query result
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
    -- state. By moving to another state directly without a 'MsgRelease' it
    -- enables optimisations on the server side (e.g. moving to the state for
    -- the immediate next block).
    --
    -- Note that failure to re-acquire is equivalent to 'MsgRelease',
    -- rather than keeping the exiting acquired state.
    --
    MsgReAcquire
      :: Target point
      -> Message (LocalStateQuery block point query) StAcquired StAcquiring

    -- | The client can terminate the protocol.
    --
    MsgDone
      :: Message (LocalStateQuery block point query) StIdle StDone


  type StateAgency StIdle              = ClientAgency
  type StateAgency StAcquired          = ClientAgency
  type StateAgency StAcquiring         = ServerAgency
  type StateAgency (StQuerying result) = ServerAgency
  type StateAgency StDone              = NobodyAgency

  type StateToken = SingLocalStateQuery


instance ( forall result. NFData (query result)
         , NFData point
         )
         => NFData (Message (LocalStateQuery block point query) from to) where
  rnf (MsgAcquire mbPoint)   = rnf mbPoint
  rnf MsgAcquired            = ()
  rnf (MsgFailure af)        = rnf af
  rnf (MsgQuery qr)          = rnf qr
  rnf (MsgResult qr r)       = rnf qr `seq` rwhnf r
  rnf MsgRelease             = ()
  rnf (MsgReAcquire mbPoint) = rnf mbPoint
  rnf MsgDone                = ()

data AcquireFailure = AcquireFailurePointTooOld
                    | AcquireFailurePointNotOnChain
  deriving (Eq, Enum, Show, Generic, NFData)


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


type State :: LocalStateQuery block point query -> Type
data State st where
    StateIdle      :: State StIdle
    StateAcquiring :: State StAcquiring
    StateAcquired  :: State StAcquired
    StateQuerying  :: query result
                    -> State (StQuerying result :: LocalStateQuery block point query)
    StateDone      :: State StDone

instance Show (State st) where
    show StateIdle         = "StateIdle"
    show StateAcquiring    = "StateAcquiring"
    show StateAcquired     = "StateAcquired"
    show (StateQuerying _) = "StateQuerying *"
    show StateDone         = "StateDone"
