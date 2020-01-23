{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- | The type of the local ledger state query protocol.
--
-- This is used by local clients (like wallets and CLI tools) to query the
-- ledger state of a local node.
--
module Ouroboros.Network.Protocol.LocalStateQuery.Type where


import Network.TypedProtocol.Core
import Ouroboros.Network.Block (Point, StandardHash)


-- | The kind of the local state query protocol, and the types of
-- the states in the protocol state machine.
--
-- It is parametrised over the type of block (for points), the type of queries
-- and query results.
--
data LocalStateQuery block query result where

  -- | The client has agency. It can ask to acquire a state or terminate.
  --
  -- There is no timeout in this state.
  --
  StIdle :: LocalStateQuery block query result

  -- | The server has agency. it must acquire the state at the requested point
  -- or report a failure.
  --
  -- There is a timeout in this state.
  --
  StAcquiring :: LocalStateQuery block query result

  -- | The client has agency. It can request queries against the current state,
  -- or it can release the state.
  --
  StAcquired :: LocalStateQuery block query result

  -- | The server has agency. It must respond with the query result.
  --
  StQuerying :: LocalStateQuery block query result

  -- | Nobody has agency. The terminal state.
  --
  StDone   :: LocalStateQuery block query result


instance Protocol (LocalStateQuery block query result) where

  -- | The messages in the state query protocol.
  --
  -- The pattern of use is to 
  --
  data Message (LocalStateQuery block query result) from to where

    -- | The client requests that the state as of a particular recent point on
    -- the server's chain (within K of the tip) be made available to query,
    -- and waits for confirmation or failure.
    --
    MsgAcquire
      :: Point block
      -> Message (LocalStateQuery block query result) StIdle StAcquiring

    -- | The server can confirm that it has the state at the requested point.
    --
    MsgAcquired
      :: Message (LocalStateQuery block query result) StAcquiring StAcquired

    -- | The server can report that it cannot obtain the state for the
    -- requested point.
    --
    MsgFailure
      :: AcquireFailure
      -> Message (LocalStateQuery block query result) StAcquiring StIdle

    -- | The client can perform queries on the current acquired state.
    --
    MsgQuery
      :: query
      -> Message (LocalStateQuery block query result) StAcquired StQuerying

    -- | The server must reply with the query results.
    --
    MsgResult
      :: result
      -> Message (LocalStateQuery block query result) StQuerying StAcquired

    -- | The client can instruct the server to release the state. This lets
    -- the server free resources.
    --
    MsgRelease
      :: Message (LocalStateQuery block query result) StAcquired StIdle

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
      -> Message (LocalStateQuery block query result) StAcquired StAcquiring

    -- | The client can terminate the protocol.
    --
    MsgDone
      :: Message (LocalStateQuery block query result) StIdle StDone


  data ClientHasAgency st where
    TokIdle      :: ClientHasAgency StIdle
    TokAcquired  :: ClientHasAgency StAcquired

  data ServerHasAgency st where
    TokAcquiring  :: ServerHasAgency StAcquiring
    TokQuerying   :: ServerHasAgency StQuerying

  data NobodyHasAgency st where
    TokDone  :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle     tok = case tok of {}
  exclusionLemma_ClientAndServerHaveAgency TokAcquired tok = case tok of {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


data AcquireFailure = AcquireFailurePointTooOld
                    | AcquireFailurePointNotOnChain
  deriving (Eq, Enum, Show)

deriving instance (StandardHash block, Show query, Show result) =>
                   Show (Message (LocalStateQuery block query result) from to)

instance Show (ClientHasAgency (st :: LocalStateQuery block query result)) where
  show TokIdle     = "TokIdle"
  show TokAcquired = "TokAcquired"

instance Show (ServerHasAgency (st :: LocalStateQuery block query result)) where
  show TokAcquiring = "TokAcquiring"
  show TokQuerying  = "TokQuerying"
