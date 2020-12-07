{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Scratch.ThreadNet.Types (
  -- * Planned events
  ConnectionId (..),
  Delays (..),
  NetPlan (..),
  NodeId (..),
  NodeUpData (..),
  PeerId (..),
  PlannedEvent (..),
  emptyNetPlan,
  eventNetPlan,
  eventsNetPlan,
  plannedNumCoreNodes,
  truncateNetPlan,
  validNetPlan,
  -- * Client-server dichotomy
  ClientServer (..),
  ClientServerPair (..),
  csPairConst,
  -- * Local-remote dichotomy
  LocalRemotePair (..),
  -- * ThreadNet tracers
  BeginEnd (..),
  ThrottlerEvent (..),
  ThreadNetEvent (..),
  UNDERSCORE (..),
  -- * Mini protocol set
  MP (..),
  MpTuple (..),
  mpMP,
  mpProtocolNum,
  mpTupleNonEmpty,
  mpConst,
  mpZipWith,
  -- * Rest slot governor
  FlushingState (..),
  ThrottlerState (..),
  ForgeExitCase (..),
  WhetherToSuppress (..),
  ) where

import           Data.Hashable (Hashable)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set as Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))
import           System.Random (StdGen)

import           Control.Monad.Class.MonadFork (ThreadId)
import           Control.Monad.Class.MonadThrow (ExitCase (..))
import           Control.Monad.Class.MonadTime (DiffTime)

import           Cardano.Slotting.Slot (SlotNo (..))

import           Network.Mux.Types (MiniProtocolNum)

import qualified Ouroboros.Network.NodeToNode as NTN

import           Ouroboros.Consensus.Block.Abstract (BlockNo)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Test.Util.Orphans.Read ()

{-------------------------------------------------------------------------------
  The client-server dichotomy
-------------------------------------------------------------------------------}

data ClientServer = Client | Server
  deriving (Eq, Show)

-- | Client on the left, server on the right.
data ClientServerPair a = ClientServerPair {prjClient, prjServer :: a}
  deriving (Eq, Foldable, Functor, Generic, Ord, Traversable)
  deriving (Read, Show) via Quiet (ClientServerPair a)

csPairConst :: a -> ClientServerPair a
csPairConst a = ClientServerPair a a

instance Condense a => Condense (ClientServerPair a) where
  condense (ClientServerPair cl se) = condense cl <> "-" <> condense se

instance Applicative ClientServerPair where
  pure = csPairConst
  ClientServerPair f1 f2 <*> ClientServerPair a1 a2 =
      ClientServerPair (f1 a1) (f2 a2)

{-------------------------------------------------------------------------------
  The local-remote dichotomy
-------------------------------------------------------------------------------}

-- | Local on the left, remote on the right.
data LocalRemotePair a = LocalRemotePair {prjLocal, prjRemote :: a}
  deriving (Eq, Foldable, Functor, Generic, Traversable)
  deriving (Read, Show) via Quiet (LocalRemotePair a)

instance Applicative LocalRemotePair where
  pure a = LocalRemotePair a a
  LocalRemotePair f1 f2 <*> LocalRemotePair a1 a2 =
      LocalRemotePair (f1 a1) (f2 a2)

{-------------------------------------------------------------------------------
  Distinguishing instances of the same vertex
-------------------------------------------------------------------------------}

-- | The @k@-th instance of the given vertex.
data NodeId = NodeId CoreNodeId Int
  deriving (Eq, Ord, Read, Show)

instance Condense NodeId where
  condense (NodeId cid k) = condense cid <> "-" <> condense k

{-------------------------------------------------------------------------------
  Distinguishing connections between the same two vertices
-------------------------------------------------------------------------------}

-- | One peer (local or remote) in the @k@-th connection between two vertices.
--
-- Specifically, this type is used as the addresses within the node
-- applications. Node applications spawned by particular connection will use
-- the same @Int@ as the 'Connectionid' of that connection.
data PeerId = PeerId CoreNodeId Int
  deriving (Eq, Generic, Ord, Read, Show)

-- | The @k@-th connection between two vertices.
--
-- NOTE These may be active simultaneously.
data ConnectionId = ConnectionId (ClientServerPair CoreNodeId) Int
  deriving (Eq, Ord, Read, Show)

instance Hashable PeerId

instance Condense PeerId where
  condense (PeerId pid k) = condense pid <> "-" <> condense k

instance Condense ConnectionId where
  condense (ConnectionId pidPair k) = condense pidPair <> "-" <> condense k

{-------------------------------------------------------------------------------
  Planned events
-------------------------------------------------------------------------------}

-- | Each delay is used once, but the final delay is reused indefinitely
--
-- A message at time @t@ will be received no sooner than @t+delay@.
--
-- Regardless of delays, a message will not be received before the previously
-- sent message: message send order and message receive order are the same.
newtype Delays = Delays (NonEmpty DiffTime)
  deriving (Read, Show)

data PlannedEvent =
    PlannedConnect
      ConnectionId
      (MpTuple (ClientServerPair Delays))
      NTN.NodeToNodeVersion
    -- ^ node A will initiate a directed edge to node B
    --
    -- Each client and each server of the spawned mini protocol instances will
    -- use the specified 'Delays'.
    --
    -- All will use the specific serialization version.
    --
    -- TODO shrinking delays by mini protocol and/or message tag would be
    -- useful
  | PlannedDisconnect ConnectionId
    -- ^ node A will terminate its ChainSync client for node B, which should
    -- bring down the entire directed edge (ie all the mini protocol instances
    -- in that direction). It will /not/ automatically reconnect.
    --
    -- TODO this is the /graceful\/intentional/ disconnect via the MsgDone
    -- handshake (which is invoked in the real node by the p2p governor based
    -- on valency, DeltaQ preference etc -- which we abstract as sometimes
    -- causing these graceful terminations and (re)connections); we suspect
    -- we'd like another disconnect event that corresponds to a network link
    -- failing
  | PlannedNodeUp NodeId NodeUpData
    -- ^ node A is initialized
    --
    -- TODO include dynamic config info: eg credentials, maybe serialization
    -- versions?, etc
    --
    -- TODO also model client nodes that only follow, eg like a wallet

--  | PlannedNodeDown {- iso-Bool for graceful? -}   -- TODO
  deriving (Read, Show)

-- TODO include salt here?
data NodeUpData = NodeUpData
  { keepAliveRng :: StdGen
  }
  deriving (Read, Show)

data NetPlan = NetPlan {unNetPlan :: Map DiffTime (NonEmpty PlannedEvent)}
  deriving (Generic)
  deriving (Read, Show) via Quiet NetPlan

instance Monoid NetPlan where
  mempty = emptyNetPlan
instance Semigroup NetPlan where
  NetPlan l <> NetPlan r = NetPlan $ Map.unionWith (<>) l r

emptyNetPlan :: NetPlan
emptyNetPlan = NetPlan Map.empty

eventNetPlan :: DiffTime -> PlannedEvent -> NetPlan
eventNetPlan s e = NetPlan $ Map.singleton s $ e NE.:| []

eventsNetPlan :: DiffTime -> [PlannedEvent] -> NetPlan
eventsNetPlan s = mconcat . map (eventNetPlan s)

truncateNetPlan :: DiffTime -> NetPlan -> NetPlan
truncateNetPlan tot (NetPlan m) =
    NetPlan $ Map.filterWithKey (\t _ -> t < tot) m

plannedNumCoreNodes :: NetPlan -> NumCoreNodes
plannedNumCoreNodes plan =
    NumCoreNodes $ fromIntegral $
    Set.size $
    foldMap (foldMap f) $ Map.elems $ unNetPlan plan
  where
    f :: PlannedEvent -> Set CoreNodeId
    f PlannedConnect{}                  = Set.empty
    f PlannedDisconnect{}               = Set.empty
    f (PlannedNodeUp (NodeId cid _k) _) = Set.singleton cid

data State_validNetPlan = State_validNetPlan
  { svnpConnIds :: !(Map (ClientServerPair CoreNodeId) (Map Int Bool))
    -- ^ The connections we've seen so far, and whether they've been
    -- disconnected.
  , svnpNodeIds :: !(Set CoreNodeId)
    -- ^ The nodes we've seen so far.
    --
    -- TODO add support for multiple instances of the same vertex, once we make
    -- that possible.
  }

validNetPlan :: NetPlan -> Maybe String
validNetPlan =
    \(NetPlan m) ->
      go (State_validNetPlan Map.empty Set.empty) $
      foldMap NE.toList m
  where
    go ::
         State_validNetPlan
      -> [PlannedEvent]
      -> Maybe String
    go st = \case
        []     -> Nothing
        ev:evs -> case go1 st ev of
          Left s    -> Just $ s <> ": " <> show ev
          Right st' -> go st' evs

    go1 ::
         State_validNetPlan
      -> PlannedEvent
      -> Either String State_validNetPlan
    go1 st = \case
        PlannedConnect (ConnectionId cidPair k) _delays _ntnVersion ->
            if k == next
            then Left $ "not the next edge instance (" <> show next <> ")"
            else Right st
              { svnpConnIds =
                  Map.insertWith
                    Map.union   -- in this @else@, k won't collide
                    cidPair
                    (Map.singleton k True)
                    connIds
              }
          where
            next = nextConnId cidPair
        PlannedDisconnect connId ->
            if isActiveConnection connId
            then Right st
            else Left $ "no such _active_ edge instance"
        PlannedNodeUp (NodeId cid k) _nodeUpData
            | k /= 0
            -> Left "we don't yet support node restarts (k must be 0)"
            | Set.member cid nodeIds
            -> Left "we don't yet support node restarts (already up)"
            | otherwise
            -> Right st
              { svnpNodeIds = Set.insert cid nodeIds
              }
      where
        State_validNetPlan
          { svnpConnIds = connIds
          , svnpNodeIds = nodeIds
          } = st

        nextConnId cidPair =
            fromMaybe 0 $
            fmap (+1) $
            Map.lookup cidPair connIds >>= Set.lookupMax . Map.keysSet

        isActiveConnection (ConnectionId cidPair k) =
            case Map.lookup cidPair connIds of
              Nothing -> False
              Just m  -> case Map.lookup k m of
                  Nothing -> False
                  Just b  -> b

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | A dedicated type for intentionally ignoring a value.
data UNDERSCORE = UNDERSCORE
instance Show UNDERSCORE where show _ = "_"

data BeginEnd = Begin | End (ExitCase UNDERSCORE)
  deriving (Show)

-- | Events useful for debugging and observing the ThreadNet infrastructure.
data ThreadNetEvent blk =
    ChaperoneDown ClientServer PeerId
  | Handshake ClientServer PeerId
  | MpEvent MP ClientServer PeerId BeginEnd
  deriving (Show)

-- | Events useful for debugging and observing the ThreadNet infrastructure.
data ThrottlerEvent m =
    EndOfDays (ThrottlerState m)
  | FlushingOnce FlushingState
    -- ^ The flushing state that started this flush.
  | FlushingNoneInflight Word64
    -- ^ How many total messages have ever been sent (ie fingerprint).
    --
    -- As of this event, each message has also been received.
  | FlushingNewMessages Word64
    -- ^ How many total messages have ever been sent (ie fingerprint).
    --
    -- This event happens as soon as some new messages are sent.
  | FlushingQuiescence FlushingState
    -- ^ The flushing state that this quiesence ended.
  | ForgeTransition (ThrottlerState m)
    -- ^ The new state that the corresponding forge entry/exit triggered.

-- undecidable
deriving instance Show (ThreadId m) => Show (ThrottlerEvent m)

{-------------------------------------------------------------------------------
  The set of mini protocols
-------------------------------------------------------------------------------}

-- | An enumeration of the mini protocols.
data MP = BF | CS | KA | TS
  deriving (Eq, Show)

-- | One value per mini protocol.
data MpTuple a = MpTuple {mpBF, mpCS, mpKA, mpTS :: a}
  deriving (Foldable, Functor, Read, Show, Traversable)

instance Applicative MpTuple where
  pure  = mpConst
  (<*>) = mpZipWith id

mpMP :: MpTuple MP
mpMP = MpTuple {
    mpBF = BF
  , mpCS = CS
  , mpKA = KA
  , mpTS = TS
  }

mpProtocolNum :: MpTuple MiniProtocolNum
mpProtocolNum = MpTuple {
    mpBF = NTN.blockFetchMiniProtocolNum
  , mpCS = NTN.chainSyncMiniProtocolNum
  , mpKA = NTN.keepAliveMiniProtocolNum
  , mpTS = NTN.txSubmissionMiniProtocolNum
  }

mpZipWith :: (a -> b -> c) -> MpTuple a -> MpTuple b -> MpTuple c
mpZipWith f a b = MpTuple {
    mpBF = mpBF a `f` mpBF b
  , mpCS = mpCS a `f` mpCS b
  , mpKA = mpKA a `f` mpKA b
  , mpTS = mpTS a `f` mpTS b
  }

mpConst :: a -> MpTuple a
mpConst a = MpTuple a a a a

-- | In no particular order.
mpTupleNonEmpty :: MpTuple a -> NE.NonEmpty a
mpTupleNonEmpty mpTuple =
    mpBF NE.:| [mpCS, mpKA, mpTS]
  where
    MpTuple {
        mpBF
      , mpCS
      , mpKA
      , mpTS
      } = mpTuple

{-------------------------------------------------------------------------------
  Reset slots
-------------------------------------------------------------------------------}

-- | The state of the /governor/, which overrides the leader schedule in order
-- to ensure that synchronized nodes never desynchronize.
data ThrottlerState m =
    Observing BlockNo
    -- ^ Who can lead? 'DoNotSuppress' leaders in this state.
    --
    -- End of this state? Monitor the number of active slots since the last reset.
    -- At the end of the @k@-th such slot transition to 'Flushing1'.
    --
    -- The argument is the successor of the block number of the block forged in
    -- the latest reset slot, ie the " next " block number.
  |
    Flushing1 SlotNo
    -- ^ Who can lead? 'DoSuppress' all leaders in this state, except:
    -- 'DoNotSuppress' additional leaders of the @k@-th active slot.
    --
    -- End of this state? Monitor all message channels. Once they're flushed /and/
    -- will remain empty (ie the whole net is synchronized), transition to
    -- 'Extending'.
    --
    -- The argument is the slot of the first @kth@ block. We use it to
    -- implement the above exception to the leadership suppression.
    --
    -- NOTE Ideally, we'd switch from 'Observing' to 'Flushing1' " between "
    -- the @k@-th active slot and the subsequent slot. In that case, the rule
    -- would simply be 'DoSuppress' all leaders. However, such a moment is
    -- difficult to find precisely, so we instead avoid potential races by
    -- transitioning to 'Flushing1' during the @k@-th active slot (as soon as
    -- we notice that it is an active slot) and compensate with the above
    -- exception in the suppression rule.
  |
    Extending (Maybe (NodeId, ThreadId m))
    -- ^ Who can lead? 'DoSuppress' all but exactly one successful leader in
    -- this state.
    --
    -- End of this state? After the first leader successfully leads (thereby
    -- determing the @k+1@th active slot), transition to 'Flushing2'.
    --
    -- The argument is the identify of the potential leader currently
    -- attempting to forge a block. We implement 'Extending' by treating (the
    -- second half of) the forge (ie after 'TraceNodeIsLeader') as a critical
    -- section, and this argument identifies the current occupant if any.
    --
    -- TODO An individual node may spawn multiple instances of the forging
    -- logic, so we need to use a richer identifier when enforcing the critical
    -- section. EG ThreadId would work (cf 'onEachChange'). Could we use a more
    -- standard/explicit mutex cell here?
    --
    -- TODO Should we attempt to make entry fair? Maybe we should jitter the
    -- nodes' entry attempts?
  |
    Flushing2 SlotNo BlockNo
    -- ^ Who can lead? 'DoSuppress' all leaders in this state, even those that
    -- are still waiting their turn to lead during the @k+1@th active slot.
    --
    -- End of this state? Monitor all message channels. Once they're flushed
    -- /and/ would otherwise remain empty (ie the whole net is synchronized
    -- again), transition to 'Observing'.
    --
    -- The first argument is the slot of the @k+1@th block, but it's merely for
    -- more helpful error messages.
    --
    -- The second argument is the block number of the @k+1@th block.
    --
    -- NOTE Similar to the note above for 'Flushing1', we transition to
    -- 'Flushing2' during the @k+1@th slot. In this case, though, this mid-slot
    -- transition simplifies the implementation without any further
    -- complication: 'Flushing2' always suppresses all leaders, and that's what
    -- we require for the remaining potential leaders of the @k+1@th slot.

  |

    -- The aboves states cover " normal " operation. The below states are only
    -- used when ending the simulation.

    LastFlush
    -- ^ Who can lead? 'DoSuppress' all leaders in this state.
    --
    -- End of this state? Monitor all message channels. Once they're flushed
    -- /and/ would otherwise remain empty (ie the whole net is synchronized
    -- again), transition to 'Shutdown'.
    --
    -- At the prescribed time, the infrastructure forcibly truncates whichever
    -- of the preceding " normal " states state the Throttler happens to be in
    -- to this state. So: this state could be preceded by any of the above
    -- states.
  |
    Shutdown
    -- ^ Who can lead? 'DoSuppress' all leaders in this state.
    --
    -- End of this state? None, the simulation terminates during this state.

-- undecidable
deriving instance Eq   (ThreadId m) => Eq   (ThrottlerState m)
-- undecidable
deriving instance Show (ThreadId m) => Show (ThrottlerState m)

-- | The subset of flushing states
--
-- Either 'Flushing1', 'Flushing2', or 'LastFlush'.
data FlushingState = FlushingState1 SlotNo | FlushingState2 SlotNo BlockNo | FlushingStateLast
  deriving (Show)

-- | The key decision made by the governor: whether to force a node to exit
-- early from the forging logic, as if it was not scheduled to lead, even the
-- protocol did schedule it to lead.
data WhetherToSuppress = DoSuppress | DoNotSuppress

-- | How a node exited the forging logic.
--
-- We conceptually divide the forge logic in "Ouroboros.Consensus.NodeKernel"
-- into three steps.
--
--  1 Check if you're a leader. A node " exits early " when they can't even
--    check (eg are too far behind), are simply not a leader, the governor
--    suppressed them, etc.
--
--  2 Forge a block.
--
--  3 Adopt that block.
data ForgeExitCase =
    ForgeExitEarly
    -- ^ It exited before Step 2.
  |
    ForgeFail
    -- ^ It exited after Step 1 and before Step 3.
    --
    -- TODO In our current test suite design, this event only correponds to
    -- fatal errors. So remove this?
  |
    ForgeSuccess BlockNo
    -- ^ It completed all Steps, forging a block with this 'blockNo'.
  deriving (Show)
