% Example: A More Complex Example Protocol Involving Epochs

Introduction
============

This tutorial builds on the example in `Tutorial.lhs` by specifying a block,
ledger, and protocol for a blockchain that has a notion of epoch built into it -
serving as an example of a case where the state of the ledger is used as part of
the machinery of consensus.  It is highly recommended that the reader have read
`Tutorial.lhs` before this if they are not already familiar with concepts such
as `ConsensusProtocol` and `LedgerState`.

Much like the previous example of `BlockC` this blockchain (with block type
`BlockD`) models a single number resulting from a series of _increment_ and
_decrement_ transactions in the block bodies.  However, the slots are now
divided into **epochs** each of which has a fixed number of slots.

Further, the chain is set up such that there are 20 _nodes_ labelled with a
`NodeId` from 0 to 19 participating in the computation.

At the beginning of an epoch, the current value of the ledger - that is, the sum
of the _increment_ transactions minus the sum of the _decrement_ transactions is
snapshotted as part of the ledger.

During the epoch, the snapshot *from two epochs ago* determines which subset of
a set of 20 nodes is allowed to lead slots.  If the snapshot is even, then slots
contained in that epoch follow a round-robin leadership schedule among nodes 0
through 9 inclusive.  Similarly, if the snapshot is odd then the slots of that
epoch will be follow a round-robin leadership schedule from the set of nodes
from 10 to 19.

Though it is difficult to imagine a real system doing this, it is a simple
example of a case where the value computed by the blockchain (the `LedgerState`)
is relevant - through the leader schedule - to the behavior of consensus.  One
can perhaps view it as a very simplified analog to stake distribution.

Setup
-----

As before, we require a few language extensions:

> {-# OPTIONS_GHC -Wno-unused-top-binds   #-}
> {-# LANGUAGE TypeFamilies               #-}
> {-# LANGUAGE DerivingVia                #-}
> {-# LANGUAGE DataKinds                  #-}
> {-# LANGUAGE DeriveGeneric              #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DeriveAnyClass #-}
> {-# LANGUAGE StandaloneDeriving         #-}

> module Ouroboros.Consensus.Tutorial.WithEpoch () where

And imports, of course:

> import Control.Monad ()
> import Control.Monad.Except (MonadError (throwError))
> import Data.Word (Word64)
> import GHC.Generics (Generic)
> import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
> import Data.Hashable (Hashable (hash))
> import Codec.Serialise (Serialise)
> import Ouroboros.Network.Point ()
> import Ouroboros.Network.Block ()
> import Ouroboros.Consensus.Block.Abstract
>  (Header, SlotNo (..), HeaderHash, ChainHash, GetHeader (..),
>   GetPrevHash (..), HasHeader (..), HeaderFields (..), StandardHash,
>   BlockProtocol, castHeaderFields, blockNo, BlockConfig, CodecConfig,
>   StorageConfig, Point, castPoint, WithOrigin (..), EpochNo (EpochNo),
>   pointSlot, blockPoint, BlockNo (..))
> import Ouroboros.Consensus.Block.SupportsProtocol
>   (BlockSupportsProtocol (..))
> import Ouroboros.Consensus.Protocol.Abstract
>   (ConsensusConfig, SecurityParam, ConsensusProtocol (..))
>
> import Ouroboros.Consensus.Ticked (Ticked1, Ticked)
> import Ouroboros.Consensus.Ledger.Abstract
>   (LedgerState, LedgerCfg, GetTip, LedgerResult (..), ApplyBlock (..),
>    UpdateLedger, IsLedger (..))
>
> import Ouroboros.Consensus.Ledger.SupportsMempool ()
> import Ouroboros.Consensus.Ledger.SupportsProtocol
>   (LedgerSupportsProtocol (..))
>
> import Ouroboros.Consensus.HeaderValidation
>   (ValidateEnvelope, BasicEnvelopeValidation, HasAnnTip)
> import Ouroboros.Consensus.Forecast
>   (Forecast (..), OutsideForecastRange (..))
> import Ouroboros.Consensus.Ledger.Basics (GetTip(..))
> import Ouroboros.Consensus.Ledger.Tables


Epochs
------

Epochs occur at a fixed interval:

> slotsInEpoch :: Word64
> slotsInEpoch = 50

We also write a function from `WithOrigin SlotNo` to the corresponding epoch
number to express this behavior.  The `WithOrigin` type allows us to describe
the behavior of this in the presence of the first (origin) block on the chain.
We will consider `Origin` to be a special epoch before the epochs of the slots
proper:

> epochOf :: WithOrigin SlotNo -> WithOrigin EpochNo
> epochOf Origin = Origin
> epochOf (NotOrigin s) = NotOrigin $ EpochNo $ unSlotNo s `div` slotsInEpoch

From a particular `WithOrigin Slot` we can also determine when the next epoch
will begin:

> nextEpochStartSlot :: WithOrigin SlotNo -> SlotNo
> nextEpochStartSlot wo =
>   SlotNo $ case wo of
>              Origin -> slotsInEpoch
>              NotOrigin slot -> slotsInEpoch + slot' - (slot' `mod` slotsInEpoch)
>                                where
>                                 slot' = unSlotNo slot

Hashing
-------

In this tutorial, we'll use a more real (but still very contrived) hashing
infrastructure built on top of the `Hashable` class from the `hashable` package.

Our infastructure is quite simple - a `newtype` for hashes:

> newtype Hash = Hash Int
>   deriving stock (Eq, Ord, Show, Generic)
>   deriving newtype (NoThunks, Hashable, Serialise)

And a function to turn `Hashable` things into values of type `Hash`.

> mkHash :: Hashable a => a -> Hash
> mkHash = Hash . hash

We'll instantiate `Hashable` things as we need them throughout this example.


Node Identifiers
----------------

We'll also need a notion of node identity since this will be used to specify
some things with respect to leadership.  We'll assume all nodes that can lead
have identifiers in the range of 0..19.

> type NodeId = Word64

Block
=====

Block Type
----------

Our block and transaction type is structurally the same as `BlockC` - a header
followed by a list of transactions `Tx`:

> data BlockD = BlockD { bd_header :: Header BlockD
>                      , bd_body :: [Tx]
>                      }
>   deriving NoThunks via OnlyCheckWhnfNamed "BlockD" BlockD

> data Tx = Inc | Dec
>   deriving (Show, Eq, Generic, Serialise, NoThunks, Hashable)


Block Header
------------

However, the header is somewhat different as it includes the node identifier
that created the block.  Naturally in a more realistic system this would instead
be some value that provides stronger (probably cryptographic) evidence for this
block's provenance but since this is a simple example, we will simply use a
`NodeId`:

> data instance Header BlockD =
>   HdrBlockD
>     { hbd_SlotNo :: SlotNo
>     , hbd_BlockNo :: BlockNo
>     -- hash of whole block (excepting this field)
>     , hbd_Hash :: HeaderHash BlockD
>     , hbd_prev :: ChainHash BlockD
>     , hbd_nodeId :: NodeId
>     }
>   deriving stock (Show, Eq, Generic)
>   deriving anyclass (Serialise)
>   deriving NoThunks via OnlyCheckWhnfNamed "HdrBlockD" (Header BlockD)


Block Hashing
-------------

Since we're using a more complicated hashing scheme for `BlockD` than we used
for `BlockC` we'll also need to specify how we compute hashes for `BlockD`.
First, we specify the type of `HeaderHash BlockD` to be the `Hash` type we
defined earlier:

> type instance HeaderHash BlockD = Hash

> instance StandardHash BlockD

Then we define a function `computeBlockHash` which computes a `Hash` for a
`BlockD` - basically aggregating all the data in the block besides the hash
itself:

> computeBlockHash :: BlockD -> Hash
> computeBlockHash (BlockD hdr body) =
>   mkHash ( unSlotNo (hbd_SlotNo hdr)
>          , unBlockNo (hbd_BlockNo hdr)
>          , hbd_prev hdr
>          , body
>          )

Finally, a convenience function `addBlockHash` allows us to properly set the
hash of a `BlockD` to its computed value:

> addBlockHash :: BlockD -> BlockD
> addBlockHash b = b { bd_header = header' }
>   where
>     header' = (bd_header b) { hbd_Hash = computeBlockHash b }

The preceding definitions require that `ChainHash BlockD` be `Hashable` so we
derive a suitable instance here:

> deriving instance Hashable (ChainHash BlockD)


Block Header
------------

As before, we to implement a few type families to fully specify the header -
`GetHeader`, `GetPrevHash`, and `HasHeader`:

> instance GetHeader BlockD where
>   getHeader          = bd_header
>
>   blockMatchesHeader hdr blk =
>     hbd_Hash hdr == computeBlockHash blk
>
>   headerIsEBB      _ = Nothing

> instance GetPrevHash BlockD where
>   headerPrevHash = hbd_prev

> instance HasHeader (Header BlockD) where
>   getHeaderFields hdr = HeaderFields
>                           { headerFieldSlot = hbd_SlotNo hdr
>                           , headerFieldBlockNo = hbd_BlockNo hdr
>                           , headerFieldHash = hbd_Hash hdr
>                           }

> instance HasHeader BlockD where
>   getHeaderFields = castHeaderFields
>                   . getHeaderFields
>                   . bd_header

As part of our implementation of hashing, note that `blockMatchesHeader` in
`GetHeader` now checks that the hash is correct.

Block Configuration
-------------------

In this example, there is no interesting static configuration for blocks - so
the following are trivial instances:

> data instance CodecConfig BlockD = CCfgBlockD
>   deriving (Generic, NoThunks)

> data instance StorageConfig BlockD = SCfgBlockD
>   deriving (Generic, NoThunks)

> data instance BlockConfig BlockD = BCfgBlockD
>   deriving (Generic, NoThunks)


Validation
----------

Similarly, this example does not have any interesting validation logic, but a
few more trivial implementations are needed:

> instance HasAnnTip BlockD where {}
> instance ValidateEnvelope BlockD where {}
> instance BasicEnvelopeValidation BlockD where {}


Ledger
======

While this is similar to the `LedgerState` for `BlockC` the `Ledger` instance
corresponding to `BlockD` needs to hold snapshots of the count at the last two
epoch boundaries - this is the `lsbd_snapshot1` and `lsbd_snapshot2` fields
below:

> data instance LedgerState BlockD mk =
>   LedgerD
>     { lsbd_tip :: Point BlockD    -- ^ Point of the last applied block.
>                                   --   (Point is header hash and slot no.)
>     , lsbd_count :: Word64        -- ^ results of the up/down Txs
>     , lsbd_snapshot1 :: Word64    -- ^ snapshot of lsbd_count at
>                                   --   end of previous epoch (1 epoch ago)
>     , lsbd_snapshot2 :: Word64    -- ^ snapshot of lsbd_count at end
>                                   --   of epoch (2 epochs ago)
>                                   --   This will be the LedgerView that
>                                   --   influences the leader schedule.
>     }
>   deriving (Show, Eq, Generic, Serialise, NoThunks)

There is no interesting static configuration for this ledger:

> type instance LedgerCfg (LedgerState BlockD) = ()

Our `GetTip` implementation retrieves the tip from the `lsbd_tip` field:

> instance GetTip (Ticked1 (LedgerState BlockD)) where
>  getTip = castPoint . lsbd_tip . unTickedLedgerStateD

> instance GetTip (LedgerState BlockD) where
>   getTip = castPoint . lsbd_tip

Ticking
-------

`LedgerState BlockD` also needs a corresponding `Ticked1` instance which is still
very simple:

> newtype instance Ticked1 (LedgerState BlockD) mk =
>   TickedLedgerStateD {
>     unTickedLedgerStateD :: LedgerState BlockD mk
>   }
>   deriving stock (Show, Eq, Generic)
>   deriving newtype (NoThunks, Serialise)

Because the ledger now needs to track the snapshots in `lsbd_snapshot1` and
`lsbd_snapshot2` we can express this in terms of ticking a `LedgerState BlockD`.
We'll write a function (that we'll use later) to express this relationship
computing the `Ticked1 (LedgerState BlockD)` resulting from a starting
`LedgerState BlockD` being ticked to some slot in the future - assuming no
intervening blocks are applied:

> tickLedgerStateD ::
>   SlotNo -> LedgerState BlockD mk -> Ticked1 (LedgerState BlockD) mk
> tickLedgerStateD newSlot ldgrSt =
>   TickedLedgerStateD $
>     if isNewEpoch then
>       ldgrSt{ lsbd_snapshot2 = lsbd_snapshot1 ldgrSt
>                  -- save previous epoch snapshot (assumes we do not
>                  -- go a full epoch without ticking)
>             , lsbd_snapshot1 = lsbd_count ldgrSt
>                  -- snapshot the count (at end of previous epoch)
>             }
>     else
>       ldgrSt
>
>   where
>   isNewEpoch =
>     case compare
>            (epochOf (pointSlot $ lsbd_tip ldgrSt)) -- epoch of last block added
>            (epochOf (NotOrigin newSlot))           -- epoch of newSlot
>     of
>       LT -> True
>       EQ -> False
>       GT -> error "cannot tick slots backwards"

Note that this implementation merely projects the current `lsbd_count` into the
snapshot indefinitely far in the future - consistent with the assumption that no
blocks are applied during the span of time represented by the slot argument.

We can now use `tickLedgerStateD` to instantiate `IsLedger`:

> instance IsLedger (LedgerState BlockD) where
>   type instance LedgerErr (LedgerState BlockD) = String
>   type instance AuxLedgerEvent (LedgerState BlockD) = ()
>
>   applyChainTickLedgerResult _cfg slot ldgrSt =
>     LedgerResult { lrEvents = []
>                  , lrResult = tickLedgerStateD slot $ convertMapKind ldgrSt
>                  }

`UpdateLedger` is necessary but its implementation is always empty:

> instance UpdateLedger BlockD where {}

Applying Blocks
---------------

Applying a `BlockD` to a `Ticked1 (LedgerState BlockD)` is (again) the result of
applying each individual transaction - exactly as it was in for `BlockC`:

> applyBlockTo :: BlockD -> Ticked1 (LedgerState BlockD) mk -> LedgerState BlockD mk
> applyBlockTo block tickedLedgerState =
>   ledgerState { lsbd_tip = blockPoint block
>               , lsbd_count = lsbc_count'
>               }
>   where
>     ledgerState = unTickedLedgerStateD tickedLedgerState
>     lsbc_count' = foldl txDelta (lsbd_count ledgerState) (bd_body block)
>     txDelta i tx =
>       case tx of
>         Inc -> i + 1
>         Dec -> i - 1

> instance ApplyBlock (LedgerState BlockD) BlockD where
>   applyBlockLedgerResult _ldgrCfg b tickedLdgrSt =
>     pure LedgerResult { lrResult = convertMapKind $ b `applyBlockTo` tickedLdgrSt
>                       , lrEvents = []
>                       }
>
>   reapplyBlockLedgerResult _ldgrCfg b tickedLdgrSt =
>     LedgerResult { lrResult = convertMapKind $ b `applyBlockTo` tickedLdgrSt
>                  , lrEvents = []
>                  }
>
>   getBlockKeySets = const NoLedgerTables

Note that prior to `applyBlockLedgerResult` being invoked, the calling code will
have already established that the header is valid and that the header matches
the block.  As a result, we do not need to check that the leader is correct
here.  Also, for this tutorial's notion of blocks applying blocks cannot fail
because applying transactions cannot fail - even in cases where there is
overflow `Data.Word` will wrap around.

Protocol
========

Following the practice earlier established, we define an empty marker type for
the protocol - `PrtclD`:

> data PrtclD

However, due to the fact that ability to be a slot leader now depends on the
ledger we'll have a (slightly) more interesting set of evidence that a
particular `NodeId` can be a leader.  This just consists of the `NodeId` itself
- which is obviously insecure in that the proof we can be a leader should not be
easy to falsify - it is fine for our example:

> data PrtclD_CanBeLeader = PrtclD_CanBeLeader NodeId
>   deriving (Eq, Show, Generic, NoThunks)

The proof that we are a slot leader should be evident from the context - the
combination of the slot number and the ledger's snapshot parity is sufficient
evidence that a particular node is the leader for that slot - this makes our
evidence that a particular node is the leader uninteresting given that security
is not being considered in this example:

> data PrtclD_IsLeader    = PrtclD_IsLeader

With some notions of the types involved in leadership defined, we can now
instantiate the `ConsensusConfig` with our security parameter as well as the
particular `NodeId` - expressed as a `PrtclD_CanBeLeader` - that a particular
instance of the `ConsensusProtocol` should be running as:

> data instance ConsensusConfig PrtclD =
>   PrtclD_Config
>     { ccpd_securityParam :: SecurityParam  -- ^ i.e., 'k'
>     , ccpd_mbCanBeLeader :: Maybe PrtclD_CanBeLeader
>
>       -- ^ To lead, a node must have a 'ccpd_mbCanBeLeader' equal to
>       -- `Just (PrtclD_CanBeLeader nodeid)`.
>       -- We expect this value would be extracted from a config file.
>       --
>       -- Invariant: nodeid's are unique.
>     }
>   deriving (Eq, Show)
>   deriving NoThunks via OnlyCheckWhnfNamed "PrtclD_Config"
>                         (ConsensusConfig PrtclD)

We will also need to have a view of the ledger that contains enough information
for the protocol to validate the leadership claim.  Since the leader for a slot
is determined by the parity of the epoch snapshot along with the slot number,
our `LedgerView` for `PrtclD` will simply be the snapshot (though we could have
just as easily used a `Bool` representing the parity):

> newtype LedgerViewD = LVD Word64
>   deriving stock (Show, Eq, Generic)
>   deriving newtype (Serialise, NoThunks)

We also define a trivial `Ticked LedgerViewD` instance:

> newtype instance Ticked LedgerViewD = TickedLedgerViewD LedgerViewD
>   deriving stock (Show, Eq, Generic)
>   deriving newtype (Serialise, NoThunks)

The parity of the epoch snapshot and the slot are together _sufficient_ to
determine the leadership schedule.  As such, we do not need any notion of state
specific to `PrtclD`:

> data ChainDepStateD = ChainDepStateD
>   deriving (Eq,Show,Generic,NoThunks)

However, the `Ticked1` representation contains the `LedgerViewD` containing the
epoch snapshot.  This is due to functions for `ConsensusProtocol` only taking
the `LedgerView` as an argument in some cases:

> data instance Ticked ChainDepStateD =
>   TickedChainDepStateD { tickedChainDepLV :: LedgerViewD }
>   deriving (Eq, Show, Generic, NoThunks)

`ConsensusProtocol` is set up this way mostly because this is what
implementations thus far have required, but the organization of the functions
interacting with `ChainDepState` is currently under review.

We can determine if a particular node is the leader of a slot given the slot
number along with the epoch snapshot.  We can express this determination via a
function modeling the leadership schedule.  For ease of use in our instantiation
of `ConsensusProtocol PrtclD` we will represent the epoch snapshot using the
`LedgerView` we just defined:

> isLeader :: NodeId -> SlotNo -> LedgerView PrtclD -> Bool
> isLeader nodeId (SlotNo slot) (LVD cntr) =
>   case cntr `mod` 2 of
>     -- nodes [0..9]   do round-robin (if even cntr)
>     0 -> slot `mod` 10      == nodeId
>     -- nodes [10..19] do round-robin (if odd cntr)
>     1 -> (slot `mod` 10)+10 == nodeId
>     _ -> error "panic: the impossible ..."

Now we can instantiate `ConsensusProtocol PrtclD` proper with the types and
functions defined above:

> instance ConsensusProtocol PrtclD where
>
>   type ChainDepState PrtclD = ChainDepStateD
>   type IsLeader PrtclD = PrtclD_IsLeader
>   type CanBeLeader PrtclD = PrtclD_CanBeLeader
>
>   -- | View on a block header required for chain selection.  Here, BlockNo is
>   --   sufficient. (BlockNo is also the default type for this type family.)
>   type SelectView PrtclD = BlockNo
>
>   -- | View on the ledger required by the protocol
>   type LedgerView PrtclD = LedgerViewD
>
>   -- | View on a block header required for header validation
>   type ValidateView  PrtclD = NodeId  -- need this for the leader check
>                                       -- currently not doing other checks
>
>   type ValidationErr PrtclD = String
>
>   -- | checkIsLeader - Am I the leader this slot?
>   checkIsLeader cfg _cbl slot tcds =
>     case ccpd_mbCanBeLeader cfg of
>       Just (PrtclD_CanBeLeader nodeId)
>         -- not providing any cryptographic proof
>         | isLeader nodeId slot (tickedChainDepLV tcds) -> Just PrtclD_IsLeader
>       _                             -> Nothing
>
>   protocolSecurityParam = ccpd_securityParam
>
>   tickChainDepState _cfg tlv _slot _cds = TickedChainDepStateD lv
>     where
>       TickedLedgerViewD lv = tlv
>
>   -- | apply the header (hdrView) and do a header check.
>   --
>   -- Here we check the block's claim to lead the slot (though in Protocol D,
>   -- this doesn't give us too much confidence, as there is nothing that
>   -- precludes a node from masquerading as any other node).
>
>   updateChainDepState _cfg hdrVw slot tcds =
>     if isLeader hdrVw slot (tickedChainDepLV tcds) then
>       return ChainDepStateD
>     else
>       throwError $ "leader check failed: " ++ show (hdrVw,slot)
>
>   reupdateChainDepState _ _ _ _ = ChainDepStateD

Integration
===========

Block/Protocol Integration
--------------------------

Our implementation of `BlockSupportsProtocol BlockD` supports our definition of
`ConsensusProtocol PrtclD` closely, with `validateView` extracting the `NodeId`
from the block header, and `selectView` projecting out the block number:

> instance BlockSupportsProtocol BlockD where
>   validateView _bcfg hdr = hbd_nodeId hdr
>
>   selectView _bcfg hdr = blockNo hdr

All that remains is to establish `PrtclD` as the protocol for
`BlockD`:

> type instance BlockProtocol BlockD = PrtclD

Ledger/Protocol Integration
---------------------------

Implementing `LedgerSupportsProtocol` requires us to put a little more thought
into forecasting.  Our range of forecasting now ends at the last slot in the
following epoch.  There are two cases for which we can forecast the ticked
ledger view: (1) the slot (`for` in the code below) is in the current epoch and
(2) the slot is in the following epoch.

> instance LedgerSupportsProtocol BlockD where
>   protocolLedgerView _ldgrCfg (TickedLedgerStateD ldgrSt) =
>     TickedLedgerViewD (LVD $ lsbd_snapshot2 ldgrSt)
>       -- note that we use the snapshot from 2 epochs ago.
>
>   -- | Borrowing somewhat from Ouroboros/Consensus/Byron/Ledger/Ledger.hs
>   ledgerViewForecastAt _lccf ldgrSt =
>     Forecast { forecastAt = at
>              , forecastFor = \for->
>                  if NotOrigin for < at then
>                    error "this precondition violated: 'NotOrigin for < at'"
>                  else if for >= maxFor then
>                    throwError
>                      OutsideForecastRange
>                         { outsideForecastAt     = at
>                         , outsideForecastMaxFor = maxFor
>                         , outsideForecastFor    = for
>                         }
>                  else
>                    return
>                      $ TickedLedgerViewD $ LVD
>                      $ if for < nextEpochStartSlot at then
>                          lsbd_snapshot2 ldgrSt
>                            -- for the rest of the current epoch,
>                            -- it's the same as 'protocolLedgerView',
>                            -- using snapshot from 2 epochs ago.
>                        else
>                          lsbd_snapshot1 ldgrSt
>                            -- we can forecast into the following epoch because
>                            -- we have the snapshot from 1 epoch ago.
>              }
>
>     where
>     -- | the current slot that the ledger reflects
>     at :: WithOrigin SlotNo
>     at = pointSlot $ lsbd_tip ldgrSt
>
>     -- | 'maxFor' is the "exclusive upper bound on the range of the forecast"
>     -- (the name "max" does seem wrong, but we are following suit with the names
>     -- and terminology in the 'Ouroboros.Consensus.Forecast' module)
>     --
>     -- In our case we can forecast for the current epoch and the following
>     -- epoch.  The forecast becomes unknown at the start of the epoch
>     -- after the following epoch:
>     maxFor :: SlotNo
>     maxFor = nextEpochStartSlot at + SlotNo slotsInEpoch

Summary and Review
==================

Above, we defined a block, ledger, and consensus protocol as well as wrote the
class/family instances necessary to connect them together.  The behavior of the
blockchain modeled by these is very simple and does not deal with security
considerations in any depth but does implement behavior - the leadership
schedule - such that the valid future states of the chain depend on the value
(aka `LedgerState`) computed by the chain in previous epochs.

To review, we made a few changes from our even more trivial prior example
involving `BlockC`:

- We implemented a slightly more realistic version of hashing
- Nodes participating in the protocols were given identifiers allowing a less
  trivial version of leadership to be implemented
- A node identifier was added to our block type
- The `LedgerState` for our block type required the relevant data to be
  snapshotted so that it could be tracked
- The logic for applying blocks to the ledger in `ApplyBlock` needed to be aware
  of the epoch changes such that it could update the snapshots accordingly when
  blocks are applied
- The `checkIsLeader` in the protocol changed to reflect the leadership schedule
- The protocol's `LedgerView` also needed access to the relevant snapshot for
  the epoch
- Forecasting, as implemented in `LedgerSupportsProtocol`, needed to know when
  to stop being able to forecast - namely when the supply of snapshot data is
  exhausted

While this is a large ecosystem of interrelated typeclasses and families, the
overall organization of things is such that Haskell's type checking can help
guide the implementation.

Appendix: UTxO-HD features
==========================

For reference on these instances and their meaning, please see the appendix in
[the Simple tutorial](./Simple.lhs).

> instance HasLedgerTables (LedgerState BlockD) where
>   data LedgerTables (LedgerState BlockD) mk =
>        NoLedgerTables
>        deriving (Generic, Eq, Show, NoThunks)
>
> instance HasTickedLedgerTables (LedgerState BlockD) where
>   withLedgerTablesTicked (TickedLedgerStateD st) tbs =
>     TickedLedgerStateD (withLedgerTables st tbs)
>
> instance CanSerializeLedgerTables (LedgerState BlockD)
> instance CanStowLedgerTables (LedgerState BlockD)
>
> instance LedgerTablesAreTrivial (LedgerState BlockD) where
>   convertMapKind (LedgerD t c a b) = LedgerD t c a b
>   trivialLedgerTables = NoLedgerTables
