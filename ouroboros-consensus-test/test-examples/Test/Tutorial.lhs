% Example: Implementing a Simple Protocol Using `ouroborus-consensus`

Introduction and Motivation
===========================

This example is a compilable Literate Haskell (`.lhs`) file that
instantiates the `ConsensusProtocol` typeclass to serve as an
example of some of the high-level concepts in `ouroboros-consensus`

This example uses several extensions:

> {-# LANGUAGE TypeFamilies               #-}
> {-# LANGUAGE DerivingVia                #-}
> {-# LANGUAGE DataKinds                  #-}
> {-# LANGUAGE DeriveGeneric              #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE DeriveAnyClass             #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE StandaloneDeriving         #-}

> module Test.Tutorial() where

First, some imports we'll need:

> import Data.Void(Void)
> import Data.Set(Set)
> import qualified Data.Set as Set
> import Data.Word(Word64, Word8)
> import GHC.Generics (Generic)
> import Codec.Serialise (Serialise)
> import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
> import Ouroboros.Consensus.Block.Abstract
>   (blockNo, blockPoint, castHeaderFields, castPoint, BlockNo, SlotNo,
>    BlockConfig, BlockProtocol, CodecConfig, GetHeader(..), GetPrevHash(..),
>    Header, StorageConfig, ChainHash, HasHeader(..),
>    HeaderFields(HeaderFields, headerFieldSlot, headerFieldBlockNo,
>                  headerFieldHash),
>     HeaderHash,
>     Point,
>     StandardHash )
> import Ouroboros.Consensus.Protocol.Abstract
> import Ouroboros.Consensus.Ticked
> import Ouroboros.Consensus.Block
>   (BlockSupportsProtocol (selectView, validateView))
> import Ouroboros.Consensus.Ledger.Abstract
>   (GetTip(..), IsLedger(..), LedgerCfg,
>    LedgerResult(LedgerResult, lrEvents, lrResult),
>    LedgerState, ApplyBlock(..), UpdateLedger)
> import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol(..))
> import Ouroboros.Consensus.Forecast (trivialForecast)
> import Ouroboros.Consensus.HeaderValidation
>   (ValidateEnvelope, BasicEnvelopeValidation, HasAnnTip)

Conceptual Overview and Definitions of Key Terms
================================================

The object of interest to consensus is the **blockchain**.

Within the context of this discussion a blockchain is linked-list-style
chain of **blocks**, but its behavior is also subject to an integer-valued
logical clock whose value is known as a **slot**.  The event that
increments this clock is called a **tick**.

Each block is associated single slot, though not every slot is associated with a block.
No two blocks have the same slot.  With that in mind, another way to consider the structure
of a chain is to think of it as as a list of blocks each of which is separated by one or
more ticks.

We can then think of folding over this blockchain structure to compute some
value that summarizes the entire history of the chain (blocks and ticks) in some way.
This same value might also be used to determine if a subsequent block is valid.
Computing this value is the responsibility of the **ledger** and the **ledger state**
is the computed value.

`ouroborus-consensus` combines features of much of this infrastructure taking
(possibly simplified) views of blocks and the ledger and using them to decide
between different proposed chains to implement eventual consistency across
the nodes.

The `Ticked` Family - Modeling the Passage of Time
--------------------------------------------------

Instances of the `Ticked` type family represents things that can evolve with respect
to ticks - `Ticked a` is the type representing an `a` at some number of slots
in the future.

In this tutorial none of the implementations of `Ticked` will be especially
interesting and will more or less be isomorphic to the `Identity` functor.
Even if this is the case, `Ticked` helps us maintain some invariants -
such as it being important that at least one tick happens between blocks.


The `ConsensusProtocol` typeclass
=================================

The central abstraction of `ouroborus-consensus` is the `ConsensusProtocol`
typeclass.  This class captures the relationship between consensus and the
rest of the system (in particular the ledger) as a set of type families.

To demonstrate these relationships, we will begin by defining a simple
protocol creatively named `SP`.

First, we define the type of the protocol itself.  This is a type-level "tag", this does not exist
at the value level.

> data SP

The static configuration for `SP` is defined by defining an instance for the
`ConsensusConfig` type family.  Some of the methods in `ConsensusProtocol` class such as
`checkIsLeader` require an associated `ConsensusConfig p` so we define a simple one here:

> data instance ConsensusConfig SP =
>   SP_Config  { cfgsp_slotsLedByMe :: Set SlotNo
>              }

Next, we instantiate the `ConsensusProtocol` for `SP`:

> instance ConsensusProtocol SP where
>   type SelectView    SP = BlockNo
>
>   type LedgerView    SP = ()
>
>   type IsLeader      SP = SP_IsLeader
>   type CanBeLeader   SP = SP_CanBeLeader
>
>   type ChainDepState SP = ()
>   type ValidateView  SP = ()
>   type ValidationErr SP = Void
>
>   checkIsLeader cfg SP_CanBeLeader slot _tcds =
>       if slot `Set.member` cfgsp_slotsLedByMe cfg
>       then Just SP_IsLeader
>       else Nothing
>
>   protocolSecurityParam _cfg = k
>
>   tickChainDepState _ _ _ _ = TickedTrivial
>
>   updateChainDepState _ _ _ _ = return ()
>
>   reupdateChainDepState _ _ _ _ = ()

Finally we define a few extra things used in this instantiation:

> data SP_CanBeLeader = SP_CanBeLeader -- Evidence that we /can/ be a leader
> data SP_IsLeader = SP_IsLeader       -- Evidence that we /are/ leader
>
> k :: SecurityParam
> k = SecurityParam { maxRollbacks = 1 }

Let's examine each of these in turn:

Chain Selection: `SelectView`
-----------------------------

One of the major decisions when implementing a consensus protocol is encoding a
policy for chain selection.  The `SelectView SP` type represents the information
necessary from a block header to help make this decision.

The other half of this - which explains how a `SelectView` is derived from
a particular block - is expressed by the block's implementation of the
 `BlockSupportsProtocol` typeclass.

The `preferCandidate` function in `Ouroboros.Consensus.Protocol.Abstract`
demonstrates how this is used.

Note that instantiations of `ConsensusProtocol` for some protocol `p`
consequently requires `Ord (SelectView p)`.

For `SP` we will use only `BlockNo` - to implement the simplest
rule of preferring longer chains to shorter chains.


Ledger Integration: `LedgerView`
--------------------------------

Some decisions that a consensus protocol needs to make will depend
on the ledger's state, `LedgerState blk`.  The data required from the ledger
is of type `LedgerView p` (i.e., the protocol determines what is needed).
Similar to `SelectView` the projection of `LedgerState blk` into `LedgerView p` exists
in a typeclass, namely `LedgerSupportsProtocol`.

For `SP` we do not require any information from the ledger to make
decisions of any kind.  In the Praos protocol, the `LedgerView`
contains information about the stake distribution among other things.

Notably, this is used in the `tickChainDepState` function elsewhere in the
`ConsensusProtocol`.


Protocol State: `ChainDepState`, `ValidateView` and `ValidationErr`
----------------------------------------------------------------

`ChainDepState` describes the state of the protocol that evolves with the chain.
Note, from [Cardano Consensus and Storage Layer]: ``we are referring to this as
the “chain dependent state” to emphasize that this is state that evolves with
the chain, and indeed is subject to rollback when we switch to alternatives
forks. This distinguishes it from chain independent state such as evolving
private keys, which are updated independently from blocks and are not subject to
rollback.''

`ValidateView` is a 'view' of a block (header) providing enough information to validate
the block header.
It is called `ValidateView` because the functions used to
compute new states from some combination of a prior `ChainDepState`
and a `ValidateView` can _fail_ - producing a `ValidationErr`.

There are some interesting constraints governing what can appropriately
be used as a type fulfilling the requirements of `ValidateView` - in
particular the fact that `ConsensusProtocol` instances are sometimes called
upon to do _prediction_ rather than just as a pure summary of history - and
as such may not be able to witness a chain in its entirety.

For more details, see the definition of `ConsensusProtocol`.


Protocol State: `tickChainDepState`, `updateChainDepState` and `reupdateChainDepState`
-----------------------------------------------------------------------------------

These three functions model state transitions of values of type `ChainDepState`

`tickChainDepState` computes a new `ChainDepState` from a prior state though
a computation that models the (logical) passage of time.  In particular,
it evolves the `chainDepState` some number of ticks given by the `SlotNo` argument.

Unlike `updateChainDepState` this cannot fail under normal circumstances - if
it could, that would mean there is some failure that is inevitable given
the passage of time and if that is the case there would have been no reason
not to throw such an error immediately.

`updateChainDepState` (a better name would be "applyHeader") computes a new `ChainDepState` from a prior state and
the needed view of the header, `ValidateView p`.  This could fail, producing a
`ValidationErr p` instead of a `ChainDepState p`

`reupdateChainDepState` is an optimization of `updateChainDepState` which
is called when the header is known to be good (e.g., from a previous call to `updateChainDepState`)
and the header check is unneeded.

In the case of `SP` since the `chainDepState` is `()` these functions are
not very interesting.  In the case of `tickChainDepState`, `TickedTrivial`
is simply the `Ticked` instance for `()`.

Leader Selection: `IsLeader`, `CanBeLeader`, `checkIsLeader`
------------------------------------------------------------

The type family `CanBeLeader` represents the ability for a particular node
in the protocol to be a leader for a slot.  Put another way, a value of `CanBeLeader p` for a particular `p`
witnesses the potential for a consensus participant to be a leader for a particular slot.

In the same way, a value `IsLeader` witnesses the fact that a particular node is a leader for a slot.

This notion of leadership is used to validate whether blocks are correct with
respect to having been provably created by the leader of the slot the block
appears in.  However, the details of what constitutes proof is specific
to a particular blockchain, which is why it is dealt with abstractly
in `ConsensusProtocol`.  Generally values of `CanBeLeader p` and `IsLeader p`
are some sort of cryptographic evidence substantiating a claim to leadership.

However, since we are less concerned about security in `SP`, we will use two simple singleton types -
nothing cryptographic is happening at all.

The `checkIsLeader` function uses these types in its determination of whether or not a node is a leader
for a slot - returning `Nothing` if the node is not a slot leader or `Just (IsLeader p)`
if it is.

`SP` implements leadership by specifying, in the static `ProtocolConfig` for `SP`,
a set of slots for which the particular node running the protocol is the leader.  `checkIsLeader`
then looks up the slot number in this set and returns `Just SP_IsLeader` (aka `IsLeader SP`) if
the node is configured to be a leader in this slot.


The Security Parameter `k`: `protocolSecurityParam`
---------------------------------------------------

`ConsensusProtocol` requires that its static configuration --
which is to say the associated `ConsensusConfig p` for a particular
`ConsensusProtocol p` -- provide a security parameter (`SecurityParam`).
This requirement is embodied in the `protocolSecurityParam` method.

For all known/current protocols, the security parameter is fixed for each
blockchain (a protocol could be instantiated with different k's, but it should
be configured the same for each node in that blockchain).

The `maxRollbacks` field on the `SecurityParam` record (often referred to as `k`)
describes how many blocks can be rolled back - any number of blocks greater than
this should be considered permanently part of the chain with respect to the protocol.

In the case of `SP` we allow 1 rollback.

Further reading about Consensus
-------------------------------

The `ConsensusProtocol` class is also dealt with in some detail
and with additional context in the
[Cardano Consensus and Storage Layer](https://hydra.iohk.io/build/15874054/download/1/report.pdf) report.

The `Ouroboros.Consensus.Protocol.Praos` module contains the
instantiation of `ConsensusProtocol` for Praos.


Blocks: The View From Consensus
===============================

In the discussion above, the reader may have noticed that we have only
presented _views_ of some of the things consensus deals with.  This is
to reduce coupling between `ConsensusProtocol p` and any particular
block or ledger implementation.

To enhance our example we'll implement a simple block
and ledger that can be used with `SP` that logically keeps track of a
single number.  Each block contains a list of transactions that either
increment or decrement the number.  At any point in time, the
ledger's state can be thought of the net effect of all these
transactions - in other words, the number of increment transactions
minus the number of decrement transactions.

Defining the Block
------------------

We'll start by defining the transaction type - this is what the block
will contain:

> data Tx = Inc | Dec
>   deriving (Show, Eq, Generic, Serialise)

Next, we'll define the block itself:

> data BlockC =
>   BlockC { bc_header :: Header BlockC
>          , bc_body :: [Tx]
>          }

Which is to say, a block is just a header (`Header BlockC`) followed by a
list of transactions (`[Tx]`) - we'll need to instantiate the `Header` family for `BlockC`.

We'll deal with `Header BlockC` in the next section.

Block Headers
-------------

The block header describes the _structure_ of the block chain - for example
the hash of this block and that of the block before it.  As a side note,
in the case of the genesis block, this "previous" hash will also be its own hash.
This corresponds to the `Header` data family (from `Ouroboros.Consensus.Block.Abstract`)
which we'll instantiate as:

> data instance Header BlockC =
>   HdrBlockC { hbc_SlotNo :: SlotNo
>             , hbc_BlockNo :: BlockNo
>             , hbc_Hash :: HeaderHash BlockC
>             , hbc_prev :: ChainHash BlockC
>             }
>   deriving stock (Show, Eq, Generic)
>   deriving anyclass (Serialise)

The `HeaderHash` type family describes the type used to represent
hashes of headers - while the `ChainHash` type is either
the `HeaderHash` of the prior block or `Genesis` if this is the
genesis block.

Accordingly, we'll instantiate `HeaderHash BlockC` as a list of bytes:

> type instance HeaderHash BlockC = [Word8]

We'll also instantiate the empty `StandardHash` class which
does nothing that place additional constraints (already fulfilled by `[Word8]`)
on `HeaderHash`.

> instance StandardHash BlockC

Because `Header` is a data family, functions using instantiations of this
family will know nothing about the structure of the data - instead there
are other typeclasses needed to build an interface to derive things that
are needed from this value.  We'll implement those typeclasses next.



Interface to the Block Header
-----------------------------

**`GetHeader`**

The `GetHeader` class describes how to project a header -
which is a value of type `Header BlockC` in our example - out of a block representation.
The implementation for `getHeader` is fairly straightforward -
we can just use the record accessor `bc_header`:

> instance GetHeader BlockC where
>    getHeader = bc_header
>    blockMatchesHeader = \_ _ -> True
>    headerIsEBB = const Nothing


**`GetPrevHash`**

The `GetPrevHash` class contains a function that gets the hash of a
previous block from the header - which is very simple for `Header BlockC`:

> instance GetPrevHash BlockC where
>  headerPrevHash = hbc_prev

**`HasHeader`**

The `HasHeader` typeclass has the `getHeaderFields` function which projects the
information in the header to a `HeaderFields` record containing the slot, block number, and
block hash.

We implement this both for `Header Block`:

> instance HasHeader (Header BlockC) where
>   getHeaderFields hdr = HeaderFields
>                          { headerFieldSlot = hbc_SlotNo hdr
>                          , headerFieldBlockNo = hbc_BlockNo hdr
>                          , headerFieldHash = hbc_Hash hdr
>                          }

As well as `BlockC` itself - which calls the `getHeaderFields` defined for `Header BlockC`:

> instance HasHeader BlockC where
>   getHeaderFields = castHeaderFields
>                   . getHeaderFields
>                   . bc_header

**Validation**

These classes require implementation but for this tutorial we don't
really need to deal with them - so we'll leave them empty for now:

> instance HasAnnTip BlockC where {}
> instance ValidateEnvelope BlockC where {}
> instance BasicEnvelopeValidation BlockC where {}

Associating the Block and the Protocol - `BlockSupportsProtocol` and `BlockProtocol`
------------------------------------------------------------------------------------

So far, we've made no mention of `SP` in any of the definitions for `BlockC` -
similarly, we've made no mention of `BlockC` in any of the definitions
for `SP` we have to implement a few more typeclasses that define
how the two are associated.

More generally, a block has one and only one type of protocol - but the converse
is not true - a protocol may have many types of block.  As such, the association
between the two specifies the protocol for a particular type of block.
The type family establishing this relationship is the `BlockProtocol` family.

Here, we define the protocol type for `BlockC` as `SP`:

> type instance BlockProtocol BlockC = SP

Also, the other half of `ValidateView SP` needs to be defined as well -
which is how do we create a value of `ValidateView SP` given a block.  To
do this, we instantiate the `BlockSupportsProtocol` typeclass.  Note that
we do not need to say _which_ protocol is supported since there is only
ever one protocol for a block, again established by our prior instantiation of
 `BlockProtocol`:

> instance BlockSupportsProtocol BlockC where
>   validateView _ _ = ()
>   selectView _bcfg hdr = blockNo hdr

Given that `ValidateView SP` is of type `()` there is only one possible implementation
for this typeclass.  Later examples will require more interesting views of the block.

Our example requires some additional configuration instances to be defined -
we'll gloss over these for the time being but they allow for some additional
static configuration of different things pertaining to blocks:

> data instance BlockConfig BlockC = BCfgBlockC
>   deriving (Generic, NoThunks)
> data instance CodecConfig BlockC = CCfgBlockC
>   deriving (Generic, NoThunks)
> data instance StorageConfig BlockC = SCfgBlockC
>   deriving (Generic, NoThunks)


Consensus and The Ledger
========================

The _ledger_ specifies a state of the system represented by the blocks
in a blockchain but also characterizes what transitions are valid for
any particular state.

Below we'll define a group of typeclasses that together implement a simple
ledger that uses `BlockC` and that is suitable for our consensus protocol `SP`.


`LedgerCfg` - Ledger Static Configuration
-----------------------------------------

Much like `ConsensusProtocol` and its `ConsensusConfig` configuration class,
the ledger has an associated static configuration which is represented using
the type family `LedgerCfg`.  For our example, we have nothing
interesting to configure, thus:

> type instance LedgerCfg (LedgerState BlockC) = ()

`LedgerState` - The Value Computed by the Blockchain
----------------------------------------------------

`LedgerState` is a family which logically represents the value computed by
the blockchain.  Put another way, it's a value derived from observing the
the passage of time of the logical clock (aka slots) as well as any blocks
inhabiting those slots - something like the result of a fold.

Given that the `BlockC` transactions consist of incrementing and decrementing
a number, we materialize that number in the `LedgerState`.  We'll also need to
keep track of some information about the most recent block we have
seen.

> data instance LedgerState BlockC =
>
>   LedgerC
>     -- the hash and slot number of the most recent block
>     { lsbc_tip :: Point BlockC
>     -- the computed result of applying all the transactions
>     , lsbc_count :: Word64
>     }
>   deriving (Show, Eq, Generic, Serialise)

The `Point` type (defined in `Ouroboros.Network.Block`) describes a particular
place in the blockchain - a pair of a slot and a block hash.

`Ticked (LedgerState BlockC)`
---------------------------------------

Again, the slot abstraction defines a logical clock - and instances of the `Ticked` family
describe values that evolve with respect to this logical clock.
As such, we will also need to define an instance of `Ticked`
for our ledger state.  In our example, this is essentially an `Identity` functor:

> newtype instance Ticked (LedgerState BlockC) =
>   TickedLedgerStateC
>     { unTickedLedgerStateC :: LedgerState BlockC }
>   deriving (Show, Eq, Generic, Serialise)


`IsLedger`
----------

The `IsLedger` class describes some of the basic functionality and associated
types for a ledger.  Though we are here using

> instance IsLedger (LedgerState BlockC) where
>   type instance LedgerErr  (LedgerState BlockC) = Void
>   type instance AuxLedgerEvent (LedgerState BlockC) = Void
>
>   applyChainTickLedgerResult _cfg _slot ldgrSt =
>     LedgerResult { lrEvents = []
>                  , lrResult = TickedLedgerStateC ldgrSt
>                  }

The `LedgerErr` type is the type of errors associated with this ledger that can be
thrown while applying blocks or transactions.  In the case of `LedgerState BlockC`
we are not expecting any errors, so we'll use `Void` here.

The `AuxLedgerEvent` type describes events that can occur as output
while applying blocks.  We will also not be using this for our example -
as such we will also use `Void` here.

The `applyChainTickLedgerResult` function 'ticks' the `LedgerState`,
resulting in an updated `LedgerState` that has witnessed a change
in slot (which, again, corresponds to a logical clock.)  Note that
this function _does allow failure._  If it did, that means the
`LedgerState` is such that it is in a state that will
eventually fail due to the passage of time and such errors
should have been signalled earlier (for example, when applying
blocks.)


`ApplyBlock` - Applying Blocks to `LedgerState`
-----------------------------------------------

A block `b` is said to have been `applied` to a `LedgerState` if that
`LedgerState` is the result of having witnessed `b` at some point.
We can express this as a function:

> applyBlockTo :: BlockC -> Ticked (LedgerState BlockC) -> LedgerState BlockC
> applyBlockTo block tickedLedgerState =
>   ledgerState { lsbc_tip = blockPoint block
>               , lsbc_count = lsbc_count'
>               }
>   where
>     ledgerState = unTickedLedgerStateC tickedLedgerState
>     lsbc_count' = foldl txDelta (lsbc_count ledgerState) (bc_body block)
>     txDelta i tx =
>       case tx of
>         Inc -> i + 1
>         Dec -> i - 1

We use a `Ticked (LedgerState BlockC)` to enforce the invariant that we should
not apply two blocks in a row - at least one tick (aka slot) must have elapsed
between block applications.


The interface used by the rest of the ledger infrastructure to access this
is the `ApplyBlock` typeclass:

> instance ApplyBlock (LedgerState BlockC) BlockC where
>   applyBlockLedgerResult ldgrCfg block tickedLdgrSt =
>     pure $ LedgerResult { lrEvents = []
>                         , lrResult = block `applyBlockTo` tickedLdgrSt
>                         }
>
>   reapplyBlockLedgerResult ldgrCfg block tickedLdgrSt =
>     LedgerResult { lrEvents = []
>                  , lrResult = block `applyBlockTo` tickedLdgrSt
>                  }
>
>

`applyBlockLedgerResult` tries to apply a block to the ledger and fails
with a `LedgerErr` corresponding to the particular `LedgerState blk`
if for whatever reason the block could not be applied.

We previously defined `LedgerErr` as `()`
This might seem troubling - for example, `BlockC` contains a slot number -
what if we try to apply a block labelled with a slot that is already in
the past?  However, this is something that should be checked by
callers of this code.

`reapplyBlockLedgerResult` similar but is meant to be called by code path
that has previously established that the application of a block would not fail,
so it admits no possibility for failure.

Both of these return a `LedgerResult` record containing both the updated
state as well as a list of `AuxLedgerEvent (LedgerState BlockC)` -
the `AuxLedgerEvent` type family is intended to allow ledgers to emit
extra "events" as part of applying blocks, but for our simple example
we do not need to use this feature.

Once we've defined `ApplyBlock` we can also instantiate the empty
`UpdateLedger` class which captures the `ApplyBlock` relationship
between a block type `block` and its ledger `LedgerState block` and indexes
it by `block`.  We'll need this later for `LedgerSupportsProtocol`

> instance UpdateLedger BlockC where {}

`GetTip` - The Most Recently Applied Block
------------------------------------------

The `GetTip` typeclass describes how to get the `Point` of the tip -
which is the most recently applied block.  We need to implement
this both for `LedgerState BlockC` as well as its ticked version:

> instance GetTip (Ticked (LedgerState BlockC)) where
>    getTip = castPoint . lsbc_tip . unTickedLedgerStateC

> instance GetTip (LedgerState BlockC) where
>    getTip = castPoint . lsbc_tip

Associating Ledgers to Protocols
--------------------------------

Similar to blocks, a typeclass is used to associate a ledger to a protocol.
Note that since a block is associated with one and only one protocol, we
can use the block to index both the ledger and the protocol.

> instance LedgerSupportsProtocol BlockC where
>   protocolLedgerView _lcfg  _tl = TickedTrivial
>   ledgerViewForecastAt _lccf = trivialForecast

The `protocolLedgerView` function describes how to project the consensus-specific
`LedgerView` out of `LedgerState` and `LedgerCfg` together - however `SP` does
not use any information from the ledger to make any decisions and since
`LedgerView SP` is simply `()` - we use `TickedTrivial` here for
the implementation of protocolLedgerView which constructs a `Ticked ()` value.

`ledgerViewForecastAt` returns a `Forecast` (defined in `Ouroboros.Consensus.Forecast`)
of a `LedgerView` - where a `Forecast` is a starting point `forecastAt` together with a
function `forecastFor` which takes a slot number and either produces a forecasted value
for that slot - in this case a possible future `LedgerView` at that slot.

This `Forecast` is closely related to and is required to be
consistent with `tickChainDepState` in `ConsensusProtocol` - the documentation
for `LedgerSupportsProtocol` explains the relationship in more detail.

Appendix: NoThunks Instances
============================

`NoThunks` is a class that comes from the `nothunks` package
(https://hackage.haskell.org/package/nothunks) and helps
diagnose various kinds of memory leaks having to do with thunks.
Many of the above classes require that `NoThunks` be instantiated
as a prerequisite.

To focus on the salient ideas of this document, we've put all the derivations
of `NoThunks` here instead:

> deriving via OnlyCheckWhnfNamed "SP_Config" (ConsensusConfig SP)
>   instance NoThunks (ConsensusConfig SP)
> deriving via OnlyCheckWhnfNamed "BlockC" BlockC
>   instance NoThunks BlockC
> deriving via OnlyCheckWhnfNamed "HdrBlockC" (Header BlockC)
>   instance NoThunks (Header BlockC)
> deriving via OnlyCheckWhnfNamed "LedgerC" (LedgerState BlockC)
>   instance NoThunks (LedgerState BlockC)
> deriving instance NoThunks (Ticked (LedgerState BlockC))