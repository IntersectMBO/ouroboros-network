title: Consensus UTxO HD Integration Plan
author: Consensus Team
date: 2021 December

This document contains the plan for integrating the UTxO HD back-end into
Consensus. It focuses on the Consensus perspective, but does reference
neighboring teams, particularly Ledger and Node.

# Introduction

The Shelley ledger state includes the set of Unspent Transaction Outputs, the
UTxO set. After so many epochs, this set has grown quite large on the Cardano
`mainnet`, with millions of entries. In particular, a machine with 8 gigabytes
of memory running a node, the wallet, will have very little free memory with our
latest releases of the node and wallet.

Our long-term solution for this is the UTxO HD initiative, whereby we relocate
the UTxO set of the Shelley ledger state from in-memory data structures to
exclusively and explicitly on-disk data structures. This trade-off wins back
memory headroom at the cost of IO space, IO delays, and increased code
complexity.

This document does not assess that trade-off; we take its worth for granted
herein. This document also does not discuss short-term workarounds that may
temporarily win back a helpful amount of memory headroom. It only regards the
architectural changes in the Consensus layer necessary for the long-term UTxO HD
initiative.

TODO cite the specification/motivation document Duncan and Douglas wrote

TODO cite the interface documentation Duncan and Douglas are writing (incl
Duncan's presentation recordings)

# Status quo and the general plan

The Shelley ledger state contains a map from a UTxO identifier (ie the `TxIn`
type) to the identified UTxO (ie the `TxOut` type) -- the values in this map
constitute the UTxO set. The data type representing Shelley ledger states is
complete and fully in-memory, as a typical immutable/persistent/pure/etc Haskell
data structure. This is a boon for the developers (on multiple teams) that are
managing the specification, code, tests, tools, etc that involve ledger states
in any way. That boon, however, comes at a cost: every use of a `mainnet` ledger
state incurs the memory footprint of `mainnet`'s huge UTxO set.

The key insight underlying the UTxO HD plan is that, for a running node, the
primary use of the UTxO set is to validate transactions. There simply aren't
that many transactions in-memory at once, and each transaction can only involve
a small number of UTxO entries. It's less straight-forward for the other uses of
the UTxO set, but it remains the case that the core operation of the node never
requires the full UTxO set all at once.

Transactions are of a limited size, so the consumed UTxO and new UTxO will only
ever be a tiny fraction of the full UTxO set. Blocks are also of limited size,
so even considering all the UTxO entries relevant to the sequence of
transactions in a given block also only affects a small fraction of the full
UTxO set. Finally, even `k=2160` blocks corresponds to roughly 12 hours worth of
`mainnet` activity; so the latest `k` blocks' transactions' UTxO entries are
still multiple orders of magnitude smaller than the full UTxO set resulting from
all users' cumulative activity over the years of `mainnet`'s lifetime.

Therefore, the abstract plan is to store the full UTxO set exclusively on-disk
and to only read-in and write-out the subsets necessary for the recent
transactions (both those inside and also those not-yet-inside blocks).

# The plan

## Which data we will maintain on-disk

The Consensus architecture takes advantage of the Ouroboros Common Prefix
Theorem by never (cumulatively) rolling back more than `k` blocks. As a result,
whenever the node's selected chain contains at least `k` blocks, the chain
prefix from the genesis block up to the chain's `k` block suffix will forever be
the prefix of the node's subsequently selected chains (excluding eg disk
corruption, crash, and subsequent recovery). We therefore refer to those blocks
as the _immutable chain_ and the youngest such block as the _immutable tip_. In
contrast, the `k` block suffix is referred to as the _volatile chain_, since
subsequently selected chains (ie better/longer chains) may or may not include
those blocks.

***Draft Design Decision*** We only store one UTxO set on-disk: the UTxO set as
of the immutable tip.

The `k` blocks on the volatile chain also make changes to the UTxO set, but we
track those changes as a corresponding sequence of explicitly-represented
differences (eg insertions, deletions). Each difference encodes the exact
cumulative affect that some block's transactions have on the UTxO set when
applying that block to a ledger state. Once that block becomes immutable, we can
flush the corresponding difference to the on-disk UTxO set and then never again
need to track that difference in memory.

In actuality, we won't necessarily want to or even be able to flush every time
our immutable chain grows: performance might call for batching writes, and
coherency might prevent flushing during certain intervals (see The Lock below).
Therefore, in addition to the volatile chain's differences we can also track a
_write buffer_ of some suffix of the immutable differences that simply haven't
yet been written to the on-disk UTxO set.

***Design Decision*** We only store one UTxO set on-disk: the UTxO set as of the
beginning of the write buffer, which does not itself extend past the immutable
tip.

As our selected chain grows, the write buffer does as well. At opportune times
(elaborated below), we will flush some prefix of the write buffer to disk.

## The flush lock

The Consensus layer exhibits non-trivial concurrency. The ChainDB component
maintains the node's selected chain, the best chain it has received and
validated so far. Other Consensus components are free to query this selection at
any time, including the ledger state at that chain's tip. The Mempool acquires
such a ledger state in order to validate incoming and pending transactions. The
LocalStateQuery mini protocol server acquires a ledger state in order to answer
most queries sent by the client (eg the wallet).

Before UTxO HD, the ledger state that these other components acquired was
self-contained and persistent. In particular, they could theoretically use it
for an indefinite amount of time -- it never expired. On the other hand, the
UTxO HD design means that acquired ledger state is incomplete: it excludes the
UTxO set. The Mempool and LocalStateQuery server's semantics both involve the
UTxO set, so they will need to both inspect the acquired in-memory ledger state
and also read the _corresponding_ on-disk data. For example, if the
LocalStateQuery server acquired the ledger state, then the ChainDB switched to a
different chain, and then the LocalStateQuery attempted to read the on-disk
data, it would not necessarily read values from disk that correspond to the
in-memory ledger state it is holding, since that ledger state is no longer at
the end of the ChainDB's selected chain.

There are multiple ways to resolve the risk of incoherence due to the acquired
in-memory ledger state now having the potential of expiry. We have come up with
two that effectively preserve the persistent nature of the complete ledger state
prior to UTxO HD -- this effective persistence is attractive because it means
these Consensus components do not need a new code path to handle the expiry
case. The first way is to enrich the interface to the on-disk UTxO set to allow
querying its former states. We'll discuss this functionality below in the Node
start-up section, and in fact there we consider it essential. However, even
though we ultimately need it, we're unsure if its specific realization will be
suitable for this problem of effective ledger state persistence. So we instead
turn to our second idea.

Every Consensus component that acquires a ledger state only needs to use it very
briefly. Because of persistence, they _could_ hold it forever -- and therefore
have never needed to even consider the risk of expiry, let alone include code to
handle it -- but none actually even needs to hold it for appreciable durations.
We therefore can simply add a lock that prevents the ChainDB from altering the
on-disk data while some component is interacting with that data.

***Design Decision*** The ChainDB will maintain a lock. Other Consensus
components will hold this lock whenever they are reading from the on-disk UTxO
set under the assumption that the results of their reads correspond to whichever
in-memory ledger state the component recently acquired. Therefore, while this
lock is held, the ChainDB will not flush the write buffer.

If the on-disk interface eventually supports ephemeral restore points in a
sufficiently cheap and robust way, then we could use those instead of the lock
to maintain the illusion of persistent coherence between an in-memory ledger
state and the on-disk UTxO set. But until then, we will instead ensure it via
the lock.

The Consensus component holding the lock is reading from the on-disk data and
using the acquired in-memory differences to handle whatever blocks are between
the on-disk data and the in-memory ledger state they acquired. This is possible
as long as the difference sequence they acquired is anchored at the on-disk
data, and that is ensured by this locking policy since it prevents the ChainDB
from flushing the write buffer. In particular, the ChainDB can still switch its
selected chain etc -- nothing but flushing alters the on-disk data. All other
operations can at most extend the write buffer. Not all flushing would spoil the
anchor relationship protected by the lock, but it's simpler and we think
affordable to do no flushing at all while the lock is held.

Because no Consensus components will hold the lock for a significant duration,
this lock mechanism will not cause undue bloat of the write buffer. This point
deserves emphasis for the LocalStateQuery server in particular, since the
duration it holds the lock is in fact determined by the messages sent by the
LocalStateQuery client (eg the wallet). If the wallet were abusive, then it
could cause the server to hold the lock for too long, causing the write buffer
to bloat and thereby threatening the node's memory footprint/computational
latencies/etc. However, we fundamentally _trust_ local clients, so such an
abusive wallet would be the result of a wallet bug. If we wished to detect such
a bug at run-time, our only choice would be disconnecting from the wallet (TODO
which might be fine): the LocalStateQuery mini protocol does not have any
message that could convey expiry to the client. We prefer the complexity of the
lock and the risk of a wallet bug as opposed to "unnecessarily" complicating the
mini protocol itself.

## Mempool

TODO how to avoid holding the flush lock for too long? is it possible to cache
read results within the mempool so that we'll never need disk access
when_re_-applying transactions

# Node start-up

Before UTxO HD, the ChainDB occasionally writes a snapshot of the ledger state
to disk. This snapshot contains the ledger state as of some immutable block.
Therefore, assuming no disk corruption, the node can re-initialize itself by
deserializing the ledger state and replaying the immutable blocks that are
younger than it.

An attractively simple option for UTxO HD would be to continue to serialize a
complete ledger state to disk as the snapshot file. However, this is contrary to
the UTxO HD initiative: Over time, such snapshots will become prohibitively
large. Their disk size will grow larger and larger, but more importantly their
serialization time will become too great.

***Design Decision*** The on-disk interface will allow for sufficiently cheap
creation of restore points. Whenever the node serializes an in-memory ledger
state snapshot to a file, it will also create a corresponding restore point. A
simple rotation of two restore points would suffice to easily resolve the
possible race condition on update. More might be desirable for cheaper
recoveries from eg disk corruption.

At the moment, the team developing the on-disk interface has not provided such a
restore point functionality. We therefore use a temporary workaround.

***Interim Design Decision*** Whenever the node flushes (some prefix of) the
write buffer, it will also serialize the corresponding in-memory ledger state to
a file. This establishes the invariant that the cumulative on-disk content
always corresponds to a complete ledger state as of some immutable block.

Though temporary, this is an example reason to intentionally batch write buffer
flushes.

Before UTxO HD, the node does not take ledger snapshots during replay. However,
the UTxO HD node will need to flush during replay, since we sometimes replay a
huge number of blocks and they would collectively induce a write buffer with a
huge memory footprint. And since we've -- at least temporarily -- coupled
flushing and snapshotting, that means we'll now need to take snapshots during
replay. This doesn't seem particularly onerous or awkward, but it's a new
behavior we haven't considered before.

## The kind of the `LedgerState` data family

Before UTxO HD, the ledger state is a self-contained persistent data structure.
It is identified by the type `LedgerState blk`, for which block type is in use.
The UTxO HD plan involves -- at least logically -- splitting this `LedgerState
blk` type into two parts: the small part that can remain in-memory and the large
part intended for relocation to the disk (the UTxO set, most notoriously).

A simple and intuitive option is to split `LedgerState blk` into
`SmallLedgerState blk` and `LargeLedgerState blk`. The type for a self-contained
complete ledger state would then be the tuple `(SmallLedgerState blk,
LargeLedgerState blk)`.

The team developing the on-disk interface has used the Higher-Kind Data (HKD)
pattern for their ledger state. Therefore, the `LargeLedgerState` family
actually takes an additional parameter `table` which determines what is actually
stored for the on-disk data. For example `LargeLedgerState blk Values` would
represent the actual map from `TxIn` to `TxOut` while `LargeLedgerState blk
Keys` would merely store the set of `TxIn`. There will be others as well, most
notably `LargeLedgerState blk Diff`, the aforementioned in-memory represent ions
of the UTxO differences induced by applying a block. With HKD, the simple option
uses the tuple `(SmallLedgerState blk, LargeLedgerState blk Values)` as the type
of a self-contained complete ledger state.

To help us implement this UTxO HD plan, we intentionally forego this simplest
option.

***Design Decision*** We add the `table` parameter directly to `LedgerState blk
table` and introduce the `Empty` type so that `LedgerState blk Empty` is
isomorphic to `SmallLedgerState blk`. We do also have `LargeLedgerState blk
table`, but it instead has a containment relationship with `LedgerState` as
witnessed by the type-varying lens `forall. Functor f => (LargeLedgerState blk
table1 -> f (LargeLedgerState blk table2)) -> LedgerState blk table1 -> f
(LedgerState blk table2)`.

This is ultimately isomorphic to the simpler option above, but this helpfully
forces us to explicitly consider this new concern at each necessary point in the
codebase -- there is no risk of some type variable `l` tacitly and
unintentionally having its semantics change from a complete ledger state to just
the small parts. (TODO hmm... wouldn't that mistake be impossible, since `l` is
just the small part and anything that needs the large part would also need a
corresponding new value `LargeLedgerState blk`, separate from `l`?) (TODO ...
actually, would we want `LargeLedgerState l` instead of `LargeLedgerState blk`
if we went down this route?)

## The `HardForkBlock` combinator

***Design Decision*** TODO Discuss the sum-type problem and our best solution so
far.

***Design Decision*** TODO Emphasize that the on-disk content for the type
`LargeLedgerState (HardForkBlock xs)` must also be valid on-disk content for the
type `LargeLedgerState (HardForkBlock (xs ++ ys))`. This makes the era
transition a no-op for the on-disk content.

## Maintaining the changelog

TODO explain that what has been called the "sequence of differences" is now part
of the LedgerDB

## Maintaining the on-disk handle

TODO explain that LgrDB carries a new handle for the on-disk content

## Pipelining disk access

For a node that is already caught up to `mainnet`, the additional latency
incurred by UTxO HD's extra disk access is relatively minor. On the other hand,
this same latency must be mitigated for a node that is bulk fetching the
historical chain. A fresh node must be able to validate historical blocks orders
of magnitude faster than the chain's expected growth (~20s per block) to have
any hope of catching up to `mainnet` quickly enough to satisfy the end-user.

***Future Design Decision*** We will pipeline the reading and writing of the
on-disk ledger data. Specifically, we will initiate the read for the latest
received block B_j's necessary on-disk ledger data while validating one of its
recent ancestors B_i (such that i < j), so that B_j's read results are available
by the time we have the in-memory ledger state necessary for validating B_j (ie
once we've finished validating block B_{j-1}).

TODO explain the necessary changelog gymnastics

TODO explain that this concern is entirely local to the ChainDB's `ChainSel`
logic, and so is (TODO totally?) orthogonal to the flush lock, since it's just
extensions and many fewer than `k`

## Other large parts of the ledger state

TODO the UTxO set is just the first part we're moving to disk

TODO explain why moving ledger snapshots to disk and/or moving incremental
computations to disk means that even just ticking a ledger state requires
on-disk data
