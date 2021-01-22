# Property testing in the consensus layer

> This document corresponds to 95c765081c0edf233f16f5efe046ede523e70cc5 .

The vast majority of the tests in the consensus layer are QuickCheck property
tests, and many of these are model based. There are only a handful of unit
tests. The consensus layer is an intricate piece of software with lots of
components, which we set at virtually every level of granularity. Below we  give
an overview of the tests that we do per component. For the detailed listing,
please refer to the various test suites within the
[repository](https://github.com/input-output-hk/ouroboros-network/); at the time
of writing, this is

* `ouroboros-consensus-test/test-infra/Main.hs`
* `ouroboros-consensus-test/test-storage/Main.hs`
* `ouroboros-consensus-test/test-consensus/Main.hs`
* `ouroboros-consensus/ouroboros-consensus-mock-test/test/Main.hs`
* `ouroboros-consensus-byron-test/test/Main.hs`
* `ouroboros-consensus-shelley-test/test/Main.hs`
* `ouroboros-consensus-cardano/test/Main.hs`

Throughout this document we mention rough lines of code statistics, in order to
get a sense of the cost of the testing. Some common code that is used by many
components is not included in these statistics. Moreover, the consensus layer
contains some 10k additional lines of code that is not accounted for in any
of the statistics below; this is generic infrastructure that is used by all
components.

## Testing the test infrastructure

The library `ouroboros-consensus-test` provides a bunch of test utilities that
we use throughout the consensus layer tests. It comes with a few tests of its
own. Some examples:

* Some consensus tests override the leader schedule from the underlying
  protocol, instead explicitly recording which nodes lead when. If we use a
  round-robin schedule for this, and then compute the expected fork length, we'd
  expect to get no forks at all.
* Some invariants of various utility functions.

There isn't too much here; after all, if we start testing the test, where does
it stop? :) That said, `test-consensus` contains a few more tests in this same
category (see below).

**Stats.** The `ouroboros-consensus-test` library itself is 8,700 lines of code.
Since this library is used throughout all tests, I've not added that line count
to any of the other statistics. The test suite for the library is minute, a mere
140 loc.

## The storage layer (`test-storage` test suite)

The storage layer is a highly specialized database for storing the blockchain.
It consists of five subcomponents:

* An abstract file system API that smoothes out over some differences between
  the file systems of different operating systems and, more importantly,
  allows us to simulate all kinds of failures. This is then used for
  stress-testing the other components below.
* The _Immutable DB_ stores the part of the chain that is immutable, that is,
  no longer subject to rollback. It is an append-only database, providing
  efficient access to the chain.
* The _Volatile DB_ stores the part of the chain near its tip. This doesn't
  really store a _chain_ as such, but rather simply a collection of blocks
  from which we might _construct_ a chain.
* The _Ledger DB_ stores the state of the ledger. On disk it only stores
  snapshots of the ledger state that correspond to immutable blocks; in memory,
  it stores various snapshots of the ledger state corresponding to blocks near
  the current tip of the chain, and an efficient way of computing any ledger
  state for the last `k` blocks of the chain.
* The _Chain DB_ finally combines all of these components. It makes decisions
  about which chains to adopt (chain selection), switches to forks when needed,
  deals with clock skew, and provides various interfaces to the rest of the
  consensus layer for things like finding out which blocks were invalid
  (so we can disconnect from the clients who sent them), cursors that follow
  the tip of the chain (so that we can inform our downstream peers of how
  our chain evolves), etc. In many ways, the chain DB is the component that
  is responsible for "consensus": deciding which chain is the one true chain.

### The file system abstraction (`Test.Ouroboros.Storage.FS.StateMachine`)

This is the first of the many model based tests we have. Edsko's blog post [An
in-depth look at `quickcheck-state-machine`](http://www.well-typed.com/blog/2019/01/qsm-in-depth/)
uses these tests of the file system abstraction as a case study and outlines the
general approach. Like all of the model based tests, this consists of

1. A set of commands
2. A model against which we execute those commands
3. An interpreter of those commands against the real implementation
4. A generator and a shrinker for sequences of these commands
5. A way to _label_ generated sequences so that we can verify that the tests
   are covering what we think they should be covering.

The sequences generated are then executed against both the model and the
implementation, and the results are compared.

In this particular case, the commands are file system operations such as
create a directory, open a file, write some bytes, etc. The model is a very
simple one: we simply model a file system as a tree of `ByteString`s (the raw
bytes in the files). The goal of the model, of course, is not to provide one that
mirrors the low-level details of the real implementation, but rather one that
abstracts over such details and provides a _specification_.

The file system abstraction however is a bit of an unusual test in that this is
really "reverse model testing": the tests compare the model to the real file
system, but we are of course not developing a file system. Instead, the tests
serve to make sure that we got the model right, which we can then use
in the tests of the rest of the consensus layer.

**Stats.** The implementation, including the OS specific parts, is 1200 loc.
The tests are 1500 loc.

### The Immutable DB (`Test.Ouroboros.Storage.ImmutableDB`)

The immutable DB bundles a (configurable) number of blocks into "chunk files".
By design, chunk files are literally just the raw blocks, one after the other,
so that we can efficiently support binary streaming of blocks.

Every chunk file is accompanied by two indices: a _primary_ index that for
each slot in the chunk file provides an offset into a _secondary_ index, which
stores some derived information about the blocks in the chunk file for
improved performance. Both the primary and the secondary index can be
reconstructed from the chunk file itself.

The tests for the immutable DB consist of a handful of unit tests, a set of
property tests of the primary index, and then the main event, model based
checking.

**Stats.** The implementation is 6000 loc. The tests are 2700 loc.

#### The primary index (`Test.Ouroboros.Storage.ImmutableDB.Primary`)

This is a sequence of relatively simple property tests:

* Writing a primary index to disk and then reading it again is an identity
  operation (`prop_write_load`)
* We can create new primary indices by appending new entries to them
  (`prop_open_appendOffsets_load`)
* We can truncate primary indices to particular slot.
* Finding and reporting "filled slots" (not all slots in a chunk file, and
  hence in a primary index, need to contain a block) works as expected.
* Reconstructing a primary index from the same data results in the same
  primary index.

Of course, these (and all other) property tests are QuickCheck based and so
generate random indices, random slot numbers, etc., and come with a proper
shrinker.

#### Model based testing (`Test.Ouroboros.Storage.ImmutableDB.StateMachine`)

This is the main test for the immutable DB. As in any model based, we have a
set of commands, which in this case corresponds to things like

* Read a block, or information about a block, from the DB
* Append a block to the database
* Stream blocks from the DB
* etc.

In addition, there are commands that model disk corruption, so that we can test
that the DB does the right thing in the presence of disk failure. The consensus
storage layer has a simple policy for disk corruption: _it is always sound to
truncate the chain_; after all, we can always get the remaining blocks from
other peers again. This means that in the models, disk corruption is simply
modelled as truncation of the chain; the real thing of course needs to be able
_detect_ the corruption, minimize quite how far we truncate, etc.

The model (defined in `Test.Ouroboros.Storage.ImmutableDB.Model`) is essentially
just a mapping from slots to blocks. It needs to maintain a _bit_ more state
than that, in order to deal with stateful API components such as database
cursors, but that's basically it.

### The Volatile DB (`Test.Ouroboros.Storage.VolatileDB.StateMachine`)

The set of commands for the volatile DB is similar to the immutable DB, commands
such as

* Get a block or information about a block
* Add a block
* Simulate disk corruption

in addition to a few commands that are supported only by the volatile DB,
such as "find all blocks with the given predecessor" (used by chain selection).
The model (defined in `Test.Ouroboros.Storage.VolatileDB.Model`) is a list
of "files", where every file is modelled simply as a list of blocks and some
block metadata. The reason that this is slightly more detailed than one might
hope (just a set of blocks) is that we need the additional detail to be able
to predict the effects of disk corruption.

**Stats.** The implementation is 1600 loc, the tests are 1300 loc.

### The Ledger DB (`Test.Ouroboros.Storage.LedgerDB`)

The ledger DB consists of two subcomponents: an in-memory component, which is
pure Haskell (no IO anywhere) and so can be tested using normal property tests,
and the on-disk component, which is tested with a model based test.

**Stats.** The implementation is 1400 loc, the tests are 1600 loc.

#### In-memory (`Test.Ouroboros.Storage.LedgerDB.InMemory`)

The in-memory component of the ledger DB is a bit tricky: it stores only a few
snapshots of the ledger state, in order to reduce memory footprint, but must
nonetheless be able to construct any ledger state (within `k` blocks from the
chain tip) efficiently. The properties we are verify here are various
invariants of this data type, things such as

* Rolling back and then reapplying the same blocks is an identity operation
  (provided the rollback is not too far)
* The shape of the datatype (where we store snapshots and how many we store)
  always matches the policy set by the user, and is invariant under any of
  the operations (add a block, switch to a fork, etc.)
* The maximum rollback supported is always `k` (unless we are near genesis)
* etc.

#### On-disk (`Test.Ouroboros.Storage.LedgerDB.OnDisk`)

This is a model based test. The commands here are

* Get the current ledger state
* Push a block, or switch to a fork
* Write a snapshot to disk
* Restore the ledger DB from the snapshots on disk
* Model disk corruption

The model here is satifyingly simple: just a map from blocks to their
corresponding ledger state.

### The Chain DB (`Test.Ouroboros.Storage.ChainDB`)

The chain DB is the most complicated part of the storage layer, and this is
visible in the tests also. We have separate tests for

* The GC schedule: the chain database occassionally garbage collects blocks
  from the volatile DB that it is sure it won't need anymore (because adopting
  them would result in switching to a fork that is too distant from our own
  chain).
* Iterators. Iterators are cursors that allow to read a sequence of blocks
  from the DB. The iterators provided by the chain DB can span both the
  immutable DB and the volatile DB, some of those blocks might be _moved_ from
  the volatile DB _to_ the immutable DB during the iterator's life-time, and
  some of those blocks might be garbage collected from the volatile DB. This
  makes the iterator code quite complex, having to deal with a lot of edge cases.
  Their main tests are part of the model based test (see below), but we also
  test some specific properties of the iterators themselves.
* The model of the chain DB _itself_ is reasonably complex also, and so we have
  some properties that verify that that the model behaves the way we think it
  should.
* The main DB itself

**Stats.** The chain DB implementation is 7900 loc; the tests are 3600 loc.

#### The GC schedule (`Test.Ouroboros.Storage.ChainDB.GcSchedule`)

The real implementation of course is stateful, running some code at various
intervals. We have a model of the implementation along with some QuickCheck
properties checking various invariants of the model such as

* The length of the queue is bounded
* The overlap between the volatile DB and immutable DB (due to blocks that
  could have been moved but haven't yet) is bounded.

We then test that the real implementation behaves exactly as the model predicts.

#### Iterators (`Test.Ouroboros.Storage.ChainDB.Iterator`)

This is a test of unit tests that check for specific bugs discovered during
other testing. The more important tests for the iterators is the main
model based test of the chain DB.

#### Properties of the model (`Test.Ouroboros.Storage.ChainDB.Model.Test`)

The model for the chain DB (`Test.Ouroboros.Storage.ChainDB.Model`) contains
a quite a bit of info, but that is primarily because it needs to support
stateful APIs such as followers (that follow the tip of the chain) and
iterators (which stream a chunk of the chain). The main part of the model is
it's model of the volatile DB and the immutable DB, which is again satisfyingly
simple: the volatile DB is modelled simply as a set of blocks, and the
immutable DB is modelled simply as a list of blocks (i.e., a chain).

Nonetheless, the implementation of the operations on that model is subtle.
In particular, the chain DB is responsible for chain selection, and so the
model must too. So we have a few properties checking some aspects of the model;
in particular, we verify that no matter in which order we add blocks to the
chain DB, we always pick the most preferred chain.

#### Model based testing (`Test.Ouroboros.Storage.ChainDB.StateMachine`)

These are the main tests for the chain DB. Commands include

* Add a block
* Add a block with a `SlotNo` that is ahead of the wall-clock.
* Get the current chain and/or ledger state
* Create a new iterator and use it to stream blocks
* Create a new follower and use it to follow the chain
* (Limited) disk corruption (the chain DB relies on the immutable DB and
  volatile DB for the storage proper and _they_ have extensive disk corruption
  tests, so we don't need to repeat that here).

The model was described in the previous section.

Note that it is important to tests blocks with a `SlotNo` ahead of the
wallclock separately, because the Ouroboros protocol says such blocks should
not be adopted, but we do want to allow for some clock skew in upstream nodes;
this means that such "blocks from the future" are stored without being added to
the chain just yet, to be considered later. Moreover, we have to be very careful
in how we do this "from the future" check; for example, if the ledger state is
far behind the wallclock, we might not have sufficient knowledge to translate
the wallclock to a `SlotNo`, although we _can_ always translate the `SlotNo`
at the tip of the chain to a `UTCTime`.

## Miscellanous tests (`test-consensus` test suite)

This test suite contains tests for a number of components of the rest of the
consensus layer.

### Resource registry (`Test.Consensus.ResourceRegistry`)

The resource registry is a component throughout the consensus layer that helps
us keep track of resources and makes sure that all resources that we allocate
are eventually also deallocated.

The tests for the registry are model based. The model records which resources
we expect to be alive and which we expect to have been deallocated. The only
resources we are modelling here are threads; the commands we then execute are

* Fork a thread from some other thread
* Terminate a thread
* Have a thread crash
* Collect all live threads

We then verify that the resource registry behaves like the model, cleaning
up resources as threads terminate or crash.

**Stats.** The implementation is 1200 loc, the tests are 600 loc.

### Locking (`Test.Consensus.Util.MonadSTM.RAWLock`)

The volatile DB uses an abstraction we call a `RAWLock`, a lock that allows
the following combinations of readers, appender and writer:

```
         │ Reader │ Appender │ Writer │
─────────┼────────┼──────────┼────────┤
Reader   │   V    │     V    │    X   │
Appender │░░░░░░░░│     X    │    X   │
Writer   │░░░░░░░░│░░░░░░░░░░│    X   │
```

It improves concurrent access. In the test we generate lots of threads, some
readers, some appenders, some writers, each concurrently accessing
some data protected by the lock. We then record the access pattern; the test
would fail if at any point it would see a forbidden combination (for example,
a writer and a reader both having access at the same time).

**Stats.** The implementation is 500 loc, the tests are 250 loc.

### Blockchain time (`Test.Consensus.BlockchainTime.Simple`)

The `BlockchainTime` in consensus used to be ubiquitous throughout the code
base, but is now only used in one place: when we are checking if we should
produce a block. It is a simple abstraction that returns the current slot
number, _if it is known_ (it might be unknown of the current ledger state is too
far behind the wallclock). In addition to the problem of the current slot
being unknown, it must also deal with discontinuities in the system's wallclock:
NTP might adjust the clock forward or backward, or, worse, the user might change
their wallclock by a large amount. We don't try to deal with all of these cases:

* if the clock jumps forward (so we "skip slots") this is no problem
* if the clock is moved back a small amount so that we are still in the same
  slot when we expected to be in the next, also okay
* if the clock is moved back by more than that, so that the current slot would
  actually _decrease_, we throw an exception; it's then up to the user (or the
  wallet) to restart the node).

Since all our tests run in an IO simulator, we can test this by having the clock
behave very erratically. We then compute (in a model) what we expect the
behaviour of the `BlockchainTime` to be given a specific erratic behaviour,
and then verify that it matches the model.

**Stats.** The implementation is 600 loc; the tests are 350 loc.

### Mempool (`Test.Consensus.Mempool`)

The mempool collects transactions from down-stream nodes, makes them available
to up-stream nodes, and of course provides the pool of transactions that we use
when forging blocks.

The tests for the mempool are not model based, but instead check various
simple properties and invariants:

* After adding valid transactions to the mempool, they can be retrieved.
* Adding invalid transactions from the mempool will report them as invalid,
  and they are not added.
* Transactions cannot be retrieved after they are removed.
* The mempool capacity is not exceeded
* etc.

**Stats.** The implementation is 1400 loc, the tests are 1100 loc.

### Chain sync client (`Test.Consensus.MiniProtocol.ChainSync.Client`)

The chain sync client is a stateful component that tracks the chain of an
upstream peer. It validates the headers that it receives from the peer;
validated headers are then reported to the block fetch client which will
download them and offer them to the chain DB, which makes the final choice
whether or not to adopt those blocks.

The tests mock a series of state changes of the up-stream node as well as the
node's own state (the node's own state is relevant because if the node and the
up-stream peer diverge too much we are not interested in their chain anymore,
and we might not be able to validate their headers). We then check that the
chain sync client is reporting the right exceptions if and only if we expect
them to be thrown based on the mock state changes (exceptions such as
"fork is deep", "up-stream node asked for an invalid rollback", etc.).

**Stats.** The implementation is 1100 loc, the tests are 750 loc.

### Local state query server (`Test.Consensus.MiniProtocol.LocalStateQuery.Server`)

The local state query protocol allows clients such as wallets to query the state
of the ledger at any point within `k` blocks from the tip. The test for this is
quite minimal at present: it prepopulates a ledger DB with a bunch of blocks,
and then verifies that requesting the ledger tip corresponding to the these
blocks gives the right answers, and that asking for blocks not on the chain
results in the right error message.

Note that the query protocol is abstract in the ledger (like everything else
in the consensus layer, of course), and the query _language_ we offer (the
kinds of queries that can be asked) of course depends on the ledger. The tests
use a mock ledger for this purpose.

**Stats.** The implementation is 75 loc, the tests are 250 loc.

### Versioned serialization (`Test.Consensus.Util.Versioned`)

Some of our serialization code supports some limited migration capability. This
module contains a few unit tests that check that the migration infrastructure we
provide works as expected. There is not much here.

**Stats.** The implementation is 120 loc, the tests are 110 loc.

### DB marker and DB lock (`Test.Consensus.Node`)

When the consensus layer is integrated into the main node, it provides two
safe guards to avoid data loss and/or corruption:

* When the database is opened, it locks the database so that there can be no
  other processes trying to access it at the same time.
* When we create a database directory, we place a "magic marker" in that
  directory. This allows us to distinguish the database directory from other
  directories, and avoids that we would try to "truncate" a "chain" in a
  directory which doesn't contain a DB at all (due to a misconfiguration),
  thereby potentially deleting a user's files.

This modules contains a bunch of unit tests to make sure that these locks and
markers are created correctly and behave as expected.

**Stats.** The implementation of the db lock and marker is 200 loc; the tests
are 300 loc.

### The hard fork combinator: time infrastructure

One of the responsibilities of the HFC is to offer time conversions (slot to
epoch, wall clock to slot, etc.) across era transitions (which might change
parameters such as the slot length and the epoch size). It does this by
constructing a "summary" of the current state of the chain, basically recording
what the various slot lengths and epoch sizes have been so far, and how far
ahead we can look (the so-called "safe zone": if the transition to the next era
is not yet known, there must exist a limited period after the ledger tip in
which we can still time conversions).

**Stats.** The HFC history implementation (not the combinator) is 1300 loc;
the tests are also 1300 loc.

#### Summary (`Test.Consensus.HardFork.Summary`)

This module verifies the property that _no matter how the summary is constructed_,
as long as it satisfies its invariants, we should have roundtrip properties:

* Converting time to a slot and then back to time should be an identity
  (modulo the time spent in that slot)
* Converting a slot to time and then back should be an identity
* Converting slot to an epoch and then back to a slot should be an identity
  (modulo the time spent in that epoch)
* Converting an epoch to a slot and then back should be an identity.

#### History (`Test.Consensus.HardFork.History`)

This is the more interesting test of the hard fork history. We construct a mock
chain, consisting of events (events are roughly, but not quite, "blocks"). For
every event we record its slot number, epoch number, wall clock, etc. Since we
are constructing this chain as a whole, from genesis to its tip, constructing
these events is trivial. We then split this chain in half, and construct a
`Summary` from the first half. We then use that summary to do conversions for
any event on the chain. Since every event records all information, we can
easily verify whether the answers we are getting back are correct. Moreover,
since the summary is constructed from only the first part of the chain,
but is used to do conversions across the entire chain, we verify that
predictions about the "future" also work as correctly (including that the
conversions say "outside range" if and only if the model expects them to be).

## Consensus tests

The main consensus tests are the consensus layer's sophisticated tests. They are
"system level" test, in which we set up a mock network of nodes, where we can
simulate things like nodes joining late, network message delay, etc. We
then have these nodes run the full protocol, exchanging blocks, doing chain
selection, the whole shebang, and then verify that the nodes can reach
consensus. We have some generic infrastructure for doing all of this, and then
have specific tests for each of the protocols/ledgers we support.

In a way, these are the most important tests we have, as they are testing
properties of the system as a whole. Of course, that also means that if
something goes wrong, debugging these tests can be difficult, and it would be
better to have such problems caught by the other, more fine-grained, tests.

We run these tests for

* A mock ledger (containing bare bones UTxO style transactions) using a variety
  of consensus protocols: BFT, PBFT, Praos, and a version of Praos where we
  override the leader schedule.
* Byron/PBFT
* DualByron: This runs the Byron ledger and the Byron specification in lockstep,
  verifying that they agree at every point.
* Shelley/TPraos
* The hard fork combinator transitioning from a mock ledger A to a mock ledger B;
  these mock ledgers are absolutely minimal ledgers: the only transaction supported
  on the A ledger is "initiate transition to B", and the B ledger can only contain
  blocks with a header only, no body at all.
* `HardForkBlock '[Byron, Shelley]`: the hard fork combinator instantiated with
  the Byron and Shelley ledgers, running `PBFT` before the transition and
  `TPraos` after.

**Stats.** The stats are as follows:

* BFT protocol: 200 loc
  PBFT protocol, not including PBftState : 500 loc.
  Explicit leader schedule combinator: 110 loc.

  Mock ledger implementation and integration: 2800 loc.
  Tests: 1100 loc.

  Note that in this case, the implementation of the mock ledger itself should
  probably also be considered test code, but not the implementation of the
  protocols.

* Byron integration: 3000 loc.
  Integration of the dual ledger, comparing Byron to the Byron spec: 1200 loc.
  The Byron tests (pure byron, dual byron, but excluding golden tests): 3800 loc.

* Shelley integration: 2750 loc.
  Tests, not including golden tests: 1100 loc.

* Hard fork combinator implementation: 6900 loc.
  HFC consensus tests, including mock ledger: 1350 loc.

* Cardano block integration (i.e., HFC applied to Byron-Shelley): 1700 loc.
  Tests: 700 loc.


Note that all of these consensus tests reuse the same testing infrastructure
(the library mentioned above), which is not included in the line counts here.
