2020 March 27 Nicolas Frisby <nick.frisby@iohk.io>

# Introduction

This draft document proposes an implementation plan for testing richer
timelines and event interleavings by introducing clock skew and latencies in
the `ouroboros-consensus` package's `ThreadNet` test suite. It addresses the
primary difficult: defining the test suite's expectations in the context of the
more fluid timeline.

See the accompanying `test-suite-status-quo.md` document for context.

# Executive Summary

The problem: the current test suite is significantly incomplete since all
events occur on a global and regularly space timeline.

The ultimate solution: introduce clock skew, network latencies, and
computational latencies.

The proposed solution: introduce clock skew and network latencies; we defer
computational latencies for now.

These features are actually relatively simple to introduce into the test nodes
themselves. On the other hand, they are difficult to introduce in the existing
test properties. The key challenge is determining what usefully high-level
behavior can be expected in the presence of particular skews and latencies.

The proposed mechanism: define, detect, and validate _steady-states_ of the net
and ensure that each test case occasionally visits a steady-state.

  * A node net is in a steady-state when all threads in all joined nodes are
    blocked (STM transaction, thread delay, other IO) and all network channels
    are empty.

  * Due to the absence of computational latencies, a net in a steady-state
    remains there until one of the node's clock ticks.

  * Each such steady-state should satisfy several invariants. Since all
    messages have been received and processed, all mini protocol instances will
    have fully synced and should be in an idle state. Therefore, the nodes'
    ChainDBs should also be up-to-date.

  * TODO converse: threads should block and channels remain empty as soon as
    possible once the mini protocols are idle and the ChainDBs are up-to-date.

## State Composition

Within the scope of this plan, a net state consists of the current contents of
several state variables within each node that has already joined the net.

  * The current slot according to the node's internal clock.

  * The current tip of the node's ChainDB.

  * The latest candidate chain reported by each of the node's active ChainSync
    clients.

  * The latest peer fetch status reported by each of the node's active
    BlockFetch clients.

  * The latest accumulated fetch request provided to each of the node's active
    BlockFetch clients.

  * Th latest exception that downed each of the node's inactive ChainSync and
    BlockFetch clients.

This initial proposal intentionally omits the transaction mempool and the TxSub
mini protocol for simplicity; we anticipate that their eventual inclusion will
not introduce any novel concerns.

## Steady-State Invariants

These invariants enumerate the detailed qualities of a steady-state, ie the
state, within the scope of this plan, of a net that is _fully synchronized and
at rest_.

  * `[SteadyChan]` - All network channels are empty; no messages are in-flight.

  * `[SteadyThread]` - All nodes' threads are blocked.

  * `[SteadyCausalCDB]` - A node's ChainDB tip is not ahead of the node's own
    internal clock.

  * `[SteadyOutputBF]` - All BlockFetch clients report their server's status is
    `Ready` and `Idle` with no in-flight requests nor blocks.

  * `[SteadyInputBF]` - There are no outstanding BlockFetch fetch requests.

  * `[SteadyOutputCS]` - No ChainSync candidate is preferable to the node's
    current ChainDB tip.

  * `[SteadyInputCS]` - All ChainSync candidates match the corresponding
    server's ChainDB tip.

  * `[SteadyTopoVertex]` - The steady-state contains exactly the nodes that
    have joined the net.

  * `[SteadyTopoEdge]` - The steady-state includes a ChainSync client and a
    BlockFetch client for every directed edge in the topology of joined nodes,
    either as the clients' state or else as the `ForkTooDeep` or
    `NoMoreIntersection` exception that most recently terminated that mini
    protocol instance.

## Detecting and Validating Steady-States

The proposed mechanism detects only `[SteadyChan]` and `[SteadyThread]`.
Steady-state validation then requires that the rest of the invariants also hold
as a consequence. This can be intuitively summarized as "if the nodes are doing
nothing, then there must be nothing for the nodes to do".

TODO Also test the converse: "if there's nothing to do, the nodes are doing
nothing".

The initial proposal relies on the `io-sim` infra-structure providing
infinitely-fast computation: time only passes (in the sense of `threadDelay`)
when all `io-sim` threads are blocked. In order to detect `[SteadyThread]`
without requiring invasive instrumentation of the proper node implementation,
the test suite spawns a single additional thread that records the contents of
all of the relevant state variables (almost all of the necessary handles are
available via a node's `NodeKernel` handle) and explicitly checks for
`[SteadyChan]` via `check` in a single `STM` transaction. Once that transaction
returns a candidate steady-state, the thread yields (via `atomically (pure
[])`) N-many times for an N so large that -- excepting bugs -- we're confident
that all the nodes' proper threads will have become blocked before the Nth
yield (eg 10000 yields). After yielding N times, the thread again reads the
state variables and checks `[SteadyChan]`. If the state before and after the N
yields are equivalent, we conclude that candidate steady-state is actually
steady. The thread then enters an STM transaction that only returns once one of
the relevant state variables changes again and then repeats.

## Advantages of the Proposal

This proposal primarily generalizes the existing test suite. In particular,
exactly one steady-state is detected per slot in the existing test suite. This
is expected: all events happen instantaneously "during" a slot onset, since
there is no clock skew, no network latency, and no computational latency. The
introduction of clock skew and network latency will cause steady-states to
instead arise at arbitrary times, zero or many per slot.

The `[CommonPrefix]`, `[MessageDelay]`, and `[ChainGrowth]` tests of the
existing test suite relate to the `[SteadyOutputBF]`, `[SteadyInputBF]`,
`[SteadyOutputCS]`, `[SteadyInputCS]`, `[SteadyTopoVertex]`, and
`[SteadyTopoEdge]` steady-state invariants. In particular, if the steady-state
invariants held during a run "as often as possible" given the random clock skew
and message latency, then that run will also satisfy the "as expected" clause
common to the existing properties. TODO mention something about the converse:
steady-states not being needlessly spoiled.

The remaining properties of the current test suite other than `[PBftSim]` must
either still be checked seperately (eg `[NoFileLeaks]`) or else require
additionally tracking transactions in mempools (Byron rekeys and Byron
updates). TODO determine how to check those properties once the mempool is
included in the steady-state.

## Comparison to Praos Paper

[The first Praos paper](https://eprint.iacr.org/2017/573/20170614:205845)
defines an uninterpreted specification constant Delta to be the greatest
message delay that "the adversary" can induce. In that highly abstract
specification, though, there is only one kind of inter-node message: an entire
chain. Thus *message delay* is the time taken for one node to send an entire
chain to another node. The real implementation doesn't send entire chains as a
single message, and moreover it divides sending even a single block into a
multi-step process (ChainSync and BlockFetch). Thus the paper's message delay
is a higher-level concept than the latency of a concrete message exchanged by
two nodes as implemented. In fact, some blocks may never arrive (not even the
header) at all nodes, which would be strictly interpreted as an indefinite
message delay. The steady-state invariants related to message delay are those
relating ChainSync clients' candidates and ChainDBs, `[SteadyOutputCS]` and
`[SteadyInputCS]`.

The Praos protocol is actually named for the "silent periods" that are induced
by having a relatively low value for the *active slot coefficient* `f`. The
proposed tests can take inspiration from that concept in order to ensure that
steady-stated do occur during test runs. Specifically, the test suite can
temporarily suppress all network latencies (ie enable infinitely-fast message
delivery) until a steady-state is detected.

## Remaining Work (Disadvantages)

1. How to ensure the chain density invariant? Or else what to do when a sparse
   chain arises?

1. How to set expectations regarding deleg cert txs, proposals, and votes?

1. When to expect consensus/no wedges? And is that answer a prerequisite for
   re-using the cardano-ledger-specs tx generator?
