2020 March 27 Nicolas Frisby <nick.frisby@iohk.io>

# Introduction

This draft document summarizes the current state of the `ouroboros-consensus`
package's `ThreadNet` test suite and concludes by listing primary desiderata.

# Tested Properties

The test suite checks the following properties for the `ThreadNet` tests.

  * `[CommonPrefix]` - Whether the common intersection of all the nodes' final
    chains is as recent as expected.

  * `[MessageDelay]` - Whether blocks forged at one node arrive at other nodes
    as promptly as expected.

  * `[ChainGrowth]` - Whether the longest chain in the net grows as expected
    (eg according to a statically known leadership schedule)

  * `[ValidRejections]` - Whether a rejection of a block as invalid is
    unexpected.

  * `[ValidInclusions]` - Whether an invalid block ever appears on a selected
    chain.

  * `[NoFileLeaks]` - Whether a file handle was not properly closed by the end
    of the execution.

  * `[PBftSim]` - For mock PBFT, whether the observed behavior of the node net
    matches that predicted by a PBFT reference simulator (discussed below).

  * `[ByronRekey]` - For Byron, whether a genesis key (and its corresponding
    node) can successfully redelegate (restart to switch its static
    configuration over) to a fresh operational key.

  * `[ByronParamUpd]` - For Byron, whether a proposed parameter update is accepted.

  * `[ByronSoftwareUpd]` - For Byron, whether a proposed software version
    update is accepted.

  * `[ByronEBBs]` - For Byron, whether all expected EBBs are present in the
    final chains. (We forge EBBs during the test to be confident that the main
    net correctly handles historical EBBs.)

# Test Configurations

The configuration of each `ThreadNet` test varies the following factors.

  * A PRNG seed.

  * The slot length.

  * Which protocol all nodes are running: mock BFT, mock PBFT, mock Praos with
    a fixed leader schedule, mock Praos with its VRF-determined leader
    schedule, real PBFT (ie Byron), and dual PBFT (ie a dual ledger with the
    Byron implementation and also the Byron executable specification).

  * The security parameter `k`. It takes on values from 1 or 2 (depending on
    the protocol) to 10.

  * The number of nodes, currently 2 to 5.

  * The number of slots, currently 1 to 100.

  * When each node joins the node net and the ultimate topology of the node
    net. We make some simplifying assumptions: all nodes are core nodes (as
    opposed to relay nodes), the order in which nodes join the network is
    non-descending according to numerical node-id, and the topology of the
    joined nodes is always a connected graph.

  * When nodes should restart themselves; such restarts are effectively
    instantaneous. For Byron, up to one restart during each test may
    additionally change the node's operational key and simultaneously begin
    issuing the corresponding redelgation certificate transaction.

  * For LeaderSchedule, which nodes lead each slot.

  * For Byron, one sequence of transactions per node for it to immediately and
    repeatedly attempt to propagate at every opportunity. This is for testing
    the proposal and vote mechanism.

The configuration notably excludes the following factors, despite each posing a
non-trivial challenge for real node nets.

  * Clock skew. All test nodes share a single perfect global clock that ticks
    at the onset of each regularly spaced time slot.

  * Computational latency. All computation on a test node is effectively
    instantaneous, ordered only by data dependencies and the @io-sim@ package's
    scheduler.

  * Network latency. All messages between test nodes arrive immediately after
    being sent. Individual network channels preserve the order multiplicity of
    messages.

  * Network partitions. Network channels connecting test nodes have 100%
    up-time once they are created.

In the absence of clock skew and any latencies, _every event_ happens during
the instantaneous onset of some slot. The comprehensive timeline is thus a
discrete grid, regularly spaced according to the slot length. Even with
non-zero but static clock skew, every event would happen during an instant that
some node considered to be a slot onset, so the comprehensive timeline would be
a union of shifted copies of the base grid, one copy per node.

# Unsatisfiable Test Configurations

We constrain the generation of test configurations to avoid those that violate
assumptions designed into the implementation, since we can not generally expect
a net using such a configuration to pass the tests.

  * For LeaderSchedule, the test configuration is constrained so that it does
    not induce an otherwise-correct behavior that creates a wedge in the nodes'
    selected chains. For example if both nodes in a two node net lead `k` slots
    in a row, they will necessarily be wedged (recall that both nodes will
    forge simultaneously, at the slot onset).

  * For Byron, the test configuration is constrained so that it does not induce
    an otherwise-correct behavior that violates the chain density invariant.
    For example, if the first node joins too many slots before the second node,
    then the round-robin schedule will prevent it from producing the requisite
    `k` blocks in `2k` slots. Moreover, in some circumstances where the
    round-robin schedule alone would not be problematic, the PBFT signing
    threshold will prevent some leaders from forging a valid block, and thereby
    similarly prevent the requisite chain density.

For mock PBFT and Byron, we allow test configurations that put the node net
into an unpredictable state, but we relax many of the tests in such cases. We
are able to do statically recognize such test cases via the PBFT reference
simulator. The PBFT reference simulator also serves as an oracle so that a node
that is changing its operational key does not issue a corresponding
redelegation certificate transaction that expires before it is included in the
nodes' common chain. (The Byron specification requires that a redelegation
certificate includes the epoch number of the block which includes its
transaction.)

# Desired Improvements

The following three test suite improvements are the most important for the near
future.

  * Test richer timelines and event interleavings by introducing clock skew and
    latencies. The artificially simple timeline is the most glaring
    incompleteness of the current test suite.

  * Test in the presence of network partitions. The 100% network up-time is the
    next most glaring incompleteness.

  * The real Praos protocol will rely on some chain density invariant, as does
    Byron. The PBFT reference simluator used to recognize unsatisfiable Byron
    test configurations already has non-trivial dependencies on various parts
    of the test configuration (node join plan, node topology, etc); we
    anticipate a Praos reference simulator would be even more complicated. We
    therefore desire a different strategy for testing the real Praos protocol.
