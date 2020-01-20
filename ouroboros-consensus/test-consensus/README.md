# Consensus Testing

This document overviews the `ouroboros-consensus:test:test-consensus` test
suite. The test suite consists of several QuickCheck property tests and
comparatively few hard-coded test cases. The tests can be divided into those
testing the package's _components_ and those testing its primary functionality:
_protocols_ for decentralized nodes to use in order to maintain a consensus
over the course of several slots.

## Component Tests

These modules test the components defined by the `ouroboros-consensus` package.

  * `Test.Consensus.BlockchainTime.*` - Tests the component used to divide the
    execution's duration into (sometimes uniform) slots.

  * `Test.Consensus.ChainSyncClient` - Tests key invariants of a ChainSync
    client synchronizing with a mock ChainSync server.

  * `Test.Consensus.Ledger.*` - Tests serialisation and hashing of blocks.

  * `Test.Consensus.Mempool` - Tests the component used to manage the sequences
    of transactions that comprise the bulk of each block's payload.

  * `Test.Consensus.Node` - Tests the "DB markers" that avoid accidental DB
    deletion.

  * `Test.Consensus.Protocol.PBFT` - Tests the data structure used to maintain
    the chain state of the Permissive BFT protocol.

  * `Test.Consensus.ResourceRegistry` - test the component used to prevent
    resource leaks (e.g. the OS process's file handles).

  * `Test.ThreadNet.Util.Test` - Tests some of the auxiliary definitions used
    in turn to define the protocol tests.

## Protocol Tests

These modules test the instances of the Ouroboros protocol family defined by
the `ouroboros-consensus` package.

  * `Test.ThreadNet.BFT` - Tests the BFT protocol with the mock ledger.

  * `Test.ThreadNet.PBFT` - Tests the Permissive BFT protocol with the mock ledger.

  * `Test.ThreadNet.RealPBFT` - Tests the PBFT protocol with the real ledger
    ("Byron").

  * `Test.ThreadNet.Praos` - Tests the Praos protocol with the mock ledger.

  * `Test.ThreadNet.LeaderSchedule` - Tests the Praos protocol with the mock
    ledger with and an fixed /a priori/ leader schedule.

These tests use unrealistic but illustrative values for the protocol
parameters. For example, the security parameter `k` is only several blocks so
that motivating examples and counterexamples are legible, while the Praos
active slots coefficient `f` is set to 0.5, which is aggressively high compared
to such a small `k` and so explores interesting leader schedules.

The tests defined by each of these modules execute a network of several nodes
for a duration of tens of slots and then inspect the final chains and logged
instrumentation data in search of invariant violations. The expected invariants
range from lower-level concerns, such as no file handle leaks, to analogs of
the primary properties from the 2017 paper by Bernardo et al "[Ouroboros Praos:
An adaptively-secure, semi-synchronous proof-of-stake
protocol](https://eprint.iacr.org/2017/573/20171115:001835)", such as Common
Prefix and Chain Growth (§2.1). The generators for these QuickCheck tests
explore various combinations of number of nodes, number of slots, delays before
each node joins the network, network topologies, protocol parameters (like `k`)
PRNG seeds, when nodes restart, if restarting nodes change their operational
key (including the subsequent heavyweight delegation transaction), with or with
Epoch Boundary Blocks (EBBs), etc.

The development team is working to extend our tests to cover more real-world
cases and edge cases. These tests will check that a node network reaches
consensus when it should and otherwise handles the edge cases as expected.
These edge cases will only rarely occur in practice and will be extremely
difficult to trigger by QA or using a testnet, so property testing is an
efficient alternative approach.

  * The current tests do not involve significant network latency or network
    partitions. See Issues #229 and #230.

  * The current tests do not involve any Byzantine faults, only faults akin to
    fail-stop. Without Byzantine faults there are no adversarial blocks, and so
    there can be no violations of the third primary paper property, Chain
    Quality. See Issue #236.

  * The current tests do not involve disk corruptions/failures. See Issue #269.

  * The current tests generate mock transactions. See Issue #888.

  * The current tests generate timelines known to not inherently cause
    errors/prevent consensus. We will introduce a separate generation scheme
    alongside the existing one. The new one will be less restricted, but it
    will consequently be paired only with weaker properties ("smoke tests",
    "sanity tests").

  * Other more-specific Issues: [those with "Protocol testing" in the Issue
    name](https://github.com/input-output-hk/ouroboros-network/issues?utf8=✓&q=is%3Aissue+is%3Aopen+protocol+testing)
    and/or [the `consensus` and `testing`
    labels](https://github.com/input-output-hk/ouroboros-network/issues?utf8=✓&q=is%3Aissue+is%3Aopen+label%3Atesting+label%3Aconsensus).

## Update 2020 January

Recent work has focused on increasing coverage of the test suite in a few
fundamental ways and finding/debugging/fixing related bugs, many of which
involved Epoch Boundary Blocks (EBBs). Most of this recent work was not
initially on our roadmap, having been identified during/by other planned work.

The primary previously-planned addition we have implemented is the restarting
and sometimes rekeying of nodes during the test, Issue #1202. This line of work
found some bugs involving the anachronistic ledger and/or EBBs and several bugs
involving graceful shutdown of a node.

  * Issues #1257, #1352, and #1435 - anachrony/backtracking and EBBs (along
    with Issue #1197). An additional consequence of this bug's fix required
    that we take significant care when generating a node join plan for the
    RealPBFT test; see Issue #1283 and the `Test.ThreadNet.Ref.RealPBFT`
    module. If some nodes were to join too late, then the ubiquitous assumption
    that "the chain contains at least `k` blocks in every span of `2k` slots"
    would be violated even due only to the PBFT constraints themselves
    (specifically a node can only forge so many of the last `k` blocks on the
    chain).

    As part of fixing these, we decided to include test cases that generate
    EBBs in the same way that the Byron era did: one per epoch boundary. This
    requires essentially complete support for EBBs, which ensures that we'll be
    able to handle the more modern chain's "historical" EBBs.

  * Issues #1448, #1452, and #1470 - Storage layer's handling of exceptions and
    termination. Needing to restart nodes within a simulation explored many new
    coverage paths.

  * Issues #1147 and #1435 - BlockFetch bugs. These bugs arise from a set of
    intricate relative timings, which the property tests' generators just
    happened to inducea and that lead to a genuine `NoBlocks` message that is
    then mishandled. Some of these were discovered on a work-in-progress PR for
    adding latency to network messages (Issue #229, with partial PR #1131).

The planned additions to the test suite moving forward have not significantly
changed: we are interested in message latency, node absences, more fault
variety, etc. The notable new addition is the unrestrained test generators and
their corresponding weaker properties.
