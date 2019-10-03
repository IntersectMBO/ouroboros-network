# Consensus Testing

This document overviews the `ouroboros-consensus:test:test-consensus` test
suite. The test suite consists of several QuickCheck property tests and
comparatively few hard-coded test cases. The tests can be divided into those
testing the package's _components_ and those testing its primary functionality:
_protocols_ for decentralized nodes to use in order to maintain a consensus
over the course of several slots.

## Component Tests

These modules test the components defined by the `ouroboros-consensus` package.

  * `Test.Consensus.BlockchainTime` - Tests the component used to divide the
    execution's duration into uniform slots.

  * `Test.Consensus.ChainSyncClient` - Tests key invariants of a ChainSync
    client synchronizing with a mock ChainSync server.

  * `Test.Consensus.Mempool` - Tests the component used to manage the sequences
    of transactions that comprise the bulk of each block's payload.

  * `Test.Consensus.Protocol.PBFT` - Tests the data structure used to maintain
    the chain state of the Permissive BFT protocol.

  * `Test.Consensus.ResourceRegistry` - test the component used to prevent
    resource leaks (e.g. the OS process's file handles).

  * `Test.Dynamic.Util` - Tests some of the auxiliary definitions used in turn
    to define the protocol tests.

## Protocol Tests

These modules test the instances of the Ouroboros protocol family defined by
the `ouroboros-consensus` package.

  * `Test.Dynamic.BFT` - Tests the BFT protocol with the mock ledger.

  * `Test.Dynamic.PBFT` - Tests the Permissive BFT protocol with the mock ledger.

  * `Test.Dynamic.RealPBFT` - Tests the PBFT protocol with the real ledger
    ("Byron").

  * `Test.Dynamic.Praos` - Tests the Praos protocol with the mock ledger.

  * `Test.Dynamic.LeaderSchedule` - Tests the Praos protocol with the mock
    ledger with and an fixed /a priori/ leader schedule.

These tests use unrealistic but illustrative values for the protocol
parameters. For example, the security parameter `k` is only several blocks so
that motivating examples and counterexamples are legible, while the Praos
active slots coefficient `f` is set to 0.5, which is aggressively high compared
to such a small `k`and so explores interesting leader schedules.

The tests defined by each of these modules execute a network of several nodes
for a duration of tens of slots and then inspect the final chains and logged
instrumentation data in search of invariant violations. The expected invariants
range from lower-level concerns, such as no file handle leaks, to analogs of
the primary properties from the 2017 paper by Bernardo et al "[Ouroboros Praos:
An adaptively-secure, semi-synchronous proof-of-stake
protocol](https://eprint.iacr.org/2017/573/20171115:001835)", such as Common
Prefix and Chain Growth (§2.1). The generators for these QuickCheck tests
explore various combinations of number of nodes, number of slots, delays before
each node joins the network, network topologies, PRNG seeds, etc. TODO How to
characterize the synthetic transactions?

The development team is working through several important Issues on the GitHub
repository's backlog.

  * The current tests do not involve significant network latency or network
    partitions. See Issues #229 and #230.

  * The current tests do not involve any Byzantine faults, only faults akin to
    fail-stop. Without Byzantine faults there are no adversarial blocks, and so
    there can be no violations of the third primary paper property, Chain
    Quality. See Issue #236.

  * The current tests do not involve disk corruptions/failures. See Issue #269.

  * Other more-specific Issues: those with "Protocol testing" in the Issue name
    and/or the `consensus` and `testing` labels.
