# Consensus

This package contains:

* `src`: The implementation of the Ouroboros consensus protocols and required
  components, e.g., the storage layer, mempool, protocol clients and servers,
  etc. This library abstract over the ledger.

* `test-consensus`: Tests of the various consensus components which are
  unrelated to storage.

* `test-storage`: Tests of the storage layer.

* `ouroboros-consensus-mock`: integration with a mock ledger, includes
  protocol tests simulating various node setups. This is both a separate
  package, because `cardano-node` depends on it, and an internal library of
  this package, because `test-consensus` depends on it. This dual use of the
  same folder could be avoided if Cabal supported multiple public libraries in
  a single cabal project.

* `ouroboros-consensus-test-infra`: infrastructure for running tests. This is
  used by `test-consensus`, `test-storage`, `ouroboros-consensus-mock/test`,
  and `../ouroboros-consensus-byron/test`. For the same reason as
  `ouroboros-consensus-mock`, this is both a separate package and an internal
  library of this package.

Related packages:

* `../ouroboros-consensus-byron`: integration with the Byron ledger, including
  protocol tests simulating various node setups.

* `../ouroboros-consensus-byronspec`: integration with the Byron spec ledger.
  This is used to run the Byron protocol tests in lockstep with the spec to
  detect any discrepancies.

* `../ouroboros-consensus-shelley`: integration with the Shelley ledger,
  including protocol tests simulating various node setups.

* `../ouroboros-consensus-cardano`: the consensus instantiated to the ledgers
  the `cardano-node` currently supports.
