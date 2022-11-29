# Consensus

If you're beginning to contribute to the Consensus Layer, start with
[Contributing.md][consensus-contributing].

This package contains:

* `src`: the implementation of the Ouroboros consensus protocols and required
  components, e.g., the storage layer, mempool, protocol clients and servers,
  etc. This library abstracts over the ledger.

* `docs`: documentation, in particular, `docs/report` contains the technical
  report about the consensus and storage layer.

* `src-docs`: a collection of tutorials that are helpful for understanding the
  abstract structure of the consensus protocol. The structure of these is
  explained in [docs/Tutorials.md](docs/Tutorials.md).

The following packages use `ouroboros-consensus` to integrate specific ledgers:

* `../ouroboros-consensus-byron`: integration with the Byron ledger, including
  protocol tests simulating various node setups.

* `../ouroboros-consensus-byronspec`: integration with the Byron spec ledger.
  This is used to run the Byron protocol tests in lockstep with the spec to
  detect any discrepancies between the specification and the implementation.

* `../ouroboros-consensus-shelley`: integration with the Shelley ledger,
  including protocol tests simulating various node setups.

* `../ouroboros-consensus-cardano`: the consensus instantiated to the ledgers
  the `cardano-node` currently supports.

[consensus-contributing]: docs/Contributing.md
