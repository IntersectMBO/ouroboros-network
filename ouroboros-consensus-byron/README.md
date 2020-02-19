# Consensus-Byron

This package contains:

* `src`: integration of the Byron ledger with the consensus layer.

* `ouroboros-consensus-byrondual`: integration of the Byron ledger paired with
  the Byron spec ledger. This should be a separate package, but it depends on
  `src`, and `test` depends on it, so it cannot be separated.

* `test`: Byron ledger tests, protocol tests simulating various node setups,
  both running (just) the Byron ledger, and the Byron ledger in lockstep with
  the Byron spec ledger to detect any discrepancies. Hence the dependency on
  `ouroboros-consensus-byrondual`.

* `tools/db-analyser`: tool to show some information of a database containing
  Byron blocks.

* `tools/db-converter`: tool to convert an old `cardano-sl` database to a new
  `ouroboros-consensus` database.
