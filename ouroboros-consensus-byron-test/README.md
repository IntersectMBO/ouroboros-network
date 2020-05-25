# ouroboros-consensus-byron-test

The reason for this being a separate package from `ouroboros-consensus-byron`
and not just its testsuite, is that `ouroboros-consensus-cardano` depends on
the test code of Byron to test the Cardano chain, of which Byron is a part.

This package contains:

* `src`: test generators and test infrastructure for testing the integration
  of the Byron ledger with the consensus layer. It also contains the
  integration of the Byron ledger paired with the Byron spec ledger. Not the
  actual tests.

* `test`: Byron ledger tests, protocol tests simulating various node setups,
  both running (just) the Byron ledger, and the Byron ledger in lockstep with
  the Byron spec ledger to detect any discrepancies.
