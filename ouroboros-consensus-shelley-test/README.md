# ouroboros-consensus-shelley-test

The reason for this being a separate package from
`ouroboros-consensus-shelley` and not just its testsuite, is that
`ouroboros-consensus-cardano` depends on the test code of Shelley to test the
Cardano chain, of which Shelley is a part.

This package contains:

* `src`: test generators and test infrastructure for testing the integration
  of the Shelley ledger with the consensus layer. Not the actual tests.

* `test`: the actual Shelley tests
