# Ouroboros-consensus-mock

This package provides an integration of the consensus protocols with a mock
ledger. This also includes protocol tests that simulate various node setups.

This is both a separate package, because `ouroboros-consensus-cardano` depends
on it, and an internal library of the `ouroboros-consensus` package, because
`test-consensus` depends on it. This is the reason why it lives inside
`ouroboros-consensus` and not in the root of the repository. This dual use of
the same folder could be avoided if Cabal supported multiple public libraries
in a single cabal project.
