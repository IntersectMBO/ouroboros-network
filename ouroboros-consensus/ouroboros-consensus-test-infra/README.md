# Ouroboros-consensus-test-infra

This package provides infrastructure for running consensus-related tests.

This is both a separate package, because `ouroboros-consensus-byron` and other
ledger integrations depend on it, and an internal library of the
`ouroboros-consensus` package, because `test-consensus` and `test-storage`
depend on it. This is the reason why it lives inside `ouroboros-consensus` and
not in the root of the repository. This dual use of the same folder could be
avoided if Cabal supported multiple public libraries in a single cabal
project.
