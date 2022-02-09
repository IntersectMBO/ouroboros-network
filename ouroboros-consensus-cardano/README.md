# ouroboros-consensus-cardano

This package contains:

* `src` : Support for Hard Fork between Cardano eras.

* `tools/db-analyser`: tool to analyse or validate a database containing blocks
  of some Cardano era.

* `tools/ledger-db-backends-checker`: simple tool (for the UTxO-HD feature) to compare the contents of an in-memory backing store and an LMDB backing store (i.e., it checks if they have the same slot number and values).