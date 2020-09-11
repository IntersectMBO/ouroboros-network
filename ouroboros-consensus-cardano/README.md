# ouroboros-consensus-cardano

This package contains:

* `src` : Support for Hard Fork between Cardano eras.

* `tools/db-analyser`: tool to analyse or validate a database containing blocks
  of some Cardano era.

   ```
   cabal run db-analyser --          \
     --minimum-block-validation      \ # Don't validate the blocks
     --db ../cardano-node/db_mainnet \
     --onlyImmDB                     \ # Don't open the VolatileDB and LedgerDB, just the ImmutableDB
     cardano                         \ # CardanoBlock
     --configByron   ../cardano-node/mainnet-byron-genesis.json                       \
     --configShelley ../cardano-node/mainnet-shelley-genesis.json                     \
     --genesisHash   5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb \
     --nonce         1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81 \
     measure-ledger-validation       \ # Measure how long it takes to validate all blocks
   ```


