# db-analyser

## About
This tool was initially developed to help Consensus debugging some issues, while the team was still working on Shelley. Later it was recognized that db-analyser might be potentially used by other teams when benchmarking / profiling some part of the code base.

## Running the tool

When you run db-analyser without any arguments, it will print out a nice helper message:

```
cabal build ouroboros-consensus-cardano-tools:db-analyser

Missing: --db PATH COMMAND

Usage: db-analyser --db PATH [--verbose]
            [--only-immutable-db [--analyse-from SLOT_NUMBER]]
            [--validate-all-blocks | --minimum-block-validation] COMMAND
            [--show-slot-block-no | --count-tx-outputs |
                --show-block-header-size | --show-block-txs-size |
                --show-ebbs | --store-ledger SLOT_NUMBER | --count-blocks |
                --checkThunks BLOCK_COUNT | --trace-ledger |
                --repro-mempool-and-forge INT]
            [--num-blocks-to-process INT]
            [--inmem-backingstore | --lmdb-backingstore [--mapsize NR_BYTES]]
  Simple framework used to analyse a Chain DB
```

Let's now try to break each option down.

### --db PATH

```
Missing: --db PATH COMMAND

Usage: db-analyser --db PATH
```

The tool works on a cardano-node's ChainDB. Thus the user must provide an obligatory `--db PATH` argument pointing to the particular DB.

### --verbose

db-analyser will get quite noisy

### --only-immutable-db

By default db-analyser will process all blocks from the current chain. That is from the genesis up to the current tip. In order to do this it must first properly initialize the whole ChainDB. That means that before it even starts processing blocks it will:

1. look for latest snapshot stored in DB_PATH/ledger
2. load that snapshot into memory
3. start replaying blocks
   * starting from that ledger state
   * while updating the ledger state in the process for each replayed block
   * keeping the intermediate results (ledger states) in memory while replaying blocks that live in the volatile DB (less than k blocks from the tip)

This may heavily impact any profiling that the user might be interested in doing.

To counter that problem `--only-immutable-db` flag was introduced.

```
[--only-immutable-db [--analyse-from SLOT_NUMBER]]
```

When enabled, db-analyser will work only with blocks from immutableDB, thus initialization described above will not happen.

This flag comes with an additional `--analyse-from` flag. It allows to start processing blocks from the requested slot number. A snapshot at that slot number must exist in `DB_PATH/ledger/SLOT_NUMBER_db-analyser` - where `SLOT_NUMBER` is the value provided by the user with the `--analyse-from` flag.
The user can use snapshots created by the node or they can create their own snapshots via db-analyser - see the `--store-ledger` command

### COMMAND

There are three options: `byron`, `shelley`, `cardano`. When in doubt which one to use, use `cardano`.

* `byron`

User should run this if they are dealing with Byron only chain. When the command is `byron` then user must provide `--configByron PATH` pointing to a byron configuration file.

* `shelley`

User should run this if they are dealing with Shelley only chain (neither Byron nor Allegra or any other era that comes after). When the command is `shelley` then user must provide `--configShelley PATH` pointing to a shelley configuration file. They may also provide `--genesisHash HASH` and `--threshold THRESHOLD`

* `cardano`
User should run this if they are dealing with `cardano` chain - that is a chain that has Byron, Shelley, Allegra, Mary, Alonzo blocks in it. When the command is `cardano` user must provide configuration for both `byron` and `shelley` (as described above) with additional `--configAlonzo PATH` pointing to Alonzo configuration file

### --num-blocks-to-process

```
[--num-blocks-to-process INT]
```

The user can limit the maximum number of blocks that db-analyser will process.

### Analysis

Lastly the user must provide the analysis they want to run on the chain. They must select one of below:

* `--show-slot-block-no` Will print the slot and block number of each block it process

* `--count-tx-outputs` Will print the block and slot number, tx out output for given block and the cumulative tx out output for all the blocks seen so far

* `--show-block-header-size` Will show block header size for each block and also the maximum head size it has seen in the whole chain it processed

* `--show-block-txs-size` Will print number of transactions and transactions size per each block

* `--show-ebbs` Will print all EBB blocks including their hash, previous block hash and a boolean value whether it is a known EBB (list of known EBBs stored in module `Ouroboros.Consensus.Byron.EBBs`)

* `--store-ledger SLOT_NUMBER` Will store a snapshot of a ledger state under `DB_PATH/ledger/SLOT_NUMBER_db-analyser`. If there is no block under requested slot number, it will create one on the next available slot number (and issue a warning about this fact).

* `--count-blocks` Will print out the number of blocks it saw on the chain

<<<<<<< HEAD:ouroboros-consensus-cardano-tools/Documentation.md

# db-synthesizer

## About
This tool synthesizes a valid ChainDB, replicating cardano-node's UX. The blocks forged to synthesize the ChainDB won't contain any transactions.

A minimal test case is provided which incorporates a staked genesis and credentials for 2 forgers (cf. `test/config`).

## Running the tool
When you run db-synthsizer without any arguments, it will print out usage information:

```
cabal build ouroboros-consensus-cardano-tools:db-synthesizer

Usage: db-synthesizer --config FILE --db PATH
            [--shelley-operational-certificate FILE]
            [--shelley-vrf-key FILE] [--shelley-kes-key FILE]
            [--bulk-credentials-file FILE]
            ((-s|--slots NUMBER) | (-b|--blocks NUMBER) |
            (-e|--epochs NUMBER)) [-f]

Available options:
  --config FILE            Path to the node's config.json
  --db PATH                Path to the Chain DB
  --shelley-operational-certificate FILE
                           Path to the delegation certificate
  --shelley-vrf-key FILE   Path to the VRF signing key
  --shelley-kes-key FILE   Path to the KES signing key
  --bulk-credentials-file FILE
                           Path to the bulk credentials file
  -s,--slots NUMBER        Amount of slots to process
  -b,--blocks NUMBER       Amount of blocks to forge
  -e,--epochs NUMBER       Amount of epochs to process
  -f                       Force overwrite an existing Chain DB
```

* Providing a `cardano-node` configuration file is mandatory. However, only very few values regarding geneses and protocol are actually required. Using a configuration stub would be possible; for expected key-value pairs see the `NodeConfigStub` type as well as its deserialization in module `Cardano.Tools.DBSynthesizer.Orphans`.
* Specifying a path on the local file system where the ChainDB should be written to is mandatory.
* Credentials for block forging are mandatory and may be provided as seperate files or in bulk as a single file.

## Options

The options for configuration and credentials are identical to those of `cardano-node`. The other options have the following meaning:

### forcing (over)write

The tool expects the given ChainDB path (`--db` option) to *not* be present. Should a directory by that name already exist, you can tell the tool to clear and reuse it with the flag `-f`. For safety deliberations, it will do so if and only if the directory is either empty, or contains a ChainDB.

### limiting synthesis

A limit must be specified up to which the tool synthesizes a ChainDB. Possible limits are either the number of slots processed (`-s`), the number of epochs processed (`-e`) or the absolute number of blocks in the resulting ChainDB (`-b`).
=======
### --inmem-backingstore and --lmdb-backingstore MAP_SIZE

```
[--inmem-backingstore | --lmdb-backingstore [--mapsize NR_BYTES]]
```

UTxO HD introduces optional functionality to store parts of the ledger state, such as the UTxO, on disk. The user can choose whether to store these parts of the ledger state in memory by providing the `--inmem-backingstore` flag, or use the functionality to store the ledger state on disk using the `--lmdb-backingstore` flag. In case the latter flag is provided, the `--mapsize NR_BYTES` option defines the maxmimum size of the on-disk database in nr. of bytes `NR_BYTES`. Note that the `NR_BYTES` should be a multiple of the OS page size. The maximum database size defaults to ~`6_000_000` bytes if this option is not provided. By default, `db-analyser` uses the in-memory backing store.
>>>>>>> 106ee892b (Integrate UTxO-HD):ouroboros-consensus-cardano/tools/db-analyser/Documentation.md
