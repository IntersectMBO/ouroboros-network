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

By default db-analyser will process all blocks from the chain database
(`ChainDB`), from the genesis block up to the chain tip. In order to do this it
must first properly initialize the whole database. That means that before it
even starts processing blocks it will:

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

Lastly the user must provide the analysis that should be run on the chain:

* `--show-slot-block-no` prints the slot and block number of each block it
  process.

* `--count-tx-outputs` prints the block and slot number, tx out output for given
  block and the cumulative tx out output for all the blocks seen so far.

* `--show-block-header-size` shows block header size for each block and also the
  maximum head size it has seen in the whole chain it processed.

* `--show-block-txs-size` prints number of transactions and transactions size
  per each block.

* `--show-ebbs` prints all EBB blocks including their hash, previous block hash
  and a boolean value whether it is a known EBB (list of known EBBs stored in
  module `Ouroboros.Consensus.Byron.EBBs`).

* `--store-ledger SLOT_NUMBER` stores a snapshot of a ledger state under
  `DB_PATH/ledger/SLOT_NUMBER_db-analyser`. If there is no block under requested
  slot number, it will create one on the next available slot number (and issue a
  warning about this fact).

* `--count-blocks` prints out the number of blocks it saw on the chain

* `--benchmark-ledger-ops` applies the main ledger calculations to each block in
  the chain database, and collect different metrics such as total time spent,
  time spent doing garbage collection, etc. The ledger operations that this
  command benchmarks are: forecasting, ticking and applying the header, and
  ticking and applying a block. The benchmarking results are stored in the file
  specified by the `out-file` option. In the [`scripts`](./scripts) directory we
  provide a `plot-ledger-ops-cost.gp` script that can be used to plot the
  benchmarking results. See this file for usage information.
  
## Examples

The use of this tool requires a chain database together with the configuration
files that were used (by `cadano-node`) to populate it (by means of chain
syncing). Assuming you ran `cardano-node` on your machine, it is quite handy to
save the location to the working directory from which it was run. Eg:

```sh
export NODE_HOME=/path/to/local/copy/of/cardano-node/working/dir/
```

### Saving a snapshot

Suppose we have a local chain database in reachable from `$NODE_HOME`, and we
want to take a snapshot of the ledger state for slot `100`. Then we can run:

```sh
cabal run exe:db-analyser -- cardano \
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --only-immutable-db --store-ledger 100
```

If we had a previous snapshot of the ledger state, say corresponding to slot
`50`, it is possible to tell `db-analyser` to start from this snapshot:

```sh
cabal run exe:db-analyser -- cardano \
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --analyse-from 50 \
    --only-immutable-db --store-ledger 100
```

### Running the ledger operations benchmark

To benchmark the ledger operations, using the setup mentioned in the foregoing
examples, one could run the tool as follows:

```sh
cabal run exe:db-analyser -- cardano
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --analyse-from 100 \
    --benchmark-ledger-ops \
    --out-file ledger-ops-cost.csv \
    --only-immutable-db
```

The benchmarking command can be combined with `--num-blocks-to-process` to
specify the application of how many blocks we want to process. Eg:

```sh
cabal run exe:db-analyser -- cardano
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --analyse-from 100 \
    --benchmark-ledger-ops \
    --out-file ledger-ops-cost.csv \
    --num-blocks-to-process 200
    --only-immutable-db
```

#### Plotting the benchmark results

Assuming you are at the top level of `ouroboros-network`, and the benchmark data
has been written to a fine named `ledger-ops-cost.csv`, a plot can be generated
by running:

```sh
gnuplot -e "bench_data='ledger-ops-cost.csv'" \
        -e "out_file='results.png'" \
           ouroboros-consensus-cardano-tools/scripts/plot-ledger-ops-cost.gp 
```

The plot will be written to a file named `results.png`. See the script file for
more usage options.

# db-synthesizer

## About
This tool synthesizes a valid ChainDB, replicating cardano-node's UX. The blocks
forged to synthesize the ChainDB won't contain any transactions.

A minimal test case is provided which incorporates a staked genesis and credentials for 2 forgers (cf. `test/config`).

## Running the tool
When you run db-synthesizer without any arguments, it will print out usage information:

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

# ImmDB Server

A standalone tool that serves a Cardano ImmutableDB via ChainSync and BlockFetch.

This can be useful to locally test syncing without having to run a full node just for serving the chain.

## Usage

```
 $ cabal run exe:immdb-server -- --help
Usage: immdb-server --db PATH [--port ARG] --config PATH

  Serve an ImmutableDB via ChainSync and BlockFetch

Available options:
  -h,--help                Show this help text
  --db PATH                Path to the ImmutableDB
  --port ARG               Port to serve on
  --config PATH            Path to config file
```
