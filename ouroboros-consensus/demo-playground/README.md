
## Demo playground

Little playground to try in the wild the chain-following protocol where
consumers and producers run on named pipes and the communication happens out-of-process.

## Running the demo

There are two options:

  - using an example cluster with a fully-connected three-node topology,
  - specifying your own.

### Using an example cluster

Simply launch `demo-playground/start-demo-cluster.sh`, optionally passing it a cluster type,
which defaults to `--real-pbft` (the other options including `--praos`, `--bft` and `--mock-bft`).

You will see that the three nodes syncs with each other and agree on a common chain.

The topology file used in this cluster is `demo-playground/simple-topology.json`.
The 3 core nodes defined in this topology file all follow each other.

The script also takes care of shutting down nodes.

### Rolling your own

If you want to to specify your own network topology, you'll have to edit the aforementioned example cluster topology json file and start up the nodes accordingly.

The node startup part must correspond to the topology file, but this is mostly handled
by the `start-node.sh` script, which only really needs to know the node ID,
which is passed as the first argument, and also the cluster type:

```
./demo-playground/start-node.sh 0 --bft
./demo-playground/start-node.sh 1 --bft
./demo-playground/start-node.sh 2 --bft
```

You'll have to take care of shutting down the nodes.

## Submitting transactions

To submit transactions, first spin up the node you want to target, then type:

```
./demo-playground/submit-tx.sh -n 2 --address a --amount 1000
```

This would send the transaction to the node 2. The node would then add the Tx
to its mempool and incorporate it into a block when it's elected leader.
You can chain transactions once you know the hash, which is printed after you
submit the transaction:

```
> ./demo-playground/submit-tx.sh -n 2 --address a --amount 1000
Up to date
The Id for this transaction is: 6f6e1118
```
