
## Demo playground

Little playground to try in the wild the chain-following protocol where
consumers and producers run on named pipes and the communication happens out-of-process.

## Running the demo

You have to specify your network topology in a json file. There are two
examples inside `demo-playgroud`. The node 0 has a special meaning: it's a
"logger" node which can be attached to all the different nodes in the network
to inspect the traffic. For example, to setup a minimal example, runs in three
separate terminal (in this order):

```
cabal new-run demo-playground -- node -t demo-playground/simple-topology.json -n 0
cabal new-run demo-playground -- node -t demo-playground/simple-topology.json -n 2
cabal new-run demo-playground -- node -t demo-playground/simple-topology.json -n 1
```

You will see that the logger waits to receive traffic from the core node 2,
and when 1 start, the logger will receive the (up-to-date) chain from both peers,
which means that node 1 and node 2 synced and now node 1 is following the chain
of node 2.

## Submitting transactions

To submit transactions, first spin up the node you want to target, then type:

```
cabal new-run demo-playground -- submit -t demo-playground/simple-topology.json \
  -n 2 --address a --amount 1000
```

This would send the transaction to the node 2. The node would then add the Tx
to its mempool and incorporate it into a block when he's elected leader.
You can chain transactions once you know the hash, which is printed after you
submit the transaction:

```
> cabal new-run demo-playground -- submit -t demo-playground/simple-topology.json -n 2 --address a --amount 1000
Up to date
The Id for this transaction is: 6f6e1118
------------------------------------------------------------
> cabal new-run demo-playground -- submit -t demo-playground/simple-topology.json -n 2 --txin 6f6e1118 --txix 0 --address a --amount 200 --address b --amount 800
Up to date
The Id for this transaction is: 6dc4c580
```
