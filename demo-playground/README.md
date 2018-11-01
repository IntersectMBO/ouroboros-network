
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
cabal new-run demo-playground -- -t demo-playground/simple-topology.json -n 0
cabal new-run demo-playground -- -t demo-playground/simple-topology.json -n 2
cabal new-run demo-playground -- -t demo-playground/simple-topology.json -n 1
```

You will see that the logger waits to receive traffic from the core node 2,
and when 1 start, the logger will receive the (up-to-date) chain from both peers,
which means that node 1 and node 2 synced and now node 1 is following the chain
of node 2.

## Picking a block type

By default, if you run the example the way specified above, the underlying
protocol will exchange messages where the "block" is a simple `DummyPayload`,
which is virtually just an integer. However, this can be replaced by more
interesting (and in the future realistic) implementations for a block by 
specifying the `-p` flag at the startup. For example:

```
cabal new-run demo-playground -- -t demo-playground/simple-topology.json -n 2 -p dummy
```

Would run `Node 2` with the `DummyPayload`, whereas:

```
cabal new-run demo-playground -- -t demo-playground/simple-topology.json -n 2 -p mock
```

Will use a `MockPayload` which uses a BlockBody which can host (mock) transactions
within it.
