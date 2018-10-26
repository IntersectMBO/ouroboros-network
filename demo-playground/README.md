
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
