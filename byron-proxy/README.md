# Byron proxy

A library for getting data to and from a Byron net.

## How to run it

You need `cardano-sl` yaml configurations: one for the blockchain network
(mainnet, etc.) and one for the network topology.
You can also give the `--db-path` argument to change from
the default `./db-byron-adapter`. A block index will be created at
`index-byron-adapter`. At the moment this cannot be changed.

```sh
$ nix-build -A byron-adapter
$ ./result/bin/byron-adapter --configuration-file <cardano-sl config> --configuration-key <cardano-sl config key> --topology <cardano-sl net topology>
```
