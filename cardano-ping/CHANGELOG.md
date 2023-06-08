# Revision history for cardano-ping

## 0.2.0.2 -- 2023-06-08

* Support `NodeToNodeV_11` and `NodeToClientV_16`.
* Fix delay/timeout bugs (miliseconds were used instead of seconds).
* Print query even if --quiet flag is given.
* Instead of a boolean flag print `InitiatorOnly` or `InitiatorAndResponder`.
* Fixed encoding of `NodeToNodeV_11`.


## 0.2.0.1 -- 2023-05-26

* Support `ghc-9.6`.

## 0.2.0.0 -- 2023-05-08

* Support for `NodeToNodeV_12` and `NodeToClientV_16`, e.g. support for
  querying `NodeToNodeVersionData` / `NodeToClientVersionData`.
* Support `NodeToNodeV_11` and `NodeToClientV_15` (peer sharing).

## 0.1.0.0 -- 2022-12-14

* This code was originally from the cardano-ping executable component of the `network-mux` package.
