### Breaking

- `cardano-ping` has been deprecated and it is recommended to switch to
  `cardano-diffusion:ping` library.  The API has changed.  The logger is
  instantiated by the top level `pingClients` function.  It supports
  connecting to multiple nodes at once and supports SRV records. Note that
  `pingClient` is now an internal, not-exported function. The new API suports
  querying tip over node-to-node and node-to-client protocols.  An
  optparse-applicative parser for `PingOpts` is provided.
  - minimal node-to-node version support is raised from `NodeToNodeV_7` to `NodeToNodeV_14`.
  - minimal node-to-client version support is raised from `NodeToClientV_9` to `NodeToClientV_16`.
  - `pingOptsJson` is now a `LogFormat` rather than a `Bool`.
  - logger is now an internal funcionality, no need to initialise it on the user side.

