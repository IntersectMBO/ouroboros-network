### Breaking

- `cardano-ping` has been deprecated and it is recommended to switch to
  `cardano-diffusion:ping` library.  The API has slightly changed.  The logger
  is instantiated by the top level `pingClients` function.  It supports
  connecting to multiple nodes at once. Note that `pingClient` is now an
  internal, not-exported function. The new API suports querying tip over
  node-to-node and node-to-client protocols.

