### Breaking

- Require `MonadUnique` in protocol drivers with time limits, handshake and
  connection setup, and `Diffusion.runM`, following the timeout scope identity
  change in `network-mux`. Require `io-classes ^>=1.9`.
