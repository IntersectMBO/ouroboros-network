### Breaking

- `Ouroboros.Network.Server.Simple.with` now requires tracers as argument,
  these should not be nullTracers in production code, although
  `Network.Mux.Trace.ChannelTrace` and `Network.Mux.Trace.BearerTrace` should
  be off by default as they can be extensive, the `Network.Mux.Trace.Trace` can
  be on by default, while `Ouroboros.Network.Server.Simple.ServerTrace` must be
  on as it traces important exception.
