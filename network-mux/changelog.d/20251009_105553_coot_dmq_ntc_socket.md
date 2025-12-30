### Breaking

- Tracers are now passed to `Network.Mux.new` rather than `Network.Mux.run`.
- Added new trace points to `Network.Mux.Trace.Trace`:
  - `NewMux` which logs all `MiniProtocolInfo`s used to create the new `Mux` interface.
  - `MuxStarting` which logs that `Mux` is starting.

