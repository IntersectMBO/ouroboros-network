### Non-Breaking

- `Mux.Trace`'s `CleanExit` is traced at `Info` instead of `Notice`, as
  "cardano-tracing: CleanExit - Info" intended. That change had landed only in
  `framework/tracing/Network/Mux/Tracing.hs`, an unbuilt leftover of the former
  `framework-tracing` sublibrary, and not in the built
  `tracing/Network/Mux/Tracing.hs`.
