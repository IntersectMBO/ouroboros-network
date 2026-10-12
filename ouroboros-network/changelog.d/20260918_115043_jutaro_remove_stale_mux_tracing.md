### Non-Breaking

- Removed `framework/tracing/Network/Mux/Tracing.hs`, an unbuilt leftover of the
  former `framework-tracing` sublibrary that no cabal stanza references. It
  differed from the built `tracing/Network/Mux/Tracing.hs` in one line: the
  `CleanExit` severity `Info` from "cardano-tracing: CleanExit - Info" had
  landed in the leftover only. The built module now has it, so `CleanExit` is
  traced at `Info` instead of `Notice`.
