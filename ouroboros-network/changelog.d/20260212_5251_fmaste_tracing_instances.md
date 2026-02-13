### Non-Breaking

- Moved `LogFormatting` and `MetaTrace` instances from `cardano-node` to `ouroboros-network`.
- Added `framework-tracing` and `tracing` sub-libraries to `ouroboros-network.cabal` to support these instances with correct dependencies.
- `framework-tracing` contains instances for lower-level components (e.g. `network-mux`) and depends only on `framework`.
- `tracing` contains instances for higher-level components (e.g. PeerSelection) and depends on `ouroboros-network`.
- Generalized `DebugPeerSelection` and `PeerSelectionCounters` tracing instances to use generic types, removing dependencies on `cardano-diffusion` and `ouroboros-consensus`.