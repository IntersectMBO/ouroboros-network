### Breaking

- Unified two traces into one.  The following two records fields of
  `Ouroboros.Network.Diffusion.Tracers` were removed:
  - `dtDebugPeerSelectionInitiatorTracer`
  - `dtDebugPeerSelectionInitiatorResponderTracer`
  The were replaced with a single field `dtDebugPeerSelectionTracer` which
  combines both traces.

