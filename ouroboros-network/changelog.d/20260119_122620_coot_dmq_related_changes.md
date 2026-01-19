### Breaking

- Removed `dtTraceChurnCounters` from `Ouroboros.Network.Diffusion.Tracers`, it
  duplicated information already available in `dtTracePeerSelectionTracer`.  As
  a consequence, `ChurnCounters` is removed as well as `pcaChurnTracer` record
 field from `PeerChurnArgs`.
