### Breaking

- `diffusionSimulation`: removed tracer argument, no longer polymorphic in
  monad - using `IOSim` only.  `diffusionSimulationM` is available but not
  exported.

### Non-Breaking

- Testing improvements in net-sim.

