### Breaking

- Tracers are now `hermod-tracing-api`'s `Trace` (spelled `Tracer` via the
  `Hermod.Tracing.API.Tracer` facade, a `contra-tracer`-compatible vocabulary):
  every `Tracer m a` in the public API is Hermod's `Trace m a`, so applications
  can pass the traces they construct and retain, and Hermod's configuration and
  documentation control messages flow end to end. The package depends on
  `hermod-tracing-api:public ^>=1.1` instead of `contra-tracer`; consumers
  replace `import Control.Tracer` with `import Hermod.Tracing.API.Tracer`
  (`Tracer (..)` becomes `Tracer`: there is no constructor).
