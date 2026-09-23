### Breaking

- Tracers are now `hermod-tracing-api`'s `Trace` (spelled `Tracer` via the
  `Hermod.Tracing.API.Tracer` facade); the package depends on
  `hermod-tracing-api:public ^>=1.1` instead of `contra-tracer`. Consumers
  replace `import Control.Tracer` with `import Hermod.Tracing.API.Tracer`.
