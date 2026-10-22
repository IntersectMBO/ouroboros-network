### Breaking

- The `tracing` sublibrary now provides `LogFormatting` / `MetaTrace` instances
  for `hermod-tracing-api` (the successor of `trace-dispatcher`); it depends on
  `hermod-tracing-api:public ^>=1.1` instead of `trace-dispatcher`. Consumers on
  `trace-dispatcher` (e.g. `dmq-node` 0.7.x, `cardano-node` 11.1.x) must migrate
  to `hermod-tracing` to use this release.
