### Breaking

- `TraceChurnMode`'s `MetaTrace` instance has the inner name `ChurnMode`
  (previously empty) and gives its severity for the namespace. A tracer that
  wants to keep the namespace it had drops `ChurnMode` from its prefix.
