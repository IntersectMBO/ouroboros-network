### Breaking

- Require `MonadUnique` for `withTimeoutSerial`,
  `withTimeoutSerialAlternative` and the mux entry points. Timeout scopes now
  use the hash of a fresh unique value to identify their exceptions.

### Patch

- Fix exception cleanup in `withTimeoutSerialAlternative`, including cancelling
  a timeout exception already being delivered when the action ends.
- Distinguish timeout exceptions from nested, independent scopes.
- Update the documentation of deadline changes and timer amortisation.
