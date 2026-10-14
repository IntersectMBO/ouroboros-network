### Patch

- Fix exception cleanup in `withTimeoutSerialAlternative`, including cancelling
  a timeout exception already being delivered when the action ends.
- Update the documentation of deadline changes and timer amortisation.
