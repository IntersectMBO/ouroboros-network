### Patch

- The inbound governor now traces `TrUnexpectedlyFalseAssertion` when a
  responder of a registered connection starts or terminates while the
  governor no longer tracks that connection's responders. The trace identifies
  the connection and mini-protocol; previously this condition was not traced.
- Removed the unused internal `InboundGovernorLoop` constructor from
  `IGAssertionLocation` in `Ouroboros.Network.InboundGovernor`.
