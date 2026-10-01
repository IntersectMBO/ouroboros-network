<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `Ouroboros.Network.InboundGovernor.Trace`: added `TrStaleMuxFinished`.

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->

### Patch

- The inbound governor checks `MuxFinished` without blocking. If the event
  belongs to an earlier connection with the same `ConnectionId`, and the
  currently registered mux has not stopped, the event is ignored and traced
  as `TrStaleMuxFinished`. Previously this could stall the entire inbound
  governor until the replacement mux stopped, potentially waiting indefinitely,
  and then unregister the replacement connection.
