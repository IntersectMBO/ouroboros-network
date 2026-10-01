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

- The inbound governor no longer blocks on `MuxFinished` emitted by a mux
  whose `ConnectionId` is reused by a newly registered connection.
  Previously it waited for the running mux of the new connection to stop,
  which stalled the inbound governor, and then unregistered the new
  connection.  Such `MuxFinished` is now ignored and traced as
  `TrStaleMuxFinished`.
