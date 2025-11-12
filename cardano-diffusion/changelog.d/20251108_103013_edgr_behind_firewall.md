<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Modified `establishPeerConnection` in `Test.Cardano.Network.PeerSelection.MockEnvironment`:
  - Now only creates a new connection if no inbound connection is found and `ConnectionMode` allows it.
  - Added tracing for newly created connections.

### Non-Breaking

- Added a new tracer: `TraceEnvNewConnCreated`.
- Added a property test to verify that the node never connects to peers behind a firewall.
