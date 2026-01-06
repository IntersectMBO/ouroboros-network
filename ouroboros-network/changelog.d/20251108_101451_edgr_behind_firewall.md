<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Changed the type of `localRoots` to `LocalRoots`.
- Modified `AcquireOutboundConnection` to include an additional parameter: `Provenance`.
- `acquireOutboundConnectionImpl` only creates a new connection if `Provenance` permits it.
- `jobPromoteColdPeer` only creates a new connection if no inbound connection is found and provenance is set to `Outbound`.

### Non-Breaking

- Added `LocalRoots` type in `Ouroboros.Network.PeerSelection.State.LocalRootPeers` with the following fields:
  - `rootConfig` of type `RootConfig`
  - `provenance` of type `Provenance`
- Added `localProvenance` field to `LocalRootConfig`.
- Added a new constructor `InboundConnectionNotFound` for `ConnectionManagerError`.
- Renamed `Test.Ouroboros.Network.Orphans` to `Test.Ouroboros.Network.OrphanInstances`.
- Moved the following instances from `Test.Ouroboros.Network.PeerSelection.LocalRootPeers` to `Test.Ouroboros.Network.OrphanInstances`:
  - `Arbitrary WarmValency`
  - `Arbitrary HotValency`
- Removed duplicated code from `Test.Ouroboros.Network.PeerSelection.Instances` and `Test.Ouroboros.Network.PeerSelection.RelayAccessPoint`, and moved it to `Test.Ouroboros.Network.OrphanInstances`:
  - `genIPv4`
  - `genIPv6`
  - `Arbitrary SlotNo`
  - `Arbitrary PeerAdvertise`
  - `Arbitrary PeerSharing`
  - `Arbitrary AfterSlot`
  - `Arbitrary UseLedgerPeers`
  - `Arbitrary PortNumber`
  - `Arbitrary RelayAccessPoint`
  - `Arbitrary LedgerRelayAccessPoint`
  - `Arbitrary (LocalRootConfig extraFlags)`
