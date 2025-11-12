<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Changed the type of `localRoots` to `LocalRoots`.
- Modified `AcquireOutboundConnection` to include an additional parameter: `ConnectionMode`.
- `acquireOutboundConnectionImpl` only creates a new connection if the `ConnectionMode` function permits it.
- `jobPromoteColdPeer` only creates a new connection if no inbound connection is found and the peer is not behind a firewall.

### Non-Breaking

- Added `LocalRoots` type in `Ouroboros.Network.PeerSelection.State.LocalRootPeers` with the following fields:
  - `rootConfig` of type `RootConfig`
  - `behindFirewall` of type `Bool`
- Added `localRootBehindFirewall` field to `LocalRootConfig`.
- Added a new sum type: `ConnectionMode`.
- Added a new constructor `InboundConnectionNotFound` for `ConnectionManagerError`.
