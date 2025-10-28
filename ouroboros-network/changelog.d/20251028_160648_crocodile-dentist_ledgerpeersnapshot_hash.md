<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

ouroboros-network:api:
- Added tags `LedgerBigPeerSnapshotV23` and `LedgerAllPeerSnapshotV23`
  to identify network magic and tip block hash when snapshot was recorded
- removed compareLedgerPeerSnapshotApproximate

### Non-Breaking

ouroboros-network:api:
- Added {To,From}JSON instances to `Point` and `Block`
- added {encode,decode}LedgerPeerSnapshotPoint
- added {encode,decode}StakePools

ouroboros-network:
- Removed cardano-slotting dependency
- moved `jobVerifyPeerSnapshot` to cardano-diffusion
