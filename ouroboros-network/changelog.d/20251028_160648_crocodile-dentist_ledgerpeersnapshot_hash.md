<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

o-n-api:
- Added tag `LedgerPeerSnapshotV3`
- removed compareLedgerPeerSnapshotApproximate

### Non-Breaking

o-n-api:
- Added {To,From}JSON instances to `Point` and `Block`
- added {encode,decode}LedgerPeerSnapshotPoint
- added {encode,decode}StakePools

o-n:
- Removed cardano-slotting dependency
- moved `jobVerifyPeerSnapshot` to cardano-diffusion
