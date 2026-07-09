<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `Ouroboros.Network.TxSubmission.Inbound.V2.Registry.awaitSharedChange` is now
  an STM action, but it requires to pass a registered delay.

### Non-Breaking

- Added `Ouroboros.Network.RegisteredDelay` to `ouroboros-network:framework`

<!--
### Patch

- A bullet item for the Patch category.

-->
