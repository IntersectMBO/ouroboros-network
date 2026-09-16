<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `Snocket`'s `addrFamily` API change
  * Simplified `AddressFamily` data type, now it's a simple enumerationo of `AFInet`, `AFInet6` and `AFLocal`, removed `TestFamily`.
- The `Simulation.Netork.Snocket` is now monomorphic over address type, e.g.
  `NetworkAddress` (brought from diffusion testnet), which simplifies test
  cases.  This makes the API easier to use in tests.

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
