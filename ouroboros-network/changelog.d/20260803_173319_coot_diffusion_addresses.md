<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `Ouroboros.Network.Diffusion.Configuration` now has a single `dcAddresses ::
  [Either ntnFd ntnAddr]` field, instead of the two `dcIPv[46]Address`.  This
  allows us to support multiple interfaces.  Use
  `Ouroboros.Network.Diffusion.readIPAddressAndPort` to parse `IP:Port` pari
  from a command line.

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
