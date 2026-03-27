<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Removed `ouroboros-network:framework-tracing`.  All  instances are moved to
  `ouroboros-network:tracing`.  The latter package exposes now only two modules:
  * `Network.Mux.Tracing`
  * `Ouroboros.Network.Tracing`
  which expose all instances.

### Non-Breaking

- Fixed `MetaTrace (Simple.AnyMessage ps)` instance.

<!--
### Patch

- A bullet item for the Patch category.

-->
