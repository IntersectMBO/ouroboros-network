<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->


### Breaking

- Added a negotiated data flow callback to 'Server.Simple.with'
- 'mkMiniProtocolInfo' and 'toMiniProtocolInfos' now take Provenance
  and DataFlow parameters to filter supported set of miniprotocols
- connectToNode, connectToNode', connectToNodeWithMux, connectToNodeWithMux',
  connectToNodeSocket now take a negotiated data flow callback like Simple.with


<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
