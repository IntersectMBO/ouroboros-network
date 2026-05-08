<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

<!--
### Breaking

- A bullet item for the Breaking category.

-->

### Non-Breaking

- When a node is started and syncing begins using bootstrap peers, only two outbound active connections
  should be established to reduce the load on bootstray relays. Prior to this change, until the first
  churn cycle 15 minutes into operation, a full set of active peers would be connected to.

<!--
### Patch

- A bullet item for the Patch category.

-->
