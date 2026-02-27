<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->


### Breaking

- Removed extraCounters type variable from various places in the library,
  replacing its uses with 'ViewExtraPeers extraPeers'
- Removed daPeerSelectionStateToExtraCounters field from Diffusion
  Arguments record. This functionality was subsumed by the new
  SupportsPeerSelectionState class.
- Added SupportsPeerSelectionState constraint to required places.


<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
