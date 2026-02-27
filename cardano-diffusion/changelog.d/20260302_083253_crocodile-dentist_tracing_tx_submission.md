<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Removed ExtraTrace from Monitor module, and moved its data constructors
  to ToExtraTrace data instance of SupportsPeerSelectionState instance for
  ExtraPeers type. Similarly, ExtraPeerSelectionSetsWithSizes was moved to
  the instance as well.
- removed cardanoPeerSelectionStateToCounters
- Adjusted various types to integrate the changes related to the introduction
  of SupportsPeerSelectionState class
  - CardanoTracers, CardanoTraceLocalRootPeers, CardanoPeerSelectionCounters,
  - peerChurnGovernor, cardanoPeerSelectionGovernorArgs

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
