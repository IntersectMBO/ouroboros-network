<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->


### Non-Breaking

- Added types for running ouroboros-network without any extra peer types:
  - NoExtraPeers, NoExtraDebugState, NoExtraState, NoExtraFlags
- Added SupportsPeerSelectionState instance for NoExtraPeers.
- Added some convenience type aliases for running diffusion using NoExtraPeers:
  - OuroborosTracePeerSelection, OuroborosDebugPeerSelection,
    OuroborosPeerSelectionCounters
