<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->



### Non-Breaking

- Introduced SupportsPeerSelectionState class which enhances
  ouroboros-network as a reusable diffusion library.
- The class collects the types of the extra peers and extra tracing.
  It also provides a method to retrieve a view with counters of those
  extra peers as well as exposes the PublicExtraPeersAPI type. The latter
  motivates the removal of extraPeersAPI from PeerSelectionActions record.
