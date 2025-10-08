<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `linger` function's arm callback now returns a `Maybe Bool`
- `keyedLinger'`s arm callback now returns a `Maybe (Set b)`
- `keyedLinger'`'s arm callback now returns a `Maybe (Set b, DiffTime))`
- The above changes allow those functions to reset signal state on `Nothing`

### Non-Breaking

- Added latch function to `Signal`
- bugfix missed promotion/demotion opportunities in:
  - `ActivePeers.aboveTargetBigLedgerPeers`
  - `ActivePeers.aboveTargetOther`
  - `EstablishedPeers.aboveTargetOther`
  - `EstablishedPeers.aboveTargetBigLedgerPeers`
  - `EstablishedPeers.belowTargetLocal`
  - `EstablishedPeers.belowTargetOther`
  - `ActivePeers.belowTargetLocal`
