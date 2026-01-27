### Breaking

- Trace only current peer selection targets, the previous targets are easily
  available in the log. The `ToJSON` instance for `PeerSelectionTargets` has
  been changed, it now inlines the current targets in the event object, and
  thus there's no `"kind":"PeerSelectionTargets"` any more, only
  `"kind":"TargetsChanged"`. 

### Non-Breaking

- Fixed a bug in ouroboros churn (used by `dmq-node`), IntersectMBO/ouroboros-network#5290.

