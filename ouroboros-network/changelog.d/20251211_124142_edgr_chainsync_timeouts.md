<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Moved `timeLimitsChainSync` from `Ouroboros.Network.Protocol.ChainSync.Codec` to `Cardano.Network.Protocol.ChainSync.Codec.TimeLimits`.
- Added type variable `extraFlag` to `HandleWithExpandedCtx`.
- Added type variable `extraFlag` to `ConnectionManagerWithExpandedCtx`.
- Added type variable `extraFlag` to `ExpandedInitiatorContext`.
- Added type variable `extraFlag` to `OuroborosBundleWithExpandedCtx`.
- Added type variable `extraFlag` to `MiniProtocolWithExpandedCtx`.
- Added type variable `extraFlag` to `RunMiniProtocolWithExpandedCtx`.
- Added type variable `extraFlag` to `Applications`.
- Added type variable `extraFlag` to `NodeToNodeHandle`.
- Added type variable `extraFlag` to `NodeToNodeConnectionManager`.
- Added type variable `extraFlag` to `NodeToNodePeerConnectionHandle`.
- Added type variable `extraFlag` to `PeerStateActions`.
- Added type variable `extraFlag` to `ApplicationHandle`.
- Added type variable `extraFlag` to `PeerConnectionHandle`.
- Added type variable `extraFlag` to `PeerStateActionsArguments`.
- Added parameter `extraFlags` to `PeerSelection.Governor.ActivePeers.belowTarget`.
- Added parameter `extraFlags` to `PeerSelection.Governor.ActivePeers.jobPromoteWarmPeer`.
- Added parameter `extraFlags` to `PeerSelection.Governor.EstablishedPeers.belowTarget`.
- Added parameter `extraFlags` to `PeerSelection.Governor.EstablishedPeers.jobPromoteColdPeer`.
- Added parameter `extraFlags` to `PeerStateActions.establishPeerConnection`.
- Added parameter `extraFlags` to `PeerStateActions.activatePeerConnection`.

### Non-Breaking

- Added field `eicExtraFlags` to `ExpandedInitiatorContext`.
- Added field `defaultExtraFlags` to `PeerSelectionGovernorArgs`.
