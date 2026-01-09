<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Moved `timeLimitsChainSync` from `Ouroboros.Network.Protocol.ChainSync.Codec` to `Cardano.Network.Protocol.ChainSync.Codec.TimeLimits`.
- Added parameter `PeerTrustable` to `timeLimitsChainSync`.
- Changed timeout behavior: No timeout when peer is trusted and ChainSync state is `StNext StMustReply`.

### Non-Breaking

- Added property tests to validate the timeout behavior of the `timeLimitsChainSync` function.
