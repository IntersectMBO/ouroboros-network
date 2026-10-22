<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Added `NodeToNodeV_17`.  Peras support (the `perasSupport` field of
  `NodeToNodeVersionData` and the Peras mini-protocols) moved from
  `NodeToNodeV_16` to the experimental `NodeToNodeV_17`.  `NodeToNodeV_16` is
  now the version which introduces tx-submission outbound version 2
  (`TxOutboundV_2`).  Since its version data no longer carries `perasSupport`,
  `NodeToNodeV_16` is not compatible with `NodeToNodeV_16` of previous
  releases.
  - The CDDL specification `node-to-node-version-data-v16.cddl` was renamed to
    `node-to-node-version-data-v17.cddl`; in `handshake-node-to-node-v14.cddl`
    versions 14 to 16 use `node-to-node-version-data-v14.cddl`.
- `MiniProtocolParameters`: the `txDecisionPolicy` field was replaced by
  `txSubmissionConfigV1` and `txSubmissionConfigV2`, which default to
  `defaultTxSubmissionConfigV1` and `defaultTxSubmissionConfigV2`.
- `chainSyncProtocolLimits`, `blockFetchProtocolLimits`,
  `txSubmissionProtocolLimits`, `keepAliveProtocolLimits`,
  `peerSharingProtocolLimits`, `perasCertDiffusionProtocolLimits` and
  `perasVoteDiffusionProtocolLimits` take the negotiated `NodeToNodeVersion`.
  `txSubmissionProtocolLimits` sizes the ingress queue with
  `txSubmissionConfigV1` for versions before `NodeToNodeV_16`, and with
  `txSubmissionConfigV2` otherwise.
- `cardano-diffusion:cardano-diffusion-tests-lib`:
  - `Test.Cardano.Network.Diffusion.Testnet.Simulation`: `SimArgs` has new
    `saTxSubmissionConfig` and `saTxOutboundVersion` fields, and
    `mainnetSimArgs` takes a `TxSubmissionConfig` and a `TxOutboundVersion`.
  - `Test.Cardano.Network.Diffusion.Testnet.MiniProtocols`: `AppArgs` has new
    `aaTxSubmissionConfig` and `aaTxOutboundVersion` fields.

### Non-Breaking

- Added `minPerasVersion` to `Cardano.Network.NodeToNode.Version`.
- `cardano-diffusion:orphan-instances`: the JSON instances of
  `NodeToNodeVersion` support `NodeToNodeV_17`.

<!--
### Patch

- A bullet item for the Patch category.

-->
