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
  `txSubmissionLogicVersion :: TxSubmissionLogicVersion` and
  `nodeTxSubmissionConfig :: NodeTxSubmissionConfig`, the node's
  tx-submission settings (e.g. from its configuration).  A connection's
  `TxSubmissionConfig` is `txSubmissionConfigForVersion` of its negotiated
  `NodeToNodeVersion`: with `TxSubmissionLogicV1`, or before
  `NodeToNodeV_16`, it is `defaultTxSubmissionConfigV1` (window
  `txSubmissionMaxUnacknowledgedTxIdsV1 = 10`); otherwise the node's
  settings.  The node's `TxDecisionPolicy` is computed and validated with
  `mkNodeTxDecisionPolicy`.
- `defaultMiniProtocolParameters` was replaced by
  `mkDefaultMiniProtocolParameters`, which takes the node's
  `TxSubmissionLogicVersion` and `NodeTxSubmissionConfig`
  (e.g. `defaultNodeTxSubmissionConfig`).  The same change applies to
  the re-export from `Cardano.Network.Diffusion.Configuration`, which also
  re-exports `NodeTxSubmissionConfig` and exports
  `defaultNodeTxSubmissionConfig`.
- `Cardano.Network.Diffusion.Configuration.defaultBlockFetchConfiguration` was
  renamed to `mkDefaultBlockFetchConfiguration` and takes the
  `MiniProtocolParameters`: `bfcMaxRequestsInflight` is their
  `blockFetchPipeliningMax`.
- `chainSyncProtocolLimits`, `blockFetchProtocolLimits`,
  `txSubmissionProtocolLimits`, `keepAliveProtocolLimits`,
  `peerSharingProtocolLimits`, `perasCertDiffusionProtocolLimits` and
  `perasVoteDiffusionProtocolLimits` take the negotiated `NodeToNodeVersion`.
  `txSubmissionProtocolLimits` sizes the ingress queue with the window of
  the connection's `TxSubmissionConfig`.
- `cardano-diffusion:cardano-diffusion-tests-lib`:
  - `Test.Cardano.Network.Diffusion.Testnet.Simulation`: `SimArgs` has a new
    `saTxOutboundVersion` field, `NodeArgs` has a new `naTxSubmissionConfig`
    field (nodes can use different configurations with `TxOutboundV_2`), and
    `mainnetSimArgs` takes a `TxOutboundVersion`.
  - `Test.Cardano.Network.Diffusion.Testnet.MiniProtocols`: `AppArgs` has new
    `aaTxSubmissionConfig` and `aaTxOutboundVersion` fields.

### Non-Breaking

- Added `minPerasVersion` to `Cardano.Network.NodeToNode.Version`.
- Added `mkNodeTxDecisionPolicy` to `Cardano.Network.NodeToNode`: it computes
  the node's `TxDecisionPolicy` from its `MiniProtocolParameters` and checks
  that it is sane for the `TxSubmissionConfig` of every `NodeToNodeVersion`.
- `Cardano.Network.NodeToNode` exports `NodeTxSubmissionConfig`,
  `txSubmissionConfigForVersion` and `txSubmissionMaxUnacknowledgedTxIdsV1`,
  and re-exports `TxSubmissionConfig`,
  `TxSubmissionLogicVersion`, `defaultTxSubmissionConfigV1` and
  `defaultTxSubmissionConfigV2`.
- `cardano-diffusion:orphan-instances`: the JSON instances of
  `NodeToNodeVersion` support `NodeToNodeV_17`.

<!--
### Patch

- A bullet item for the Patch category.

-->
