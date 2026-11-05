<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Added `TxSubmissionConfig` to
  `Ouroboros.Network.TxSubmission.Inbound.V2.Policy`: tx-submission options
  shared by the inbound and outbound sides (`maxNumUnacknowledgedTxIds` and
  `maxNumTxIdsToRequest`).  `TxDecisionPolicy` no longer has the
  `maxNumTxIdsToRequest` and `maxUnacknowledgedTxIds` fields; use
  `maxNumTxIdsToRequest` and `maxNumUnacknowledgedTxIds` (a `NumTxIdsToAck`)
  of `TxSubmissionConfig` instead.
- Added the `txsSizePerBatch` field to `TxDecisionPolicy`: a hard limit on the
  total advertised size of the txs requested in a single `MsgRequestTxs`,
  except for the first tx, which is requested even if it is larger.
  `mkTxDecisionPolicy` sets it about 2% below the size limit of
  `MsgReplyTxs`; `saneTxDecisionPolicy` checks that it is positive and that
  the reply to the largest batch (including the
  `const_MAX_TX_SIZE_DISCREPANCY` tolerance of each tx) fits within the
  `MsgReplyTxs` size limit.
- `defaultTxDecisionPolicy` is replaced by `mkTxDecisionPolicy`, which takes
  a `TxSubmissionConfig`, computes `txsSizeInflightPerPeer` from its
  `maxNumTxIdsToRequest` (the value is unchanged for
  `defaultTxSubmissionConfigV2`), and returns
  `Either TxConfigurationError TxDecisionPolicy`: the node should report
  a `TxConfigurationError` and halt.
- `saneTxDecisionPolicy` also checks a `TxSubmissionConfig`, and returns
  `Either TxConfigurationError ()` instead of a `Bool`.  Added
  `TxConfigurationError` to
  `Ouroboros.Network.TxSubmission.Inbound.V2.Policy` (re-exported by
  `Ouroboros.Network.TxSubmission.Inbound.V2`); it names the first violated
  bound and carries the offending values.
- `Ouroboros.Network.TxSubmission.Inbound.V2.Registry.withPeer`, and
  `nextPeerAction` and `nextPeerActionPipelined` in
  `Ouroboros.Network.TxSubmission.Inbound.V2.State`, take a
  `TxSubmissionConfig`.
- `txSubmissionOutbound` takes a `TxSubmissionConfig` instead of the maximum
  number of unacknowledged txids, and a `TxOutboundVersion` instead of a
  `version` argument.  With `TxOutboundV_1` a request that would exceed
  `maxNumUnacknowledgedTxIds` unacknowledged txids is a protocol error
  (`ProtocolErrorRequestedTooManyTxids`), as before.  With `TxOutboundV_2` it
  replies with only as many txids as fit, which may be none for a non-blocking
  request.
- `Ouroboros.Network.TxSubmission.Inbound.V1.txSubmissionInbound` takes a
  `TxSubmissionConfig` and a `NumTxsToReq` instead of the maximum number of
  unacknowledged txids, and no longer takes a `version` argument.  The numbers
  of txids and txs it requests at once are no longer hard-coded: they are
  `maxNumTxIdsToRequest` and the `NumTxsToReq` argument.  The latter is
  clamped to the range checked by the new `saneMaxNumTxsToRequest`: at least
  1, and at most `maxSaneNumTxsToRequest` (38), so that an honest reply fits
  within the `MsgReplyTxs` size limit (see also `clampMaxNumTxsToRequest`).
  `defaultTxSubmissionConfigV1` (10 unacknowledged txids, 3 txids per request)
  and `defaultMaxNumTxsToRequest` (2 txs per request) keep the previous values.
- Added `ProtocolErrorTxSizeTooLarge` to `TxSubmissionProtocolError` in
  `Ouroboros.Network.TxSubmission.Inbound.V2.Types`: the V2 inbound
  disconnects from a peer which advertises a tx larger than `max_TX_SIZE`,
  on every node-to-node version, including those before `NodeToNodeV_16`.
- `ouroboros-network:ouroboros-network-tests-lib`: `Impairment` in
  `Test.Ouroboros.Network.TxSubmission.Impaired` has new
  `impairSplitAcknowledgements` and `impairOversizedTxIds` fields.

### Non-Breaking

- Added `TxOutboundVersion` (`TxOutboundV_1`, `TxOutboundV_2`) to
  `Ouroboros.Network.TxSubmission.Outbound`.
- Added `defaultTxSubmissionConfigV1` and `defaultMaxNumTxsToRequest` to
  `Ouroboros.Network.TxSubmission.Inbound.V1`, and
  `defaultTxSubmissionConfigV2` to
  `Ouroboros.Network.TxSubmission.Inbound.V2.Policy` (re-exported by
  `Ouroboros.Network.TxSubmission.Inbound.V2`).
- Added `max_TXID_SIZE` to `Ouroboros.Network.TxSubmission.Inbound.V2.Policy`.
  `saneTxDecisionPolicy` checks that a `MsgRequestTxs` with
  `maxNumUnacknowledgedTxIds` txids of size `max_TXID_SIZE` fits within the
  `StIdle` size limit (at most 1489 unacknowledged txids).
- `TxSubmissionConfig` is re-exported by
  `Ouroboros.Network.TxSubmission.Inbound.V1`,
  `Ouroboros.Network.TxSubmission.Inbound.V2` and
  `Ouroboros.Network.TxSubmission.Outbound`.
- `ouroboros-network:protocols`: added `NumTxsToReq` to
  `Ouroboros.Network.Protocol.TxSubmission2.Type`.
- `ouroboros-network:ouroboros-network-tests-lib`:
  - `Test.Ouroboros.Network.TxSubmission.Types` exports the
    `ArbTxSubmissionConfig` and `ArbTxOutboundVersion` generators and
    re-exports `TxSubmissionConfig`.
  - `Test.Ouroboros.Network.TxSubmission.Impaired` exports
    `splitAcknowledgements`, `impairmentSplitAcknowledgements` and
    `oversizedTxIds`.

<!--
### Patch

- A bullet item for the Patch category.

-->
