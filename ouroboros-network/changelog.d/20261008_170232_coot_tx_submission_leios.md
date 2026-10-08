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
  total advertised size of the txs requested in a single `MsgRequestTxs`.
  `defaultTxDecisionPolicy` sets it about 2% below the size limit of
  `MsgReplyTxs`.
- `defaultTxDecisionPolicy` takes a `TxSubmissionConfig` and computes
  `txsSizeInflightPerPeer` from its `maxNumTxIdsToRequest` (the value is
  unchanged for `defaultTxSubmissionConfigV2`).  `saneTxDecisionPolicy` also
  checks a `TxSubmissionConfig`.
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
  `maxNumTxIdsToRequest` and the `NumTxsToReq` argument.
  `defaultTxSubmissionConfigV1` (10 unacknowledged txids, 3 txids per request)
  and `defaultMaxNumTxsToRequest` (2 txs per request) keep the previous values.
- `ouroboros-network:ouroboros-network-tests-lib`: `Impairment` in
  `Test.Ouroboros.Network.TxSubmission.Impaired` has a new
  `impairSplitAcknowledgements` field.

### Non-Breaking

- Added `TxOutboundVersion` (`TxOutboundV_1`, `TxOutboundV_2`) to
  `Ouroboros.Network.TxSubmission.Outbound`.
- Added `defaultTxSubmissionConfigV1` and `defaultMaxNumTxsToRequest` to
  `Ouroboros.Network.TxSubmission.Inbound.V1`, and
  `defaultTxSubmissionConfigV2` to
  `Ouroboros.Network.TxSubmission.Inbound.V2.Policy` (re-exported by
  `Ouroboros.Network.TxSubmission.Inbound.V2`).
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
    `splitAcknowledgements` and `impairmentSplitAcknowledgements`.

<!--
### Patch

- A bullet item for the Patch category.

-->
