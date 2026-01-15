<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `TxSubmissionMempoolWriter` added tx validation error type parameter,
  `mempoolAddTxs` returns a list of accepted `txids` and a list of rejected
  `txids` with errors.
- `Ouroboros.Network.TxSubmission.Mempool.Simple.getMempoolWriter` changes:
  - supplies the current time to the validation function,
  - removed `ctx` paramenter,
  - validation happens in an `STM` transaction, which allows acquiring and
    updating validation `ctx`.

### Non-Breaking

- Improved `Message TxSubmission` ToJSON instance
- Improved `Message KeepAlived` ToJSON instance
- `Ouroboros.Network.TxSubmission.Mempool.Simple` re-export `TxSubmissionMempool{Reader,Writer}` data types.
- `Ouroboros.Network.TxSubmission.Mempool.Simple` is assiging stable `idx`s to `tx`s.
