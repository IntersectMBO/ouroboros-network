<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Non-Breaking

- `txSubmissionOutbound`: each entry of the unacknowledged txids can be
  requested only once.  A request for a `txid` is served from its first
  entry which was not requested yet, so a `txid` announced again (its tx left
  the mempool and was added again) can be requested again.  Requesting
  a `txid` all of whose entries were already requested is a protocol error
  (`ProtocolErrorDuplicateTxIds`).
- `mkMiniProtocolInfos` and `toMiniProtocolInfos` set the start mode of each
  `MiniProtocolInfo`: initiators are started eagerly, responders according to
  `miniProtocolStart`.
- `Ouroboros.Network.Server.Simple` and `connectToNode` start responders
  according to `miniProtocolStart` (on demand for `NodeToNode` and
  `NodeToClient` protocols), rather than eagerly.
- The `ToJSON` instance of `MiniProtocolInfo` includes
  `miniProtocolStartMode`.

<!--
### Patch

- A bullet item for the Patch category.

-->
