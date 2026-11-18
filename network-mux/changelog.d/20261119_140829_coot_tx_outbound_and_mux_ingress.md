<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `TraceMiniProtocolKilled` is traced when a mini-protocol is killed by an async exception.
- `Error` has a new constructor `MiniProtocolAsyncException`, which is thrown
   by mux thread when an asynchronous exception interrupts mini-protocol job.
- `Control.Concurrent.JobPool.cancelGroup` returns the results of all
  completed jobs which were not yet waited for.
- `MiniProtocolInfo` has a new field, `miniProtocolStartMode`: how the
  mini-protocol is started (`StartEagerly`, `StartOnDemand` or
  `StartOnDemandAny`).  It's a static property of a mini-protocol, so
  `runMiniProtocol` and `runMiniProtocolSTM` no longer take a
  `StartOnDemandOrEagerly` argument.
- Added `UnexpectedIngress` to `Network.Mux.Trace.Error`.

### Non-Breaking

- The demuxer throws `UnexpectedIngress` when it receives data for
  a mini-protocol started eagerly (`StartEagerly`) which is not running.
  Such a mini-protocol sends the opening message, so it cannot
  legitimately receive data while it is not running.  Before, the data was
  queued, up to the mini-protocol's `maximumIngressQueue`, e.g. on
  a duplex connection for an initiator which was never started.
- A mini-protocol started eagerly which terminates leaving trailing bytes
  fails with `UnexpectedIngress`; trailing bytes are kept for the next run
  only for mini-protocols started on demand.
- `UnexpectedIngress` is counted as a protocol error (`mfProtocolErrors`).

<!--
### Patch

- A bullet item for the Patch category.

-->
