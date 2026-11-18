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

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
