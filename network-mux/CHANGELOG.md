# network-mux changelog

<!-- scriv-insert-here -->

<a id='changelog-1.1.0.0'></a>
## 1.1.0.0 -- 2026-11-24

### Breaking

- `Control.Concurrent.JobPool.readGroupSize` is removed; the mux no longer
  uses it.
- `Network.Mux.new` takes the node-wide `Counters` the mux counts in, or
  `Nothing`.
- `Network.Mux.Egress.muxer` takes the mux's byte counters, a slot per
  mini-protocol and direction. A mini-protocol's `IngressQueue` holds an
  `IngressQueueState`: what is queued, and the bytes read for the protocol.

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

- Added `Network.Mux.Counters`: node-wide `Counters` in which every mux
  made with them counts how it failed, once, as it dies:
  SDU write timeouts and write errors on the write side; SDU read timeouts,
  ingress overruns, protocol errors and read errors on the read side; the
  peer closing, read as its end while a mini-protocol ran, or met as a
  reset or a broken pipe by a read or a write; the muxer or the demuxer
  dying of an exception of any other kind; and mini-protocols that died
  with an exception, by protocol and direction. `withCounters` makes a
  remote set and, if asked, a local one, and traces each every interval it
  is given, a second at least, as a `CountersTrace`, for as long as its
  body runs; an interval of zero, or less, makes none.
- A close the demuxer reads ends `run` as `Stopped` when no mini-protocol's
  status is running and as `Failed` otherwise; the job count no longer
  decides. A mini-protocol started eagerly whose job is not yet forked now
  counts as running, and one past its last transaction but not yet
  deregistered as finished; one whose thread was killed from outside by an
  asynchronous exception, which the job pool does not report, sets its
  status to the new `StatusKilled` on the way out, so it counts as finished
  too, and `runMiniProtocol` refuses to start it again on that mux, as it
  refuses one running.
- The counters also carry the bytes written and read, SDU headers included,
  by mini-protocol number and our side's direction (`ByteCounts`). A mux's
  counts move into a retired total when it ends, so totals never go down.
  The demuxer counts in the write that queues the SDU, so a node that keeps
  no counters pays nothing for it. An SDU that overruns its queue is not
  counted, the mux ending on it; nor is one for an unknown protocol, or for
  a direction the mux does not run. The bearer's read and write run
  unmasked and the count after each masked, so a mux cancelled in the
  bearer's tail, after the kernel's hand-off, may leave its last SDU or
  batch uncounted.

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

<a id='changelog-1.0.0.0'></a>
## 1.0.0.0 -- 2026-10-21

### Breaking

- `TraceChannelSendStart` no longer carries the message's length;
  `TraceChannelSendEnd` carries it, as the receive events do, counted as the
  event is rendered rather than before the message is queued.

- version bump

### Non-Breaking

- Added `egressSoftBufferLimit`, the soft limit of a mini-protocol's egress
  buffer, 0x3ffff bytes, to `Network.Mux.Egress`.

### Patch

- A mini-protocol's `send` encodes a message up to the channel's soft buffer
  limit, 0x3ffff bytes, to a chunk boundary, in the mini-protocol's thread, so
  a message waiting in its egress buffer does not keep alive whatever its
  encoder refers to; a longer tail is still encoded by the muxer as it is
  sent.

<a id='changelog-0.12.0.0'></a>
## 0.12.0.0 -- 2026-10-01

### Breaking

- Create two kinds of JobPools, one with a completion queue and one without
- Add asserts cabal flag

### Non-Breaking

- More efficient codec for mux header

- Added `runMiniProtocolSTM`, an `STM` variant of `runMiniProtocol`, which
  allows starting a mini-protocol atomically with other `STM` actions.
- `runMiniProtocol` no longer requires `MonadThrow m`.

<a id='changelog-0.11.0.0'></a>
## 0.11.0.0 -- 2026-07-28

### Breaking

- Evaluate mini-protocol errors to WHNF, before writing them to completion
  variable.

- Upgraded to `contra-tracer ^>=0.2.1`. The `Tracer` data constructor is no
  longer exported; use `mkTracer` instead. `nullTracers`, the `TracersI`
  pattern synonym, `contramapTracers'`, `tracersWithBearer` and
  `traceBearerState` now require `Monad m` rather than `Applicative m`.
- Capped `QuickCheck < 2.18`.

### Non-Breaking

- Added `MonadReadBuffer`, `withReadBufferIO` is now exposed as an `IO` instance of `MoandReadBuffer`.
  This allows us to use `ReadBuffer` in a polymorphic way (parametrised by a monad).

- Removed synchronous exception from mux job handler

- Split `network-mux/demo/mux-leios-demo.sh` into two parts:
  - `network-mux/demo/ns-setup.sh` - network setup
  - `network-mux/demo/mux-leios-demo.sh -run the mux leios demo using `ns-setup.sh`

- When mux is in `Failed` state, throw the failure exception when starting `runMiniProtocol`

- Removed the `QuickCheck < 2.18` upper bound, allowing QuickCheck 2.18+.
- Added `cardano-base:testlib >=0.1.5.0` dependency.

<a id='changelog-0.10.1.0'></a>
## 0.10.1.0 -- 2026-03-06

### Non-Breaking

- Added `network-mux:mux-leios-demo`
- Added binary encoding to `Test.Mux.ReqResp`:
  * `runClient` renamed as `runClientCBOR`
  * `runServer` renamed as `runServerCBOR`
  added API:
  * `runClientBin`
  * `runServerBin`
  * `runClientBurstBin`
  * `runServerBurstBin`
  * `runClientBurstCBOR`
  * `runServerBurstCBOR`

- Added `network-mux/wireshark-plugin/` - a WireShark dissector plugin for mux SDUs.

- Update dependencies.

- Support `Win32-network ^>=0.1`.

- Add support for ghc-9.14.

<a id='changelog-0.10.0.0'></a>
## 0.10.0.0 -- 2026-01-20

### Breaking

- Tracers are now passed to `Network.Mux.new` rather than `Network.Mux.run`.
- Added new trace points to `Network.Mux.Trace.Trace`:
  - `NewMux` which logs all `MiniProtocolInfo`s used to create the new `Mux` interface.
  - `MuxStarting` which logs that `Mux` is starting.

### Non-Breaking

- Patched so that it compiles to wasm

- Update dependencies.

- Fixed handling of trailing bytes in presence of received data.
<!-- scriv-end-here -->

## 0.9.1.0 -- 2025-09-10

### Non-breaking changes

* Exposed `MakeBearerCb` to expose its haddocks.

## 0.9.0.0 -- 2025-06-28

### Breaking changes

* run, miniProtocolJob, monitor now accept Tracers record
  instead of `Tracer m Trace` type.
* Removed handshake trace events from mux trace.
* `Trace` was split into three traces: `Trace` of mux events, `ChannelTrace`
   & `BearerTrace`.  As a result `run` & `Bearer` API were modified.

## 0.8.0.2 -- 2025-07-17

### Breaking changes

* `IngressQueue` is using strict `Pair` type

### Non-breaking changes

## 0.8.0.1 -- 2025-06-02

### Breaking changes

### Non-breaking changes

* Properly unmask async exceptions in the job pool

## 0.8.0.0 -- 2025-05-13

### Breaking changes

* `MakeBearer` accepts optional `ReadBuffer`
* added fields `egressInterval`, `writeMany`, `batchSize` to `Bearer`
  * writeMany provides vector IO, egressInterval supports polling of egress queue
    for tuning latency vs. network efficiency
* `socketAsBearer` additionally takes `ReadBuffer`, egress
  interval `DiffTime` for egress polling, and batchSize
* changed `IngressQueue` type synonym
* Added `TraceRecvRaw` tag to `Trace`

### Non-breaking changes

* added `makeSocketBearer'`, `ReadBuffer`, `withReadBufferIO`
* Define msHeaderLength instead of using '8'
* Benchmark for Socket Bearer
* Use ByteString.Builder for the ingress queues
* Signal the kernel that we require at least the full SDU's worth of data

## 0.7.0.0 -- 2025-02-25

### Breaking changes

* Use the correct tracing message for `StartOnDemand`.
* Implement `StartOnDemandAny`, which will start the miniprotocol as soon as
  any `StartOnDemand` protocol starts.

## 0.6.0.0 -- 2025-01-02

### Breaking changes

* Removed `Mux` prefix from many data types & functions.  Mux library should be
  imported qualified.

### Non-breaking changes

## 0.5.0.0 -- 2024-10-17

### Breaking changes

* Removed `Network.Mux.Compat` module with legacy API.
* `Ouroboros.Network.Mux.toApplication` was removed.
* `Ouroboros.Network.Mux.mkMiniProtocolBundle` was renamed to
  `mkMiniProtocolInfos`, its type changed.
* Removed `MiniProtocolBundle` newtype wrapper.
* Generalised `Channel` type and provide `ByteChannel` type alias.
* Provide additional APIs in the `Network.Mux.Channel` for creating channels
  and byte channels.
* `MuxBearer` has a `name` field.

### Non-breaking changes

* Fix compilation with `tracetcpinfo` flag.

## 0.4.5.3 -- 2024-08-07

### Breaking changes

### Non-breaking changes

* Make it build with ghc-9.10

## 0.4.5.2 -- 2024-06-07

### Breaking changes

### Non-breaking changes

- Bump io-sim and io-classes

## 0.4.5.1 -- 2024-01-22

### Non-breaking changes

* Use io-sim-1.4.1.0
- Bump `Win32-network` version

## 0.4.5.0 -- 2024-01-22

### Non-breaking changes

* ghc-9.8 support.

## 0.4.3.0 -- 2023-11-16

### Non-breaking changes

* Make sure jobs are removed from the `JobPool`.
* Use `io-sim-1.3.1.0`.

## 0.4.3.0 -- 2023-11-16

### Non-breaking changes

* Use `io-sim-1.3.0.0`.

## 0.4.2.0

### Breaking

### Non-breaking

* `asserts` cabal flag was removed, one can use `ghc-options` directly
* Restructured `step` to prevent an impossible case and eliminate the associated
  `error`.
* Restructured `setupDispatchTable` to prevent an impossible case and eliminate
  the associated error.

## 0.4.1.0

* Use `io-classes-1.2`

## 0.4.0.0 -- 2023-04-28

### Breaking

* Use `io-classes-1.1`.
* Renamed `MuxTraceShutdown` as `MuxTraceStopping`.
* Fixed a typo now the mux stopping exception carries message: `Mux stopping`.


## 0.3.0.0 -- 2023-01-25

* Provide a `MakeBearer` newtype: a function to constructs a `MuxBearer`.
* Fix NodeToNodeV10 support
* Fix invalid Haddock markup
* Support `ghc-9.2`

## 0.2.0.0 -- 2022-11-11

* Bump versions of packages
* Platform independent TCP info trace
