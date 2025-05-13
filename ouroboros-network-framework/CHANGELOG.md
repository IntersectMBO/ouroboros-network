# Revision history for ouroboros-network-framework

## next release

### Breaking changes

### Non-breaking changes

## 0.18.0.0 -- 2025-05-13

### Breaking changes

* `ConnectionHandlerFn` takes an optional `ReadBuffer` for mux socket bearers
* ConnectionManager's `Args` record exposes a `withBuffer` combinator
  for working with buffered socket bearers introduced in the network-mux package

### Non-breaking changes

## 0.17.0.0 -- 2025-02-25

### Breaking changes

*  Add `miniProtocolStart` to `MiniProtocol` to control starting strategy.

## 0.16.0.0 -- 2025-02-03

### Breaking changes

* `simpleSingletonVersions`: takes a callback which receives negotiated version data.

## 0.15.0.0 -- 2025-01-02

### Breaking changes

* `Ouroboros.Network.Subscription` removed.
* `Ouroboros.Network.ErrorPolicy` removed.
* APIs removed from `Ouroboros.Network.Socket`:
  * `NetworkMutableState` & friends,
  * `withServerNode` and `withServerNode'`,
  * `NetworkServerTracers`,
  * `fromSnocket`,
  * `beginConnection`
* `Ouroboros.Network.Server.Socket` replaced with a simpler server
  implementation in `Test.Ouroboros.Network.Server` (in `ouroboros-network:testlib` component).
* Use `IOError` in `BearerInfo`.
* Addapted to `network-mux` changes in https://github.com/IntersectMBO/ouroboros-network/pull/4999
* Addapted to `network-mux` changes in https://github.com/IntersectMBO/ouroboros-network/pull/4997
* Removed deprecated `Ouroboros.Network.Channel.{to,from}Channel` functions.
* Renamed `requestOutboundConnection` to `acquireOutboundConnection` and
  `unregister{Inbound,Outbound}Connection` to `release{Inbound,Outbound}Connection`.
  `AssertionLocation` constructors were renamed as well.
* Added `RawBearer` API (see https://github.com/IntersectMBO/ouroboros-network/pull/4395)
* Connection manager is using `ConnectionId`s to identify connections, this
  affects its API.
* Added `connStateSupply` record field to
  `Ouroboros.Network.ConnectionManager.Core.Arguments`.
* Renamed modules in `ouroboros-network:testlib`:
  `Ouroboros.Network.Test.Orphans -> Test.Ouroboros.Network.Orphans`
  `Ouroboros.Network.ConnectionManager.Test.Experiments -> Test.Ouroboros.Network.ConnectionManager.Experiments`
  `Ouroboros.Network.ConnectionManager.Test.Timeouts -> Test.Ouroboros.Network.ConnectionManager.Timeouts`
  `Ouroboros.Network.ConnectionManager.Test.Utils -> Test.Ouroboros.Network.ConnectionManager.Utils`
  `Ouroboros.Network.InboundGovernor.Test.Utils -> Test.Ouroboros.Network.InboundGovernor.Utils`


### Non-breaking changes

* Dropped all node-to-client versions < `NodeToClientV_16`.

## 0.14.0.0 -- 2024-10-17

### Breaking changes

* Added `createConnectedBufferedChannelsUnbounded`.
* Use `typed-protocols-0.3.0.0`.
* Removed `Ouroboros.Network.Mux.toApplication`
* Renamed `Ouroboros.Network.Mux.mkMiniProtocolBundle` as `mkMiniProtocolInfos`
  (its type has changed).
* Added `Ouroboros.Network.Mux.toMiniProtocolInfos`.
* Added `ConnectToArgs` for `Ouroboros.Network.Socket.connectToNode` & friends.
* `Ouroboros.Network.Socket.connectToNode` & friends return result (or an
  error) of the first terminated mini-protocol.
* Added `Ouroboros.Network.Socket.connectToNodeWithMux` and
  `connectToNodeWithMux'`.  They give control over running mux, e.g. one can
  start some of the mini-protocols, or implement a re-start policy.
* `Ouroboros.Network.InboundGovernor` module is supposed to be imported
  qualified. Type names and record fields were modified.
* Added `Ouroboros.Network.InboundGovernor.Arguments` record type.
* `Ouroboros.Network.Server2` module is supposed to be imported qualified.
  Type names and record fields were modified.
* `Ouroboros.Network.ConnectionManager.InformationChannel.OutboundGovernorInfoChannel` was removed.
* `Ouroboros.Network.ConnectionManager.ConnectionManagerArguments`:
  `connectionDataFlow` doesn't take `versionNumber` as an argument.
* `Ouroboros.Network.ConnectionManager.Core` must be imported qualified.
* `ConnectionManagerTrace` moved from `Ouroboros.Network.ConnectionManager.Types`
  to the `Core` module & renamed as `Trace`.
* RawBearer API (typeclass and instances) added.

### Non-breaking changes

* Added tracing on CM connVars for testing purposes.

## 0.13.2.5 -- 2024-10-11

### Breaking changes

### Non-breaking changes

* bump for version bounds

## 0.13.2.4 -- 2024-08-27

### Non-breaking changes

* bump for bad ref in chap for 0.13.2.3

## 0.13.2.3 -- 2024-08-22

### Non-breaking changes

* version bump for build depends

## 0.13.2.2 -- 2024-08-07

### Non-breaking changes

* Make it build with ghc-9.10
* Improve memory footprint of tests by using strict version of STM

## 0.13.2.1 -- 2024-06-26

### Non-breaking changes

- Fix `InboundGovernorCounters`

## 0.13.2.0 -- 2024-06-07

### Non-breaking changes

- Fixed `InboundGovernorCounters` tracing frequency
- Bump io-sim and io-classes
- Fixed connection manager transition test

## 0.13.1.0 -- 2024-05-08

### Non-breaking changes

* New tests
* Using `quickcheck-monoids`

## 0.13.0.0 -- 2024-05-08

### Breaking changes

* connection-manager: maintain it's own source of randomness for `PrunePolicy`.
  The types `PrunPolicy`, `ConnectionManagerArguments` changed.
* server accepts a callback which receives an `STM` action allowing to observe
  public part of `InboundGovernorState`.  The refactorisation changed how
  exceptions are propagated through from the threads run by the server to the
  main thread.  `InboundGovernorObservableState` was replaced with
  `PublicInboundGovernorState`.
* removed the outbound information channel between the connection manager
  & outbound governor; the outbound governor now can use the
  `PublichInboundGovernorState`.
* Added `serverDebugInboundGovernor` tracer was added to `ServerArguments`.

## 0.12.0.0 -- 2024-03-15

### Breaking changes

* Let light peer sharing depend on the configured peer sharing flag

### Non-breaking changes

* Added `Generic` and `NFData` instance derivations for `NodeToNodeVersion`
  data type
* Added `NFData` for `Handshake` protocol related types

## 0.11.1.0 -- 2024-02-21

### Breaking changes

### Non-breaking changes

* Fixed correct transition tracing in the case of self connect

## 0.11.0.0 -- 2024-01-22

### Breaking changes

* Moved `configureOutboundSocket` to `ouroboros-network` (it's not exported
  anymore).

### Non-breaking changes

* ghc-9.8 support.
* Add Socket.NoDelay option to `configureOutboundSocket`

## 0.10.2.0 -- 2023-12-14

### Non-breaking changes

* Use `io-sim-1.3.1.0`.

## 0.10.1.0 -- 2023-11-16

### Non-breaking changes

* Updated code to accommodate changes on `PeerSharing` data type.
* Fixed Server2 [sim test](https://github.com/intersectmbo/ouroboros-network/issues/4607) by synchronizing connection/disconnection events.
* Changed connection manager `readState` function to be in `STM` instead of `m`
* Added `waitForOutboundDemotion` function to Connection
  Manager's API
* Use `io-sim-1.3.0.0`.

## 0.10.0.1 -- 2023-11-02

### Non-breaking changes

* Updated code to accommodate changes on `PeerSharing` data type.
* Updated version bounds.

## 0.10.0.0 -- 2023-10-26

### Breaking changes

* `ResourceException` is now an existential type.

### Non-breaking changes

* An inbound peer is considered hot if any hot protocol is running.
* Split `test` component into `io-tests` and `sim-tests`.
* `demo-ping-pong`: improved tracer.
* Fixed a bug in `connection-manager` which could result in leaking
  a connection.

## 0.9.0.0 -- 2023-08-21

### Breaking changes

* Pass `Maybe InformationChannel` to connection manger. This gives us a way to
  disable light peer sharing.

### Non-breaking changes

## 0.8.0.0 -- 2023-08-09

### Breaking changes

* `MuxProtocolBundle` type alias was removed, since it was just reduced to
  a list of 'MiniProtocol's.

* Added `ExpandedInitiatorContext`, `MinimalInitiatorContext` and
  `ResponderInitiatorContext` types in a new module:
  `Ouroboros.Network.Context`.  The module also re-exports `ConnectionId`,
  `IsBigLedgerPeer` and `ControlMessageSTM` thus an unqualified import might
  cause some warnings.

* `RunMiniProtocol` now contains callbacks which receive a context.  The type
  is polymorphic over initiator and responder contexts.  We also provide type
  aliases for `RunMiniProtocolWithExpandedCtx` and
  `RunMiniProtocolWithMinimalCtx` which instatiate initiator and responider
  contexts.

* Added `OuroborosBundleWithExpandedCtx` and `OuroborosBundleWithMinimalCtx`
  type aliases.

* Added `OuroborosApplicationWithMinimalCtx` type aliases.

* Added `contramMapInitiatorCtx` which contramaps the initiator context of an
  `OuroborosApplication`.

* Added `fromOuroborosBundle` which creates `OuroborosApplication` from
  `OuroborosBundle` by forgetting the hot / warm / established distinction
  between all mini-protocols.

* Removed `MuxBundle` and `mkMuxApplicationBundle` which is no longer needed.

* Due to the above changes the following APIs changed their type signatures:

  - `Ouroboros.Network.Socket.connectToNode`
  - `Ouroboros.Network.Socket.connectToNode'`
  - `Ouroboros.Network.Socket.connectToNodeSocket`
  - `Ouroboros.Network.Socket.SomeResponderApplication`
  - `Ouroboros.Network.Socket.withServerNode`
  - inbound governor API

* `MuxPeer` changed it's kind and it was renamed to `MiniProtocolCb`, the old
  type is still provided but deprecated.  The `MuxPeerRaw` constructor was
  renamed to `MiniProtocolCb` (the old one is still provided but deprecated).
  `MuxPeer` and `MuxPeerPipelined` constructors also changed its type and are
  now deprecated.  Use `mkMiniProtocolCbFromPeer` and
  `mkMiniProtocolCbFromPeerPipelined` instead.

  Also note that even the deprecated constructors have changed their types.

* `runMuxPeer` change its type but also is now deprecated in favour of `runMiniProtocolCb`.  The latter
  receives two arguments: the context and `Network.Mux.Channel.Channel` rather
  than `Ouroboros.Network.Channel.Channel` (no need to use
  `Ouroboros.Network.Channel.fromChannel`).  `runMuxPeer` accepts the context (added argument) and
  `Ouroboros.Network.Channel.Channel`.

### Non-breaking changes

* Added `Functor` instance for `ConnectionId`.

## 0.7.0.0 -- 2023-07-17

### Breaking changes

* light peer sharing:
  * Added `cmGetPeerSharing` field to `ConnectionManagerArguments`.
  * Added `getProtocolPeerSharing` field to `DataFlowProtocolData` record.
  * Renamed `serverControlChannel` as `serverInboundInfoChannel` of the `ServerArguments` record.
  * Moved `OurboundGovernorInfoChannel` to `ouroboros-network`.

### Non-breaking changes

* Fixed query shutdown timeout in the legacy (non-p2p) mode (20s).

## 0.6.0.1 -- 2023-05-15

* Updated to use `ouroboros-network-api-0.5.0.0`.

## 0.6.0.0 -- 2023-05-08

### Breaking changes

* Handshake support for querying:
  * Use `ouroboros-network-api-0.4.0.0`
  * Added `haQueryVersion` to `HandshakeArguments`
  * `handshakeServerPeer` recieves extra argument `vData -> Bool`
  * Added `MsgQueryReply` to `Handshake` mini-protocol.
  * Added `Ouroboros.Network.Protocol.Handshake.Client.handshakeCleintPeerTestVersions`
  * Added `HandshakeResult` and `HandshakeException` types.

## 0.5.0.0 -- 2023-04-28

### Breaking changes

* Use `io-classes-1.1`.

### Non-breaking changes

* `ghc-9.4` and `ghc-9.6` compatibility.

## 0.4.0.0 -- 2023-04-19

### Non-breaking changes

- Fix interop problems between NonP2P and P2P nodes (PR #4465)
- Fix incorrect transition order (issue #4370)

### Breaking

- Removed `TrImpossibleConnection` trace (PR #4385)
- Peer Sharing integration

## 0.3.0.0 -- 2023-01-25

* Removed `toBearer` method of `Snocket`, instead the `Ouroboros.Network.Snocket` module exposes `makeSocketBearer`, `makeLocalBearer` and re-exports `MakeBearer` newtype wrapper.
* Update dependencies after repository restructure.
* Added `ipv6` cabal flag.
* Support `ghc-9.2`

## 0.2.0.0 -- YYYY-MM-DD

* Export `WithAddr` from `Simulation.Network.Snocket`
* Use `io-sim-0.3.0.0`
* `ExceptionInHandler` is an existential type which makes it easier to catch.
* Connection handler rethrows exceptions wrapped in `ExceptionInHandler`.
* We don't configure sockets in `bind` method anymore, many functions accept an argument to configure a socket, e.g. `ConnectionManagerArguments`.  Added `configureSocket`, `configureSystemdSocket` and `configureOutboundSocket` functions in `Ouroboros.Network.Socket` module.  Also added `SystemdSocketTracer`
* Removed `StructLinger` (it's available from the `network-3.1.2.2` package)
* Renamed `TrError` as `TrConnectionHandlerError` which is a constructor of `ConnectionHandlerTrace` type.
* Changed `Show` instance of `TestAddress`
* Removed `TrUnknownConnection` trace (connection-manager).
* Changed type of `serverInboundIdleTimeout` field of `ServerArguments` from `DiffTime` to `Maybe DiffTime`.
* Renamed `Ouroboros.Network.Mux.TokProtocolTemperature` as `Ouroboros.Network.Mux.SingProtocolTemperature`.
* Renamed `Ouroboros.Network.Mux.Bundle` as `Ouroboros.Network.Mux.TemperatureBundle`.
* Connection manager's `ControlChannel` type changed (internal).

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
