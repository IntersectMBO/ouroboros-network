# Revision history for ouroboros-network-framework

## next release

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

### Non-breaking changes

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
