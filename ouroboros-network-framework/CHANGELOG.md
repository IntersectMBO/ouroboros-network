# Revision history for ouroboros-network-framework

## 0.5.0.0 -- 2023-04-28

### Breaking changes

* Use `io-classes-1.1`. 

### Non-breaking changes

* `ghc-9.4` and `ghc-9.6` compatibility.

## 0.4.0.0 -- 2023-04-19

### Non breaking

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
