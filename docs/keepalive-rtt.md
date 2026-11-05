# Cached KeepAlive RTT observations

## Observation

`KeepAliveRtt` contains the raw elapsed time of the latest successful KeepAlive
exchange and its monotonic completion time. The client observes completion on
entering the successful response continuation, after the protocol has checked
the response cookie. This includes local driver/channel overhead; it does not
claim a timestamp at the physical socket boundary.

`keepAliveClientWithRtt` publishes both fields together, in the same STM
transaction as the existing GSV update and before optional tracing. Consumers
read functional state, including when tracing is disabled or slow. GSV smoothing
and its send-based `sampleTime` retain their previous meaning. The new raw
observation neither changes the GSV estimator nor interprets it as a percentile.

Network publishes measurements. Consumers choose freshness thresholds,
fallbacks, headroom, caps and any subsequent smoothing.

## Connection lifetime

`bracketKeepAliveClientWithRtt` allocates a new observation cell alongside the
existing GSV registration. `lookupKeepAliveRtt` returns an opaque read-only
`KeepAliveRttReader` for that cell. A missing registration returns `Nothing`;
`readKeepAliveRtt` returns `Nothing` until the first successful exchange. Neither
operation waits for a new measurement.

Capture the reader in the consumer's connection context, then retain it across
requests and fetch-session restarts on that live registration. Do not look up
the peer key afresh for every RTT read: that would lose the lifetime binding.

When KeepAlive exits, cleanup permanently retires the cell and removes its
registry entry before waiting for any BlockFetch cleanup. Existing readers then
return `Nothing`. A replacement registration at the same key gets a fresh cell.
An old producer context cannot revive a retired cell or write into its
replacement. The underlying GSV and BlockFetch shutdown ordering remains intact.

Scope follows the KeepAlive registration associated with the connection. A fetch
restart alone does not remove its sample. Restarting KeepAlive itself creates
a fresh registration and conservatively returns to absence, even if the
transport remains live.

## Compatibility and integration

The existing `keepAliveClient` and `bracketKeepAliveClient` signatures remain
available. The old client keeps producing GSV estimates without publishing raw
observations. Applications that need raw measurements pair
`bracketKeepAliveClientWithRtt` with `keepAliveClientWithRtt`, passing the
registration's opaque `KeepAliveClientContext` to the client.

The producer context contains the existing GSV map and a private observation
cell. Its `keepAliveGsvContext` accessor supports the shared implementation;
`publishKeepAliveRtt` publishes a successful observation and ignores retirement.
The registry's new `rttRegistry` contains read-only handles and is independent of
the hot BlockFetch-client set. Warm peers can therefore acquire RTT measurements.

KeepAlive tests cover publication timing, absence, raw versus smoothed data,
reader reacquisition, reconnect, late producers, cancellation, unanswered probes
and legacy behavior. The existing BlockFetch lifecycle check includes the new
registry in its final leak assertion. Consumers own their own freshness policy
and deadline calculations.
