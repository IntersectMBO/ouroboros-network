<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Added support for burst and weighted fair queuing:
  - Added `wLastSent` and `wBucket` to Wanton
  - Introduced `ProtocolBurst` type, holding token bucket size and refill rate
  - Added `ProtocolBurst` to `TLSRDemand`
  - Added `burst` to `MiniProtocolLimits`. Value of Nothing denotes no burst
    ability for the protocol, a Just holds a `ProtocolBurst` value.
  - Added `miniProtocolWeight` to `MiniProtocolInfo`, denoting the queue weight
    the protocol shares with other protocols of the same weight.
  - Enhances 'starvation' test to properly handle weighted fair queueing

-->
<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
