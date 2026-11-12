# Bounded incremental receive observations

## Adapter and codec

`Ouroboros.Network.Driver.Progress.progressDriverWithLimits` is an opt-in factory
around `annotatedDriverWithLimits`. Existing drivers and runners keep their
behavior. Protocol states and element values are generic; transport input is
lazy bytestrings, matching the annotated CBOR route. There is no Leios import,
new mux runner, channel-observer API or typed-protocols core modification.

The caller supplies a codec builder taking a `ProgressEmitter`, protocol size
and fatal time limits, a physical input-chunk bound, a state-specific receive
scope acquisition action, a send-completion hook and the ordinary channel.
Run the resulting driver with the existing peer interpreter. Its opaque
`ProgressState` carries trailing bytes and their observations. Return
`progressTrailingBytes` at the usual mux continuation boundary.

The emitter accepts the existing `Codec.Utils.WithByteSpan element` wrapper.
That utility now belongs to the common `api` component and is re-exported by
`protocols`, preserving its module name and existing downstream imports. Its
signed CBOR offsets are checked before conversion to unsigned byte counters.
Fresh staged decoders must rebase their local spans to the message origin.

A pipelined receiver installs its immutable request context in `ReceiverEffect`
before `ReceiverAwait`. The factory acquires that context once for each receive.
The codec must genuinely parse complete elements incrementally and call its
bound emitter with their half-open encoded-byte ranges. Staged monadic CBOR
decoding already supports this; wrapping an unchanged whole-vector decoder
cannot manufacture element progress.

The codec still returns the ordinary final typed message and annotation. That
message retains its normal invariants, including any final element vector;
the terminal handler does remaining accounting rather than ingesting the same
elements again. Protocol-specific framing, declared counts, hashes, sizes and
request membership must be checked by the codec and its consumer. The generic
adapter checks resource bounds and reported ranges; it trusts the codec to
report actual parsed boundaries.

## Receive context and FIFO replies

Here the immutable context is the `ReceiveScope`: a record of fixed limits and
callback closures selected for one message. In a request/response client those
closures must capture the original request identity and its immutable validation
descriptors. They may read/update shared acquisition, cancellation and storage
state. Immutable binding does not mean that all state accessed by a callback
is frozen.

The pipelined receiver already knows which request's reply comes next. It
installs that request's scope in `ReceiverEffect` before its `ReceiverAwait`.
When the driver begins the corresponding `recvMessage`, it calls the supplied
scope-acquisition action once and retains the returned record throughout that
decode. The driver does not select a request from a queue or infer request
identity from the protocol state token.

For example, with requests A then B awaiting FIFO replies, A's decoder retains
A's original descriptors, progress handler, terminal handler and abort handler.
Replacing the installed scope with B's or logically reassigning A's work cannot
redirect those callbacks. Only when A's wire reply ends does the receiver move
to the next response and capture B's scope. Cancel acknowledgments/control
replies need their own appropriate scope, including zero-element limits.

The generic interface also permits non-FIFO protocols and server-side receives:
the caller always owns the choice of scope for the message about to be decoded.
The FIFO rule belongs to the client's request/response contract.

## Observation boundaries

The send hook runs after the ordinary channel write returns, with a monotonic
time observed at that boundary. It includes channel backpressure. It receives
the original typed message so the caller can correlate the attempt registered
before sending. A blocked or interrupted write has no successful completion
observation. There is no ordering requirement relative to optional tracing.

`receiveStarted` begins the pinned response before any retained input is
reported. After a new nonempty channel read passes its chunk bound,
`receiveInputStarting` runs before taking its timestamp or updating private
input bookkeeping. `receiveInput` publishes the original read observation before
decoding. A consumer with soft deadlines can atomically protect this bounded
publication interval, including a suspension between timestamp capture and its
publication. Empty chunks and EOF never invoke `receiveInputStarting`.

`receiveAwaitInput` runs when available input has been exhausted, before waiting
for another read. It must release any local publication/validation protection;
a stalled network read remains subject to the original deadline. The next
nonempty read starts another bounded publication step. These callbacks must not
renew deadlines or leave a protection pending while awaiting network input.

`receiveComplete` reports the terminal receipt and whether nonempty input remains
for the next message, before returning to the typed receiver. A FIFO consumer
must publish that retained-input fact before terminal settlement can activate
the successor's timer. The successor's start callback atomically replaces the
buffer protection with its own publication step. If it needs more input, its
await callback releases that protection. The input ledger retains every
original observation; buffered messages are never restamped. Protection belongs
to the original receive/session and must be cleared on abort or close. Known
late input remains late even if later replies are buffered. Validation tokens,
soft deadlines and the atomic coordination of these facts remain consumer-owned.

Each `ProgressEvent` has its zero-based element index, encoded range, value and
the read observation of its final required byte. `MessageReceipt` similarly
contains the original terminal byte count and time. A second message decoded
from an earlier read retains that read's time even if its decoder, tracer or
consumer runs later. These are local channel observations, not physical socket
timestamps or fresh transport reads at each decoding stage.

## Bounds and input ownership

Each scope supplies `ProgressLimits`: maximum consumed message bytes, encoded
element bytes and element count. The message bound is intersected with the
existing protocol-state byte limit. Before invoking a progress consumer, the
adapter checks:

- The range is nonempty and does not overlap an earlier element.
- Its endpoint is within already supplied input and the message limit.
- Its encoded length and index fit the element and count bounds.

These checks apply even when the final physical read also contains the next
reply. Charging the entire read to the current message would reject legitimate
coalescing; relying solely on the old driver's final check would publish an
oversized element too early. Replies without element events still receive a
checked terminal byte bound. Set the element count to zero for such contexts.

Physical reads have an explicit positive chunk bound matching the caller's
ingress capacity. Message/element/chunk bounds and element counts use `Word32`.
Total decoder input is bounded by the message bound plus one physical chunk;
that sum and cumulative/cursor arithmetic use `Word64`, so a sum of two
`Word32` bounds cannot wrap. Counters also respect the existing driver's native
`Word` and lazy bytestring's `Int64` limits. Narrowing to `Word32` occurs after
checking the corresponding bound. Oversized reads fail before input/progress
callbacks. The adapter has no arbitrary-precision counters.

Each retained metadata span contains a positive byte count and one timestamp.
Span prefixes are dropped as checked endpoints advance. The last endpoint time
is retained for a terminal coincident with the final element. The number of
spans is bounded by the represented nonempty input bytes; empty-read floods
cannot grow the ledger or annotation collector and remain interruptible by the
existing fatal timeout. Completed scopes clear their temporary state.

The existing annotated driver retains its decoding, byte-limit, timeout and
tracing loops. A small decoder-outcome decorator trims the annotation's input
at the checked current-message endpoint, excluding the following reply without
re-encoding. Lazy slices can retain an underlying input chunk; the physical
chunk bound caps that excess. Consumers must account for their own retained
decoded values, annotations and storage handoffs.

## Receive lifetime

Scope acquisition runs under `mask` to prevent asynchronous cancellation in the
handoff between `getScope` returning and the adapter installing its abort
handler. This follows the usual bracket pattern: acquire while masked, install
cleanup, then use `restore` for interruptible receive work. Masking postpones
asynchronous exceptions; synchronous failures still propagate, and blocking
interruptible operations can still be canceled.
Private driver bookkeeping is allocated before acquiring the scope, so no
effectful setup lies between the successful scope return and handler installation.

The **scope provider** (the supplied `getScope` action) owns any partial
acquisition before it successfully returns. If it claims a receive token and
then fails before returning the scope, it must undo that claim itself. The
adapter has not received a scope and cannot invoke its abort callback. A
provider that only reads an existing scope has nothing extra to undo. On a
successful return, abort-cleanup ownership transfers to the adapter, which
installs its handler before running scope callbacks or decoding. All callbacks
are synchronous, masked and bounded. They
must not wait on storage or create unbounded queues/threads. Request validation
and the transfer of accepted values into reserved storage capacity are
consumer responsibilities.

Decode, size, time, consumer and asynchronous failures abort the original scope
once. Accepted earlier elements remain consumer-owned. A completion callback
which throws is an unsuccessful receive and also invokes abort cleanup, so
terminal acceptance and storage ownership must be updated atomically by the
consumer. An abort callback must perform bounded cleanup; if it throws, that
exception propagates and cleanup is not invoked again. Both success and failure
permanently close the emitter; retaining it cannot retarget a later response.

The old fatal message timeout remains distinct from a consumer's soft checkpoint
deadline. Scope acquisition and start/terminal callbacks must themselves remain
bounded; the existing decoder guard is not a general callback watchdog.

## Validation and consumer integration

The framework driver tests exercise staged CBOR, ordinary mux runners,
fragmentation, coalescing, original annotations, pre-callback limits, callback
failures, fatal timeouts and cancellation. Protocol-specific codec validation and storage
ownership remain consumer integration responsibilities.
