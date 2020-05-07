# A buffer to ensure epoch properties are irrevocable

Let `sne` be the first *s*lot of the *n*ext *e*poch. Without further context
and assumptions, the slot `sne` is the last slot for which the current era
certainly determines the wallclock time of its onset. Since the next epoch may
be part of the next era, `sne` may have a different slot duration than `sne-1`.
Thus the current era does not determine when slot `sne` ends, which is also
when slot `sne+1` begins. When `sne` ends cannot be known for certain until the
next epoch's era is known for certain.

The era is one of a few properties of the next epoch that depend on the ledger
state at the end of the current epoch. Additional examples of such properties
include other era parameters alongside slot duration, updateable protocol
parameters, and also Shelley's epoch nonce. To ensure that such properties
depend only on the net's immutable common prefix as of the end of the current
epoch (motivated below), the ledger rules delineate a buffer of `BufferSize`
slots at the end of every epoch such that blocks within the buffer cannot
affect the next epoch's properties. By default, unless a certain sequence of
transactions occurs in blocks preceding the buffer, the next epoch will have
the same properties as the current epoch. We call the last block in such a
sequence a *keyblock*.

There may be zero or many keyblocks in an epoch. This document focuses on when
and how a node can conclude that there will be no more keyblocks in the current
epoch.

# Why irrevocable?

We want the next epoch's properties to be irrevocable before it begins because
nodes base actions on those properties with irrevocable real-world
consequences. For example, if a node is on a chain for which the next epoch's
era is defaulting to Byron and it considers itself a leader of `sne`, it will
forge a Byron block once its wallclock reaches `sne`. In the absence of our
buffer, the node might then switch to a chain that instead changes the new
epoch's era to Shelley. If the node still leads `sne` (which it actually would,
due to Shelley decentralization paramter starting at 0), then it might also
forge a Shelley block for slot `sne`. There are two problems with this
behavior.

  * The node has forged two blocks that claim slot `sne`, which constitutes
    adversarial behavior. (Each of its blocks are otherwise valid in isolation,
    though.) The alternative is to not forge a Shelley block, thereby having
    wasted its leadership opportunity.

  * The node will have flushed the contents of its mempool into the now
    superseded Byron block, and so those transactions will *not* be included in
    its Shelley block -- that Shelley block will be mostly empty. We have not
    designed the mempool to be unflushable.

We can avoid addressing the first concern by instead considering a more
artificial scenario in which the second chain causes the node to instead lead
`sne+1`. Its mempool will still have been very recently flushed, so it will
still essentially waste its `sne+1` lead.

# Buffer size for Byron

For Byron, a `BufferSize` of `2k` ensures that any keyblock must be on the
net's immutable common prefix by the time the next epoch begins.

  * By synchrony (ie `Δ=1`), every node will have selected one of the longest
    chains that was forged prior to `sne` (ie forged by the end of `sne-Δ =
    sne-1`).

  * By Byron's Chain Growth (CG) invariant, the latest `2k` slots `sne-2k ...
    sne-1` contain at least `k` blocks. Since those slots constitute the
    buffer, any keyblock on this chain will be in the immutable prefix.

  * By Byron's Common Prefix (CP) invariant, the immutable prefix is common to
    all nodes in the net. So any keyblock in any selected (ie longest) chain
    will be in the immutable prefix.

At any earlier time (ie during the current epoch) a keyblock might still exist
outside of the immutable common prefix, and so not all nodes have necessarily
selected it yet.

  * During `sne-1`, not all nodes have necessarily received the `k`-th block
    guaranteed to succeed the keyblock.

  * During previous slots, the keyblock might be one of the latest `k` blocks
    guaranteed by CG.

# Expanding the buffer to provide forewarning

Knowing the next epoch's properties for certain only once the wallclock reaches
the next epoch provides exactly zero "forewarning" in the worst-case. That
seems undesirable, so we discuss how to improve upon it.

Every increment to `BufferSize` will allow us to shift that claim and argument
to the preceding slot (we apply CG to the first `2k` slots in the buffer) (TODO
does the CP invocation still work?). So a `BufferSize` of `2k+Forewarning`
would ensure for a current epoch in Byron that the next epoch's properties are
only guaranteed to be fixed as of the onset of slot `sne-Forewarning`.

# Adapting the buffer to Shelley

There are two alterations necessary for Shelley. First, the Shelley Chain
Growth property instead ensures at least `k` blocks in every `3k/f` slots;
though, we've been advised by the researchers that `4k/f` is more likely to be
future-proof. Second, Shelley only assumes semi-synchrony instead of synchrony,
so we must also consider the maximum chain-diffusion delay `Δ`. Based on the
interpretation of synchrony as `Δ=1` used in the above Byron argument, it seems
that the Shelley `BufferSize` should become `4k/f+Δ-1+Forewarning`. Note that
the Ouroboros paper's are careful to assume that the nodes themselves do not
need to know `Δ`'s value, and this seems design seems counter to that ideal.

(TODO Do we also need to adapt the CP invocation?)

# Header/body split also needs the buffer

A `BufferSize` of `2k` for Byron also suffices for the header/body split as we
have so far understood it.

The ChainSync client connects to a peer's server, finds an intersection along
the local node's fragment, rollsback the local node's latest ledger state to
immediately after that intersection, and then reuses the intersection's ledger
state to validate the peer's headers since it doesn't yet have their bodies.
Our assumption so far has been that the client would only need to validate up
to `k` headers with that ledger state; after that many it would start fetching
the bodies of the peer's chain.

Let `I` be the slot of the intersection. The client will only need to rely on
the intersection's ledger state to validate up to `k` headers, which by Byron
CG are all within the range `I+1 .. I+2k`. If any of those headers are from the
next epoch, then we would have `sne <= I+2k`. This would imply `I > sne-2k-1`.
Since the intersection is thus in the buffer, any relevant keyblock would
already be on the two nodes' shared prefix.

We have recently considered that the ChainSync client may actually need to
validate `k+1` headers in the worst-case. By Byron CP, the intersection is at
least the client node's anchor point. But in that case, the client cannot
consider the peer's chain worthy of selection until it validates `k+1` headers
-- the first `k` would only identify it as equally-preferable. We have not
discussed the corresponding Chain Growth property for ensuring at least `k+1`
blocks in `s` many slots, but we suppose it's only slightly inflated compared
to the `k`-block-ensuring CG properties' window of `2k` slots for Byron and
`4k/f` for Shelley. We'd need to correspondingly inflate `BufferSize` as well.

Another alternative would avoid the last of the `k+1` headers. Latency hiding
has separately motivated having ChainSync/BlockFetch begin fetching blocks once
the peer chain is equally-preferable to the local chain. With that
modification, the worst-case would again only require validating `k` headers.
