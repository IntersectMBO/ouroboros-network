# Consensus Testing Plan

This document discusses plans for and obstacles to improving the
`ouroboros-consensus` integration testing, the `test-consensus` test suite. See
`ouroboros-consensus/test-consensus/README.md` for a less technical overview.

The core plan is to test both low-level concerns such as closing file handles
promptly and also the high-level behavior of nodes. The high-level behavior
consist of both the collective behavior of a node network as well as the
behavior of each individual node therein. For example, individual nodes should
never rollback their chain by more than `k` blocks, and groups of nodes should
reach consensus whenever network latencies, disk failures, adversarial
behavior, etc allow them to do so.

This current draft focuses on the difficulties involved in determining whether
a concrete set of specific network latencies allow for consensus. We discuss
the [specification][sec:specification] of a node, its interface's messages, and
the interconnect between nodes; then the intentional
[discrepancies][sec:discrepancies] between that specification and the
implementation; and finally the [design][sec:design] of the current test suite
and its planned improvements.

## Specification
[sec:specification]: #specification

The most obvious specification against which to test the implementation is the
paper that defines Ouroboros Praos
<https://eprint.iacr.org/2017/573/20171115:001835>.

  * Figure 4 defines the π_SPoS protocol by specifying the behavior of an honest
    party: any node that follows those steps and is not offline too long or too
    often qualifies as an _honest party_.

  * Section 2.1 defines three properties that the authors' prior work
    established as sufficient conditions for ensuring correct behavior of a
    node network: Common Prefix, Chain Growth, and Chain Quality.

This section discusses these definitions, and the next section demonstrates
that they are difficult to use as a specification.

### π_SPoS Protocol

Figure 4 of <https://eprint.iacr.org/2017/573/20171115:001835> defines the
protocol π_SPoS by listing the steps for initializing a node and executing a
node in each uniform time slot of the execution. This protocol is not the final
protocol defined by the paper, but it is the most relevant analog for our
testing purposes (assuming we interpret `maxvalid` as the second definition
thereof, which ignores chains that do not intersect the `k` most recent blocks
of the current chain).

We reproduce the relevant excerpts here for convenience.

> 2. Chain Extension. …, for every slot slj ∈ S, every online \[party\] Ui
  performs the following steps:

  * (a) …

  * (b) Ui [\inserts\] all valid chains received via diffusion into \[its
    ChainDB\] ℂ, …. Ui \[reads its local chain as C and selects\] C1 =
    maxvalid(C, ℂ), sets \[local chain to C1\] and sets state st \[to\]
    H(head(C1)).

  * (c) Ui checks whether it is in the slot leader set of slot slj …. If yes,
    it generates a new block B = (st, d, slj , Bπ, σ) where st is its current
    state, …. Ui \[forges\] C2 = C1|B, sets \[local chain C to C2\] and sets
    state st \[to\] H(head(C2)). Finally, if Ui has generated a block in this step,
    it diffuses C2.

The meaning of _diffuse_ is fixed by the paper's "Diffuse Functionality". It
models a network that relays a chain diffused in slot `s` to every party's
inbox by the end of slot `s + Δ` and perhaps sooner but -- as far as I can
tell, based on at least Definition 6's _strictly increasing labels_ invariant
and phrases such as "At the end of each round [i.e. slot]" in the "The Diffuse
Functionality" paragraph -- not before the end of slot `s`. (Moreover, it seems
that the Diffuse Functionality only delivers messages at the end of a slot,
never during.) The messages may arrive in the inbox out-of-order. Step 2(b) of
π_SPoS for a slot `s` begins with the party retrieving the chains that are in
its inbox.

We summarize π_SPoS with the following pseudo-code.

```haskell
paperNode c0 cs0 sl = do
    newCs <- readDiffusionInbox   -- see The Diffuse Functionality in Section 2.2
    let cs' = Set.union cs newCs
    let c1  = maxValid c0 cs'

    isLeading <- checkIsLeader c0 sl   -- see Figure 4
    c' <- if isLeading then pure c1 else do
        c2 <- forge c1 sl
        diffuse c2   -- see The Diffuse Functionality in Section 2.2
	pure c2

    sl' <- waitUntilNextSlot
    paperNode c' cs' sl'
```

### Primary Properties

Section 2.1 of <https://eprint.iacr.org/2017/573/20171115:001835> defines the
three Common Prefix, Chain Growth, and Chain Quality properties as follows.

> Common Prefix (CP); with parameters k ∈ N. The chains C1, C2 possessed by two
  honest parties at the onset of the slots sl1 < sl2 are such that `C1^k ≤ C2`,
  where `C1^k` denotes the chain obtained by removing the last k blocks from
  C1, and ≤ denotes the prefix relation.

> Chain Growth (CG); with parameters τ ∈ (0, 1], s ∈ N. Consider the chains C1,
  C2 possessed by two honest parties at the onset of two slots sl1, sl2 with
  sl2 at least s slots ahead of sl1. Then it holds that `len(C2) − len(C1) ≥
  τs`. We call τ the speed coefficient.

> Chain Quality (CQ); with parameters µ ∈ (0, 1] and k ∈ N. Consider any
  portion of length at least k of the chain possessed by an honest party at the
  onset of a \[slot\]; the ratio of blocks originating from the adversary is at
  most 1 − µ. We call µ the chain quality coefficient.

(Remark. The notion of _chain length_ in CG and CQ excludes Epoch Boundary
Blocks (EBB). This exclusion complies with the definition of an EBB's block
number as the block number of its predecessor.)

The authors prove that the parameters of the π_SPoS protocol (security
parameter k, active slots coefficient f, maximum message delay Δ, etc)
determine an upper-bound on the probability of violating each property CP, CG,
and CQ. These theorems motivate the choice of parameter values for a proper
deployment of the implementation. During testing we instead use comparatively
aggressive parameter values so that, for example, the counterexamples are small
enough for a human to interpret and the tests executions' node networks
thoroughly explore the state space.

To emphasize: the authors do not prove that CP, CG, and CQ are invariants. They
instead show that the π_SPoS protocol parameter values can be chosen so that
violations of CP, CG, and CQ are extremely unlikely. Our tests do not choose
such parameter values since they are unwieldy; as a result, the tests are
likely to involve violations of CP, CG, and CQ.

### Mechanics of Property Violations

TODO this only discusses CP; discuss CG and CQ too

We have identified two mechanisms of Common Prefix violations: sequences of
multi-leader slots and message delays.

Let C(i,s) denote the chain that node i has selected as of the onset of slot s.

TODO better term than _wedged/wedge_? It was well received on Slack and its
concreteness has nice visual imagery. The mnemonic is that a wedged has been
driven between the two chains/nodes and cannot be removed (because of the
security parameter `k` preventing sufficient roll back).

Multi-leader slots can cause CP violations as follows. Suppose both nodes of a
two-node network are honest and begin slot s with the same chain. Suppose also
that the nodes both lead each slot in the sequence of k + 1 slots beginning
with slot s and ending with s + k. Even if the nodes begin with the same chain
and no messages are delayed, the two nodes' selected chains will have
irreparably diverged by the end of the sequence. Formally, with C1 = C(1,s+k+2)
and C2 = C(2,s+k+2), C1^k ≰ C2^k AND C2^k ≰ C1^k which is equivalent to (C1 ∩
C2 < C1^k) AND (C1 ∩ C2 < C2^k). We say that such chains -- and nodes with such
selected chains -- are _wedged_. Once such a wedge exists, every subsequent
slot s' starting with s + k + 3 will violate CP in at least two ways since C1^k
≰ C(2,s') and C2^k ≰ C(1,s'). If the nodes' two chains continue grow, the
number of CP violations will increase accordingly.

We depict that worked example as follows.

```
k = 3
slot:     0    1    2    3    |
leaders:  1,2  1,2  1,2  1,2  | Wedged!
delay:    0,0  0,0  0,0  0,0  |
```

TODO better term than _tilted/tilt_? The notion of "leaning" nicely captures
the asymmetry of `C2^k ≤ C1 ∩ C2 < C1^k`. And its concreteness has nice visual
imagery, like that of _wedge_.

Message delays cause CP violations in a slightly different way. Suppose both
nodes of a two-node network are honest and begin slot s with the same chain.
Suppose also that node 1 is the sole leader of the sequence of k + 1 slots
beginning with slot s and ending with slot s + k, but its messages sent in
those slots are all delayed at least until the slot s + k + 1. By the end of
the sequence, node 1 will have k + 1 blocks on its chain that are not on node
2's chain. Formally, with C1 = C(1,s+k+2) and C2 = C(2,s+k+2), C1^k ≰ C2 which
is equivalent to C2^k ≤ C1 ∩ C2 < C1^k in this context where C2 ≤ C1 by
assumption. We say that such chains -- and nodes with such selected chains --
are _tilted_. Such a tilt will violate CP in every subsequent slot s' starting
with s + k + 3 until node 2 selects the chains carried by the delayed messages,
since C1^k ≰ C(2,s'). Once node 2 "catches up", the tilt is eliminated and
causes no further CP violations; unlike wedges, tilts are not necessarily
permanent. Unless node 2 leads a slot before the first message arrives, it will
select that message's chain. This will eliminate the tilt, though node 1 could
have created another tilt by leading an additional slot in the mean-time, and
so on. Similarly, the tilt becomes a wedge if node 2 leads enough slots before
each of node 1's delayed messages arrive, such that node 2 never selects any of
the chains carried by those messages -- it's a race. Lastly, if the messages
never arrive and node 2 also never leads `k + 1` subsequent slots, then the
tilt would effectively be permanent without becoming a wedge.

We depict this example the minimal requisite delays as follows.

```
k = 3
slot:     0    1    2    3    |
leaders:  1    1    1    1    | Tilted!
delay:    4+   3+   2+   1+   |
```

These worked examples above involve either multi-leader slots or message delays
but not both. The following depiction shows the two mechanisms cooperating to
cause a wedge.


```
k = 3
slot:     0    1    2    3    4  |
leaders:  1,2  1,2  1,2  1    2  | Wedged!
delay:    0,0  0,0  0,0  1    0  |
```

If the message from slot 3 hadn't been delayed, then node 2 would have selected
the carried chain before forging in slot 4, thereby avoiding the wedge.
(Remark. The nodes became tilted first in slot 3 and then wedged in slot 4.)

TODO better term than _rivaled/rivalry_? _balanced/balance_ is attractive, but
sees like an over-statement, since chains/nodes that are neither tilted nor
wedged can still have unique suffixes, even of different lengths. Something
like _balanced_ would continue the teeter-totter/see-saw imagery of _wedge_ and
_tilt_: when rivaled, the nodes are ratcheting back-and-forth on the see-saw as
normal with slight intended wobbles, when tilted one of the nodes is too heavy
and the other is stuck in the air, when wedged, the plank snapped and the nodes
can never play again. That's intuitive and suggestive imagery, but too silly to
codify -- _wedged_+_tilted_+(something like _balanced_) are pretty close
though.

Wedges and tilts are two of three mutually exclusive and exhaustive
classifications. The third classification is a _rivalry_ between two
chains/nodes: C1^k ≤ C2 and C2^k ≤ C1, which is equivalent to C1^k ≤ C1 ∩ C2
and C2^k ≤ C1 ∩ C2. A rivalry does not cause a CP violation; it is the expected
typical state of any two nodes. The word choice is motivated because each node
in the pair could "win the contest" by extending its current chain in a way
that would force the other to switch to it. In wedges and tilts, on the other
hand, either the nodes will never switch to the other's chain or else one of
the two nodes has already "lost the contest" such that its only useful option
for extending its current chain is to adopt blocks from the winner's chain,
respectively. These classes are in one-to-one correspondence with the range of
the sum `1_(C1 ∩ C2 < C1^k) + 1_(C1 ∩ C2 < C2^k)` where `1_P` denotes the
indicator function for the formula P, wedged is paired with 2 (AND), tilted is
paired with 1 (XOR), and rivaled is paired with 0 (NOR).

## Intentional Discrepancies
[sec:discrepancies]: #intentional-discrepancies

The intentional discrepancies between the specification as outlined in the
previous section and the nodes' actual implementation make it difficult to test
its adherence. This section compares and contrasts the nodes as implemented
with the paper's model of honest parties running the π_SPoS protocol. The major
points correspond to the paper's notions of _honesty_, _onset_, _message_, and
_message delay_.

### Messages and Message Delays

In the paper, each message is an entire chain. An honest node sends messages
immediately after forging a new block and never otherwise. Each message carries
the new chain to every other node (actually to the sender itself too, but that
seems ultimately inconsequential). There are no other types of messages.
Messages essentially arrive _between_ slots, since if they arrive during a
slot, they are ignored until the onset of the next slot (see discussion and
pseudo-code in the The π_SPoS Protocol section above).

Many definitions, theorems, and proofs in the paper refer to the maximum
message delay Δ. If Δ is 0, messages arrive at the end of the same slot they
are sent; if Δ is 1, they arrive no later than the end of the subsequent slot;
and so on. Different messages carrying the same chain may arrive at different
parties after different delays, but no delay will exceed Δ. The parties and
nodes themselves are both unaware of Δ, so they aren't, for example, using it
to check if a sent message has "timed-out". The theorems and proofs, on the
other hand, do explicitly rely on Δ.

The implementation's messages are very different than the paper's for practical
reasons that are lower-level than the paper addressed, such as keeping the
message size relatively small. Asymmetric client-server pairs of nodes
communicate via _mini protocols_, most notably ChainSync and BlockFetch. The
two nodes in a mini protocol instance exchange messages of various types, each
of which corresponds to an action in a labeled transition system that specifies
the mini protocol. These messages are all much smaller than a whole chain; the
largest message contains exactly one block. These mini protocols cumulatively
achieve the same effect as a message from the paper: communication of a forged
chain to the other nodes in the network. However, the mini protocols do it via
a sequence of small, generic handshakes between two nodes and along paths of
node-to-node edges in a connected forwarding network that may have fewer edges
than the paper's fully connected topology. As a concrete contrast, the paper's
parties only ever send a block immediately after they forge it, whereas the
nodes in a _non-fully_ connected network must relay blocks they did not
themselves forge.

The delay of a single mini protocol message is only one part of the whole sum
that is analogous to the delay of a paper message. The test suite therefore
considers a _message_ to be a pair of a block and a node and such a message's
_delay_ to be the difference between the slot in which the block was forged and
the slot in which it was added to the node's chain database. This definition
preserves the property from the paper that a message delay is the duration (in
slots) of the communication of a forged chain (headed by the new block) to
another node in the network.

(Remark. It is possible for different nodes to forge the same EBB in either the
same or different slots. This particular definition of message delay therefore
might be incompatible with EBBs.)

Even if the network has no latency and all computation is instantaneous, the
ChainSync and BlockFetch mini protocols may introduce non-zero message delay.
The mini protocols are designed to achieve _just-in-time_ downloads: the
ChainSync client will download block _headers_ only if the two nodes are
rivaled, and the BlockFetch client will analyze the downloaded headers and only
download the corresponding _bodies_ if the client's node would _plausibly_
switch to a chain containing them. For example, if two leaders of a slot
simultaneously forge a new block atop the same chain, their corresponding
messages to each other will not be received in that same slot (thus each of the
two message's delay is at least 1 slot), since an (honest) node would not
switch to a chain that differs only in the head block.

This discrepancy requires the test suite design to address the following
question.

> Q1 How to determine whether a non-zero _message delay_ should be expected?

(Remark. Even without the complexity of mini protocols and forwarding/relaying,
the consequences of message delays within the paper itself can be difficult to
intuit. For example, even if a block B forged in some previous slot does not
occur on any of the parties' selected chains at the onset of a slot, it is
possible for B to occur on some nodes' selected chain as of the onset of the
next slot. This seems like a "non-monotonicity" result, but that's only because
the statement is ignoring the blocks/chains lurking in delayed (and possibly
also arrived) message as of the first slot.)

### Onset

The CP, CG, and CQ properties all consider the chains selected by parties as of
the _onset_ of a slot. The implementation and the paper essentially have the
same definition of _slot_: one of the uniformly-sized sequence of intervals
that partition the duration during which the node network runs. So the two
agree on the onsets of each slot as well. There is a discrepancy, however,
because the chains selected by the implementation's nodes as of the onset of a
slot are not analogous to those selected by the paper's parties. This is
because nodes do not exactly implement the π_SPoS protocol.

The key difference is that -- message delays permitting -- nodes can select a
block in the same slot it is received, whereas parties must wait until the
subsequent slot. Whereas the paper's protocol specifies that a party selects
from among its received chains once per slot, the implementation's node
re-selects its chain immediately upon receiving a new block. The simplest
example is a two-node network with the first slot being led by exactly one
node. The leader will forge its block once the slot starts and immediately
select that block. The other node will receive the block almost immediately
(assuming no network latencies), and then immediately select that same 1-block
chain, since it's longer than the genesis chain. Therefore both nodes have
selected the new chain as of the onset of the second slot. The analogous
example in the paper also has the leader forge a block, immediately select it,
and send a message to the other node. As we have interpreted the paper, The
Diffuse Functionality will not deliver that block until the end of the current
slot, by which time the node will have already checked its inbox for this slot,
and so it won't consider the new chain for selection until the second slot.
Only the first slot's leader will have selected the new chain as of the onset
of second slot. This discrepancy is depicted as follows.

```
onset of slot:            0    1

In implementation
leader's selection:       g    g,B
recipient's selection:    g    g,B

In paper
leader's selection:       g    g,B
recipient's selection:    g    g
```

The only way the π_SPoS protocol could permit the recipient to select the chain
in the same slot it was forged is if the recipient was asleep/blocked/etc until
after the Diffuse Functionality had delivered the message. In our judgment, the
paper does not consider such computational delays; otherwise, the π_SPoS
protocol as written would also permit a leader to forge a block whose
predecessor was forged in the same slot. That would violate Definition 6's
_strictly increasing labels_ invariant, and the π_SPoS protocol does not seem
to guard against that in any way other than by the assumption that the
recipient will have checked its inbox before the requisite message arrives. In
other words, the paper assumes that parties are perfectly synchronized (no
clock skew) and execute the π_SPoS protocol in total isolation _during_ each
slot, while messages are instead only delivered _between_ slots.

(Remark. With computational delays and/or clock skews, the current
implementation's node could forge a block with the same slot as its
predecessor. However, the current test suite has no clock skews etc, and the
forging of a block currently always wins the corresponding race.)

(Remark. As a result of the above, we somewhat surprisingly observe that the
paper's parties can only reach a collective state in which they have selected
the same chain, i.e. "consensus", during an _empty_ slot, i.e. one with no
leaders.)

This discrepancy requires the test suite design to address the following
question.

> Q2 When should the CP, CG, and CQ properties hold, since the implementation's
  node network does not necessarily have a state analogous to the _onset_ of a
  slot in the paper?

TODO Can we possibly answer this question by establishing that the properties'
definitions do not meaningfully depend on the state of the parties' at an
onset? In particular, maybe they just needed a way to describe two moments that
are separated by (at least) one slot's duration?

### Honesty

The previous sections explained that the implementation's nodes and the paper
parties communicate in very different ways and moreover that each node doesn't
implement the π_SPoS protocol.

These discrepancies require the test suite design to address the following
question.

> Q3 Can the implementation's node be considered _honest_?

This question is further complicated by the fact that we expect property
violations during our tests.

> Q4 Can any node be considered _honest_ once a property has been violated?

TODO The properties' definitions only qualify the nodes as being _honest_, so
this question's phrased is forced since dishonesty is the only way to excuse a
node's behavior -- too pedantic? How else to phrase this, "Which property
violations excuse other property violations?"?

## Test Suite Design
[sec:design]: #test-suite-design

TODO Discuss the "distribution testing" alternative: run a network many times
and see if CP violations happened "as often as" the paper predicts -- test
suite execution time and the acceptable threshold tolerance on the observed
violation rate seem like the major challenge with this approach.

This section discusses phases of the test suite design, and how each handles
the discrepancies between the specification and the implementation.

Phase 1 is almost complete in the current test suite implementation. The
remaining phases are planned work. We do not anticipate the order/priority
listed here to be respected during development.

### Phase 1: Negligible Network Latency

In the current test suite

  * all mini protocol messages arrive essentially immediately: messages are
    communicated via shared variables within a single process's memory space,
    and we do not inject any artificial networking latencies;

  * the computational delays/latencies are negligible such that every
    interaction and event happens promptly;

  * The Fast Forge Assumption: the node's "clocks" are sufficiently
    synchronized and the networking/computational latencies balanced such that
    a leader always forges a block before receiving any blocks in that same
    slot;

  * The Quiescence Assumption: the duration of each slot is large enough
    compared to the minimal networking/computational latencies that the node
    network reaches an idle steady-state before the end of each slot;

  * we allow some nodes to join the network later than others; every node
    starts with the genesis chain.

Under these conditions, we can address the discrepancies as follows.

Without artificial messages delays, all property violations are caused by long
sequences of multi-leader slots, late joins, and/or bugs. It is relatively
straight-forward for the test suite to disregard the property violations that
can be attributed to the leader schedule and/or node join plan. The special
cases include the following.

  * A sufficiently long sequence of multi-leader slots can create a wedge (c.f.
    Mechanics of Property Violations above). Some sequences certainly create a
    wedge, and some only make wedges possible (depending on the chain selection
    of a non-leader that leads later slots in the sequence). Such a sequence is
    easy to recognize in the leader schedule, whether it is fixed (e.g.
    round-robin), generated before the test (e.g. `WithLeaderSchedule`), or
    recorded and analyzed after the test (e.g. Praos).

  * When a node joins a node network that already has a long enough chain, it
    immediately creates a tilt, since the node starts at genesis. By the The
    Quiescence Assumption, the node will have caught up to the network by the
    end of the slot, except -- by the Fast Forge Assumption -- for the case
    where the network's chain had 1-block and the new node leads in the same
    slot it joins. Unless k is 0, that will not cause a CP violation, though it
    could also be considered to effectively begin a multi-leader sequence.

Currently, we assume all tilts are due to late joins and hence expected. We
therefore only check the final chains for wedges, since they are permanent. If
we find no wedges, then we conclude there were no CP violations.

In this setup, we are able to additionally check that no final forks are deeper
than expected, even if they are shallower than k blocks. We compute the
expected maximum fork depth by processing the leader schedule and node join
plan as a sequence of commands for an abstract virtual machine.

> Q1 How to determine whether a non-zero _message delay_ should be expected?

All non-zero message delays should be due to late joins and/or the leader
schedule (via just-in-time downloads), by The Quiescence Assumption. Whenever a
node add receives a block (i.e. adds it to its ChainDB), we compare the current
slot to the maximum of the block's record slot and the slot in which the node
joined the network (we can't expect a node to receive a block before the two
both exist). If the difference is non-zero then we fail unless the block was
forged in a multi-leader slot or it has a block number of 1, due to the 1-block
chain late join corner case.

> Q2 When should the CP, CG, and CQ properties hold, since the implementation's
  node network does not necessarily have a state analogous to the _onset_ of a
  slot in the paper?

If nodes never rolled back more than k blocks and the nodes' final chains have
no wedges, then we assume CP was not violated, thereby side-stepping this
question for CP.

For CG, we schedule a thread for each node that wakes-up at the beginning of
every slot and merely records the block number of the head of the the node's
selected chain. Because this thread only reads a single shared `TVar` and
writes to an unshared `TVar`, we assume it succeeds before the node is able to
alter its chain in that slot (this is similar to the Fast Forge Assumption).
That assumption and The Quiescence Assumption ensure that these threads record
the block number of each node at the onset of each slot. The leaves only one
relevant discrepancy between the implementation and paper: blocks forged in a
slot may already have be selected by other nodes as of the subsequent onset. We
account for this when determining values for the s and τ parameters of CG: we
check for all s and all slot intervals, and we calculate τ by counting the
number of active slots (i.e. at least 1 leader) in the interval's leader
schedule. There are two corner cases to consider in the calculation of τ.

  * We allow for leaders that failed to generate a block (e.g. late joins
    inducing `PBftExceededSignThreshold`).

  * We ignore CG violations if there were any CP violations.

We do not test for CQ violations, since our test infrastructure does not
generate any adversarial blocks.

> Q3 Can the implementation's node be considered _honest_?

In this context with various latency/timing assumptions and in particular no
unexpected message delays, there is only one relevant discrepancy: block
recipients may select that block in the same slot it was forged. This is most
explicitly addressed in the calculation of τ within our CG test and also in our
CP-like test of expected fork depth in the final chains.

> Q4 Can any node be considered _honest_ once a property has been violated?

We answer both Yes and No, for various concerns.

  * No, since we do not check CG if CP was violated.

  * Yes, since we ignore the tilts inherent to nodes joining late with the
    genesis chain.

  * Yes, since our tests fail if the final chains have deeper forks than
    expected, even if those expected forks include wedges. (This test
    technically supersedes the CP property.)

  * TODO more?

### Phase 2: Sub-Slot Network Latency

TODO elaborate?

This planned Phase will inject some network latencies, but without spoiling The
Quiescence Assumption. The goal is to permute the order in which
non-sequentially dependent mini protocol messages are exchanged.

A robust -- though possibly difficult to maintain -- mechanism for this purpose
might involve additional TVars in the test node tracers, mock networking, etc
that detect steady-state/quiescence and block the slot from incrementing until
then.

We do not anticipate needing to change the definition of the tested properties.

### Phase 3: Sub-Slot Clock Skew and/or Computational Latency

TODO elaborate?

As with sub-slot network latency, we do not anticipate sub-slot clock skew
and/or computational latency will require significant changes, other than
likely spoiling the Fast Forge Assumption.

### Phase 4: Super-Slot Network Latency and/or Clock Skew and/or
    Computational Latency

TODO elaborate

This phase is desirable, since it's ultimately the only realistic one. However,
the first discrepancy-induced design question

> Q1 How to determine whether a non-zero _message delay_ should be expected?

becomes very difficult to answer, due to the potential for a single injected
significant latency to cascade through not only the mini protocol layers but
also the nodes' "internal" layer of chain selection, forging, etc.

This difficulty in turn cascades to convolute the remaining design questions as
well. In particular, it is unclear to me how to test determine if a CP
violation is excusable given the networking/computational latencies we injected
into the test execution. Determining that would seem to require a reference
(abstract) implementation consisting of a "bag of related state machines" with
one state machine per mini protocol instance and one per node.

### Phase 5: Byzantine Faults

TODO e.g. disk failures causing a node to "switch" to a short chain

### Phase 6: Antagonistic Adversarial Behavior

TODO currently, our only adversarial behavior is "being offline too long" or
"joining late";

TODO an antagonistic adversary would in particular create adversarial blocks, a
prerequisite for CQ violations

## Miscellaneous Concerns

  * As far as I've looked, the `SimM` monad implements `MonadSTM` in a fully
    deterministic and reproducible way. But does that simultaneously risk omit
    schedules that the actual `STM` might realize at run-time? TODO It's
    generally not deterministic, if I recall correctly.
