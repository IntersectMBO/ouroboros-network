The Genesis initiative for the Consensus team, from the top.

# Executive summary

When communicating with the net after not having access to the honest chain for
a significant amount of time (eg computer was asleep, eclipse attack just now
ended, etc), a node will use _the Genesis rule_ to determine which blocks to
fetch from its peers. We plan to implement this as follows.

  * Don't download any blocks while we have less than NumPeers-many peers. TODO:
    work with Networking Team and Researchers to choose the exact number. This
    is the _Connecting_ state.

  * Each of our NumPeers-many peers will have sent us (via `MsgIntersectFound`
    and `MsgRollForward`) both a sequence of headers that extends our immutable
    tip and also the tip `Point` of the chain they claimed to have selected. We
    have no practical way of validating their claim, because this `Point` may
    indeed be arbitrarily many blocks and slots ahead of our selected chain.

  * To choose which block to download from among our peers' offered header
    sequences, we consider only a fixed number of slots after their common
    intersection. This window of slots is called _the Genesis window (for this
    set of header sequences)_. It is unsound to consider blocks beyond this
    window, because doing so increases the probability that an adversarial peer
    has been able to influence the leader schedule and hence undeservedly
    increase their chain growth as compared to the honest chain.

  * We only download a block once we have identified a subset of offered header
    sequences whose represented chains necessarily have strictly more blocks in
    this Genesis window than do any of the chains represented by all the offered
    sequences not in this subset. This is the _Syncing_ state.

  * We might not yet have enough information to identify that subset, but once
    we do, every sequence in it will share the same unique header by the
    following two assumptions.

      * Assumption 1: NumPeers-many peers is enough to probabilistically ensure
        we're connected to the honest network, and so one of the offerings will
        indeed be the honest chain.

      * Assumption 2: In any Genesis window, the honest chain has strictly more
        blocks than any other chain. This inherited from the Ouroboros Genesis
        security argument.

      * Thus, we download that block, certain that we will never roll it back,
        and continue the process from there. (Until we're "caught up"; keep
        reading.)

  * We cannot download any blocks until we have identified that subset, so we
    wait for our peers to send us more information.

      * Thus, an adversarial/buggy peer could claim a maximally-appealing tip of
        their selection (recall we can't validate their claimed tip) and then
        withhold any further information, thus preventing us from downloading
        any blocks.

      * To prevent this, we require that each peer sends us a new best header
        (strictly better than any they've previously sent) with "sufficient
        frequency", eg as determined by a leaky token bucket abstraction. Any
        peer worth connecting to will be able to send more headers frequently
        enough, since a header is only ~1100 bytes.

      * This is also the _Syncing_ state.

  * There are three reasons a peer wouldn't send a new best header soon enough
    to satisfy our "sufficient frequency" demand.

      * It's adversarial, trying to stall us. (We should disconnect.)

      * It's buggy and/or has a bad network connection. (We should disconnect.)

      * It has no more headers to offer. In which case its latest
        `MsgRollForward` message carried a header and also claimed that same
        header is the tip of its selected chain. (We should be patient, since
        the next header indeed might not even exist yet.)

  * Once all of our peers have no more headers to offer and we've downloaded and
    selected one of their chains, we have successfully _rejoined_ the honest
    net, and we no longer need to use the Genesis rule.

      * We can instead simply run Praos, soundly interpreting every offered
        header sequence as the end of the represented chain. The Praos paper's
        analysis, combined with the assumption that we maintain access to at
        least one peer offering the honest chain (and not just a prefix of it),
        ensures we'll be able to fetch, select, and extend the honest chain.

      * The Network Layer's P2P governor actively ensures we do indeed have
        ongoing access to the honest chain. Roughly: it will continually sample
        relays registered on the chain according to their relative stake and
        confirm that our selected chain is within `k` of the relay's tip for
        some number of relays whose cumulative relative stake probabilistically
        ensures we're on the honest chain.

      * This is the _CaughtUp_ state and is equivalent to today's node's
        behavior.

      * TODO Naively, we also need the Network Layer to checkin with the relays
        ___while we're bulk syncing___ in Syncing. However:

          * If our latest ledger state is years old, how likely are the relays
            registered on it to still be responsive? Even if they are, we're
            certainly not within `k` of their tip; so what precisely should we
            be asking them?

      * TODO can we allow less than NumPeers-many peers while CaughtUp?

      * If the Network Layer's check ever fails to confirm our chain, we cycle
        our peers and return to the Connecting state, followed by the Syncing
        state until we've again rejoined. The fundamental assumption is that the
        Network Layer will detect we've lost access to the honest net _soon
        enough_. Specifically, it will do so before we could have made any
        irrevocable block selections (ie well before any adversary would be able
        to mint a valid chain that has more than `k` blocks after its
        intersection with the honest chain).

      * TODO Can we transition to CaughtUp sooner? Or do we really need to wait
        until we're all the way to the peers' tips? We're a bit worried about
        livelock risk with such a demanding trigger for Syncing->CaughtUp.

  * In each of the Connecting, Syncing, and CaughtUp states, we mint blocks
    whenever we lead the current slot, as usual.

      * When not in CaughtUp, it is likely that we're either minting a
        non-compettive block because our selected chain is historical or even
        possibly minting a block that extends an adversary's short fork.

      * However, both of those are unavoidable (unless we wait to finish
        rejoining and then mint our block retroactively, which an honest node
        should never do and would rarely actually benefit from) and more
        importantly less risky than arming an unknown attack vector that might
        be able to prevent large numbers of honest nodes from contributing to
        the growth of the honest chain.

  * TODO recall and explain why the Genesis designing made us worry about how
    BlockFetch was allocating download bandwidth as a limited resource (and that
    we concluded, at least for important nodes, we should just request the block
    for every header that's better than our selected chain)

TODO make sure each of the above points is explicitly elaborated below

# Relevant attacks

1) "fake stake" attack (our current code already mitigates this)

  * https://medium.com/@dsl_uiuc/fake-stake-attacks-on-chain-based-proof-of-stake-cryptocurrencies-b8b05723f806

  * Adversary can send invalid blocks and still thereby cause me to waste
    enough network&compute resources on it to DoS me. (EG fetch an old state
    from disk before realizing you don't need it.)

  * Mitigation: Immediately ignore blocks that are older than `k`. Checked via
    the ~1100 byte block header. Ouroboros papers' Common Prefix theorems
    ensure this is sound for nodes that have been "caught up" for long enough.

  * (TODO Confirm.) We disconnect from peer if they send a block that doesn't
    match the header they used to convince us to download the block.

2) "long range" attacks (Genesis Paper)

  * With just Praos, the adversary can isolate any node that hasn't been
    "caught up" to `mainnet` _recently_ enough, such as an entirely fresh node,
    a node that's been offline a month, etc.

  * Current code hasn't worried about this because our deployment instructs
    people to sync from one of the IOG relays, which have forever been "caught
    up".

  * We want to shutdown the relays, which will arm the long range attack. So we
    need to mitigate directly.

  * See below for more details dynamics and the mitigation scheme.

3) "eclipse" attack (no IOG paper for this)

  * Even with Genesis, if all our peers are adversarial, we're doomed. The
    Genesis paper can kind of tolerate it, because it rolls back by more than
    `k` whenever it reconnects to the honest chain. But because of our fake
    stake attack mitigation, we can't do that: we'll disconnect from peers
    offering the honest chain (the `ForksTooDeep` ChainSync outcome).

    Actually, in his eleventh hour, Edsko and I realized we could actually roll
    back by more than `k` by maintaining a relatively lightweight on-disk
    database to make it sufficiently cheap to check whether a deep forking
    header we just learned about is in fact denser than our immutable chain. We
    were excited about this, but then we realized that no user would want to
    roll back by more than `k` -- what they want to know is that they can't be
    eclipsed that long! IE Being able to roll back more than `k` is a
    consolation convenience: having to roll back by more than is still extremely
    bad scenario for a user to be in. Whether the node can recognize the need
    and/or do it automatically/without manual intervention isn't really the
    point.

  * The Networking Team's plan from the start had been for the P2P design to try
    to _prevent_ eclipses, but ultimately rely on Consensus to handle
    _detecting_ eclipses when the peer selection went wrong. Once detected, the
    plan for _escaping_ eclipses has been to cycle our peers, starting a fresh
    P2P connection effort. Remark: if we're fundamentally eclipsed (eg at the
    hardware level), then the best we can hope for is refusing to commit to any
    blocks until we're reconnected to the honest chain. (Or just dying with an
    indiciative log message, I suppose.)

  * However, in early 2022, a few conversations among subsets of Frisby,
    Alexander Russell, and Networking discussed what the _escape_ action would
    look like and why it might do any better than the peer selection that lead
    to an eclipse. The result of those conversations was a change in the design:
    instead of only doing the escape action when we detect eclipses, the
    Networking layer will simply do something along the same lines "frequently
    enough". Thus, there is a limit how long a (non-physical/non-hardware)
    eclipse attack could last, and that relieves Consensus of the obligation to
    detect eclipses (eg by noticing a significant drop off in chain density,
    which is a tricky probablistic decision to make binary).

# Mitigating long range attacks

In the Genesis paper's model, each message contains an entire chain and the
nodes are allowed to rollback more than `k` blocks; the Common Prefix theorem
doesn't apply to nodes that have been offline recently, nodes that aren't
"caught up". The Genesis paper phrases the fix as a new way to compare chains,
but I favor Edsko's phrasing focused in _prefixes_, which the researchers said
they quite liked.

The key insight is that our nodes (via ChainSync and BlockFetch) aren't really
exchanging _chains_, they're exchanging _prefixes_ of chains. One of our core
assumptions is often paraphrased as: the longest _chain_ in the system is the
honest chain. On the other hand, the longest prefix a node has received at any
given time might just be the one that node happened to receive first. Thus the
Genesis paper (via Edsko's interpretation) instead defines _prefix selection_,
which is to prefer the prefix that is denser in fixed-size window of slots that
starts just after the intersection with the competing candidate prefix --
called _the (comparison's) Genesis window_.

> Given a large enough window starting at two chain's intersection, the honest
chain should always be densest, for effectively the same reasons that the
honest chain should always be longest in the system.

Prefix selection alone doesn't prevent long range attacks, though, since
there's no guarantee that a node has even been sent an honest prefix! For that
concern, we've decided to play a numbers game: trust in the P2P peer selection
algorithm insofar as to assume that if we have ~20 peers, then at least one of
them is feeding us (prefixes of) the honest chain. If that's not true, then
we've been eclipsed (likely a botnet attack).

## Another reason the intersection matters

The above prefix selection perspective on the Genesis rule is particularly
pertinent for our code because our actual node, in contrast to the model in the
papers, exchanges blocks at a time instead of whole chains, and hence, when
(re)joining, is comparing prefixes until catches back up to `mainnet`. However,
even if we did (somehow!) exchange entire chains, the Genesis rule is still
necessary.

Given enough wallclock time and enough legitimate leadership opportunities, an
adversary could craft a chain (TODO eg via brute-force search/grinding?) that
allots them a larger and larger share of the active slots. In various ways,
such a chain could actually become longer than the honest chain. That would not
fool the Genesis rule, though, since such a chain would be comparatively sparse
immediately after the intersection, because the Genesis window is so short that
the adversary would not have enough elections to manipulate the leader schedule
within it.

In summary, without the Genesis rule, total chain length might be an
apples-to-oranges comparison, since, for historical chains the adversary has had
sufficient wallclock time and leadership opportunities to manipulate the chain
that consists only of their blocks. On the other hand, the Genesis rule's
density comparison in the Genesis window after an intersection is an
apples-to-apples comparison.

## The Genesis state machine

Therefore the implementation sketch is a new top-level state machine.

* Start in Connecting.

* Connecting - never advances the immutable tip. Transition to Syncing once
  connected to enough peers.

* Syncing - runs prefix selection, which advances the immutable tip only when
  all peers have given enough headers to reveal which successor of the
  immutable tip is part of the densest in the corresponding Genesis window.
  Transition to CaughtUp once all of peers have sent their best headers and the
  node has selected one of the corresponding chains.

* CaughtUp - runs Praos as usual, ie simply selects the longest prefix (which
  is safe to consider as a full chain, not just a prefix, for a node that is
  indeed "caught up"). (TODO mention the need for new BlockFetch download
  decision logic: because we're no longer connected to at least one relay, we
  may need to allocate our download capacity more carefully; cf Tse paper
  https://eprint.iacr.org/2021/1545)

## Prefix selection

The prefix selection rule, used in Syncing, is rigorously defined as follows.
Given a set of prefixes S (ie the latest (valid) headers received from peers
via ChainSync), find the least `j` such that `Anchor_{j+1} = Anchor_j` and
select an arbitrary element of `Bests_j` truncated so that it extends
`Anchor_j` by no more than `k` blocks. (TODO hmm, indeed arbitrary among
`Bests_j`? Maybe truncate them all and then apply Praos chain selection? Which
could be implemented as truncate them all before passing them as options to
BlockFetch, except for blocks I meant (which currently circumvent BlockFetch).)

```
    Anchor_0 :: Point
    Anchor_0 = intersection(S)

Candidates_i :: Set AnchoredFragment
Candidates_i = subset of S that extends Anchor_i

     Bests_i :: Set AnchoredFragment
     Bests_i = the (necessarily non-empty) subset of Candidates_i
               whose density in the Genesis window starting at Anchor_i
               could be the greatest
               if some headers extending the element were to suddenly arrive

Anchor_{i+1} :: Point
Anchor_{i+1} = intersection(Bests_i)
```

The definition of `Bests_i` has some subtleties. In particular, if some element
of `Bests_i` has a header that is at the end of or even beyond the Genesis
window starting at `Anchor_i`, then that element has a fixed density in that
window, since any new header extending the element would be outside of the
Genesis window. If the element's tip occurs earlier within the window, then
technically one extending header could arrive in each remaining slot in the
window. So the maximum possible density for an element of `Bests_i` is
conservatively defined as the number of its headers in the window plus the
number of slots after its last header in the window.

As our Syncing progress reaches the tip of the peers' chains, though, our peers
will begin informing us that they have no additional headers. This means the
above definition of `Bests_i` is too conservative, since the peer has informed
us that no headers extending beyond their claimed tip could possibly "suddenly
arrive" (TODO unless they're also syncing!). Thus we have the following refined
definition.

```
    Cutoff_i :: Natural
    Cutoff_i = the minimum over every prefix in Candidates_i of
               the count of headers in the Genesis window starting at Anchor_i

     Bests_i :: Set AnchoredFragment
     Bests_i = the (necessarily non-empty) subset of Candidates_i
               that contains each prefix for which
               the count of headers in the Genesis window starting at Anchor_i
               plus the count of slots that are
                   in the Genesis window starting at Anchor_i
                   > the slot of the prefix's tip
                   and <= the slot of the tip of peer's claimed selection
               is >= Cutoff_i
```

Indeed, this rule converges on Praos once the peer's have no more headers to
offer beyond the Genesis window, since we then actually have all the headers on
the peer's selected chain, not just a prefix of it (TODO unless they're also
syncing!). And that happily suggests a smooth transition from Syncing to
CaughtUp is possible.

Intuitively, an inferior candidate prefix is eliminated from consideration (and
thereby stops holding back `Anchor_j`) in one of two ways. Either that chain
ends in the window (while the better chains don't) or else the node eventually
receives enough of each prefix in the window to see that the number of remaining
slots in the window for a relatively sparse prefix is too small for that prefix
to ever be able to catch up to however many headers the prefix with the most
headers has.

TODO the above is sound as a description of overall chain selection. But if
we're going to leave the logic in the ChainDB untouched (ie select the longest
path in the VolDB), as we had hoped to do, then we won't be able to prevent the
node from immediately selecting a block it just forged. Which is a problem.
However we resolve that impedance mismatch might have similarities with
Deferred Validation (being able to judge (speculatively) and propagate a block
we haven't yet validated).

It's a notable special case that Genesis prefix selection is nearly as
aggressive as Praos chain selection when all peers are offering the same chain.
The only delay is waiting for every peer to give us enough headers instead of
just the one. TODO Mention the jumping optimization idea.

## A new DoS vector in Syncing

While we're in the Syncing state, an adversarial peer could claim their tip is
at or beyond the Genesis window (even if they don't have such a header). This
would maximize the count of remaining slots in their Genesis window, which
vastly increases the chance that their chain remains in the `Bests` set, which
in turn causes `Anchor_j` to be shorter than it could have been. They would
thus prevent a syncing node from selecting any more of the honest chain, since
prefix selection wouldn't be able to choose between their offering and the
honest chain.

* Their selection has a low block count, but a high number of remaining slots.

* The honest chain has a much higher block count, but a low number of remaining
  slots.

* Since both are about the lowest options block count, both will remain in
  `Bests`.

For this reason, while in Syncing, a node must require that the headers sent by
each peer improve over time. EG on average (ie leaky token bucket), every 10
seconds or so they have sent a header with a higher block number than any they
had sent before. If a peer can't even maintain that average for ~1100 byte
messages, then they're probably not actually sending me a real historical chain
(ie one that does not require any ChainSync `MsgRollback`s). Or their network
connection is so poor that I should replace them with a new peer anyway.

# Why Genesis prefix selection can be slower than Praos chain selection

It was decided the node should still mint blocks when in Connecting and Syncing.
Any honest nodes minting stale blocks (creating several short forks) could
slightly help the adversary create alternative chains, but the risk of many
honest nodes skipping many slots in which they could have created a competitive
chain is more dangerous. If a node leaves CaughtUp just before the slot it
leads, then it will likely mint the same block it would have minted in CaughtUp.
There are two subtleties.

* In that case of such lucky timing, the node won't select the block it just
  minted! Because doing so would advance its immutable tip, which is never
  allowed in Connecting and only allowed in Syncing when all of our peers
  similarly advance their immutable tip. TODO Note that Deferred Validation
  would let us propagate our minted block without having selected it! If all
  our peers select it, then we could too, even in Syncing. TODO Would DV be
  able to thereby unclog the network if all honest nodes were in Syncing? I
  think so, unless a peer of ours has a Syncing peer that branches off `k`
  before our minted block?

* If we were in Connecting or Syncing for a while before leading, it's possible
  that our minted block will not be competitive, since our selection may have
  become conservative (eg truncated to a `k`-extension of `Anchor_j`).

# BlockFetch in CaughtUp

TODO download capacity could be a limiting factor: so which blocks to download
when faced with competing chains? The Tse paper
https://eprint.iacr.org/2021/1545 says to download blocks along the chain with
the freshest block.

TODO The current code does a hybrid of preferring the longest blocks offered by
the peers that are most likely to be able to send us all the requested blocks
within two slots (magic number!).

TODO We discussed this with the researchers and Alex proposed a couple options,
but then they found the Tse paper and we seem to all think that rule is sound.
Something like: if you purposely withhold the block I want, you can only do
that so long, since someone else will forge a new most desirable block soon
enough.

TODO What do I do once I have that freshest block? How do I spend my excess
download capacity? Favor the second-freshest block? Favor any chains that are
longer than the freshest block? Something else?

# Appendix: the former Eclipse _detection_ plan

As of 2022, the following text is archived here merely in case we later revisit
the idea of detecting eclipses by monitoring the chain density -- we are not
currently planning to do so.

The above describes the expected path through the Genesis state machine for a
(re)joining node, but there are other paths as well. If at any moment we detect
an eclipse (see below), we transition back to Connecting as an attempt to
escape. (TODO Maybe we need to check for eclipse before transition to CaughtUp
so that a node that is fully eclipsed won't only try to escape once ever and
then just give up?)

The rule that triggers this transition has non-trivial consequences.

## Total versus active stake

There's an important distinction to start with.

> _Total stake_ is all stake registered on the chain. _Active stake_ is the
  stake held by CaughtUp block-producing nodes.

The Shelley leadership schedule randomly distributes leadership opportunities
according to total stake, such that there's an active slot (ie slot with >0
leaders) on average once per 20 slots (ie a Geometric distribution). But only
the holders of active stake will actually mint blocks in the slots they lead.
(Technically, a node could mint a "late" block sometime after its labeled slot,
but honest nodes aren't supposed to do that.)

The underlying Ouroboros assumption is that, at all times, the _active_ stake
is dominated by the honest stakeholders (ie the honest stakeholders, with
properly configured nodes, that remain in CaughtUp enough of the time, and so
on). If that's always true, then (prefixes of) the honest chain will win every
density comparison forever.

## Detection scenarios

If I'm eclipsed, then I won't see (prefixes of) the honest chain. How can I
notice? We consider two cases.

* For a fresh node that's joining the network for the first time -- one that
  was never before CaughtUp -- the only hope for detecting eclipse is for the
  chain you're fed to be objectively too sparse. EG a chain that has blocks in
  less than 40% of the active slots that that chain's ledger states' leadership
  schedules identify as active. But this rule would backfire catastrophically
  if Cardano mainnet ever reached (near) that threshold, thus this rule would
  be introducing a new failure mode if enough honest stakeholders gave up
  Cardano (without unregistering their stake) even in the absence of an
  adversary.

* For a node that's rejoining the network, there's another option, since it
  already has selected a prefix of the (presumably) honest chain. If we were to
  become eclipsed by an adversary, which can never have more than half of the
  active stake, then -- given a "long enough" slot interval on either side of
  density change point -- we should see a portion of our recent chain when the
  density plummeted.

We could instead collapse those two by defining the density up to the genesis
block to be two times whatever the absolute cutoff is. So I'll only discuss the
rejoining case from now on.

## Balancing false positives and missed alarms

The key challenge is false positives -- a node wrongly concluding it's
eclipsed. This possibility is unavoidable, since we're working with partial
information, much of which is random. As a drastic example: there is a
probability that some epoch has no active slots, an empty leader schedule! That
probability is astronomically small, but not zero. So we have to balance the
sensitivity of our detection against its robustness, false alarms versus missed
alarms. TODO also its promptness: we'd ideally detect an eclipse sooner rather
than later... is a slow alarm just a partially/temporarily missed alarm?

We can't judge that balance well without understanding the costs of each.

* A missed alarm means some nodes were eclipsed and did not notice. In this
  case the eclipsed nodes suffer the direct consequence, such as selling a
  yacht without receiving payment on the honest chain.

  The network as a whole wouldn't really notice the absence of a few nodes'
  stake, which is inactive while they're eclipsed. On the other hand, it could
  be disastrous if a large portion of the network were eclipsed simultaneously
  for an appreciable duration/frequency/duty cycle (eg by a botnet saturating
  our P2P peer selections).

* A false alarm means that a node concluded it is eclipsed even though it
  wasn't. The direct consequence is that the node leaves CaughtUp and goes back
  to Connecting. If we put enough defensive mechanisms in place in Connecting
  and Syncing (mostly timeouts), then such a (non-eclipsed!) node will take at
  most a few minutes to return to CaughtUp (each false alarm).

  A node in Connecting or Syncing is more likely to waste its leadership
  opportunities by minting blocks that do not extend the best chain in the
  network. This is because no growth (in Connecting) and Genesis prefix
  selection (in Syncing) is slower to select that chain than is Praos chain
  selection (in CaughtUp).

  Again, the network as a whole wouldn't notice the absence of a few nodes'
  stake, but it would be disastrous if the adversary could induce false alarms
  in significant portions of the honest stake with appreciable
  duration/frequency/duty cycle.

## The eclipse detection rule

TODO look for a recent drop in density

TODO if "recent" is too short of a duration, it's not enough coin tosses to be
robust: the false alarm rate will be too high

TODO if "recent" is too long of a duration, then we won't detect eclipses very
quickly

TODO False alarms may harm the eclipsed node because it might mint a
non-competitive block. If that happens to too many nodes/too often, then it
lowers the honest chain growth.

TODO Missed alarms harm the eclipsed node, because 1) it mints non-competitve
blocks while eclipsed and 2) the table of settlement times are misleading during
an eclipse (eg a yacht merchant could get bamboozled). If an eclipse lasts long
enough, then the node can't rejoin the honest network without intervention. If
too many nodes are eclipsed at once (or for so long they can't recover). If that
happens to too many nodes/too often, then it lowers the honest chain growth.

TODO False alarms inherently cause harm in the absence of attacks, but might
potentially be easy enough for an adversary to trigger that they become an
attack vector of their own!
