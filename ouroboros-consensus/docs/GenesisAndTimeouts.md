# The Problem

Genesis as currently described in the chapter is vulnerable to an attack. The
attacker can prevent any of its honest peers from fetching blocks past a point
essentially of its choice. We're brainstorming how to address this. This
document explains the attack and lays out is one possible way to eliminate it.

Background definition. My peer's offering (chain/prefix) has a _known density_
in a particular Genesis window when either (i) the peer sends me a header that
is beyond that window or else when (ii) it claims that it has no more headers
(ie the latest header it sent me is labelled as the its current tip).

Minimal demonstration. Suppose I have two peers, one honest peer and one
attacker. The whole attack, to a first approximation, is simply to stop sending
me headers despite having already claimed to have more. This ensures I will
eventually know the density of the honest peer's offering without knowing the
density of the attacker's offering. The Genesis prefix selection rule does not
commit prematurely, so it will assume that an offering of unknown density could
end up being the most dense, and will therefore wait until we also know the
attacker's offering's density before selecting any part of the honest peer's
offering. Thus, in this state, I won't fetch any blocks until the attacker
informs me of their offering's density or disconnect from them for some reason.

Immediate minor refinement. We can fetch blocks from amongst the known density
offerings, essentially up to the block that would prevent us from rolling back
to any of the unknown density offerings (since one such offering might
eventually be revealed as the densest). However, this refinement only allows me
to fetch ~k more blocks than I otherwise would have; I'll still get stuck once
that happens. But it does delay becoming truly stuck until very many blocks
later (~1.5 days worth assuming the vast majority of stake in the net is
extending my honest peer's offerings). (TODO The possibility of a peer with an
unknown density rolling back makes this logic -- at least as written here --
unsound, doesn't it? Our intersection could get deeper. TODO But I don't want
to mention rollbacks yet in the narrative here.)

Existing timeouts. The current ChainSync protocol does require some degree of
promptness. If the attacker simply refuses to respond at all to my requests for
new headers, I will disconnect from them. Here are relevant code excerpts.

```
stdChainSyncTimeout :: IO NTN.ChainSyncTimeout
stdChainSyncTimeout = do
    -- These values approximately correspond to false positive
    -- thresholds for streaks of empty slots with 99% probability,
    -- 99.9% probability up to 99.999% probability.
    -- t = T_s [log (1-Y) / log (1-f)]
    -- Y = [0.99, 0.999...]
    -- T_s = slot length of 1s.
    -- f = 0.05
    -- The timeout is randomly picked per bearer to avoid all bearers
    -- going down at the same time in case of a long streak of empty
    -- slots. TODO: workaround until peer selection governor.
    mustReplyTimeout <- Just <$> randomElem [90, 135, 180, 224, 269]
    return NTN.ChainSyncTimeout
      { canAwaitTimeout  = shortWait
      , intersectTimeout = shortWait
      , mustReplyTimeout
      }
  where
    randomElem xs = do
      ix <- randomRIO (0, length xs - 1)
      return $ xs !! ix

shortWait :: Maybe DiffTime
shortWait = Just 10

-- | Time Limits
--
-- > 'TokIdle'               'waitForever' (ie never times out)
-- > 'TokNext TokCanAwait'   the given 'canAwaitTimeout'
-- > 'TokNext TokMustReply'  the given 'mustReplyTimeout'
-- > 'TokIntersect'          the given 'intersectTimeout'
timeLimitsChainSync :: forall header point tip.
                       ChainSyncTimeout
                    -> ProtocolTimeLimits (ChainSync header point tip)
timeLimitsChainSync csTimeouts = ProtocolTimeLimits stateToLimit
  where
    ChainSyncTimeout
      { canAwaitTimeout
      , intersectTimeout
      , mustReplyTimeout
      } = csTimeouts

    stateToLimit :: forall (pr :: PeerRole) (st :: ChainSync header point tip).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit (ClientAgency TokIdle)                = waitForever
    stateToLimit (ServerAgency (TokNext TokCanAwait))  = canAwaitTimeout
    stateToLimit (ServerAgency (TokNext TokMustReply)) = mustReplyTimeout
    stateToLimit (ServerAgency TokIntersect)           = intersectTimeout
```

For our minimal demonstration, this codes means that, as a ChainSync client, we
will wait 10 seconds for a response to our `MsgRequestNext` (which we send
ASAP, as pipelining allows), unless they respond with `MsgAwaitReply`, in which
case we'll wait a large additional delay before disconnecting, somewhere
between 90 to 269 seconds (the value is fixed per connection). Note that the
protocol prevents them from following their `MsgAwaitReply` with another
`MsgAwaitReply`.

Naive reason that this timeout is insufficient. At first glance, it seems this
timeout requires a peer to send at least one header somewhere between every
10+90 seconds (best-case) and every 10+269 seconds (worst-case) or else we'll
disconnect. Even if the attacker merely feeds us the honest chain that slowly,
we'll fall very far behind our peers. Eventually the attacker's latest header
will cause their offering to have known density, so we'll select a few blocks
from it, advance the Genesis window, and then find that their offering again
has unknown density, repeating the cycle as they slowly send us more headers.

Actual reason that this timeout is insufficient. The reality is worse than
that. The timeout doesn't require that the peer's next message is a
new/competitive header. Nothing currently prevents them from responding with a
`MsgRollBackward` and then sending us even the very same offering. Today, they
could simply defeat the timeout by rolling back 1 and rolling forward that same
1 in a (slow) wasteful loop. Such an attacker could indefinitely prevent us
from advancing past the block they're providing. (Which would likely cause us
to violate our peers' `mustReplyTimeout`, thereby causing our peers to
disconnect from us.)

That completes the description of the problem and the current state of the
relevant logic (eg the existing timeouts and when they apply).

# Fix: Leverage `Δ`

Can we improve our timeout scheme, since it can't currently defend against this
attack?

Stepping back, the first question is: How short could we make the timeout
without risking disconnecting from honest peers? In other words, what is the
most promptness we could require?

Context: Generally, we've made the assumption that the `Δ` parameter in the
security analysis is about 5-10 seconds. That means: Under nominal
circumstances amongst honest peers, a new block is forged somewhere in the
network will have reached all nodes within `Δ` seconds. Specifically, in the
simplest case, that `Δ` limit means some spanning tree of edges will each have
to send one `MsgRollForward` before 5-10 seconds have passed. (TODO Say more
about how 5-10 seconds is *much* more demanding, even during nominal
circumstances, in the non-simplest case? EG If two longest chains exist that
have a, say, 5 block deep intersection. That doesn't seem to help this
narrative, at least not yet.)

I realize now that it is no coincidence that `canAwaitTimeout` and our choice
for `Δ` have similar scale.

Refinement #1a. This scale of the `Δ` value, then, does suggest a much more
aggressive timeout, but only under the assumption that some "new" block is
in-flight. The existing `mustReplyTimeout` is so long because we're try to
account for the fact that a new block might not be forged for a while: there
could be a long sequence of empty slots in the leader schedule. Therefore,
`mustReplyTimeout` should only be relevant once the peer has sent us their
current selection (which means their offering would have known density,
unblocking us!). In other words, my peer can't send `MsgAwaitReply` to me
unless we're already fully synced. That seems easy to enforce and would, at
least under the naive reasoning, mean the attacker has to send a new header
every `canAwaitTimeout = 10` seconds. That's 9x to 27x faster than before.

But 10 seconds still seems too slow. If there was just one attacker and it was
part of our only path to some portion of the network, then this would
technically double `Δ`: it'd be 5-10 seconds already for the honest nodes in
the network, plus 10 more for the one attacker node between us and whichever
node in that portion of the network just forged the new block. Recall that `Δ`
is the total propagation time, and total propagation requires several hops.
Here, instead, we're applying a timeout to our immediate peer, so presumably
they should be able to satisify a much tighter per-message timeout than 100% of
`Δ` itself (TODO this is where the richer competing-chains semantics of `Δ`
would suggest an even smaller fraction of `Δ`).

Refinement #1b. This all suggests that the timeout for a peer who has not yet
sent us their tip should be something like half a second, likely less. And if
we know there are several headers remaining for them to send us, their average
per-message latency should be much much lower, since pipelining and TCP
congestion window growth should quickly allow for multiple headers per TCP
packet. (TODO Appropriate to use some sort of leaky bucket formulae to control
this?)

Using a much more aggressive timeout when they haven't yet sent us their tip
would prevent the naive attacker from slowing us much. However, the attacker
using rollbacks could still easily circumvent that.

# Fix: Prevent wasteful rollbacks

(TODO We already spent a long while thinking about this -- this is a key part
of the ChainSync rate-limiting design work we were doing circa June 2021. For
now I'm just going to discuss what I think of as the simplest option we ever
considered, even though we had so far thought we could do better. Maybe
starting with the simplest fix we can tolerate is good enough for now if only
to unblock Genesis.)

Refinement #2a. As with `MsgAwaitReply`, We can require that the peer can only
send `MsgRollback` once they've finished sending us their entire (previous)
selection. The downside is that this may -- rarely -- cause a slight slow down
in the propagation of the honest chain, but only when multiple longest chains
with a relatively deep fork are propagating at the same time, which is a rare
and ephemeral event. (TODO worry about cascading errors?) But the upside is
that the attacker can't send us a wasteful rollback: since we're seeing all
tips they've told us about, we can confirm that the sequence of their tips is
strictly increasing in preference (eg each is denser/longer than the previous
one was or else has better tiebreakers/etc). And once they send us a rollback
then they have to meet the newly-aggressively per-message timeout until we've
caught back up to their tip.

Refinement #2b. TODO Do we also need to restrict the evolution of the
annotation they send that identifies their current tip? I'm not sure. They
can't rollback until they tell us we have their tip. Which would unblock us.
But they could immediately send a rollback, thereby blocking us again, and so
on. However, they can't send an infinite number of rollbacks: each one
obligates them to send a tip that was better than the last. So the question is
something like: how many such tips could an adversary with 50% stake
manufacture "at once" and so all together how long could they use that to block
us? TODO Maybe we need (just slightly) debounce the knownness signal?

Refinement #3. In addition to this new requirement on when `MsgRollBackward` is
valid, we shoud also check that they're not rolling back farther than they need
to. If they have to demonstrate that each rollback is worthwhile, then the
first `MsgRollForward` after a `MsgRollBackward` must be different than the
last header we just rolled back.

(TODO We've so far avoiding making the the rules different during bulk sync
than once we're caught up, but Edsko did point out that there should be no
rollbacks during bulk sync! I wonder if the refinements in this section would
enforce that en passant.)

# Fix: Prevent OpCert ratcheting

TODO The adversary can grind each of the leadership opportunities to create an
"effectively infinite" seqeuence of strictly more preferable blocks, all in the
same slot. If they can forge that sequence fast enough, they could send us the
corresponding `MsgRollBackward` `MsgRollForward` pairs in order to avoid the
timeout, while still never providing us an offering with known density. (If our
timeout is aggressive enough, this grind would require _a lot_ of compute,
wouldn't it?)

TODO Is this as simple as not letting their _tip_ have the same (cold VK,
SlotNo) pair more than two times? TODO The previous paragraph doesn't apply
here, since the previous paragraph assumed they were never sending us their tip.

TODO Suppose the first paragraph was talking about their tips. In this scenario,
they'd be rapidly alternating between known and unknown density. Is that totally
fine -- we don't have to worry about ratcheting here?

TODO Edsko proposes our `MsgRollback` logic should actually require _increased
length_, ie it shouldn't depend on tiebreakers.

-----

Notes from discussion with team on 2021 July 30.

  * We all think Refinement #1a is sound. Also Refinement #3, though it's not
    necessarily what we had before. The wording we came up with is: when you
    send me a `MsgRollBack` you must then immediately and uninterruptedly send
    me enough headers from the chain that caused you to send that `MsgRollBack`
    for it to be better than the latest header you had sent me before that
    `MsgRollBack`. (Note: by "better" Edsko suggested that we actually use
    length, not even allowing for tiebreakers. This is a simplicity-vs-latency
    trade-off, so we need to consider. But it could be a big simplification.)

  * Refinement #2a is worded too strongly. Specifically, it's OK to roll back
    before we reach your tip.

  * Edsko schemed up an attack that would still stall us, despite all the above.
    The adversary has a lot of stake, and sends us their blocks as slowly as
    `MsgAwaitReply` will allow. Suddenly, once we're ~k ahead of them, they
    claim to have a new tip. Now they're subject to `canAwaitTimeout = 10`, but
    they have a reserve of blocks, a large fraction of k. (It's 1 - 1/x, where x
    is the ratio `mustReplyTimeout / growthRateOfHonestChain`.)

    (TODO To what extent can they do the above even with just the honest chain?
    IE with no stake?)

  * Only current idea to address that, that we're comfortable with: they
    shouldn't be able to invoke `mustReplyTimeout` more than once in a row. More
    generally: it should be an amortized timeout/leaky bucket/etc sort of thing.
    Which is more restrictive than the current timeout logic. The underlying
    justification is: why is it taking them _so long_ to learn about any new
    blocks? The fundamental notion of Chain Growth should prevent them from
    having to wait so long. Recall the comment in `stdChainSyncTimeout`. Several
    `mustReplyTimeout`s in a row corresponds to a _much more_ unlikely duration
    of empty slots.

    ```
    -- The timeout is randomly picked per bearer to avoid all bearers
    -- going down at the same time in case of a long streak of empty
    -- slots. TODO: workaround until peer selection governor.
    ```

  * ^^^ At first we were thinking of this idea, at a high-level, as a matter of
    enforcing (something akin to) `\Delta` more directly. IE, if a peer has a
    tip, they must be able to send it within `\Delta` -- regardless of how many
    MsgRollForwards it requires them to do so. However, this is not quite right.
    `\Delta` regards the propagation across the network, but here we're talking
    about a direct peer-to-peer latency, just one hop. So we can be more
    aggressive than `\Delta`.

  * ^^^ Rephrased: if some peer is preventing me from advancing, then they are
    providing headers more slowly than any honest node would. It's OK to
    disconnect from them if they're not the honest chain. So the question is:
    how to conclude "this peer is communicating so slowly that they can't be
    honest".

  * TODO A whole new question: What happens if a peer rapidly alternates between
    known density and unknown density? (For whatever reason. Adversarial or
    honest, regardless.) Would that cause problems?

  * Bigger TODO: should BlockFetch be trying to download all peers' chains, even
    the ones that aren't currently offering us the best header? In Praos, the
    "currently non-optimal chain" may end up being extending and thereby become
    the optimal chain. Relatedly, the semantics of `\Delta` in the paper might
    be matched better by an implementation that is making a best effort to
    download _all_ chains (at least within some reasonable limit).

  * The general `\Delta`-focused invariant is something like: "I know that I've
    received all honest blocks that were forged up to `\Delta` slots ago." --
    but obviously that's only true if I'm doing my part by actually trying to
    download (potentially-)relevant blocks ASAP.
