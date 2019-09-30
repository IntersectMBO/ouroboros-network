# Chain sync client specification

Note on terminology:

- By "intersection" between two chains we will always mean the most _recent_
  intersection (could be genesis).
- "Their" fragment, "their" chain refers to the chain of the upstream node;
  "our" fragment, "our" chain refers to the chain of the local node.

## Key invariant

0.  We will never roll back more than `k` blocks.
1.  This means we are not interested in upstream nodes whose chains fork from
    our chain more than `k` blocks ago.
    - If an upstream node does not satisfy this condition, we will disconnect.
    - Note that this includes nodes that are on the same chain but behind.
    - It will be the responsibility of the network layer to decide whether or
      not to try reconnecting to such nodes later.
2.  From (1), the intersection between their chain and our chain must be within
    `k` blocks from our tip.
3.  Since our fragment will be anchored `k` back, from (2) we get that the
    intersection between their chain and our chain must lie on our fragment.   
    (NOTE: In the case of corruption of volatile DB -- or being near genesis --
    our fragment may be shorter than `k`, but even in that case, the length of
    our fragment will dictate our maximum rollback.)
4.  We use their fragment as a proxy for their chain.
5.  We must be able to _adopt_ their chain, and so their _fragment_ should give
    us enough information to do so.
6.  From (5), this means that their fragment must contain all blocks after the
    intersection with our fragment (because those blocks are new for us).
7.  From (6), it means the intersection between their _chain_ and our _fragment_
    must lie on their fragment: if not, the intersection would be _before_ their
    fragment, which would mean that we would miss some blocks.

Note that if both nodes are following the same chain and are up to date, the
intersection will be the _tip_ of both fragments.    

## Chain evolution

Property (0) will continue to hold, which means that all of (1-7) must continue
to hold. If any of those _cannot_ continue to hold, we should disconnect from
the upstream node.

### Our chain changes

When _our_ chain evolves, we get a new fragment. If our new fragment still
intersects with their fragment, all is well. Otherwise, we throw away their
fragment, wait for the pipeline to drain, re-initiate the intersection finding
protocol again, and establish a new fragment for the upstream node.

Note: It's not impossible that such a new intersection might exist. For example,
if both the upstream node and we are part of a network that temporarily got
disconnected from the rest of the world, we might at roughly the same time
attempt to switch to the same, much longer, chain when we get reconnected.
The check is also not expensive, so there is no need to try to be more clever
here and decide if we can disconnect without doing the check.

This is a simple approach, but we need to argue why simply draining the pipe
(rather than executing the instructions) and throwing away their fragment is
not too wasteful.

The number of headers we might potentially have to download again due to not
executing the drained instructions is limited by the high watermark, which is
small; this is no big deal. However, the fragment we had before we threw it
away could potentially be very long, so we have to make a case that throwing
it away is okay.

The scenario is something like this:

```
                                k
                           /~~~~~~~~~~\                                 

..................\........------------ our fragment
                   \
                    ----------------------- their (potentially long) fragment
```


Since we will _never_ adopt the chain for which we have all those headers
already (since this would require us to roll back more than `k` blocks),
the headers we have downloaded will never be useful, and so we can just discard
them.

(There is one special case here, where there is no intersection between our
fragment and their fragment because we are far _ahead_ of them on the _same_
chain; but in this case, starting afresh is only beneficial.)

### Their chain changes

We get notified through the chain sync protocol about updates to their chain:

1. Roll forward. Two possibilities:
   a. This block does not exist on our fragment (they are either ahead of us on
      the same chain, or they are on another fork). The intersection point does
      not change.
   b. This block _does_ exist on our fragment (they are behind us on the same
      chain). The intersection points shifts forward one block, but must still
      exist on both fragments. Moreover, since it moved _forward_, it can't
      violate the "intersection point within `k`" condition.

2. Roll back. Two possibilities:
   a. The roll back point is at or after the old intersection point (before the
      rollback). All is fine.
   b. The roll back point is /before/ the old intersection point. All blocks
      before the old intersection point must be shared between their chain and
      our chain, so the new intersection point must /be/ the rollback point.
      If that rollback point lies on our fragment (and hence is within @k@),
      all is good; otherwise, we disconnect.
   Notes:
   - We could additionally (and optionally) check that they do not roll
     back more than `k`, and if they do, treat them as adversarial (disconnect).
     (This is anyway limited by maximum rollback supported by the `ChainState`.)
   - This might mean we could disconnect from peers that do unnecessarily
     rollbacks; we consider this acceptable (they should not be doing that in
     the first place).

## Trimming

Our fragment is always `k` long (unless near genesis or data loss), so no
trimming needed. However, to avoid unbounded memory usage, we should trim their
fragment.

We have established that the intersection between their chain and our chain
must lie on both _fragments_, and moreover, that this intersection will be
within `k` from our tip; since our fragment is `k` long, it means the
intersection will be somewhere along our fragment.

This means that we should allow the remote node to roll back from its current
intersection point with our fragment to any (earlier) point on our fragment, but
we do not need to allow it to roll back more than that. This means we can always
trim their fragment so that it is anchored at our anchor point.

- In the common case that both nodes are following the same chain and both up
  to date, this would leave both fragments identical.
- In the extreme case that they are `k` behind, it would leave their fragment
  empty.
- More generally, this may leave their fragment shorter than `k`, thus not
  allowing them their full rollback. However, this is okay: if their fragment
  _is_ shorter than `k` due to trimming to our anchor point, and they _do_ roll
  back to a point before their fragment, this new intersection point would be
  too far from our tip, and we should disconnect from them.
- This places a bound on the length of their fragment: the `ChainState` will
  place a limitation on how far into the future we can validate headers; for
  PBFT this is `2k` from the intersection point. In the worst case (in terms
  of fragment length) the intersection point is the tip, and so the longest
  fragment we will ever have in memory is `3k` headers.
- We could, if needed, bind this more tightly still; as long as we have a
  sufficient number of headers to determine if their chain is preferred over
  ours (this needs careful consideration when adopting genesis).
