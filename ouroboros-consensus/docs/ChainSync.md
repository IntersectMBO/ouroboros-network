# Chain sync client specification

Note on terminology: by "intersection" between two chains we will always mean
the most _recent_ intersection (could be genesis).

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
4.  We use their fragment as a proxy for their chain.
5.  We must be able to _adopt_ their chain, and so their _fragment_ should give
    us enough information to do so.
6.  From (5), this means that their fragment must contain all blocks after the
    intersection with our fragment (because those blocks are new for us).
7.  From (6), it means the intersection between their _chain_ and our _fragment_
    must lie on their fragment: if not, the intersection would be _before_ their
    fragment, which would mean that we would miss some blocks.

Note that if both nodes are following the same and are up to date, the
intersection will be the _tip_ of both fragments.    

## Chain evolution

Property (0) will continue to hold, which means that all of (1-7) must continue
to hold. If any of those _cannot_ continue to hold, we should disconnect from
the upstream node.

### Our chain changes

When _our_ chain evolves, we get a new fragment. If our new fragment still
intersects with their chain, all is well. Otherwise, we throw away their
fragment, wait for the pipeline to drain, initiate the intersection finding
protocol again, and establish a new fragment for the upstream node.

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
   a. This block does not exist on our fragment (they are either ahead of us,
      or they are on another fork). The intersection point does not change.
   b. This block _does_ exist on our fragment (they are behind us). The
      intersection points shifts forward one block, but must still exist on
      both fragments. Moreover, since it moved _forward_, it can't violate the
      "intersection point within `k`" condition.

2. Roll back. Two possibilities:
   a. The roll back point is at or after the old intersection point (before the
      rollback). All is fine.
   b. The roll back point is /before/ the old intersection point. All blocks
      before the old intersection point must be shared between their chain and
      our chain, so the new intersection point must /be/ the rollback point.
      If that rollback point lies on our fragment (and hence is within @k@),
      all is good; otherwise, we disconnect.
   NOTE: We could additionally (and optionally) check that they do not roll
   back more than `k`, and if they do, treat them as adversarial (disconnect).

## Trimming

Our fragment is always `k` long (unless near genesis), so no trimming needed.
However, to avoid unbounded memory usage, we should trim their fragment.

We have established that the intersection between their chain and our _chain_
must lie on both _fragments_, and moreover, that this intersection will be
within `k` from our tip; since our fragment is `k` long, it means the
intersection will be somewhere along our fragment.

This means that we should allow the remote node to roll back from its current
intersection point to our chain to any point on our fragment, but we do not need
to allow it to roll back more than that. This means we can always trim their
fragment so that it is anchored at our anchor point.

- In the common case that both nodes are following the same chain and both up
  to date, this would leave both fragments identical.
- In the extreme case that they are `k` behind, it would leave their fragment
  empty.
- More generally, this may leave their fragment shorter than `k`, thus not
  allowing them their full rollback. However, this is okay: if their fragment
  _is_ shorter than `k` due to trimming to our anchor point, and they _do_ roll
  back to a point before their fragment, this new intersection point would be
  too far from our tip, and we should disconnect from them.
