# Chain sync client specification

Note on terminology: by "intersection" between two chains we will always mean
the most _recent_ intersection (could be genesis).

## Key invariant

0.  We will never roll back more than `k` blocks.
1.  This means we are not interested in upstream nodes whose chains fork from
    our chain more than `k` blocks ago.
    (If an upstream node does not satisfy this condition, we will disconnect;
    it will be the responsibility of the network layer to decide whether or not
    to try reconnecting to such nodes later.)
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

## Chain evolution

Property (0) will continue to hold, which means that all of (1-7) must continue
to hold. If any of those _cannot_ continue to hold, we should disconnect from
the upstream node.

### Our chain changes

When _our_ chain evolves, we get a new fragment. If our new fragment still
intersects with their chain, all is well. Otherwise, we initiate the
intersection finding protocol again, establishing a new fragment for the
upstream node.

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
It should always be safe to trim to the intersection point.

NOTE: This may leave their fragment shorter than `k`, thus not allowing them
their full rollback. However, this is okay: if their fragment _is_ shorter than
`k` due to trimming to the intersection point, and they _do_ roll back to a
point before their fragment, this new intersection point would be too far from
our tip, and we should disconnect from them.
