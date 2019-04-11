# The Chain Database

The immutable database records a linear prefix of our current chain; the
volatile DB records a (possibly fragmented) tree of extensions:

```
                               /---
                              /
                             /
|---------------------------|
                             \
                              \
                               \---
       IMMUTABLE DB         VOLATILE DB
```

When we start up the system we must find the best possible path through the
volatile DB and adopt that as our current chain; then every time a new block is
added to the volatile DB we have recompute what the best possible path is now.
In other words, we maintain the invariant that

**Invariant.** The current chain is the best possible path through the volatile DB.

In an ideal world this would mean we have some kind of specialized data
structure supporting

* Efficient insertion of new blocks
* Efficient computation of the best chain

It's however not at all clear what such a data structure would look like if we
don't want to hard-code the specific chain selection rule. Instead we take a
simpler approach.

## Preliminary: consensus protocol chain selection

A choice of consensus protocol includes a choice of chain selection algorithm,
a binary relation `(⊑)` between chains indicating which chains are "preferred"
over which other chains. In the simplest case we just prefer longer chains
over shorter ones:

```
  C ⊑ C'  iff  length C ≤ length C'
```

More realistic protocols might involve checking things such as delegation
certificate issue numbers (Permissive BFT) or chain density (Ouroboros Genesis).
However, one property that all chain selection algorithms we are interested in
share is the following:

**Property "Always Extend".**

```
  ∀ C, B ∙ C ⊏ (C :> B)
```

In other words, if we can extend a chain, we should.

**Definitions.**

1.  Given a set of blocks `V`, let

    ```
      candidates(I, V)
    ```

    be the set of chain fragments anchored at `I` using blocks picked from
    `V`.[^forwardIndex] This set has some properties:

    a.  It is prefix closed

        ```
          ∀ F, B ∙ if (F :> B) ∈ candidates(I, V) then F ∈ candidates(I, V)
        ```

    b.  Conversely, we have

        ```
          ∀ F, B ∙ If F ∈ candidates(I, V) then (F :> B) ∈ candidates(I, V ∪ {B})
        ```

        provided that `F :> B` is valid chain.

    c.  Adding blocks doesn't remove any candidates

        ```
          candidates(I, V) ⊆ candidates(I, V ∪ {B})
        ```

    d.  The only new candidates in `candidates(I, V ∪ {B})` must involve `B`; i.e.

        ```
          ∀ F ∈ candidates(I, V ∪ {B}) ∙ F ∈ candidates(I, V) or F = (... :> B :> ...)
        ```

2.  We overload the notation `(⊑)` to mean that a fragment `F` is preferred over
    all fragments in a set of candidates:

    ```
      C ⊑ F  iff   F ∈ C  and  ∀ F' ∈ C ∙ F' ⊑ F
    ```

**Lemma "Prefer Empty".**

If `C ⊑ ε` (for empty fragment `ε`) then `ε` must be the _only_ candidate in `C`.

_Proof (sketch)_.

Suppose we have another candidate[^anchored] `(B :> ...)` in `C`. Then we'd have

```
  ε ⊏ (B :> ...)
```

by "Always Extend", violating the assumption `C ⊑ ε`. ∎

[^anchored]: All candidates in `C` must have the same anchor.

[^forwardIndex]: In order to compute `candidates` efficiency the volatile
DB must support a "forward chain index", able to efficiently answer
the question "which blocks succeed this one?".

**Lemma "Local Chain Selection".**

If

```
  candidates(I, V) ⊑ F
```

then for all blocks `B` such that `F :> B` is a valid chain,
there exists an `F_new` extending `F :> B` such that

```
  candidates(I, V ∪ {B}) ⊑ F_new
```

_Proof (sketch)._

Let's first consider the case where `F` is non-empty, i.e., `F = F' :> B_pred`,
with `B_pred` the predecessor of `B` (i.e., our current tip).

1.  `B_pred` cannot be the tip of any other candidate in `candidates(I, V)`
    (because two candidates with the same tip must _be_ the same candidate).

2.  Since `candidates(I, V) ⊑ F`, we know that there cannot be any extension of
    `F` in `candidates(I, V)` and hence there cannot be any other candidate that
    contains `B_pred`.

3.  Since the new candidates in `candidates(I, V  ∪ {B})` must involve `B`
    (definition 1.d, above), this therefore means they can only be `(F :> B)` or
    further extensions thereof. We can compute all possible such extensions

    ```
      candidates(B, V  ∪ {B})
    ```

    Then compare them using `(⊑)`, and use that to pick a preferred candidate
    `F_new`. Since this candidate is preferred over all extensions `(F :> B :>
    ..)`, which in turn is preferred over `F` (because they are extensions),
    which was preferred over all existing candidates, we must indeed have

    ```
      candidates(I, V ∪ {B}) ⊑ F_new
    ```

    as required.

The case where `F = ε` is simpler because in this case the empty fragment must
be the _only_ candidate (lemma "Prefer Empty", above), and so the reasoning in
step (3) applies immediately. ∎

## Invariant

Given the tip of the immutable database `I` and volatile database `V`, the
chain DB maintains a current fragment `F` such that

```
  candidates(I, V) ⊑ F
```

Technically speaking the type of `I` is `Maybe Block`, not `Block`, since the
immutable database may be empty. If that is the case, the predecessor of the
first block of `F` (if any) must be the genesis block.

## Initialization

The initialization of the chain DB proceeds as follows.

1.  Initialize the immutable DB, determine its tip `I`, and ask the
    ledger DB for the corresponding ledger state `L`.

2.  Compute

    ```
    candidates(I, V)
    ```

    ignoring known-to-be-invalid blocks (if any), and order them using (`⊑`) so
    that we process the preferred candidate first[^selectThenValidate]. We also
    ignore any candidates that are prefixes of other candidates (justified by
    the "Always Extend" property).

3.  Not all of these candidates may be valid, because the volatile DB stores blocks
    whose _header_ have been validated, but whose _body_ is still unverified
    (other than to check that it corresponds to the header).  We therefore
    validate each candidate chain fragment, starting with `L` each
    time[^ledgerState]. As soon as we find a candidate that is valid, we adopt
    it as our current chain. If we find a candidate that is _invalid_, we mark
    the invalid block and all its successors as invalid[^invalidSuccessors],
    and go back[^whyGoBack] to step (2).

[^ledgerState]: We make no attempt to share ledger states between candidates,
even if they share a common prefix, trading runtime performance for lower memory
pressure.

[^whyGoBack]: We recompute the set of candidates after marking some block(s) as
invalid because (1) those blocks may also exist in other candidates and (2) we
do not know how the valid prefixes of those candidates should now be ordered.

[^invalidSuccessors]: The chain sync client also depends on this information to
terminate connections to nodes that produce invalid blocks, so it is important
to mark _all_ invalid blocks.

[^selectThenValidate]: Technically speaking we should _first_ validate all
chains, and then apply selection only to the valid chains. We run chain selection
first, because that is much cheaper. It does not matter, since
```
  sortBy f . filter p = filter p . sortBy f
```
since `sortBy` is stable.

## Adding a block

When a new block `B` comes, we need to add it to the volatile DB and recompute
our current chain. We distinguish between the following different cases.

### Ignore

We can just ignore the block if either of the following is true.

*   the block was already in the volatile DB

    ```
      B ∈ V
    ```

*   the block is already in the immutable DB, _or_ it belongs to a branch
    which forks more than `k` blocks away from our tip

    ```     
      blockNo B <= blockNo I
    ```

    We could distinguish between between the block being on our chain or on a
    distant fork by doing a single query on the immutable DB, but it does not
    matter: either way we do not care about this block.

    We don't expect the chain sync client to feed us such blocks under normal
    circumstances, though it's not impossible (by the time a block is downloaded
    it's conceivable, albeit unlikely, that that block is now older than `k`).
    We may wish to issue a warning when this happens.

### Store but don't change the current chain

We store the block, but do nothing else, when either of the following is true.

*   We are missing one of the (transitive) predecessors of the block.

    We can check this by following back pointers until we reach a block `B'`
    such that `B' ∉ V` and `B' ≠ I`. The cost of this is bounded by the length
    of the longest fragment in the volatile DB, and will typically
    be low; moreover, the chain fragment we are constructing this way will be
    used in the switch-to-fork case.[^firstCheckTip]

    At this point we _could_ do a single query on the immutable
    DB to check if `B'` is in the immutable DB or not. If it is, then this block
    is on a distant branch that we will never switch to, and so we can ignore it.
    If it is not, we may or may not need this block later and we must store it;
    if it turns out we will never need it, it will eventually be garbage
    collected[^gc].

    Alternatively, and easier, we can also just omit the check on the immutable
    DB and just assume we might need the block and rely on GC to eventually
    remove it if we don't.

*   The block belongs to a future slot[^future]

    ```
      blockSlot B > currentSlot
    ```

[^firstCheckTip]: It might make sense to check the "Add to current chain"
case before doing the missing predecessor check (provided that the block is
not in the future).

[^future]: It will be the responsibility of the chain sync client not to accept
blocks _too_ far in the future. However, we don't want to reject blocks in
the near future as invalid since we want to allow for clock skew.

[^gc]: Blocks on chains that are never selected, or indeed blocks whose
predecessor we never learn, will eventually be garbage collected when their
block number number is more than `k` away from the tip of the selected chain.
The chain DB (more specifically, the volatile DB) can still grow without bound
if we allow upstream nodes to rapidly switch between forks; this should be
addressed at the network layer (for instance, by introducing rate limiting for
rollback in the chain sync client).

### Add to current chain

If `B` fits onto the end of our current chain `F`, i.e.

* `F = ε` and `B_pred = I`, or

* `exists F' ∙ F = F' :> B_pred`

we take advantage of lemma Local Chain Selection and run chain selection on

```
  candidates(B, V ∪ {B})
```

Apart from the starting point, chain selection will work in the same way as
described in Initialization. Note that this case takes care of the common case
where we just add a block to our chain, as well as the case where we stay
with the same branch but receive some blocks out of order. Moreover, we can use
the _current_ ledger state as the starting point for validation.

### Switch to a fork

If none of the cases above apply, we have a block `B` such that

a.   `B ∉ V`

b.   `blockNo B > blockNo I` (and hence `B` cannot be in the immutable DB)

c.   For all transitive predecessors `B'` of `B` we have `B' ∈ V` or `B' = I`

     In other words, we must have a fragment `F_prefix = I :> ... :> B` in `candidates(I, V ∪ {B})`.

d.   `blockSlot B <= currentSlot`

e.   (Either `F = ε` and `B_pred ≠ I`, or) `exists F', B' ∙ F = F' :> B'` where `B' ≠ B_pred`

Some observations:

*   point (c) rules out the first option in (e): if `B_pred ≠ I` then we must have
    `B_pred ∈ V` and moreover this must form some kind of chain back to `I`;
    this means that the preferred candidate cannot be empty.

*   By (1.d) above, the new candidates in `candidates(I, V ∪ {B})` must involve
    `B`; in other words, they must all be extensions of `F_prefix`; we can
    compute these candidates using `candidates(B, V ∪ {B})`.

*   We can then use chain selection on all of these candidates _and_ the current
    chain[^preferCandidate]; let the resulting preferred candidate be `F_new`.
    By definition we must have that `F_new` is preferred over the current chain
    and the new candidates; moreover, since the current chain is preferred over
    all _existing_ candidates, we must have by transitivity that `F_new` is
    preferred over all candidates in `candidates(B, V ∪ {B})`, and so we can
    adopt it as our new chain (this argument is a variation on the Local Chain
    Selection argument, above).

It is worth pointing out that we do _not_ rely on `F_prefix` being longer than
the current chain. Indeed, it may not be: if two leaders are selected for the
same slot, and we _receive_ a block for the current slot before we can _produce_
one, our current chain will contain the block from the other leader; when we
then produce our own block, we end up in the switch-to-fork case; here it is
important that `preferCandidate` would prefer a candidate chain (the chain that
contains our own block) over our current chain, even if they are of the same
length, if the candidate ends in a block that we produced (and the current chain
does not); however, the `ChainDB` itself does not need to worry about this
special case.

[^preferCandidate]: Chain selection may treat the current chain special, so we
have to be careful to use `preferCandidate` rather than `compareCandidates` as
appropriate.

## Short volatile fragment

Nothing above relies in any way on the length of the current fragment, but
the maximum rollback we can support is bound by the length of that current fragment.
This will be less than `k` only if

* We are near genesis and the immutable DB is empty, or
* Due to data corruption the volatile DB lost some blocks

Only the latter case is some cause for concern: we are in a state where
conceptually we _could_ roll back up to `k` blocks, but due to how we chose to
organize the data on disk (immutable / volatile split) we cannot.
One option here would be to move blocks _back_ from the immutable DB to the
volatile DB under these circumstances, and indeed, if there were other parts of
the system where rollback might be instigated that would be the right thing to
do: those other parts of the system should not be aware of particulars of the
disk layout.

However, since the `ChainDB` is _solely_ in charge of switching to forks, all
the logic can be isolated to the `ChainDB`. So, when we have a short volatile
fragment, we will just not rollback more than the length of that fragment.
Conceptually this can be justified also: the fact that `I` is the tip of the
immutable DB means that _at some point_ it was in our chain at least `k` blocks
back, and so we considered it to be immutable: the fact that some data loss
occurred does not really change that[^intersection]. We may still roll back more
than `k` blocks when disk corruption occurs in the immutable DB, of course.

[^intersection]: When the chain sync client looks for an intersection between our
chain and the chain of the upstream peer, it sends points from our chain fragment.
If the volatile fragment is shorter than `k` due to data corruption, the client
would have fewer points to send to the upstream node. However, this is the correct
behaviour: it would mean we cannot connect to upstream nodes who fork more than `k`
of what _used to be_ our tip before the data corruption, even if that's not
where our tip is anymore. In the extreme case, if the volatile gets entirely
erased, only a single point is available (the tip of the immutable DB, `I`) and
hence we can only connect to upstream nodes that have `I` on their chain --
which is precisely stating that we can only sync with upstream nodes that have a
chain that extends our immutable chain.

## Clock changes

When the system clock of a node is moved _forward_, we should run chain
selection again because some blocks that we stored because they were in the
future may now become  valid. Since this could be any number of blocks, on any
fork, probably easiest to just do a full chain selection cycle (starting from
`I`).

When the clock is moved _backwards_, we may have accepted blocks that we should
not have. Put another way, an attacker might have taken advantage of the fact
that the clock was wrong to get the node to accept blocks in the future. In this
case we therefore really should rollback -- but this is a weird kind of
rollback, one that might result in a strictly smaller current chain. We can only
do this by re-initializing the chain DB from scratch (the ledger DB does not
support such rollback directly). Worse still, we have have decided that some
blocks were immutable which really weren't.

Unlike the data corruption case, here we should really endeavour to get to a
state in which it was as if the clock was never "wrong" in the first place; this
may mean we might have to move some blocks back from the immutable DB to the
volatile DB, depending on exactly how far the clock was moved back and how big
the overlap between the immutable DB and volatile DB is.

It is therefore good to keep in mind that the overlap between the immutable DB
and volatile DB does make it a bit easier to deal with relatively small clock
changes; it may be worth ensuring that, say, the overlap is at least a few days
so that we can deal with people turning back their clock a day or two without
having to truncate the immutable database. Indeed, in a first implementation,
this may be the _only_ thing we support, though we will eventually have to
lift that restriction.

## Garbage collection

For performance reasons neither the immutable DB nor the volatile DB ever makes
explicit `fsync` calls to flush data to disk. This means that when the node
crashes, recently added blocks may be lost. When this happens in the volatile
DB it's not a huge deal: when the node starts back up and the `ChainDB` is
initialized we just run chain selection on whatever blocks still remain; in
typical cases we just end up with a slightly shorter chain.

However, when this happens in the immutable DB the impact may be larger. In
particular, if we delete blocks from the volatile DB as soon as we add them to
the immutable DB, then data loss in the immutable DB would result in a gap
between the volatile DB and the immutable DB, making _all_ blocks in the
volatile DB useless. We _can_ recover from this, but it would result in a large
rollback (in particular, one larger than `k`).

To avoid this we should introduce a delay between adding blocks to the immutable
DB and removing them from the volatile DB (garbage collection). The delay should
be configurable, but should be set in such a way that the possibility that the
block has not yet been written to disk at the time of garbage collection is
minimized. A relatively short delay should suffice (60 minutes, say, should be
more than enough), though there are other reasons for preferring a longer
delay:

* Clock changes can more easily be accommodated with more overlap (see above)
* The time delay also determines the worst-case validity of iterators
  (see detailed discussion in the `ChainDB` API)

A consequence of this delay is that there will be overlap between the immutable
DB and the volatile DB. The exact length of this overlap depends on the garbage
collection delay and the slot length; a delay of 60 minutes and a block produced
every 20 seconds would result in an overlap of at least 180 blocks. This is a
lower bound; typically the overlap will be larger because blocks are not removed
from the volatile DB on a per-block basis, but rather in groups. However, this
overlap should be an internal detail to the `ChainDB` and not visible to its
clients.
