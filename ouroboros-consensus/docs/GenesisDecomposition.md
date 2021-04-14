This file sketches the major points in the decomposition of the Genesis
implementation. Edsko de Vries and Nicolas Frisby developed this plan of attack
in 2021 March using chapter 21 of the Consensus Report.

I've also updated this file after a discussion with Duncan on 2021 Apr 13.

  * add _prefix selection_ in between ChainSync -> BlockFetch (Sections
    21.1-21.4, 21.6.2 in the Chapter)

      * implementation note: anticipate iteratively/incrementally advancing the
        window within each function invocation

        ```
        -- PRECONDITION: all the fragments are anchored at the same point [some
        -- upstream function in the pipeline should be able to ensure this
        -- given the peers' fragments (each of which intersects with the
        -- current chain) and the current chain]
        --
        -- Step 0: find the common prefix P of all fragments, anchor the lookahead window there
        -- Step 1 & 2: as in The Report chapter
        -- Step 3: prepend the common prefix P to the result of step 2
        --
        -- The chapter states "move the window", but this function doesn't do
        -- the moving: "moving up past the select blocks" is achieved by
        -- BlockFetch downloading them, ChainDB selecting, and ChainSync client
        -- trimming them off the candidate fragments.
        --
        -- POSTCONDITION: the returned fragment is a prefix of one of the given
        -- fragments. It's possible that a prefix of the returned fragment is
        -- already on our selected chain.
        selectPrefix ::
             Ord tiebreaker
          => (x -> tiebreaker)
          -> NonEmpty (AnchoredFragment x, IsPeerTip)
          ->           AnchoredFragment x   -- could be empty, indicating "wait for now"
        ```

      * prefix selection is _never_ compositional (wrt HFC combinator); it's
        inherently motivated by arbitrary prefixes of chains, which is an
        era-spanning concern.

      * Genesis is fundamentally a change to how nodes catch-up, ie how they
        sync the historical chain. Thus it must override key aspects of whatever
        the contemporary protocol happened to be for those blocks. It is
        all-or-nothing: you'd never apply Genesis to just part of a chain.

        Genesis protects against attacks that would introduce adversarial forks
        at some prefix of the current chain.

        You'd never choose to enable Genesis for new eras but disable it for
        some old eras. We want to use Genesis to compare prefixs, and a chain
        that has reached the new era still has prefixes that have only reached
        the old eras.

        You'd never choose to enable Genesis for old eras but disable it for
        some new eras. If you're using Genesis on a prefix of a chain, then
        Genesis is affecting that entire chain -- it's nonsensical to "stop"
        using Genesis during an execution. (Moreover, "opening the laptop lid"
        may cause a node to need to "sync" again; see "alertness" below.)

      * This method would only be invoked in NodeKernel.initInternalState, at
        the ChainSync -> BlockFetch boundary. Abstractly: we apply it to all
        candidates, then intersect each candidate with the result before giving
        them to BF.

      * CS now needs to include the "is peer's current tip" flag in its fragments

      * We're planning to choose an interface that can express the current
        behavior (ie longest prefix) as part of a staged implementation (ie us
        developing multiple incremental PRs).

  * managing the alert status, ie are we "caught up"? (Section 21.5)

      * when alert, we can select among however many prefixes we have,
        regardless of our valency (ie its no longer lower-bounded)

      * corollary: when alert, Network Layer can reduce valency (The goal: _once
        synced_, avoid redundant peers in order to lower resource usage. The
        "syncing vs not syncing" dichotomy is one acceptable exclusion to the
        average-case=worst-case guiding princple.)

      * the general consensus from our meeting with the researchers was that the
        most robust option is to always forge, even when behind. had considered
        only forging when we're caught up, but that seems like a surface area
        for an availability attack. (Section 21.5.5)

      * specific case: OK for a non-alert node to forge during a ThreadNet test?
        Old ThreadNet? New ThreadNet? (Frisby: it might even be _necessary_, due
        to low-density chains arising there by design.)

  * providing the alertness to the Network Layer

      * _Become alert when_ both
    
         1. We see every peer to its tip.

         2. None of the peer's chain is binary-preferable to our current chain.

      * _Become unalert when_ ... the wallclock is well ahead of the latest
        meaningful peer interaction. Specifically: we become unalert as soon any
        of our peer's tips is more than LIM behind our wallclock.

      * With this division, the question becomes "having been caught-up, how
        quickly could I fall dangerously behind?"

      * We think the researchers will like this and will offer a suggestion for
        the LIM value.

  * making Network Layer sensitivity to alertness

     * We anticipate adding an `:: STM IsAlert` field to the
       `BlockFetchConsensusInterface` record.

     * In particular, this flag can indicate that the Network Layer can "relax"
       with regards to its set of peers. When we're catching up, the Network
       Layer needs to take all sorts of precautions (eg higher valency) to
       ensure we have at least one peer offering the honest chain. This
       distinction is more severe than but similar to the existing
       `FetchModeBulkSync` versus `FetchModeDeadline` distinction.

  * clock skew in relation to DoS attacks (Section 21.5.3)

      * The `Left PastHorizonException{} -> retry` forecast case in CS client
        should fail with "header invalid" instead of retry _when the header's
        slot is ahead of the wallclock_ (cf `BlockchainTime.getCurrentSlot`).

  * Frisby document to carefully consider support for/compatibility with
    low-density chains

      * current understanding: Genesis will not be blocked by low-density
        chains, but any low-density chain will be a permanent opportunity for
        an attacker _even despite_ Genesis (this is known; cf Distaster
        Recovery Plan)

  * key simplification: leave selection in ChainDB as-is (Section 21.6.1)

      * This realization saved a lot of development time.

  * Risk 1: availability attack

      * The design written above, would prevent BlockFetch from pessimistically
        prefetching blocks from chains that are *almost* the best. EG Today
        BlockFetch can fetch the longest _available_ chain, as a fallback for
        when the only peers that sent headers indicating the best chain are slow
        to actually provide the promised blocks.

      * The idea to intersect all candidates with the result of `selectPrefix`
        would prohibit that. And rightly so, in the sense of actually
        implementing Genesis. (TODO NSF: why cann't we make some appeal to `k`
        here, to reduce the severity of this potential spec divergence?)

      * Duncan raised the concern that an attack could -- when the attackers'
        and victims' leader schedules align in a particular way -- cause the
        victim to forge a block that extends a recent prefix of the honest
        chain. In other words, the attacker may be able to induce a some more
        short forks than would naturally arise, thereby increasing the number of
        "wasted blocks".

      * Any mitigation of this would have to be guarded by `IsAlert`: the plan
        written above (ie faithful Genesis) is in principle the only valid
        option for use during catch up.

      * Relevant reminder: even when it's caught up to the wallclock, Genesis
        prefix selection only ever chooses a single chain. Any fluctuation in
        that choice is based on the peers' chains being at their reported tip
        within the density window.

  * Risk 2: performance regression

      * ChainSync validating headers is currently the bottleneck during bulk
        fetch. EG 2 peers bulk fetchs quite a bit faster than 10 peers.

      * The suggested delta is the exact opposite of the plan above: when
        catching up, we need many many peers in order to increase the chance
        that at least one has the honest chain. (We haven't considered actual
        counts rigourously.)

      * Mitigation plan: the ChainSync clients need to share some work, so that
        we don't revalidate the same header once-per-peer.

      * This violates avg-case = worst-case, but that's OK when bulk syncing.
        Perhaps we should guard this work sharing by `NotIsAlert`.

  * SEPARATE WORK:

      * TODO potential optimizations (Section 21.6.3)

      * TODO testing etc - we'd _very much really_ like to use the ThreadNet
        rewrite for this
