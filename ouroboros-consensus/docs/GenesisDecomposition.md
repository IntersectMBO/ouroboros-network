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

-----

Updated on 2021 August 9, after much additional thought and broader
reconsiderations, kicked off by Javier Sagredo's observation of a stalling
attack vector in the original sketch above.

This new sketch updates much-but-not-all of the origial sketch above.

- Execution begins in the _Syncing_ state.

- While we are Syncing:

    - If our valency falls below some threshold, then BlockFetch stops sending
      new fetch requests until sufficient valency is recovered.

    - BlockFetch can only download blocks from the headers that the density
      rule approves.

        - The density rule is: compare header chains based on the number of
          headers in the relevant Genesis window (the 3k/f slots after the
          intersection), though if the headers do not span the Genesis window
          and the peer claims to have more headers we must wait for them
          (because they might also be in the window).

        - The Ouroboros Genesis paper proves -- excepting only disasterous
          intervals -- that density rule will always strictly prefer the honest
          chain over any possible alternative.

    - Therefore, we require that each peer's highwater blockno is increasing
      "fast enough on average" until we're at their tip, with the only
      exceptional circumstance being when their latest header is beyond our
      forecast range (since we don't even request a next header while that is
      true).

        - TODO Do we actually need that exception? Under what circumstances
          would it be relevant, during Syncing?

        - TODO I'm anticipating a token bucket for enforcing "fast enough on
          average", but there remain plenty of details and thresholds to
          consider.

        - A possible refinement: if they can promise to send a specific k+1st
          block (which the honest nodes would always do, up to their immutable
          tip), then they're allowed to be somewhat slower, since we'll
          disconnect from them if either they don't deliver that block or if
          the eventual densest chain does not include that block.

        - A possible refinment: each peer can offer _jump points_ that are
          usefully ahead of their latest header. If some other peer has already
          sent the jump point's header, then we can advance the slower peer's
          ChainSync state accordingly. This can help a relatively slow
          redundant peer remain connected.

- Transition from Syncing to _CaughtUp_ whenever all of:

    - No peer has sent a header binary-preferable to my selection.

    - No peer has sent >k headers from an intersection with my selection.

    - We see every peer to its tip.

        - TODO To what extent can the adversary abuse this to prevent our
          transition? Even supposing validated, uninterruptible ChainSync
          switches?

        - TODO Perhaps we don't need it, since we assume we'll have at least
          one honest peer. Their stream of headers should race ahead of the
          corresponding stream of blocks until we're CaughtUp, and so that'll
          hold back at least one of the other conjuncts. On the other hand, it
          seems fine if we do need this, because of the timeout discussed
          above.

- While we are CaughtUp:

    - BlockFetch is free to download the blocks from any of our peers' headers.
      It has two primary requirements, which are in tension.

        - The ultimate goal of BlockFetch is to get the best blocks ASAP.
          However, an imperfect best effort is tolerable, up to a point; we
          consider the only consequences of the best effort's inefficiency to
          be additional chain propagation delay.

            - The Ouroboros protocol only considers chain length. Tiebreakers
              are out of scope, so "best block" in the requirement above only
              means greatest blockno. (BlockFetch is free to also consider
              tiebreakers; the protocol does not care.)

            - Note that the adversary claiming to have additional headers but
              refusing to send them has no effect on BlockFetch while we are
              CaughtUp. Only received headers matter. The worst the adversary
              could do by withholding headers is intentionally timeout in order
              to decrement our valency (which we might choose to require stays
              about some value, see below) -- but presumably they can't ensure
              we reconnect to them, so they've revealed their nature, losing
              access to us, in order to possibly create a short delay.

        - BlockFetch should avoid unnecessary downloads (the same block more
          than once or a block we'll never select).

            - When CaughtUp, we have a high priority design goal that
              worst-case resource utilization is approximately the same as
              average-case. If not, even well-meaning node operators will
              eventually prune their node's allocated resources, thereby
              creating a DoS attack vector.

            - This is why we can't simply download "all blocks ASAP" or even the
              same block from all peers currently offering it. Recall that the
              adversary can forge arbitrarily many blocks whenever it is
              elected, just not on the same chain.

- Transition from CaughtUp to Syncing whenever any of:

    - The wallclock is "too far ahead" of the latest "meaningful" peer
      interaction.

        - TODO Sketch: we transition as soon N (?) of our peers' tips have a
          time point that is more than LIM (?) behind our wallclock.

        - TODO Our ChainSync timeouts will disconnect naturally, right? And so
          maybe this is really just another valency limit, like that of Syncing
          above.

        - TODO It's safe to assume the computer has access to "inertial
          reckoning" via a real-time clock hardware, right? If so, we can
          immediately detect this even upon eg the machine waking from a
          hibernation state. IE instead of totally relying an NTP connection,
          which could also be compromised.

    - Some peer sends >k headers from an intersection with my selection.

        - This rule is a failsafe: We assume this shouldn't happen under
          nominal circumstances (by the Common Prefix theorem in the Ouroboros
          Praos paper; TODO Confirm with researchers), so we downgrade to the
          more conservative state if we do observe it, since we must have
          somehow fallen "too far" behind again without otherwise noticing.

-----

Updated on 2021 August 12, after a couple more conversations. In particular,
even with my recent idea of letting nodes annotate immutable headers, Edsko
observed that wouldn't let us detect eg that should transition CaughtUp ->
Syncing soon enough, eg before the adversary can send >k blocks. So in some
sense that's redundant wrt the wall-clock trigger. So what follows is the
current plan, and our work is to 1) brainstorm ways in which the wall-clock
trigger is insufficient and 2) fill in the details of eg the timeout for sending
claimed headers during Syncing.

The only differences between Syncing and CaughtUp are as follows.

- If Syncing, I won't alter my ChainDB selection (even when I mint) unless my
  number of server peers exceeds a threshold.

    - The Network Layer chooses this threshold in such a way that justifies us
      assuming that we have at least one healthy connection to at least one peer
      serving a contemporary honest chain.

- If Syncing, I'll wait for you to send me more headers if you claim to have
  them, as long as you can send them at a sufficient rate (enforcement TBD).
  Specifically, my ChainDB will not select >k blocks of a chain unless it has
  the best density among all of my current peers' claimed headers.

    - Note in particular that it's only possible to select >k blocks at time T1
      from a chain that is later determined to not be the densest if the peers
      offering the more dense chain had claimed at time T1 to have no more
      headers to send. Honest peers would only do so if the difference-making
      blocks did not exist at time T1. In which case the Praos Common Prefix
      Theorem reveals a contradiction: no adversarial chain should ever be >k
      blocks ahead of the best available honest chain. So this scenario only
      arises if eg I wasn't connected to any honest peers at time T1.

- If CaughtUp, I don't wait for claimed headers, since I don't want the
  adversary to be able to slow me down by falsely claiming to have more. The
  Praos Common Prefix Theorem says I don't need to wait and even shouldn't wait;
  ∆ is what it is. Relatedly, BlockFetch's best effort to continually maximize
  my selection's blockno is sufficient; any imperfect BlockFetch decisions
  merely inflate ∆.

    - BlockFetch cannot simply request all blocks from all peers, because that
      opens up a variety of DoS attacks. In particular, BlockFetch should only
      download necessary blocks and only download each block once. That again
      limits DoS attacks to adding some delay whose per adversarial connection
      severity is bounded by a timeout.

- Maybe the minimum valency for Syncing is higher than the minimum valency for
  CaughtUp (see below).

- During Syncing, we allow ChainSync clients to share work.

    - We'll validate a header only once even if we receive it from multiple
      peers.

    - We'll enrich the ChainSync protocol so that servers can offer jump points
      they're committed to: we can tell a peer to jump ahead if some faster peer
      already sent the header for one of the offered jump points.

    - Notably, if all our peers are on the same chain, we'll be processing
      headers primarily only from the fastest peer.

- We don't allow ChainSync clients to share work during CaughtUp to avoid
  tempting node operators to reduce their node's resource allotment.

    - If the did that, they'd fall prey to a DoS attack where the adversary
      waits a while and then suddenly increases the load from the average-case
      to worst-case, thereby crashing the node that now has fewer resources.
      This crash harms the security of the whole network.

We transition from Syncing to CaughtUp once both of the following hold.

- No peer's latest header is binary-preferable to our current selection.

- No peer claims to have more headers to send.

    - Because we wait for claimed headers during Syncing, the adversary can
      delay the transition to CaughtUp by claiming to have more headers but not
      sending them. A design that allows this minor DoS attack is acceptable as
      long as the adversary can't do it indefinitely, since we should be
      spending most of our time in CaughtUp. EG A timeout that requires a steady
      increase in the blockno of the best header they've sent would suffice as
      long as that timeout on average expires faster than a 50% adversary can
      mint another block (ie requiring approximately <40 seconds per header on
      average). We'll likely assume the honest peers can do much better than
      that, so we'll likely have an even more aggressive (ie still sufficient)
      timeout.

We transition from CaughtUp to Syncing once any of the following hold.

- We observe something we consider impossible while CaughtUp.

    - One example is: receiving headers that show a chain that is >k ahead of
      its intersection with our current selection.

    - These are failsafe measures. We regard them as contributions to the
      Network Layer's heuristics for the connection quality.

- Our number of server peers falls too low.

    - We are assuming the Network Layer will disconnect from a peer if the
      connection quality degrades too far (eg the KeepAlive falters). In
      particular, we assume the Network Layer recognizes that in a sufficiently
      prompt way. EG If my node loses all network connectivity, I expect my
      valency to fall below the threshold well before I'm likely to mint too
      many blocks.

    - Ideally the Network Layer would detect an unhealthy connection (and
      decrement our valency) before an adversary could trick me into selecting
      >k blocks. How long that would actually take unfortunately depends on many
      factors. If I was recently connected to an CaughtUp honest node, then it
      should take quite a while for the adversary to even mint such a long
      chain. But if, for example, I'm running on a computer that just woke up
      from a several day sleep, I need to revert to Syncing ASAP.

    - The threshold must be greater than one, since a common setup for
      block-producer involves running two local nodes, one proxying for the
      other. At least the proxy node of that pair needs to transition back to
      Syncing if that it loses connection to all other nodes, even if the pair
      remain connected to each other.

    - That same block-producing pair example suggests we need to allow an
      override for the valency thresholds: the internal node in that pair will
      only ever have exactly one connection, and that connection is to be
      trusted. In fact, that internal peer never needs to be in the Syncing
      state.

Note that the only way for an adversarial peer to cause me to transition from
CaughtUp to Syncing is to sufficiently lower my valency by degrading the quality
of my network connections.
