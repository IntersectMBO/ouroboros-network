# 2020 Stastical Testing Plan

## Origin of Stochasticity

There are two fundamental sources of stochasticity in the ThreadNet tests.

  - Praos inherently includes stochasticity. The leader schedule is determined
    by a nonce. The test setup generators should effectively sample the initial
    nonce from a uniform distribution.

  - Chain propagation delay introduces stochasticity into any Ouroboros
    protocol. If different candidate chains happen to arrive at a node at
    essentially the same time, the node may have to select amongst
    equally-preferable chains. The tie-breaker mechanism ought to be at least
    partially stochastic. (It appears to be uniform from my limited empirical
    analysis.)

Currently, only the Cardano ThreadNet tests involve chain propagation delays.
The test usually includes one network partition, during which the topology is
partitioned into the class of even node IDs and the class of odd node IDs. The
maximum duration of the network partition is carefully chosen to ensure that it
cannot cause a wedge during the Byron era (recall that the network is
synchronous, for now). It is more difficult to ensure that same guarantee
during the Shelley era. However, because f=0.2, it is conservative to simply
reuse the duration determined by the Byron analysis. The nodes forge at
approximately one-fifth of the rate during Shelley as during Byron and also
(for f=0.2, relatively small n) there is only at most a ~9% chance of a any
active slot having multiple leaders (so there's a ~9% chance the nodes can only
withstand a k-1 wedge during the partition if the preceding active slot had
multiple leaders, a ~0.81% chance for a k-2 wedge, etc).

## Motivation for Statistical Testing

Because of the above stochasticity, some tests will violate the protocols'
Common Prefix (CP) property even if there are no bugs. This leads to false
negatives in the test suite results, which evokes fear, uncertainty, and doubt
from the team members.

The Ouroboros publications famously derive asymptotic upper-bounds for the
probability of violation of the CP property et al. However, we are not
interested in the asymptotic behavior when testing -- we use "small" nets and
prefer to run them for as few slots as possible. In particular, there are two
principal tensions.

  - As k increases, the probability of a CP violation approaches 0. However,
    the epoch size also scales linearly with k, so we prefer a small k for
    testing (we need to include multiple epochs for coverage and/or multiple
    eras). We similarly prefer small chain fragments etc for counterexample
    interpretability and in order to more likely/easily test edge cases (eg an
    off-by-one bug that conflates k-1 vs k).

  - Similarly for f. As 1/f increases, CP violations become less likely, but
    the epoch size also scales linearly with 1/f. (Empty slots are relatively
    inexpensive during testing thanks to io-sim, but not totally free.)

Thus it is unavoidable that our tests either will require a burdensome amount
of computational resources (time/space/machines/etc) or else will occasionally
fail merely due to "bad luck".

This document therefore proposes the following high-level plan as a compromise.

  - Choose a few simple small test configurations: fix k, f, the number of
    nodes, stake distribution, the number of slots to execute, etc. --
    essentially everything other than the PRNG seeds. Use only honest node
    implementations (the only adversarial node behavior should arise from a
    bug).

  - Determine the expected CP violation rate for those configurations. Ideally
    this could be done analytically, but empirical observation with large N
    will suffice, at least at first.

  - Implement statistical properties that only check that those CP rates have
    not changed. Ideally we'd want a confidence level that corresponds to
    "never during the lifetime of the project". But we also want these tests to
    be fast enough that we can run them frequently, ideally on every PR.
    However, reserving them for the nightly run might suffice. If they are so
    slow that even nightly is too frequent, we may need to adopt an incremental
    approach. (I need to prototype these tests before I can determine how much
    execution time they will require.)
    
  - Truncate all test cases for the non-statistical properties at the first CP
    violation, if any. Include a label when that happens. Occasionally review
    those label rates, comparing them to the baseline simple small test
    configurations.

  - These non-statistical properties can use values of k, f, etc that are less
    "small" so that we truncate less often. But they'll still be "large" enough
    that the CP violation rate is non-trivial. (EG I'm hoping for something
    around 1-5%.)

That's the primary idea. It's focused on CP violations, but I anticipate we'll
be able to re-use the same iterations for testing multiple properties, such as
whether f actually induces a correct fraction of active slots.

## Steps

  - I'm starting with Mock Praos, because it is simpler and especially because
    it has no stability window limitation. Once those tests are proved out and
    relatively mature, I'll replicate it for Shelley, and then eventually
    Cardano.

  - Implement a dedicated statistical property test for the simplest stochastic
    properties: active slot coefficient, multi-leader rate, etc (see next
    section of this document). I anticipate `checkCoverage` and `coverage` and
    sufficiently well-behaved when used in isolation.

  - Tune the confidence-level, balancing it against execution time.

  - Detect CP violations so that we can count them, label them, truncate the
    test, etc. Fail fatally if the leader schedule should have precluded CP
    violations (eg there were no multi-leader slots).

  - Label CP violations in the Mock Praos non-statistical properties. (Maybe
    also Shelley, even before we have dedicated Shelley statistical tests?)

  - Empirically determine the CP violation rate. Use eg 100,000 iterations. (I
    hope QuickCheck's tabulate can withstand that.)

  - Add the CP violation rate to the statistical property test.

  - Re-tune the confidence-level and also potentially invest time to optimize
    the relevant mock/test infrastructure (at least check for low-hanging
    fruit).

  - Truncate the execution as of the first CP violation in the Mock Praos
    non-statistical properties. (Maybe also Shelley, even before we have
    dedicated Shelley statistical tests?)

  - Repeat above steps for Shelley and then Cardano.

  - Introduce semi-synchrony, ie delta greater than 1. I'm disappointed that I
    still don't have confidence regarding how exactly to inject these delays.
    My only insight so far that feels particularly helpful is to only delay
    RollForward messages and let all others travel "instantly" (this is already
    implemented and used by the Cardano network partition). RollForward is the
    mini-protocol that best corresponds to the "entire chain as a message"
    assumption made in the Ouroboros papers. However, many questions still
    remain. EG our reliable order-preserving channels can cause relayed older
    messages to get stuck behind younger messages, and -- akin to priority
    inversion -- this may delay a particular message beyond it's originally
    scheduled delay.

  - Repeat previous for Shelley and then Cardano.

## Miscellaneous Questions

  - The above proposes focusing on the probability of a CP violation occuring
    during some fixed number of slots. Should we instead focus on its arrival
    rate? This would suggest choosing a "small" configuration that essentially
    ensures a CP violation will occur within say the first 1000 slots --
    otherwise the test may have extremely variable run-times. (I'm not
    currently confident regarding concrete pros or cons for this question; the
    arrival time merely occurred to me and I have a hunch it could be more
    useful. At the very least, its semantics isn't tied to a fixed number of
    slots.)

  - What to test in addition to the CP violation rate?

      - The fraction of active slots should be f (regardless of stake
        distribution, barring empty). Trivial to compute analytically.

      - The leadership rate p_i of a node should match phi_f(its stake) (may
        need hold the stake dist constant for this). Trivial to compute
        analytically.

      - The rate of multi-leader slots varies with f, n, and stake dist (for
        small n) (constant stake dist again). Easy to compute analytically.

      - Nodes not leading should break any ties fairly when multiple of their
        immediate neighbors are leading simultaneously. Should be uniform, I
        think.

  - How to handle the "k+1st upstream header is one slot beyond stability
    window" infelicity? How common is this? Easy to robustly detect? Etc.

  - Does the stability window of 3k/f already essentially preclude "unlucky"
    Chain Growth violations?
