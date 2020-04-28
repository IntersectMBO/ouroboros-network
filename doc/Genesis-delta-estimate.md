nick.frisby@iohk.io
2020 April 22

# Introduction

This document demonstrates how the current behavior of ChainSync and BlockFetch
influences our estimate for the papers' Δ parameter. In summary, some
opportunities for latency hiding are not taken by the current designs.

# Semantics of the Δ parameter

Here is a paraphrased quote from a 2020 April 21 phone call with Peter Gaži and
Alexander Russell.

> If a(n honest) leader is about to forge a block at the onset of slot `s+Δ`,
  it must have already had the opportunity to select every (honest) block
  forged at or before the onset of slot `s`.

Note that if we assume that forging happens exactly at the onset of a slot (and
there is no clock skew/drift), then there is a total duration of exactly `Δ`
slots between the forging in `s` and the forging in `s+Δ`. Thus _maximum delay_
is a reasonable name for Δ.

I don't know if this is consistent with other presentations, but here I intend
for `Δ=1` to correspond to the _(effectively) synchronous_ case, in which all
blocks forged in one slot will have been considered for selection by the next
slot's leaders before they lead. If instead we were to use `Δ=0` for the
synchronous case, then we'd have to strew `+1`s thoughout our presentation
below.

In other words, we assume `Δ>0`.

We also have the addendum that Edsko and Peter identified during our broad call
earlier that same day.

> If any block violates the above definition of Δ, then it can be reclassified
  as adversarial behavior.

(You can probably skip to the next section, Example 1, now if you'd like.)

For quick reference, I've collated the relevant excerpts of [the Genesis
paper](https://eprint.iacr.org/2018/378) that mention "delay", though they seem
less helpful for this document's purposes than the above.

> More specifically, we will describe a parameter called `Delay` (a function of
  the network delay Δ) that determines the time sufficient for a party with
  access to all resources to become fully synchronized with the state of the
  protocol.

> A party is considered synchronized if it has been continuously connected to
  all its resources for a sufficiently long interval and has maintained
  connectivity to these resources ... until the current time. Formally, here,
  "sufficiently long" refers to `Delay`-many rounds [ie slots], where `Delay`
  is a parameter of the ledger that depends on the network delay [ie Δ].

> [The statement of Theorem 3 considers only a `Delay` of `2Δ`.]

> [T]he synchronization time does not take more than `Delay` time as given in
  the theorem statements, as this is exactly the time until the a newly joining
  party will have received a synchronizing chain and all honest transactions
  that were sent out (and still are valid) before this party joined the network
  (note that the round-trip time is just 2Δ).

> Δ [is] maximum message delay in slots [note well that a message in the paper
  consists of an entire chain]

(I'm not sure whether it does, but note that if the paper takes synchrony to
mean `Δ=0` -- whereas we here use `Δ=1` for that -- then the paper might
consider synchrony to imply `k*Δ=Δ`.)

# Example 1

Here is a small example.

 * Consider a network with just two nodes, N1 and N2.

 * Let C1 and C2 be the chains selected by N1 and N2 as of the onset of slot
   `s`.

 * Suppose `len(C1) = len(C2)`, so the two nodes are in a stand-off/steady
   state. To emphasize: no new messages will be sent until a node leads.

 * Let `0 <= div(C1,C2) <= k` denote their _divergence_, which is the depth of
   their intersection. (It's 0 iff `C1 = C2`.)

 * Suppose only N1 leads slot `s`, only N2 leads slot `s+Δ`, and no other slots
   have leaders. So there are `Δ-1` many empty/inactive slots between `s` and
   `s+Δ`, and thus a total duration of exactly `Δ` slots between the forging in
   `s` and the forging in `s+Δ`.

 * Let C1' be the chain that N1 forges in slot `s`. Note that `1 <= div(C1',C2)
   = div(C1,C2)+1 <= k+1`.

 * By the semantics of Δ, when N2 leads slot `s+Δ`, it must extend C1', since
   C1' is better than C2 and was forged `Δ` many slots ago at the onset of slot
   `s`.

 * Thus N2 has `Δ` many slots (ie `s .. s+Δ-1`) in which to fetch however many
   of the blocks in the suffix `div(C1',C2) .. C1'` N2 has never before
   downloaded. That count ranges from `1` to `k+1` blocks.

# Discussion 1

 * The last bullet point from that example means that Δ -- as worst-case bound
   -- should be the maximum time it takes to diffuse `k+1` blocks (assuming
   nominal network circumstances, at least for now). Note that because of
   pipelining, this delay will be less than `k+1` times the maximum time to
   send one block, but it will to some degree exceed that maximum time to send
   one block. I don't have an estimate for that excess.

 * An immediate and relevant question is whether we actually expect to see any
   non-negligible divergences. Moreover, even if the divergence is deep, how
   many of the suffixes blocks will have been previously fetched? Along these
   lines, see NJD1 in the Feedback appendix below. The Genesis paper's Common
   Prefix property ensures that carefully chosen parameters can essentially
   bound the divergence at `<=k`, but it offers nothing stronger than that. Do
   we have any evidence for estimating the distribution of `div(C1,C2)` and/or
   what portion of those blocks will have been previously downloaded?

 * (Duncan shared a link to this [Medium
   post](https://medium.com/@samcgill29/a-brief-analysis-of-the-poldercast-p2p-network-deployed-in-cardanos-shelley-testnet-ba817b369541),
   which discusses how often the divergence was >0 in the Shelley Incentivized
   Testnet running Jörmungandr. But it does not discuss the depth of each
   divergence. Maybe the backing data source also has that available?)

 * With a good estimate for the distribution of divergences, we can consider a
   relatively aggressive Δ by invoking the addendum to its definition: we
   choose a relatively small Δ and classify any chain-transfer delays that
   exceed it as "additional" adversarial behavior. (TODO so incorporate it
   instead by lowering the Genesis paper's α parameter?.) This ultimately
   requires two estimates: the reasonable Δ and the probability it will be
   exceeded.

 * There is also an alternative design option: adjust ChainSync and BlockFetch
   so that they also fetch chains that are as preferable as the local node's
   selected chain, instead of only chains that are more preferable. This would
   demand some additional network throughput but may be able to avoid the spike
   in latency caused by the "traffick jam" phenomenon illustrated by Example 1.
   With this change, ChainSync and BlockFetch would no longer introduce latency
   _by design_. The new behavior seems reasonable considering that it's random
   whether a different chain as preferable as our own will grow before ours
   does. This trade-off could be taken further, eg all chains whose length is
   within `n` of that of our selected chain.

 * A meta-level comment: I'm largely writing this document because it seems to
   me that the above alternative design is what (at least) Peter and Alex think
   the implementation is already doing when they ask for guidance on what
   concrete values of Δ are reasonable. Are they asking about apples and we're
   answering about oranges?

# Example 2

There is another factor that increases Δ.

 * Suppose instead that we have a linear topology N1 <-> N2 <-> N3.

 * Reconsider the above example altered only so that N2 and N3 start with the
   same chain and that N3 instead of N1 leads slot `s+Δ`.

 * Thus the necessary blocks must diffuse from N1 via N2 to N3 within `Δ` many
   slots.

 * With the current ChainSync and BlockFetch, this diffusion happens in two
   phases, since N2 doesn't begin forwarding the diffused blocks until it
   selects C1'. First, the blocks must diffuse from N1 to N2. Then -- only once
   they've all arrived at N2 -- the blocks will begin diffusing from N2 to N3.
   So, assuming comparable network edges, we have `Δ/2` slots to get the blocks
   from N1 to N2 and then `Δ/2` blocks to get them from N2 to N3. (And this is
   even ignoring the intermediate phase where N2 has to first send all the
   headers to N3, so that N3 can decide to start fetching the blocks.)

 * In more general terms, consider the best path from the latest leader (ie N1)
   to the next leader (ie N3) where the notion of best is influenced by how
   much of the necessary chain (ie C1) the intermediate nodes have already
   downloaded (in our example, the path is N2 and N3, and they've downloaded
   none of C1). Our Δ estimate must be multiplied by the length of this path
   and perhaps divided by some factor related to the number of C1 blocks
   already fetched along that path.

# Discussion 2

See NJD2 below; Neil Davies is not certain that linear topologies are
reasonable scenarios to consider. I added the last bullet point above as an
attempt to spell out how a subgraph of a more connected topology might end-up
behaving as the linear topology does in Example 2.

The only idea I have for mitigating such a cascade that linearly inflates Δ is
to refine the ChainSync server. Currently, the ChainSync server provides the
local node's selected chain's headers to its peers' ChainSync clients. So N2
cannot begin diffusing any prefix of C1' to N3 until N2 has selected the full
C1'. And it can't do so before it has fetched all of C1'.

If instead the ChainSync server were to instead/additionally provide the
headers of the chain that the local node's fetch decision logic is attempting
to fetch in anticipation of selecting, then the diffusion from N1 to N2 and the
diffusion from N2 to N3 could be pipelined. I have not considered the
ramifications of such an adjustment to ChainSync.

(If I understand correctly, this proposal is nearly the same as the "tentative
chain" from https://github.com/input-output-hk/ouroboros-network/issues/504.)

# Conclusion

The current designs of ChainSync and BlockFetch introduce latency by design by
avoiding speculative block fetches. This is a trade-off, but I do not see how
it has influenced in the current estimate for the Genesis paper's Δ parameter.

I sketched a couple ideas for relaxing that trade-off: the tentative chain
would potentially forward invalid blocks
(https://github.com/input-output-hk/ouroboros-network/issues/504) and the
speculative fetches would potentially download blocks that are never on a
competetive chain -- but both (esp. tentative chain) would hide more latency in
relatively common cases.

I also discussed the option of how to retain a better Δ (and instead worsen α)
by treating violations of it as additional adversarial behavior, in particular
what other estimates we'd likely need in order to consider how much we can
lower Δ (and how much we must worsen α).

I do not know how much a (significantly?) larger Δ estimate would affect the
researcher's current work on tuning Praos parameters.

# Appendix: Discussion beyond the current scope

In the 2020 April 21 calls, Neil Davies had mentioned a couple other scenarios
to consider.

 * network partition on the order of days, eg undersea cable cut
 > [NJD]Our approach to this is to "ensure" (e.g. get large scale / multiple 
   stakepool operators) to "circle" the world - this is the current
   approach in the federated setting. By having fixed interconnects 
   between say Frankfurt, Singapore and Ohio that ensures that (in the
   absence of major failures) the traffic goes both east and west. 
 >    
 > IP routing will "recover" but, as we saw this week (AWS S3 issues /
   github performance etc) with a US cable cut, many services are implicitly
   dependent on worst case service bounds (on the delay and loss of their
   packet exchanges) and do not degrade gracefully under such conditions.
 >  
 > My main concern here is not the steady state, but the dynamics, we can 
   quantify this during initial deployments by use of suitable tracing and
   analysis. Gut feeling is that our design/implementation should cope - but
   we need to build in the verification. A lot of these gross failures that 
   are seen in large scale services have short term failures that are not 
   captured or just plain ignored. Best intution is that this is a sort of 
   condensation phenomena that needs to be tracked for long term stablity.
 >       
 > A decentralised system does not have the fallback that there is a single
   management entity that can "fix" issues - and that is the fallback situtation
   for all the current players in the larger scale internet.
   [/NJD]
       

 * network partition on the order of minutes, eg BGP (?) reroute.

If I understood the discussion correctly, it's a business-level decision for
now to exclude those from our Δ estimate and instead regard those as a
significantly large but temporary absence of honest stake. (TODO so incorporate
them into the [the Genesis paper](https://eprint.iacr.org/2018/378)'s α and β
parameters?)

# Appendix: Feedback

I'm collating people's comments here. I'll refer to them from the relevant
parts of the document.

NJD1:

> There will be some level of "is there an eclipse trying to be be performed on
  me" detection that would have triggered a long time before `k` slots of gap
  would start to be approached.

NJD2:

> I am not certain that [the linear topology] is a reasonable scenario.
  Operational nodes will have a mimimum connection valency than 2 and they will
  actively work to maintain that (likely to be 5 to 7 range as a minimum of
  "hot" connections - ones that they subscribe to for updates).
