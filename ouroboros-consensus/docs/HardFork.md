# Details of the hard fork transition

This document attempts to describe the details of the hard fork transition
from Byron to Shelley, and from Shelley to future versions of the ledger.

## Byron

The Byron specification can be found at
https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec .

### Moment of hard fork

The Byron ledger state provides the current protocol version in

```haskell
adoptedProtocolVersion :: ProtocolVersion
```

in the `State` type from `Cardano.Chain.Update.Validation.Interface`.

This protocol version is a three-tuple `major`, `minor`, `alt`. The Byron
specification does not provide any semantic interpretation of these components.
By convention (outside of the purview of the Byron specification), the hard fork
is initiated the moment that the `major` component of `adoptedProtocolVersion`
reaches a predefined, hardcoded, value.

### The update mechanism for the `ProtocolVersion`

Updates to the `ProtocolVersion` in `Byron` are part of the general
infrastructure for changing protocol parameters (parameters such as the maximum
block size), except that in the case of a hard fork, we care only about changing
the `ProtocolVersion`, and not any of the parameters themselves.

The general mechanism for updating protocol parameters in Byron is as follows:

1. A protocol update _proposal_ transaction is created. It proposes new values
   for some protocol parameters and a greater _protocol version_ number as an
   identifier. There cannot be two proposals with the same version number.

2. Genesis key delegates can add _vote_ transactions that refer to such a
   proposal (by its hash). They don't have to wait; a node could add a proposal
   and a vote for it to its mempool simultaneously. There are only positive
   votes, and a proposal has a time-to-live (see `ppUpdateProposalTTL`) during
   which to gather sufficient votes. While gathering votes, a proposal is called
   _active_.

   Note that neither Byron nor Shelley support full centralization
   (everybody can vote); this is what the Voltaire ledger is intended to
   accomplish.

3. Once the number of voters satisfies a threshold (currently determined by the
   `srMinThd` field of the `ppSoftforkRule` protocol parameter), the proposal
   becomes _confirmed_.

4. Once the threshold-satisfying vote becomes stable (ie its containing block is
   `>=2k` slots old), a block whose header's protocol version number
   (`CC.Block.headerProtocolVersion`) is that of the proposal is interpreted as
   an _endorsement_ of the stably-confirmed proposal by the block's issuer
   (specifically by the Verification Key of its delegation certificate).
   Endorsements -- ie _any block_, since they all contain that header field --
   also trigger the system to discard proposals that were not confirmed within
   their TTL.

   https://github.com/input-output-hk/cardano-ledger/blob/172b49ff1b6456851f10ae18f920fbfa733be0b0/cardano-ledger/src/Cardano/Chain/Block/Validation.hs#L439-L444

   Notably, endorsements for proposals that are not yet stably-confirmed (or do
   not even exist) are not invalid but rather silently ignored. In other words,
   no validation applies to the `headerProtocolVersion` field.

5. Once the number of endorsers satisfies a threshold (same as for voting), the
   confirmed proposal becomes a _candidate_ proposal.

6. _At the beginning of an epoch_, the candidate proposal with the greatest
   protocol version number among those candidates whose threshold-satisfying
   endorsement is stable (ie the block is `>=2k` slots old) is _adopted_: the
   new protocol parameter values have now been changed.

   If there was no stably-candidated proposal, then nothing happens. Everything
   is retained; in particular, a candidate proposal whose threshold-satisfying
   endorsement was not yet stable will be adopted at the subsequent epoch unless
   it is surpassed in the meantime.

   When a candidate is adopted, all record of other proposals/votes/endorsements
   -- regardless of their state -- is discarded. The explanation for this is
   that such proposals would now be interpreted as an update to the newly
   adopted parameter values, whereas they were validated as an update to the
   previously adopted parameter values.

In summary, the following diagram tracks the progress of a proposal that's
eventually adopted. For other proposals, the path short circuits to a
"rejected/discarded" status at some point.

```
active proposal
    --> (sufficient votes)
confirmed proposal
    --> (2k slots later)
stably-confirmed proposal
    --> (sufficient endorsements)
candidate proposal
   --> (2k slots later)
stably-candidated proposal    (Frisby: stably-nominated?)
   --> (epoch transition)
adopted proposal
```

### Initiating the hard fork

Proposals to initiate the hard fork can be submitted and voted on before all
core nodes are ready. After all, once a proposal is "stably-confirmed", it will
effectively remain so indefinitely until nodes endorse it (or it gets superseded
by another proposal). This means that nodes can vote to initiate the hard fork,
_then_ wait for everybody to update their software, and once updated, the
proposal is endorsed and eventually the hard fork is initiated.

Endorsement is somewhat implicit. The node operator does not submit an explicit
"endorsement transaction", but instead restarts the node (probably after a
software update that makes the node ready to support the hard fork) with a new
protocol version (as part of a config file or command line parameter), which
then gets included in the blocks that the node produces (this value is part of
the static `ByronConfig`: `byronProtocolVersion`).

(Note that a node restart is necessary for _any_ change to a protocol parameter,
even though most parameters do not require any change to the software at all.)

### Software version (in block headers)

The Byron header also records a software version (`headerSoftwareVersion`). This
is a legacy concern only, and is present in but ignored by the current Byron
implementation, and entirely absent from the Byron specification.

## Shelley

### Moment of the hard fork

Similar to the Byron ledger, the Shelley ledger provides a "current protocol
version", but it is a two-tuple (not a three-tuple), containing only a
`hard fork` component and `soft fork` component:

```haskell
_protocolVersion :: (Natural, Natural)
```

in `PParams` (currently, module `PParams` in
`chain-and-ledger/executable-spec/src/PParams.hs`).

The hard fork from Shelley to its successor (Goguen?) will be initiated
once the hard fork component of this version gets incremented.

### The update mechanism for the protocol version

The update mechanism in Shelley is simpler than it is in Byron. There is no
distinction between votes and proposals: to "vote" for a proposal one merely
submits the exact same proposal. There is also no separate endorsement step
(though see "Initiating the hard fork", below).

The procedure is as follows:

1. As in Byron, a proposal is a partial map from parameters to their values.
2. During each epoch, a genesis key can submit (via its delegates) zero, one,
   or many proposals; each submission overrides the previous one.
3. "Voting" (submitting of proposals) ends `6k/f` slots before the end of the
   epoch (i.e., twice the stability period, called `stabilityWindow` in the
   Shelley ledger implementation).
4. At the end of an epoch, if the majority of nodes (as determined by the
   `Quorum` specification constant, which must be greater than half the nodes)
   have most recently submitted the same exact proposal, then it is adopted.
5. The next epoch is always started with a clean slate, proposals from the
   previous epoch that didn't make it are discarded.

The protocol version itself is also considered to be merely another parameter,
and parameters can change _without_ changing the protocol version, although
a convention _could_ be established that the protocol version must change if
any of the parameters do; but the specification itself does not mandate this.

### Initiating the hard fork

The timing of the hard fork in Shelley is different to the one in Byron;
in Byron, we _first_ vote and then wait for people to get ready, in Shelley
it is the other way around.

Core node operators will want to know that a significant majority of the
core nodes is ready (supports the hard fork) before initiating it. To make this
visible, Shelley blocks contain a protocol version. This is not related to the
current protocol version as reported by the ledger state (`_protocolVersion` as
discussed in the previous section), but it is the _maximum_ protocol version
that the node which produced that block can support.

Once we see blocks from all or nearly all core nodes with the `hard fork`
component of their protocol version equal to the post-hard-fork value, nodes
will submit their proposals with the required major version change to initiate
the hard fork.

Note that this also means that in Shelley there is no need to restart the node
merely to support a particular parameter change (such as a maximum block size).

## Byron _or_ Shelley: Publication of software versions

Both the Byron and the Shelley ledger additionally also records the latest
version of the software on the chain, in order to facilitate software
discovering new versions and subsequently updating themselves. This would
normally precede all of the above, but as far as `ouroboros-consensus` is
concerned, this is entirely orthogonal. It does not in any way interact with
either the decision to hard fork nor the moment of the hard fork. If we did
forego it, the discussion above would still be entirely correct.

## Invalid states

In a way, it is somewhat strange to have the hard fork mechanism be part of
the Byron or Shelley ledger itself, rather than some overarching ledger on top.
For Byron, a Byron ledger state where the `major` version is the (predetermined)
moment of the hard fork is basically an invalid state, used only once to
translate to a Shelley ledger. Similar, the `hard fork` part of the Shelley
protocol version _will never increase_ during Shelley's lifetime; the moment
it _does_ increase, that Shelley state will be translated to the (initial)
Goguen state.
