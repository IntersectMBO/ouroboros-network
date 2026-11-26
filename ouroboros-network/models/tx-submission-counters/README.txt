Tx-submission V2 counter and mirror audit
=======================================

Reviewed base: d36e10fcd0ca1b64ddcfe4300815549224049091
Current model includes the fresh-mempool-key wake suppression applied after
that audit. The original counterexample is retained as a legacy witness.
Checked with Agda 2.8.0 and GHC 9.14.1, 2026-11-26.

Result
------

No lost-update or missed-notification bug was found in the audited paths.
The mirror protocol is coherent under its stated assumptions. The audit
found an avoidable generation notification for a NEW shared key whose txid
is already in the mempool. That notification is now suppressed: revision
still advances and the key is retained, while unrelated waiters stay asleep.
Existing active keys still notify their other advertisers. Mixed batches
preserve the notification regardless of advertisement order.

The model describes this fixed behaviour and retains the original rule as
a separate legacy counterexample. The Haskell harness checks that the old
reproduction no longer wakes its observer. This is a performance fix; the
original example did not demonstrate lost protocol progress.

Files
-----

CounterMirrors.agda: a standalone model using only Agda's builtin library.
It uses --safe and --without-K, with no postulates, holes, or disabled
termination/coverage checks.

CounterAudit.hs: examples evaluated against the actual Haskell modules,
plus STM/IOSim reproductions. These are not replacement implementations.

Counter roles and transition audit
----------------------------------

The tuple below is (sharedRevision, sharedGeneration, sharedClaimable).
Each +1 is per PURE batch/step, not per affected transaction.

  Event                                      Pure counter delta
  -----                                      ------------------
  Fresh unknown txid advertisement            (1,0,0)
  Existing ordinary/retained advertisement     (0,0,0)
  Advertisement clearing Doomed               (1,0,0)
  Claiming a lease                            (1,0,0)
  Marking entry in submission                 (1,0,0)
  Buffering all requested bodies              (0,0,0)
  Finding an active tx in the mempool          (1,1,0)
  Fresh shared key already in the mempool      (1,0,0)
  Omitting a live requested body              (1,1,1)
  Raising a stuck entry's attempt cap          (1,1,1)
  Accepting submitted transactions            (1,1,0)
  Rejecting a live submitted entry             (1,1,1)
  Scrubbing a leaving peer's live contribution (1,1,1)
  Scrubbing only gone contributions            (0,0,0)
  Expiry, dooming, or reviewing entries         (1,0,0)
  Empty submission outcome                    (1,1,0), no payload change
  Rejecting only gone keys                     (1,1,0), no payload change

Claimable notifications also advance generation and revision. Timer-based
lease expiry does not update these counters by itself; the scheduler's
deadline wakes the peer. Fresh claimable entries need no broadcast because
only the receiving peer knows the freshly allocated key. Existing-entry
advertisements update that peer's local state and immediately re-run its
own decision, so they need no shared notification either.

The actual committed revision is oldRevision + 1 whenever a pure step's
revision differs, even if that step bumped it more than once. An example
that raises a stuck cap and marks a body in submission gives pure delta
(2,1,1); publication gives committed delta (1,1,1).

All production writes found in Registry.hs use writeSharedStateIfChanged:
the counters-thread sweeps/drains, bracket cleanup, decisions, txid replies,
body replies, and submission outcomes. Helpers that change tables without
bumping counters (internTxId, expiry helpers, lookup removal) are covered
by the revision bump in their enclosing transition.

Mirror protocol
---------------

1. A pure step proposes a new SharedTxState.
2. writeSharedStateIfChanged checks the revision. An unchanged revision
   discards the entire proposal. A changed revision publishes the proposal
   with the revision normalised to one committed write.
3. The SAME STM transaction copies generation/claimable into the separate
   mirror TVars, writing a mirror only when its value differs.
4. resolvePeerAction reads the selected mirror AFTER that transaction's
   own writes, in the SAME deciding transaction, and returns it as seen.
5. awaitParkEnd reads only the selected mirror, its timer, and this peer's
   assigned tier cell. A commit before parking is caught by the initial
   comparison. A commit after parking changes a TVar in its STM read set.

Structural writes do not touch either mirror when their values match the
state. Acceptance wakes generation subscribers but does not wake claimable
subscribers. Releases/cap increases notify both. NoCounter subscribes to
neither mirror and must have a timer; tier changes can additionally wake it.

Mirrors start at zero independently of a supplied initial SharedTxState.
The first real write synchronises them. Reading seen from the mirror avoids
immediate retry loops before that happens. Synchronising an initially stale
mirror can cause a conservative one-off notification on a structural write.

Agda results
------------

Unbounded theorems (over natural counters and arbitrary finite traces):

* Publication preserves mirror/state coherence when the input is coherent.
* A dirty proposal preserves its payload and commits revision old + 1.
* Structural-only writes do not write either mirror TVar.
* A fresh capture cannot itself end an unchanged park, even if initial
  state counters and mirrors differ.
* Every chosen-counter notification since capture is detected; unrelated
  events do not end that counter's park. The proof covers any finite list
  of the modelled events, not just traces in the bounded search.
* Timer expiry and tier changes end a park independently of counters.
* NoCounter without a timer has no valid HasWakeSource constructor.
* Every event that notifies claimable also notifies generation; every
  counter or payload change in the finite event table marks revision dirty.

An additional exhaustive reduction checks all 9,724 event sequences of
length zero through three over the 21 event classes.

Checked counterexamples expose three broken variants, not current bugs:
reading seen from the supplied shared-state counter can spin against an
initially stale mirror; forgetting revision loses a proposal; capturing
seen in a later transaction can swallow an intervening notification.

Fixed finding: a fresh mempool hit unnecessarily woke other peers
-----------------------------------------------------------------

State.handleReceivedTxIds takes the mempoolHasTx branch after interning.
Previously, that branch set wakeAcc to True unconditionally. For a fresh
key there was no active entry to remove and no other peer could reference
that key, yet sharedGeneration and its mirror advanced.

The branch now uses wakeAcc || not txKeyWasNew. Every interned key is active
or retained, and the retained branch was already handled, so a non-new key
here must be active. A fresh key needs no notification. The OR preserves a
wake required by an earlier batch item; processing the existing item later
also sets the flag correctly. Revision always advances for new retention.
No mirror or park implementation change is needed.

The Agda model proves that inserting retained membership under a distinct
fresh key preserves every other referenced key. fresh-hit-does-not-wake
proves the fixed event does not notify the generation waiter. The old
behaviour is preserved separately in legacy-fresh-hit-still-wakes.
The correspondence between fresh allocation and distinct peer references
is explicit, not a proof extracted from the Haskell allocator.

The Haskell reproducer creates a fast peer blocked on key 0 in another
peer's submission. Another advertisement allocates key 1 for a tx already
in the mempool. The existing entry and nextPeerAction result stay unchanged
(PeerDoNothing Nothing WakeCounter). Before the fix its STM waiter woke;
now the regression asserts that it remains asleep until the test timeout.
The production test also checks fresh-only, existing-only, and both mixed
batch orders, retention deadlines, and the receiving peer's ability to ack.

The detailed comments in CounterMirrors.agda retain the historical execution
and frequency analysis, clearly marked as describing the pre-fix behaviour.

Empty submission outcomes and rejection of already-gone keys also bump
generation/revision without payload changes. They are conservative cases,
not demonstrated lost progress; a well-behaved mempool writer partitions
its nonempty submission, so the empty-outcome case is not the normal path.

Validation and limitations
--------------------------

* Agda 2.8.0 type-checks the model without warnings.
* 24 Haskell transition/publication checks pass, including two Word64
  boundary cases and the composed pure revision bump.
* STM checks cover cold initial mirrors, post-own-write capture, a change
  before entering park, and a change after the waiter blocks.
* Existing TxSubmission simulation group plus the new mixed-batch
  mempool-wake regression: all 142 tests pass.
* The new regression failed before the fix (generation 18 instead of 17)
  for the fresh-only case.

The model does not formally translate Haskell, model all table contents,
or prove overall protocol liveness. The event classification was manually
audited and cross-checked with concrete Haskell examples. STM transaction
atomicity, publication through the wrapper, correct scheduler deadline
selection, and fair scheduling are assumptions.

The general counter proof uses naturals, not modular Word64. The Haskell
checks confirm single increments across maxBound wake correctly and still
publish revision zero. The general proof excludes a full 2^64-notification
cycle between capture and observation. It does not claim that such an ABA
cycle is impossible in the machine representation.

Re-run
------

From the repository root, with Agda 2.8.0 installed:

  agda-2.8.0 --no-libraries \
    -i ouroboros-network/models/tx-submission-counters \
    ouroboros-network/models/tx-submission-counters/CounterMirrors.agda

This needs no Agda standard library. If Agda's builtin data is not set up,
run agda-2.8.0 --setup first. This audit used a temporary Agda_datadir because
the sandbox cannot write the installation's default data directory.

Build the Haskell test component, then use the compiler/unit IDs matching
the project. The following command matches this audited checkout:

  cabal build ouroboros-network:ouroboros-network-sim-tests --offline -j4
  cabal exec --offline -- runghc-9.14.1 -XGHC2021 \
    -package-id=ouroboros-network-2.0.0.0-inplace \
    -package-id=ouroboros-network-2.0.0.0-inplace-ouroboros-network-tests-lib \
    ouroboros-network/models/tx-submission-counters/CounterAudit.hs

Existing suite:

  cabal run ouroboros-network:ouroboros-network-sim-tests --offline -j4 -- \
    -p '/TxSubmission/' --quickcheck-tests=100

Run logs for this audit are in /tmp/txsub-counter-audit/:
Initial audit: agda-2.8-check.log, haskell-audit.log, txsubmission-tests.log.
Fixed version: fix-agda-check.log, fix-haskell-audit.log,
fix-regression-before.log, fix-txsubmission-tests.log.
