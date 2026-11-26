{-# OPTIONS --safe --without-K #-}

module CounterMirrors where

-- Reading this model as a Haskell developer
-- ----------------------------------------
-- This is a pure state machine for the three counters and their mirrors.
-- It is a hand-written abstraction of V2.State and V2.Registry, not a
-- translation or a proof of the whole Haskell implementation. CounterAudit.hs
-- checks the chosen event classifications against actual Haskell functions.
--
-- The execution path is:
--   pureStep  : compute a proposed state after an event;
--   publish   : decide whether to commit it and update the mirrors;
--   capture   : remember the selected mirror in the deciding transaction;
--   parkEnds  : decide whether a later observation should end the wait.
--
-- Findings from the audit
-- -----------------------
-- No lost update or missed notification was found in the audited paths.
-- The main theorem, notification-exact-for-any-trace, proves notification
-- delivery for arbitrary finite sequences of the events modelled here.
--
-- The audit found and the current implementation fixes an avoidable
-- generation wake-up: a NEW shared key for a txid already in the mempool
-- used to notify peers that could not know that key. The fixed effect table
-- dirties revision without notifying generation. The last section retains
-- the original execution and a legacy witness, then proves the fixed event
-- leaves an unrelated waiter asleep. CounterAudit.hs checks the Haskell.
--
-- Empty submission outcomes and rejection of already-gone keys also notify
-- generation conservatively. Empty outcomes are not the normal path when
-- the mempool writer partitions its nonempty input as specified.
--
-- What the result assumes
-- -----------------------
-- STM commits are atomic and all relevant writes use the Registry wrapper.
-- The event table below accurately describes which counters Haskell bumps.
-- The model does NOT prove the scheduler's full table/queue semantics or
-- deadline selection, and does not prove overall protocol liveness.
-- Its counters are unbounded Nat values. A complete Word64 wrap between
-- capture and observation is outside the proof. The Haskell checks include
-- ordinary single-step wrap at maxBound, which still notifies correctly.
--
-- Validation: Agda 2.8.0 in safe mode, 24 Haskell transition/publication
-- cases, and all 142 TxSubmission simulation tests (including the new
-- mempool-wake regression) passed.
--
-- Agda notation used in the proofs
-- --------------------------------
--   Set        is roughly Haskell's Type.
--   Nat        is an unbounded natural; zero and suc are its constructors.
--   A → B      is a function type, as in Haskell.
--   (x : A) →  binds an argument that may appear in the result's type.
--   _==_       below computes a Bool; _≡_ is the TYPE of an equality proof.
--   refl       is accepted when both sides of that equality reduce to the
--              same expression. It is checked evidence, not an assertion.
--   with       splits on an expression, much like case; ... repeats the
--              preceding function's left-hand side in each clause.
--   rewrite    uses an already proved equality to simplify the current goal.
--   λ ()       handles an impossible argument, e.g. a proof of true ≡ false.
--   A × B      below is a pair: proving it means supplying both proofs.
--
-- The safe/without-K options above apply to the whole module. There are no
-- postulates or disabled termination checks hiding unproved obligations.

open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.List using (List; []; _∷_)

infixr 6 _∧_
infixr 5 _∨_
infix 4 _==_
infixr 4 _,_

_∧_ : Bool → Bool → Bool
true ∧ b = b
false ∧ b = false

_∨_ : Bool → Bool → Bool
true ∨ b = true
false ∨ b = b

not : Bool → Bool
not true = false
not false = true

or-false : (b : Bool) → (b ∨ false) ≡ b
or-false true = refl
or-false false = refl

_==_ : Nat → Nat → Bool
zero == zero = true
zero == suc _ = false
suc _ == zero = false
suc n == suc m = n == m

different : Nat → Nat → Bool
different n m = not (n == m)

same : (n : Nat) → (n == n) ≡ true
same zero = refl
same (suc n) = same n

successor-different : (n : Nat) → (suc n == n) ≡ false
successor-different zero = refl
successor-different (suc n) = successor-different n

record _×_ (A B : Set) : Set where
  constructor _,_
  field first : A; second : B
open _×_ public

-- Modelled data
-- -------------
-- These records are immutable values. Their constructors are ordinary
-- positional functions, like Haskell data constructors, with lowercase
-- names permitted by Agda. For example, store : Shared → Mirrors → Store.
-- Field names are projections: state old corresponds to Haskell state old.
--
-- Shared.revision   corresponds to sharedRevision: dirty flag / write stamp.
-- Shared.generation corresponds to sharedGeneration: general wake stamp.
-- Shared.claimable  corresponds to sharedClaimable: fetch-opportunity stamp.
-- payload is an opaque version of ALL the tables and other shared fields.
-- Changing it represents a structural change, not a count of transactions.

record Shared : Set where
  constructor shared
  field revision generation claimable payload : Nat
open Shared public

-- Values of the two mirror TVars at a transaction boundary. The model
-- contains the values, not TVars themselves; atomicity is an assumption.

record Mirrors : Set where
  constructor mirrors
  field wakeMirror claimMirror : Nat
open Mirrors public

-- Store combines shared state and the separate mirrors. Its fields are
-- state and mirror; shared/mirrors/store are the three record constructors.

record Store : Set where
  constructor store
  field state : Shared; mirror : Mirrors
open Store public

-- A proposal is the result of one pure state step. Publication follows
-- Registry.writeSharedStateIfChanged; the payload stands for all tables.
-- A False comparison means the pure step marked itself dirty. Construct
-- a NEW Store: state gets the proposed payload and wake stamps, with its
-- revision normalised to old + 1; mirror gets those same wake stamps.
-- The proposed revision is a dirty signal, not the committed write count.
-- In Haskell these changes are written in one STM transaction. Here they
-- are represented by returning one immutable value containing both.

publish : Store → Shared → Store
publish old proposed with revision proposed == revision (state old)
... | true = old
... | false =
  store (shared (suc (revision (state old)))
                (generation proposed)
                (claimable proposed)
                (payload proposed))
        (mirrors (generation proposed) (claimable proposed))

-- An invariant is a type describing evidence we want to have. Coherent s
-- says both mirror values equal their shared-state counterparts. A value of
-- this type contains the two equality proofs, joined by our pair constructor.
-- This invariant is intentionally weaker than protocol correctness: a lost
-- payload change can leave mirrors coherent with an incorrectly old state.

Coherent : Store → Set
Coherent s = (wakeMirror (mirror s) ≡ generation (state s))
           × (claimMirror (mirror s) ≡ claimable (state s))

-- This theorem permits arbitrary proposals, including a pure step that
-- bumps the revision several times: the committed revision is normalised.
publication-coherent : (s : Store) (p : Shared) → Coherent s →
                       Coherent (publish s p)
publication-coherent s p ok with revision p == revision (state s)
... | true = ok
... | false = refl , refl

-- Read this signature as: for every old store and proposal, IF their
-- revisions compare unequal, the committed revision is old revision + 1.
-- This still holds when a composed pure decision bumped revision twice.
-- The true branch below has an impossible premise, hence λ ().

dirty-revision-one : (s : Store) (p : Shared) →
  (revision p == revision (state s)) ≡ false →
  revision (state (publish s p)) ≡ suc (revision (state s))
dirty-revision-one s p with revision p == revision (state s)
... | true = λ ()
... | false = λ _ → refl

dirty-payload-preserved : (s : Store) (p : Shared) →
  (revision p == revision (state s)) ≡ false →
  payload (state (publish s p)) ≡ payload p
dirty-payload-preserved s p with revision p == revision (state s)
... | true = λ ()
... | false = λ _ → refl

-- Mirror values alone do not describe STM scheduling cost. A same-value
-- writeTVar can make a waiting transaction recheck and retry. We therefore
-- also model which TVars a publication writes, following writeIfChanged.

record Writes : Set where
  constructor writes
  field stateWrite wakeWrite claimWrite : Bool
open Writes public

-- An STM write, including a same-value write, may re-run subscribers.
-- Track writes explicitly instead of conflating them with value changes.
writeSet : Store → Shared → Writes
writeSet old proposed with revision proposed == revision (state old)
... | true = writes false false false
... | false = writes true
  (different (generation proposed) (wakeMirror (mirror old)))
  (different (claimable proposed) (claimMirror (mirror old)))

-- A convenient way to construct an initially coherent store. The later
-- unbounded theorem uses this shape with arbitrary r, g, c and p values.

aligned : Nat → Nat → Nat → Nat → Store
aligned r g c p = store (shared r g c p) (mirrors g c)

structural-write-does-not-touch-mirrors : (r g c p : Nat) →
  writeSet (aligned r g c p) (shared (suc r) g c (suc p)) ≡ writes true false false
structural-write-does-not-touch-mirrors r g c p
  rewrite successor-different r | same g | same c = refl

-- Conversely, an unchanged revision causes the WHOLE proposal to be
-- discarded, regardless of its payload. This is required by the wrapper's
-- design, so every real structural change must arrange a revision bump.
-- missing-revision-loses-change below demonstrates that caller obligation.

clean-proposal-dropped : (s : Store) (p : Shared) →
  (revision p == revision (state s)) ≡ true → publish s p ≡ s
clean-proposal-dropped s p with revision p == revision (state s)
... | true = λ _ → refl
... | false = λ ()

data Counter : Set where wake claim none : Counter

-- resolvePeerAction's assertion: NoCounter requires a timer. A tier
-- change is an additional wake source, but need not ever happen.
data HasWakeSource : Counter → Bool → Set where
  wake-source : (timer : Bool) → HasWakeSource wake timer
  claim-source : (timer : Bool) → HasWakeSource claim timer
  timer-source : HasWakeSource none true

data Empty : Set where

untimed-no-counter-impossible : HasWakeSource none false → Empty
untimed-no-counter-impossible ()

-- Registry.resolvePeerAction reads the selected mirror, not a possibly
-- different shared-state field. NoCounter's dummy value is zero.
-- capture is part of resolving a decision, AFTER that transaction's own
-- publication and BEFORE it commits. It must read the selected mirror.
-- Reading the shared-state stamp instead is unsafe for a supplied initial
-- state whose counters are ahead of the mirrors, which start at zero.
-- NoCounter needs no observed stamp; its dummy zero is never compared.

capture : Counter → Store → Nat
capture wake s = wakeMirror (mirror s)
capture claim s = claimMirror (mirror s)
capture none s = zero

-- A peer waits for inequality, not a particular next number: several
-- notifications can accumulate while it is asleep and still be detected.

counterMoved : Counter → Nat → Store → Bool
counterMoved wake seen s = different (wakeMirror (mirror s)) seen
counterMoved claim seen s = different (claimMirror (mirror s)) seen
counterMoved none seen s = false

-- Registry.awaitParkEnd also watches timer expiry and the assigned tier.
-- True means the real STM check may return; False means it retries.
-- expired and tierChanged are explicit inputs abstracting the timer and
-- peer-tier TVars. Their scheduling/implementation is outside this model.

parkEnds : Counter → Nat → Store → Bool → Bool → Bool
parkEnds k seen s expired tierChanged =
  counterMoved k seen s ∨ expired ∨ tierChanged

-- Taking a baseline from the mirror and immediately observing the same
-- store cannot cause a spontaneous wake, even if state/mirrors disagree.
-- This is stronger than assuming all initial mirrors are already coherent.

fresh-capture-does-not-wake : (k : Counter) (s : Store) →
  parkEnds k (capture k s) s false false ≡ false
fresh-capture-does-not-wake wake s rewrite same (wakeMirror (mirror s)) = refl
fresh-capture-does-not-wake claim s rewrite same (claimMirror (mirror s)) = refl
fresh-capture-does-not-wake none s = refl

timer-wakes : (k : Counter) (seen : Nat) (s : Store) →
             parkEnds k seen s true false ≡ true
timer-wakes k seen s with counterMoved k seen s
... | true = refl
... | false = refl

tier-change-wakes : (k : Counter) (seen : Nat) (s : Store) →
                   parkEnds k seen s false true ≡ true
tier-change-wakes k seen s with counterMoved k seen s
... | true = refl
... | false = refl

-- Model the actual event classes, rather than making generation equal
-- revision. 'emptySubmission' and 'rejectGone' deliberately retain the
-- implementation's unconditional generation/revision bump.
data Event : Set where
  idle freshAd duplicateAd sparedAd retainedAd leaseClaim beginSubmission
    bodyBuffered mempoolHit freshMempoolHit omitLive capBump accept rejectLive scrubLive
    scrubGone expire doom review emptySubmission rejectGone : Event

-- Per-event classification, in positional order:
--   dirty         : advance revision so the proposal is published;
--   notifyWake    : advance generation;
--   notifyClaim   : advance claimable;
--   changePayload : change some non-counter shared field.
-- These flags describe a BATCH/step, not one bump per individual tx.
-- Buffering bodies can change only peer-local state, hence no shared delta.
--
-- This table is an INPUT to the model. The proofs check consequences and
-- consistency of it; they do not mechanically extract it from Haskell.
-- CounterAudit.hs exercises actual Haskell examples for these classes.

record Delta : Set where
  constructor delta
  field dirty notifyWake notifyClaim changePayload : Bool
open Delta public

-- Notable rows to compare with the implementation:
-- * leaseClaim/beginSubmission/sweep changes only dirty the shared state;
-- * accept/mempoolHit additionally wake generation subscribers;
-- * omission/rejection/cap increase additionally wake claimable subscribers.
-- Thus generation and revision must not be treated as the same counter.

effect : Event → Delta
effect idle = delta false false false false
effect freshAd = delta true false false true
effect duplicateAd = delta false false false false
effect sparedAd = delta true false false true
effect retainedAd = delta false false false false
effect leaseClaim = delta true false false true
effect beginSubmission = delta true false false true
effect bodyBuffered = delta false false false false
effect mempoolHit = delta true true false true
effect freshMempoolHit = delta true false false true
effect omitLive = delta true true true true
effect capBump = delta true true true true
effect accept = delta true true false true
effect rejectLive = delta true true true true
effect scrubLive = delta true true true true
effect scrubGone = delta false false false false
effect expire = delta true false false true
effect doom = delta true false false true
effect review = delta true false false true
effect emptySubmission = delta true true false false
effect rejectGone = delta true true false false

bump : Bool → Nat → Nat
bump false n = n
bump true n = suc n

pureStep : Event → Shared → Shared
pureStep e s = shared (bump (dirty d) (revision s))
  (bump (notifyWake d) (generation s))
  (bump (notifyClaim d) (claimable s))
  (bump (changePayload d) (payload s))
  where d = effect e

-- One step is one committed transaction. Actual Haskell decisions may
-- compose several pure changes before publishing once. publish's general
-- revision-normalisation theorem covers that case; the Haskell harness
-- checks capBump + beginSubmission with pure revision delta 2, committed 1.

step : Event → Store → Store
step e s = publish s (pureStep e (state s))

-- This is the normal form of the real publication algorithm for the
-- event table. Rewriting by these lemmas discharges its equality test.
selectPublication : Bool → Store → Delta → Store
selectPublication false s d = s
selectPublication true s d = store
  (shared (suc (revision (state s)))
    (bump (notifyWake d) (generation (state s)))
    (bump (notifyClaim d) (claimable (state s)))
    (bump (changePayload d) (payload (state s))))
  (mirrors (bump (notifyWake d) (generation (state s)))
           (bump (notifyClaim d) (claimable (state s))))

publication-form : (d : Delta) (s : Store) →
  publish s (shared (bump (dirty d) (revision (state s)))
                    (bump (notifyWake d) (generation (state s)))
                    (bump (notifyClaim d) (claimable (state s)))
                    (bump (changePayload d) (payload (state s)))) ≡
  (selectPublication (dirty d) s d)
publication-form (delta false g c p) s rewrite same (revision (state s)) = refl
publication-form (delta true g c p) s rewrite successor-different (revision (state s)) = refl

-- A relevant event must make the revision dirty; check all finite event
-- constructors independently of the commit/read protocol.
eventWellMarked : Event → Bool
eventWellMarked e = not (notifyWake d ∨ notifyClaim d ∨ changePayload d) ∨ dirty d
  where d = effect e

events : List Event
events = idle ∷ freshAd ∷ duplicateAd ∷ sparedAd ∷ retainedAd ∷ leaseClaim ∷
  beginSubmission ∷ bodyBuffered ∷ mempoolHit ∷ freshMempoolHit ∷ omitLive ∷ capBump ∷ accept ∷
  rejectLive ∷ scrubLive ∷ scrubGone ∷ expire ∷ doom ∷ review ∷
  emptySubmission ∷ rejectGone ∷ []

all : {A : Set} → (A → Bool) → List A → Bool
all f [] = true
all f (x ∷ xs) = f x ∧ all f xs

-- This finite proof is computation: Agda evaluates all 21 table rows and
-- accepts refl only if every notification/payload change also marks dirty.
-- It checks the table's consistency, not every Haskell execution.

all-events-mark-dirty : all eventWellMarked events ≡ true
all-events-mark-dirty = refl

claimable-also-notifies-wake : Event → Bool
claimable-also-notifies-wake e = not (notifyClaim (effect e)) ∨ notifyWake (effect e)

all-claimable-events-notify-wake : all claimable-also-notifies-wake events ≡ true
all-claimable-events-notify-wake = refl

-- A concrete exhaustive search over three-event traces. Unlike an event
-- count comparison, this checks capture at a fixed point followed by
-- arbitrarily mixed events for both mirrors (including unrelated writes).
trace : List Event → Store → Store
trace [] s = s
trace (e ∷ es) s = trace es (step e s)

expectedBump : Counter → Event → Bool
expectedBump wake e = notifyWake (effect e)
expectedBump claim e = notifyClaim (effect e)
expectedBump none e = false

anyRelevant : Counter → List Event → Bool
anyRelevant k [] = false
anyRelevant k (e ∷ es) = expectedBump k e ∨ anyRelevant k es

sameBool : Bool → Bool → Bool
sameBool true true = true
sameBool false false = true
sameBool _ _ = false

coherentBool : Store → Bool
coherentBool s =
  (wakeMirror (mirror s) == generation (state s)) ∧
  (claimMirror (mirror s) == claimable (state s))

traceCheck : List Event → Bool
traceCheck es = coherentBool end ∧
  sameBool (parkEnds wake (capture wake start) end false false) (anyRelevant wake es) ∧
  sameBool (parkEnds claim (capture claim start) end false false) (anyRelevant claim es) ∧
  sameBool (parkEnds none (capture none start) end false false) false
  where
  start = store (shared (suc (suc zero)) (suc zero) (suc zero) zero)
                (mirrors (suc zero) (suc zero))
  end = trace es start

search : Nat → List Event → Bool
search zero prefix = traceCheck prefix
search (suc n) prefix = traceCheck prefix ∧ all (λ e → search n (e ∷ prefix)) events

-- search explores 1 + 21 + 21^2 + 21^3 = 9,724 trace prefixes. It checks
-- both mirrors, unrelated writes, and NoCounter, from a coherent nonzero
-- starting state. This is a bounded exhaustive check; the later theorem
-- removes the bound on trace length and initial natural counter values.

all-traces-through-three-events : search (suc (suc (suc zero))) [] ≡ true
all-traces-through-three-events = refl

-- Unbounded proofs, to avoid mistaking the finite search for a general
-- no-lost-wakeup theorem. A coherent store stays canonical after each
-- event; revision-only writes and silent events leave both stamps alone.
event-form : (e : Event) (r g c p : Nat) →
  step e (aligned r g c p) ≡ aligned
    (bump (dirty (effect e)) r)
    (bump (notifyWake (effect e)) g)
    (bump (notifyClaim (effect e)) c)
    (bump (changePayload (effect e)) p)
event-form idle r g c p rewrite same r = refl
event-form freshAd r g c p rewrite successor-different r = refl
event-form duplicateAd r g c p rewrite same r = refl
event-form sparedAd r g c p rewrite successor-different r = refl
event-form retainedAd r g c p rewrite same r = refl
event-form leaseClaim r g c p rewrite successor-different r = refl
event-form beginSubmission r g c p rewrite successor-different r = refl
event-form bodyBuffered r g c p rewrite same r = refl
event-form mempoolHit r g c p rewrite successor-different r = refl
event-form freshMempoolHit r g c p rewrite successor-different r = refl
event-form omitLive r g c p rewrite successor-different r = refl
event-form capBump r g c p rewrite successor-different r = refl
event-form accept r g c p rewrite successor-different r = refl
event-form rejectLive r g c p rewrite successor-different r = refl
event-form scrubLive r g c p rewrite successor-different r = refl
event-form scrubGone r g c p rewrite same r = refl
event-form expire r g c p rewrite successor-different r = refl
event-form doom r g c p rewrite successor-different r = refl
event-form review r g c p rewrite successor-different r = refl
event-form emptySubmission r g c p rewrite successor-different r = refl
event-form rejectGone r g c p rewrite successor-different r = refl

-- Proof helper: look only at a chosen counter through the same trace.
-- mirror-trace proves that this scalar calculation agrees with running
-- the full publication model; it does not assume that agreement.

runCounter : Counter → List Event → Nat → Nat
runCounter k [] n = n
runCounter k (e ∷ es) n = runCounter k es (bump (expectedBump k e) n)

startingValue : Counter → Nat → Nat → Nat
startingValue wake g c = g
startingValue claim g c = c
startingValue none g c = zero

mirror-trace : (k : Counter) (es : List Event) (r g c p : Nat) →
  capture k (trace es (aligned r g c p)) ≡ runCounter k es (startingValue k g c)
mirror-trace wake [] r g c p = refl
mirror-trace claim [] r g c p = refl
mirror-trace none [] r g c p = refl
mirror-trace wake (e ∷ es) r g c p rewrite event-form e r g c p =
  mirror-trace wake es (bump (dirty (effect e)) r)
    (bump (notifyWake (effect e)) g) (bump (notifyClaim (effect e)) c)
    (bump (changePayload (effect e)) p)
mirror-trace claim (e ∷ es) r g c p rewrite event-form e r g c p =
  mirror-trace claim es (bump (dirty (effect e)) r)
    (bump (notifyWake (effect e)) g) (bump (notifyClaim (effect e)) c)
    (bump (changePayload (effect e)) p)
mirror-trace none (e ∷ es) r g c p = mirror-trace none es r g c p

-- The remaining helper proofs establish that a natural counter cannot
-- return to its baseline after at least one increment. _≤_ is an indexed
-- datatype, much like a GADT whose constructors are evidence of an order:
-- zero ≤ anything, and n ≤ m implies suc n ≤ suc m.

infix 4 _≤_
data _≤_ : Nat → Nat → Set where
  z≤n : {n : Nat} → zero ≤ n
  s≤s : {n m : Nat} → n ≤ m → suc n ≤ suc m

≤-refl : (n : Nat) → n ≤ n
≤-refl zero = z≤n
≤-refl (suc n) = s≤s (≤-refl n)

≤-step : {n m : Nat} → n ≤ m → n ≤ suc m
≤-step z≤n = z≤n
≤-step (s≤s p) = s≤s (≤-step p)

≤-trans : {n m k : Nat} → n ≤ m → m ≤ k → n ≤ k
≤-trans z≤n q = z≤n
≤-trans (s≤s p) (s≤s q) = s≤s (≤-trans p q)

counter-monotone : (k : Counter) (es : List Event) (n : Nat) → n ≤ runCounter k es n
counter-monotone k [] n = ≤-refl n
counter-monotone k (e ∷ es) n with expectedBump k e
... | false = counter-monotone k es n
... | true = ≤-trans (≤-step (≤-refl n)) (counter-monotone k es (suc n))

no-bumps-unchanged : (k : Counter) (es : List Event) (n : Nat) →
  anyRelevant k es ≡ false → runCounter k es n ≡ n
no-bumps-unchanged k [] n _ = refl
no-bumps-unchanged k (e ∷ es) n with expectedBump k e
... | false = no-bumps-unchanged k es n
... | true = λ ()

some-bump-increases : (k : Counter) (es : List Event) (n : Nat) →
  anyRelevant k es ≡ true → suc n ≤ runCounter k es n
some-bump-increases k [] n ()
some-bump-increases k (e ∷ es) n with expectedBump k e
... | false = some-bump-increases k es n
... | true = λ _ → counter-monotone k es (suc n)

strict-different : (n m : Nat) → suc n ≤ m → different m n ≡ true
strict-different n zero ()
strict-different zero (suc m) _ = refl
strict-different (suc n) (suc m) (s≤s p) = strict-different n m p

-- Combine the two cases: no relevant bumps leaves the counter unchanged;
-- at least one relevant bump makes it strictly larger, so equality fails.
-- This argument relies on unbounded Nat and would need a no-full-wrap
-- assumption when applied to the actual Word64 counters.

counter-changed-exactly : (k : Counter) (es : List Event) (n : Nat) →
  different (runCounter k es n) n ≡ anyRelevant k es
counter-changed-exactly k es n with anyRelevant k es in found
... | true = strict-different n (runCounter k es n) (some-bump-increases k es n found)
... | false rewrite no-bumps-unchanged k es n found | same n = refl

none-never-relevant : (es : List Event) → anyRelevant none es ≡ false
none-never-relevant [] = refl
none-never-relevant (e ∷ es) = none-never-relevant es

-- Applies to an arbitrary finite sequence of commits after the deciding
-- transaction, whether those commits land before or after entering retry.
-- Main result in English:
-- For ANY finite event list and ANY coherent initial natural counter values,
-- with no timer expiry or tier change, a park ends exactly when the chosen
-- counter was notified at least once since capture. The NoCounter case
-- ignores all shared notifications, as intended.
--
-- Because STM gives an atomic deciding transaction, commits after capture
-- can be considered in this order whether they happen before entering the
-- blocking check or while it is blocked. This proves the model has no lost
-- notification in that interval. Fair execution of the waiting thread is
-- still an assumption; this is not a proof of full protocol liveness.
--
-- Importantly, "relevant" here means an effect-table notification for this
-- COUNTER, not necessarily new usable work for this PARTICULAR PEER. A
-- shared counter can conservatively wake several peers, only one of which
-- can act. The avoidable fresh-mempool-hit notification below is stronger:
-- no other peer can reference that freshly allocated key at all.

notification-exact-for-any-trace : (k : Counter) (es : List Event) (r g c p : Nat) →
  parkEnds k (capture k (aligned r g c p)) (trace es (aligned r g c p)) false false ≡
  anyRelevant k es
notification-exact-for-any-trace wake es r g c p
  rewrite mirror-trace wake es r g c p |
          or-false (different (runCounter wake es g) g) = counter-changed-exactly wake es g
notification-exact-for-any-trace claim es r g c p
  rewrite mirror-trace claim es r g c p |
          or-false (different (runCounter claim es c) c) = counter-changed-exactly claim es c
notification-exact-for-any-trace none es r g c p
  rewrite none-never-relevant es = refl

-- Counterexamples for broken variants. These are checked witnesses,
-- not postulates, and do not claim that those variants are in the code.
-- Regression witnesses for BROKEN ALTERNATIVES, not three current bugs.
-- Each equality is a small executable counterexample checked by reduction.
-- These explain why the implementation's publication/capture order matters.

cold : Store
cold = store (shared zero (suc zero) (suc zero) zero) (mirrors zero zero)

-- Reading seen from sharedGeneration would wake immediately on an
-- unchanged store when a supplied initial state is ahead of its mirrors.
wrong-baseline-spins : parkEnds wake (generation (state cold)) cold false false ≡ true
wrong-baseline-spins = refl

correct-cold-baseline : parkEnds wake (capture wake cold) cold false false ≡ false
correct-cold-baseline = refl

initial : Store
initial = store (shared zero zero zero zero) (mirrors zero zero)

-- If an enabling event forgets the revision bump, the wrapper silently
-- discards both its changed payload and its notification.
forgotRevision : Shared
forgotRevision = shared zero (suc zero) (suc zero) (suc zero)

missing-revision-loses-change : publish initial forgotRevision ≡ initial
missing-revision-loses-change = refl

-- A later, non-atomic capture can swallow a notification that landed
-- between the decision and capture: seen becomes the new mirror value.
afterRelease : Store
afterRelease = step omitLive initial

-- Imagine deciding with the old store, then letting a release commit,
-- then taking seen in a separate transaction. It would already include
-- that release, so the later park would incorrectly have nothing to detect.
-- The actual code resolves seen in the deciding transaction and avoids this.

separate-capture-loses-notification :
  parkEnds wake (capture wake afterRelease) afterRelease false false ≡ false
separate-capture-loses-notification = refl

atomic-capture-sees-notification :
  parkEnds wake (capture wake initial) afterRelease false false ≡ true
atomic-capture-sees-notification = refl

-- Current implementation: some generation bumps are conservative.
-- No table payload changes for an empty outcome or a rejected gone key,
-- but a WakeCounter waiter is still notified.
-- These are conservative notifications in the actual pure transition.
-- They do not demonstrate a missed wake or a lost transaction. In particular,
-- emptySubmission is a direct function-level example; a conforming mempool
-- writer normally returns a nonempty partition for a nonempty submission.

empty-submission-wakes :
  parkEnds wake (capture wake initial) (step emptySubmission initial) false false ≡ true
empty-submission-wakes = refl

gone-rejection-wakes :
  parkEnds wake (capture wake initial) (step rejectGone initial) false false ≡ true
gone-rejection-wakes = refl

-- Fixed performance finding: a fresh key already in the mempool
-- -----------------------------------------------------------------
-- The old code emitted an unnecessary NOTIFICATION, not a failed mirror
-- update. The fix changes State's decision to bump generation, while keeping
-- revision and retention updates. Mirror publication needs no change.
-- The execution and frequency discussion below describe the OLD behaviour;
-- the fixed-behaviour and legacy witnesses at the end distinguish the two.
--
-- Why this state can occur
-- ------------------------
-- The mempool and SharedTxState are different stores. A transaction can be
-- in the mempool without a current shared key: for example it arrived via
-- local submission, or its previous shared retention marker has expired.
-- Thus a newly advertised txid can be BOTH new to SharedTxState and already
-- known to the mempool. Fresh in this discussion means a freshly allocated
-- SHARED KEY, not a transaction newly accepted into the mempool.
--
-- The Haskell branch that caused the notification before the fix
-- -----------------------------------------------
-- In V2.State.handleReceivedTxIds, step calls lookupOrInternTxId before
-- classification. The mempoolHasTx branch then:
--   1. removes k from sharedTxTable (a no-op for a fresh key),
--   2. inserts k into sharedRetainedTxs,
--   3. sets the accumulated wake flag to True unconditionally,
--   4. sets the revision flag to True.
-- At the end of the batch, the wake flag causes BOTH sharedGeneration and
-- sharedRevision to advance. sharedClaimable stays unchanged. The Registry
-- publishes those values and updates generationMirror in the same STM commit.
--
-- Original execution and the fixed regression checked by CounterAudit.hs
-- -----------------------------------------------------------------------
-- Before:
--   shared counters (revision,generation,claimable) = (17,5,3)
--   mirrors (generation,claimable)                = (5,3)
--   B's unacknowledged queue                     = [key 0]
--   key 0                                        = in submission elsewhere
--   B's next action                              = PeerDoNothing Nothing WakeCounter
--   B's captured generationMirror                = 5
--
-- B cannot fetch or submit key 0 while it is in submission, and cannot
-- acknowledge it until it is resolved. There is no timer due in this example.
-- B therefore blocks waiting for generationMirror to differ from 5.
--
-- Peer A now advertises transaction X, which the mempool already holds.
-- lookupOrInternTxId allocates fresh key 1 for X. The mempool-hit branch
-- retains key 1 and sets the wake flag, giving:
--   shared counters                              = (18,6,3)
--   mirrors                                      = (6,3)
--   B's queue                                    = [key 0], unchanged
--   key 0                                        = unchanged
--
-- B wakes because 6 /= 5. It re-runs nextPeerAction and gets exactly the same
-- action: PeerDoNothing Nothing WakeCounter. It captures 6 and parks again.
-- Neither a useful acknowledgement nor a body request nor submission became
-- available to B. The harness checks its entry and scheduling result before
-- and after, and demonstrates the actual STM wait returning on the bump.
--
-- Why no OTHER peer needs this particular notification
-- ----------------------------------------------------
-- Key allocation is monotonic, so an earlier peer queue cannot contain the
-- newly allocated key 1. Only A has just learned that key. A is already
-- running its reply handler and will make another scheduling decision; it
-- does not need a broadcast notification to discover the retention.
-- Adding retained membership for key 1 changes no entry any older peer can
-- reference. The original key-allocation invariant is an assumption of this
-- footprint argument, rather than something proved from the Haskell here.
--
-- Distinguish three cases
-- -----------------------
--   Advertisement already retained: existing retained branch, no new wake.
--   Existing active key found in mempool: wake is useful; other advertisers
--     can acknowledge it, and buffered bodies for it can be dropped.
--   Fresh shared key found in mempool: no other peer references that key;
--     retaining it still needs revision, but the generation wake is avoidable.
-- The fix suppresses only the third case. Existing-active resolutions still
-- wake other peers, including in mixed batches in either advertisement order.
--
-- Impact and limits of the reproduction
-- -------------------------------------
-- The demonstrated cost is extra scheduler work for WakeCounter subscribers.
-- One such batch advances generation once, even if several txids hit this
-- branch. Multiple subscribers may wake and scan their own state; several
-- notifications can also coalesce before a subscriber runs. ClaimableCounter
-- and NoCounter subscribers are unaffected by this generation-only change.
-- The audit has not measured CPU savings or established that this path is
-- frequent enough to dominate real workloads. It has not found lost
-- transactions, a missed notification, or failed progress in this example.
--
-- How often can this happen?
-- ---------------------------
-- This analysis describes how often the OLD conservative notification
-- could occur. The fixed fresh-key path emits none of these updates.
-- The estimates come from Haskell policy; this model has no clock or traffic
-- distribution and does not derive an empirical event rate.
--
-- Defaults: retained lifetime 2 s; slow cadence 500 ms with +/-20% jitter;
-- at most 10 requested txids per message and a 10-txid unacknowledged window.
--
-- Prompt duplicate advertisements are normally suppressed by retention.
-- While a marker remains present, the retainedMember branch runs first and
-- does not advance generation. It also does not refresh that marker's TTL.
-- After expiry AND sweeping remove the key, the next mempool-known
-- advertisement may allocate another fresh key and cause another wake.
-- The first such advertisement recreates retention for the whole node, so
-- other peers advertising the same txid during that lifetime do not each
-- cause this update. For one txid, the normal case is at most one fresh
-- hit per roughly 2 s of retention; a delayed sweep extends that interval.
-- The deadline uses the reply's sampled timestamp, so this is not a strict
-- minimum between wall-clock commit times if publication is badly delayed.
-- A peer normally advances past acknowledged IDs: repeated misses for one
-- ID require another late advertisement, e.g. from a different lagging peer
-- or a reconnect, not merely repeated polling of the same peer.
--
-- This is not a global one-update-per-2-seconds bound: different txids can
-- miss the cache in different batches. A batch causes one avoidable bump
-- when it contains a fresh mempool-known key AND no other item already
-- needs a useful generation notification. If an existing active key is
-- resolved in the same batch, the generation bump would be needed anyway.
--
-- Situations where misses can be frequent:
-- * Locally submitted transactions can be in the mempool before this
--   subsystem has ever allocated shared keys for them.
-- * A connecting/reconnecting peer may advertise an old mempool backlog.
-- * A slow peer can lag beyond the 2 s cache lifetime. Full 10-id replies
--   at a nominal 500 ms cadence carry roughly 20 ids/s before round-trip
--   and processing time. About 40 queued IDs can therefore account for
--   2 s of backlog, provided the transactions remain in the peer's mempool.
--   If an incoming stream outpaces that rate, this need not be a rare case.
--
-- Illustrative workload scenario, NOT a measured rate or bound: 100 slow peers
-- replying twice per second could cause roughly 200 avoidable generation
-- updates/s if each batch misses on different old, mempool-known IDs and
-- contains no independently useful wake. RTT, smaller/empty replies,
-- common IDs protected by shared retention, and batching reduce that rate.
-- Fast peers catching up are not bounded by the slow cadence.
--
-- Update rate is not the same as scheduler/CPU cost. Only WakeCounter
-- subscribers react to this bump; slow peers parked on ClaimableCounter or
-- NoCounter do not. Several updates may coalesce before a waiter runs, and
-- a peer already running is not additionally woken from a park.
-- To quantify the opportunity, count the fresh-hit-only batches described
-- above and the redundant scheduler decisions they cause. Counting every
-- duplicate txid, or every mempool hit, would overestimate the benefit.
-- No real-workload frequency or CPU saving has been measured in this audit.
--
-- Applied fix and why mixed batches are safe
-- ------------------------------------------
-- The mempool-hit branch now uses the already computed interning flag:
--
--   wakeAcc' = wakeAcc || not txKeyWasNew
--
-- Under the shared-key invariant, every interned key is active or retained.
-- The retained case was handled earlier. Therefore a non-new key reaching
-- this branch is active, and resolving it can enable other peers to ack.
-- A fresh key is known only to this peer and needs no broadcast. The OR
-- preserves an existing item's wake when a later item is fresh; processing
-- the fresh item first also allows a later existing item to set the flag.
-- This adds no map lookup. The revision flag stays True for fresh retention.
--
-- What Agda establishes about this finding
-- ----------------------------------------
-- fresh-hit-preserves-other-reference below proves the insertion footprint:
-- for ANY table and ANY key distinct from the inserted key, lookup is
-- unchanged. fresh-hit-does-not-wake proves that the FIXED event leaves
-- a generation waiter asleep; legacy-fresh-hit-still-wakes retains the
-- original counterexample against the old unconditional-notification rule.
-- The Haskell harness checks the fixed real scheduling example;
-- this is not an automatically extracted refinement proof of the allocator,
-- all queues, or the complete Haskell scheduler.
--
-- The small table model represents lookup by a function Nat → Residence.

data Residence : Set where absent active retained : Residence

insertRetained : Nat → (Nat → Residence) → Nat → Residence
insertRetained fresh old key with key == fresh
... | true = retained
... | false = old key

-- Quantifies over any previous table and every key an observer references;
-- freshness (from Haskell's monotonically allocated keys) is explicit.
-- The distinctness premise is explicit. It follows informally from the
-- Haskell allocator/queue invariants; that allocator is not proved here.
-- This lemma also does not model every queue or the full fetch predicate.

fresh-hit-preserves-other-reference :
  (fresh key : Nat) (old : Nat → Residence) →
  (key == fresh) ≡ false → insertRetained fresh old key ≡ old key
fresh-hit-preserves-other-reference fresh key old with key == fresh
... | true = λ ()
... | false = λ _ → refl

-- The old rule is kept separate from effect so the current event table
-- continues to describe the fixed Haskell. This is a historical witness,
-- not the result expected from step freshMempoolHit now.
legacyFreshMempoolHit : Store → Store
legacyFreshMempoolHit s = publish s
  (shared (suc (revision (state s)))
          (suc (generation (state s)))
          (claimable (state s))
          (suc (payload (state s))))

legacy-fresh-hit-still-wakes :
  parkEnds wake (capture wake initial) (legacyFreshMempoolHit initial) false false ≡ true
legacy-fresh-hit-still-wakes = refl

-- Regression: dirty revision still publishes the retained key, but this
-- event no longer changes generation or its mirror, so the waiter sleeps.
fresh-hit-does-not-wake :
  parkEnds wake (capture wake initial) (step freshMempoolHit initial) false false ≡ false
fresh-hit-does-not-wake = refl
