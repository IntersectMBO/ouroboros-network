# Ledger-HD sketch (UTxO-HD v2)

This document describes the result of the discussion between Ledger and
Consensus teams on 2022-01-17 about the future steps on UTxO-HD which would not
make sense to be called this way anymore in future versions and therefore we
propose Ledger-HD as a replacement.

Below, we outline the tables that are expected to be moved to the disk, their
dynamics, the main computations that will leave the Ledger and some open
questions.

This is meant to be just a sketch, details are not worked out yet.

## Scope of Ledger-HD

The plan for Ledger-HD is to move the following tables to the disk (we will show
the "lenses" that reach each data structure from the new epoch state):

- The unified map of `rewards`, `delegations`, `pointers` and `deposits`

```haskell
( ne :: NewEpochState era )
  & ( nesEs     :: NewEpochState      era  -> EpochState         era  )
  & ( esLState  :: EpochState         era  -> LedgerState        era  )
  & ( lsDPState :: LedgerState        era  -> DPState (EraCrypto era) )
  & ( dpsDState :: DPState (EraCrypto era) -> DState  (EraCrypto era) )
  & ( dsUnified :: DState  (EraCrypto era) -> UMap    (EraCrypto era) )

data UMap c = UMap !(Map (Credential 'Staking c) (Trip c)) !(Map Ptr (Credential 'Staking c))
```

- The current stake distribution per stake credential

```haskell
( ne :: NewEpochState era )
  & ( nesEs           :: NewEpochState era -> EpochState                  era  )
  & ( esLState        :: EpochState    era -> LedgerState                 era  )
  & ( lsUTxOState     :: LedgerState   era -> UTxOState                   era  )
  & ( utxosStakeDistr :: UTxOState     era -> IncrementalStake (EraCrypto era) )

data IncrementalStake c = IStake
  { credMap :: !(Map (Credential 'Staking c) Coin)
  , ptrMap  :: !(Map Ptr Coin)
  }
```

- The stake snapshots

```haskell
( ne :: NewEpochState era )
  & ( nesEs       :: NewEpochState era -> EpochState           era  )
  & ( esSnapshots :: EpochState    era -> SnapShots (EraCrypto era) )

data SnapShots c = SnapShots
  { ssStakeMark          :: SnapShot c  -- Lazy on purpose
  , ssStakeMarkPoolDistr :: PoolDistr c -- Lazy on purpose
  , ssStakeSet           :: !(SnapShot c)
  , ssStakeGo            :: !(SnapShot c)
  , ssFee                :: !Coin
  }

data SnapShot c = SnapShot
  { ssStake       :: !(Stake c)
  , ssDelegations :: !(VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c))
  , ssPoolParams  :: !(VMap VB VB (KeyHash 'StakePool c) (PoolParams c))
  }

newtype Stake c = Stake
  { unStake :: VMap VB VP (Credential 'Staking c) (CompactForm Coin)
  }

newtype PoolDistr c = PoolDistr
  { unPoolDistr ::
      Map (KeyHash 'StakePool c) (IndividualPoolStake c)
  }

data IndividualPoolStake c = IndividualPoolStake
  { individualPoolStake    :: !Rational
  , individualPoolStakeVrf :: !(Hash c (VerKeyVRF c))
  }
```

As noted by [@JaredCorduan](https://github.com/JaredCorduan), after CIP-1694 is
complete, there will probably also be a `ssStakeMarkDRepDistr :: Map (KeyHash
'DRep c) Coin` and a `ssDRepDelegations :: VMap VB VB (Credential 'Staking c)
(KeyHash 'DRep c)`.

- The reward update

```haskell
( ne :: NewEpochState era )
  & ( nesRu :: NewEpochState era -> StrictMaybe (PulsingRewUpdate (EraCrypto era)) )
  
data RewardUpdate c = RewardUpdate
  { deltaT    :: !DeltaCoin
  , deltaR    :: !DeltaCoin
  , rs        :: !(Map (Credential 'Staking c) (Set (Reward c)))
  , deltaF    :: !DeltaCoin
  , nonMyopic :: !(NonMyopic c)
  }
```

Where the `PulsingRewUpdate` is just a mechanism in order to pulse through the
stake snapshot and in the end yield a `RewardUpdate`.

## Dynamics of these maps

### Unified map

On each application of a block (i.e. the `BBODY` rule) and each tick (i.e. the
`TICK` rules) we know what entries of the unified map are needed to execute the
rule. Therefore it fits the current design of UTxO-HD, and therefore can follow
the _same_ pattern as we currently have for the UTxO set:

- Before calling the ledger rule we can query for the needed entries
- We can present the ledger with the values they asked for, but restricted to
  the data available in the disk + changelog
- The ledger will either provide diffs for the given values or will return
  updated values that we can then diff with the provided ones
- The resulting differences can be included in the current definition of the
  `DbChangelog`.
  
In particular, we know that the sets required for the deltas on the unified map
are small and therefore fit the overall design.

> **_PLAN:_** Consensus will have a new table on the `LedgerTables` that will
> represent the unified map. Perhaps even 4 tables or a table of triplets. It
> will have its own place on the `DbChangelog` too. The flow above describes the
> general strategy for calling the `BBODY` rule, i.e. `(re)applyBlockOpts` and
> for calling the `TICK` rule, i.e. `applyTickOpts`.

### Stake distribution

The update to this map is performed by `updateStakeDistribution`. It is known
before calling the `BBODY` rule which UTxOs are going to be deleted and the rule
execution logic itself knows the UTxOs that are going to be added. Therefore we
can follow a logic similar to the above:

- Before calling the ledger rule we can query for the needed entries
- We can present the ledger with the values they asked for, but restricted to
  the data available in the disk + changelog
- The ledger will either provide diffs for the given values or will return
  updated values that we can then diff with the provided ones
- The resulting differences can be included in the current definition of the
  `DbChangelog`.

> **_PLAN:_** Consensus will have a new table on the `LedgerTables` that will
> represent the incremental stake distribution (maybe two tables, one for creds
> one for ptrs). It will have its own place on the `DbChangelog` too. The flow
> above describes the general strategy for calling the `BBODY` rule, i.e.
> `(re)applyBlockOpts`.

### Stake snapshots

The snapshots are rotated by the `snapTransition` rule (called by `TICK`). This
is the most complicated of the three because it involves accessing the unified
map and the `IncrementalStake` in their entirety in order to fold them. We do
these two steps:

```
step1 = (dom activeDelegs ◁ credStake) ∪ (dom activeDelegs ◁ ptrStake)
step2 =  aggregate (dom activeDelegs ◁ rewards) step1
```

Where
- `activeDelegs` comes from the delegations in the unified map,
- `rewards` come from the unified map,
- `credStake` and `ptrStake` come from the incremental stake

However, there is an important note here. The `ssStakeMark` is only used to be
put in `ssStakeSet` on the next snapshot rotation, then `ssStakeSet` is only
used to be put in `ssStakeGo`, and `ssStakeGo` is only used to prepare the
`PulsingRewUpdate`. This is done in `PulsingReward.startStep` and it also uses
the `rewards` map of the unified map. Assuming the reward calculation is done
outside of the ledger rules, the snapshot is not really needed by the ledger. In
that case we would avoid providing the whole map to the Ledger because the
calculation would be performed outside of the ledger rules.

The `ssStakeMarkPoolDistr` field is used to be put in `nesPd` on the
`NewEpochState` (by `NEWEPOCH`) which later will be used to provide ledger views
and calculate leader schedules. Note this is purely a Protocol concern and thus
probably a Consensus concern.

However it seems that the `ssPoolParams` are in fact modified by the Ledger.
This should not be very problematic as we would know in andvance which pools are
updating their params, and we could replicate the schema above for this map.

## Reward computation and the ADA pots

The way the rewards computation happens now (see `PulsingRewards` and
`Shelley.Rules.Rupd` and `Shelley.Rules.Tick`) is that on each ticking we pulse
a chunk of the rewards update so that when we reach the epoch boundary we want
to have pulsed through the whole `ssStakeGo` snapshot that was used when
creating the pulser.

In the end, the reward computation has to produce a `RewardUpdate`:
```haskell
data RewardUpdate c = RewardUpdate
  { deltaT :: !DeltaCoin
  , deltaR :: !DeltaCoin
  , rs :: !(Map (Credential 'Staking c) (Set (Reward c)))
  , deltaF :: !DeltaCoin
  , nonMyopic :: !(NonMyopic c)
  }
```

where we will use the `delta*` fields to update the treasury, reserves and fee
pot on the `NEWEPOCH` rule. We also use the `rs` field when calling
`updateRewards` which happens also on `NEWEPOCH`.


If Consensus can compute the `RewardUpdate` (possibly on a separate thread that
traverses the map at its own pace?) then we can provide the `RewardUpdate` to
the `NEWEPOCH` rule so that the pots can be updated.

> **_PLAN:_** Consensus will compute the rewards outside of the Ledger and will
> provide the parts of the `RewardUpdate` to the ledger. In particular, on the
> epoch boundary, it will provide `deltaT`, `deltaR`, and `deltaF`. Ledger will
> not compute the `RewardUpdate` thus the pulser becomes dead code.

> Moreover, the ledger will provide a function `f :: Block -> Set
> (StakeCredential c)` so that Consensus can supply Ledger with only the part of
> the `UMap` (the unified map) that it requires. In particular, Consensus will
> have to apply the `rs :: !(Map (Credential 'Staking c) (Set (Reward c)))` field
> of the reward update to the unified map on the epoch boundary, prior to
> computing this view for the ledger (to ensure that reward withdrawals are
> correct within the given block).

## Snapshots and Leader Schedule

The `checkIsLeader` functions for Praos and TPraos makes use of the stake
distribution by stake pool in the `LedgerView` (see the definitions in
`ouroboros-consensus-protocol`). If the snapshots (and therefore the
`ssStakeMarkPoolDistr` field) reside in the Consensus side, we can produce the
relevant stake distributions when needed and don't involve the ledger. In any
case this functionality is in between Ledger and Consensus so it makes sense to
move it out of the ledger.

> **_PLAN:_** Consensus will manage the snapshots to produce stake distribution
> by pool that can be used by Consensus later to resolve queries about the
> LeaderSchedule. Ledger will not know about the Snapshots. In particular, the
> UTxO-HD report includes the concept of Snapshots of tables, which would be
> used to manage and access these snapshots.

Note that this implies creating a new package or component at the
Consensus-Ledger boundary whose owner would probably be the Consensus team as
its responsibilities would be related with computations required for the
Consensus protocol (leader checks, and similar).

## Open questions

- Should the Ledger return the diffs? it actually internally compute diffs, but
  they the diffs are applied to the values before returning. If we wanted to
  return the diffs instead, there are many intermediate layers through which
  they will have to be floated, but it should be doable.

- Rewards withdrawals are known beforehand. Ledger could produce deltas that
  would take effect in a future epoch boundary.
