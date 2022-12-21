# Interface Change Log

- This change log is primarily intended to benefit downstream Cardano
  development teams. We therefore are not committing to discuss changes that
  aren't relevant to that audience. If such changes belong anywhere, it would
  most likely be the `cardano-node` release notes.

- So far, versioning of "internal" Cardano packages is based only on commit
  hashes, except for the `cardano-node` repo.

For those reasons, we organize this log chronologically: with most recent
changes near the top.

We recommend three ways to query this log.

- `git diff revOld revNew -- ouroboros-consensus/docs/interface-CHANGELOG.md`

  That command identifies the relevant log entries, those that discuss changes
  between `revOld` and `revNew`. This is probably what downstream maintainers
  most often need.

- `git blame [--first-parent] --
  ouroboros-consensus/docs/interface-CHANGELOG.md`

  That command identifies the commit that introduced each log entry, in case
  you'd like to learn more about the changes discussed by some entry.

  We make a best effort to add a log entry in the same PR that makes the changes
  that need to be discussed in this file. Also, the commits on our mainline
  branch -- which is the only branch we assume downstream teams are using -- are
  all PR merge commits. We don't do fast-forward merges. Each such merge commit,
  thanks to `bors`, lists the corresponding PR number. Thus the `--first-parent`
  flag will tell you which PR to investigate if you're interested in more
  details/discussion around a particular log entry.

- Manually inspect the dates in the (chronologically sorted) entries. We make a
  best effort to ensure the entry's date indicates when the corresponding PR was
  merged. We write "Circa YYYY-MM-DD" to emphasize that, for various reasons,
  the date might not be exact (time zones, pathologically slow CI runs, etc). If
  you do need precise timestamps, see instead the `git blame` output.

Inevitably, we will eventually need to add errata to a pre-existing log entry.
In order for the recommended `diff` command to be minimal and the recommended
`blame` to be most legible, we make a best effort to avoid editing the lines
within old entries when updating this file. Those lines are ideally immutable,
just like their corresponding commits. If we find a particularly problematic
mistake in an old entry, we address it by adding new lines at the top of that
log entry. The new list item within the old entry should follow this template `-
Errata Circa 2022-12-30: The original discussion, still found below,
oversimplifies the ...`. Such errata only correct mistakes in the original log
entry; crucially, the code relevant to that entry is not changing. Such code
changes would justify their own new log entry instead of adding errata to an old
one. Because of this scheme, the errata dates, unlike the top-level entry dates,
may appear out of chronological order.

The internals of each entry are organized similar to
https://keepachangelog.com/en/1.1.0/, adapted to our plan explained above.

## Circa 2022-11-23

- Removed dead `protocolInfoPraosBabbage` and `protocolInfoPraosShelleyBased`.

## Circa 2022-11-23

- Added `ouroboros-consensus-diffusion`, a top level integration package of
  `ouroboros-network` & `ouroboros-consensus`

## Circa 2022-09-30

### Fixed

- The maximum major protocol version of the Cardano protocol is now correctly
  based off of Babbage instead of Alonzo.

## Circa 2022-09-20

### Added

- `consensusStartupErrorTracer` to `Ouroboros.Consensus.Node.Tracers.Tracers'`: a tracer which logs consensus startup errors.

## Circa 2022-08-08

### Added

- `GetPoolDistr`: Get the pool distribution for the given stake pool ids

## Circa 2022-08-03
### Added

- new project `ouroboros-consensus-cardano-tools` containing two tools:
  1. `db-synthesizer` is a new contribution.
  2. `db-analyser` has been moved over from `ouroboros-consensus-cardano`.
  
### Removed
- `ouroboros-consensus-cardano/tools/db-analyser` has been removed.

## Circa 2022-07-26

### Added

- `GetPoolState`: Get the pool state for the given stake pool ids
  New addition to client version `NodeToClientV_14`

## Circa 2022-07-20

### Fixed

- The obsolete node check in the new 'Praos' protocol was not performing the
  check that was intended. It is supposed to check that the current protocol
  version is no greater than the max major protocol version. It was instead
  checking that the max major protocol version was not greater than the
  protocol version listed in the block header (which is currently not supposed
  to have any semantic meaning, and is used to manually check the readiness
  of the network for a hard fork). Note that this mistake is only in the Praos
  protocol, not in TPraos. The consequence of this incorrect check is that
  nodes will not properly halt after a hard fork when they do not have the
  required software for the hard fork.

## Circa 2022-07-13

### Added

- New supported node to client version `NodeToClientV_14` with new queries:
  - `GetPoolState`: Get the pool state for the given stake pool ids

## Circa 2022-04-26

### Added

- New 'Praos' protocol implemented in `Ouroboros.Consensus.Protocol.Praos`. This
  uses only a single VRF proof in the header, and drops support for the overlay
  schedule (so drops support for changing the decentralisation parameter). All
  of the relevant keys etc remain the same.
- A new era `BabbageEra` has been created, and included in the structure of
  `CardanoEras`. Anything using `CardanoEras` will now need an additional case
  for Babbage. Suitable pattern synonyms (for e.g. `BlockBabbage`,
  `HeaderBabbage`) have been added to `Ouroboros.Consensus.Cardano.Block`.

### Changed

- `ShelleyBlock` now takes two type arguments: `ShelleyBlock era` becomes
  `ShelleyBlock proto era`. Since 'ShelleyBasedEra' now governs constraints from
  only one axis of variance, a new class `ShelleyCompatible` is introduced in
  Ouroboros/Consensus/Shelley/Ledger/Block.hs. This should be used in place of
  `ShelleyBasedEra` in almost all uses.
- `LedgerSupportsProtocol` instances are now moved to their own module, in
  Ouroboros/Consensus/Shelley/Ledger/SupportsProtocol.hs. While we have
  generally striven to retain universal instances over `ShelleyBlock era
  proto`, this is not possible for `LedgerSupportsProtocol`, since it is the
  one place that binds the ledger and the protocol together. This module
  therefore defines instances both for Praos and TPraos. Since these instances
  rely on `ShelleyCompatible`, they themselves cannot be included in it, and
  hence bubble up to `RunNode`.

  To obtain `ShelleyCompatible` instances for various eras, you may need to
  import the `Ouroboros.Consensus.Shelley.HFEras` module.
- Add a new supported node to client version `NodeToClientV_13` and node to node
  version `NodeToNodeV_9` supporting the new Babbage era.

## Circa 2021-12-16

### Added

- New supported node to client version `NodeToClientV_12` with a new mini-protocol:
  - `LocalTxMonitor`: Query information about the node's local mempool.

## Circa 2021-10-13

### Added

- New supported node to client version `NodeToClientV_11` with new queries:
  - `GetRewardInfoPools`: Get *current* stake distribution, pool parameters
    and other information that is necessary to predict stake pool member rewards
    for the current epoch.

## Circa 2021-09-22

### Added

- New supported node to client version `NodeToClientV_10` with new queries:
  - `GetChainBlockNo`: Get the chain block number
  - `GetChainPoint`: Get the chain point, which includes the slot number and
    header hash.

## Circa 2021-08-31

### Added

- `AuxLedgerEvent` type family and interface functions for ticking and applying
  blocks that expose the ledger events that were recently added to the upstream
  `cardano-ledger-specs`. These new functions merely extend the Consensus
  interface: nothing changes for downstream uses that do not care about ledger
  events. The `LedgerResult` type is intentionally not an instance of
  `Applicative`/`Monad`, so as to discourage users from letting the event thunks
  accumulate by invoking the ledger several times before processing/discarding
  the (thunks of the) accumulated list of events. We anticipate that
  `tickThenApplyLedgerResult` and/or `tickThenReapplyLedgerResult` are the main
  ways to utilize the new ledger events.

## Circa 2021-07-29

### Added

- `ouroboros-consensus/docs/interface-CHANGELOG.md`. Now that this file exists,
  entries added to it by later PRs will be narrower than this entry. This entry
  should be the only one that is bootstrapping the file by discussing multiple
  recent changes.

- The `*MaxTxCapacityOverrides` fields to the `ProtocolParams*` record types,
  one per ledger era (eg `byronMaxTxCapacityOverrides`,
  `allegraMaxTxCapacityOverrides`, etc). In particular, these record types are
  used to organized the lost list of arguments to
  `Ouroboros.Consensus.Cardano.Node.protocolInfoCardano`. Downstream libraries
  invoke that function.

  Integration advice: These fields have type
  `Ouroboros.Consensus.Mempool.TxLimits.Overrides` and therefore can be
  defaulted with `mempty` or more explicitly with `mkOverrides
  noOverridesMeasure` (exported by that same module).

  Eventually, the SPOs should be able to set options in configuration files that
  influence the value of these new override fields in order to restrict the
  various notions of total transaction size allowed in a block they forge. These
  limits are already provided by the ledger state, but the SPO may have reason
  to reduce that limit via this override. The fields are per-era because 1) some
  eras introduce new dimensions/components of the "transaction size" concept and
  2) even for a single such component eg script complexity, it can be reasonable
  for the SPO to set different limits for different eras.

### Changed

- On-the-wire content of Alonzo transactions. Previously they were sent across
  the wire without the flag indicating whether or not their carried scripts were
  expected to pass. Now that flag is included on-the-wire.

  The original motivation is so that the local node can reject mistakenly
  invalid scripts from the local client when the client sent the positive flag
  but the scripts failed. This is a courtesy to our local client, catching
  mistakes that otherwise would have incurred on-chain penalties.

  Beyond the original motivation, it is also useful to receive this flag from
  untrusted peers. If they send the wrong flag, we correct it before propagating
  it (implemented) and then disconnect (not yet implemented), because the sender
  is either buggy or adversarial.

  Integration advice: the simplest option is to include the positive flag when
  creating a transaction to send on the wire. That's likely the intention for
  our nascent Alonzo functionalities.

  The new ubiquity of this flag also led to simplifications in the interface to
  `cardano-ledger-specs`. As a result Consensus removed all use of `TxInBlock`,
  removed the `Validated` outermost layer from the codomain of `extractTxs`, etc
  -- I don't know to what extent you'll face those transitive changes, but
  beware of them.

  A new feature could later be added eg to the Cardano CLI so that the client
  can submit txs with the flag set in the negative, eg for QA to use when
  testing non-nominal scenarios etc.
