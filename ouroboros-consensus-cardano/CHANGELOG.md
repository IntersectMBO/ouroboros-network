# Ouroboros-consensus Cardano Changelog

This Changelog describes changes on the Cardano specific Consensus packages:
- `ouroboros-consensus-byron`
- `ouroboros-consensus-shelley`
- `ouroboros-consensus-cardano`

If you don't see here the package you're interested in, see the top-level
[Consensus-CHANGELOG.md](../Consensus-CHANGELOG.md).

If you have any doubts, please consult the [release
process](../ouroboros-consensus/docs/ReleaseProcess.md).

# Changelog entries

<a id='changelog-0.4.1.0'></a>
## 0.4.1.0 — 2023-04-14

### Non-Breaking

- Updated dependencies in `ouroboros-consensus` and
  `ouroboros-consensus-protocol` as they are no longer part of the same bundle.

<a id='changelog-0.4.0.1'></a>
## 0.4.0.1 — 2023-04-10

### Patch

- `ouroboros-consensus-cardano`: Since the filesystem API that lives in
  `ouroboros-consensus` will live in the `fs-api` package for now on, start
  depending on `fs-api`, and change imports accordingly.

- Collapse all imports into one group in every file.
- Adapt to relocation of SOP-related `Util` modules.

<a id='changelog-0.4.0.0'></a>
## 0.4.0.0 — 2023-03-07

### Non-Breaking

- Fix the version bounds for the bundle and version sets the bounds for the
  `ouroboros-consensus` bundle to `^>=0.3`.

### Breaking

- Return stake snapshots for stake pools that have the `Set` or `Go` ledger
  snapshots. When querying `GetStakeSnapshots Nothing`, which means to query for
  all stake pools, previously only stake snapshots for stake pools that have the
  `Mark` ledger snapshot were returned.

<a id='changelog-0.3.0.0'></a>
## 0.3.0.0 — 2023-02-09

### Patch

- Remove redundant proxy argument for `ledgerDbTip`.

### Non-Breaking

- Adapted to the new reorganization of Mempool modules in `ouroboros-consensus`.

### Breaking

####  Added:
- `Conway` to `CardanoEras`
- `NodeToNodeV_11` and `NodeToClientV_15`, both of which enable Conway.
- Conway-related type and pattern synonyms. Eg `StandardConway`, `HeaderConway`,
  `GentTxConway`, etc.

#### Changed

- The `protocolInfoTPraosShelleyBased` and `protocolInfoPraosShelleyBased`
  functions now expect a tuple of the `AdditionalGenesisConfig` and the
  `TranslationContext` instead of just the `TranslationContext`. For all
  Shelley-based eras before Conway, those had been equal types.
