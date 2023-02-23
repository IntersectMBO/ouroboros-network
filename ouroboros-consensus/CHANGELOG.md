# Ouroboros-consensus core Changelog

This Changelog describes changes on the ledger agnostic Consensus packages:
- `ouroboros-consensus`
- `ouroboros-consensus-test`
- `ouroboros-consensus-diffusion`
- `ouroboros-consensus-mock`
- `ouroboros-consensus-mock-test`
- `ouroboros-consensus-protocol`

If you don't see here the package you're interested in, see the top-level
[Consensus-CHANGELOG.md](../Consensus-CHANGELOG.md).

If you have any doubts, please consult the [release
process](./docs/ReleaseProcess.md).

# Changelog entries

<a id='changelog-0.2.1.0'></a>
## 0.2.1.0 — 2023-02-23

### Non-Breaking

- Exposed the `Pushing` newtype wrappers for the tracing of the `LedgerDB`

<a id='changelog-0.2.0.0'></a>
## 0.2.0.0 — 2023-02-09

### Non-Breaking

- Reorganized `Ouroboros.Consensus.Storage.LedgerDB.*` modules. Old modules have
  a deprecation warning for downstream users but otherwise they still export the
  same functionality.

- Added `NodeToClientV_15`, to support the `Conway` era.

- Reorganization on the `Mempool` modules. Stub deprecated modules are in place
  which should ensure that no code breaks downstream just yet. Clients should
  directly import `Ouroboros.Consensus.Mempool`.

### Breaking

- Remove redundant proxy argument for `ledgerDbTip`.

- Removed the `idx` type variable on the `Mempool` and `MempoolSnapshot`
  datatypes in favour of using `TicketNo` always.

- `Ouroboros.Consensus.Node` and `Ouroboros.Consensus.Network` hierarchies of
  modules where moved from `ouroboros-consensus` to
  `ouroboros-consensus-diffusion` package.

<a id='changelog-0.1.0.2'></a>
## 0.1.0.2 — 2023-01-25

### Patch

- Version bump on ledger-agnostic packages to move in lockstep.

