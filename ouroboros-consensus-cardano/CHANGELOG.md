# Ouroboros-consensus Cardano Changelog

This Changelog describes changes on the Cardano specific Consensus packages:
- `ouroboros-consensus-byron`
- `ouroboros-consensus-byron-test`
- `ouroboros-consensus-byronspec`
- `ouroboros-consensus-shelley`
- `ouroboros-consensus-shelley-test`
- `ouroboros-consensus-cardano`
- `ouroboros-consensus-cardano-test`
- `ouroboros-consensus-cardano-tools`

If you don't see here the package you're interested in, see the top-level
[Consensus-CHANGELOG.md](../Consensus-CHANGELOG.md).

If you have any doubts, please consult the [release
process](../ouroboros-consensus/docs/ReleaseProcess.md).

# Changelog entries

<a id='changelog-0.2.0.0'></a>
## 0.2.0.0 — 2023-02-09

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
