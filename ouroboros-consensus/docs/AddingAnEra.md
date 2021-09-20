# Adding a new Shelley-based era to consensus

This document walks through the steps required to add a new Shelley-based era to
consensus and to add it as an extra era to the Cardano blockchain.

Prior art upon which this is based:
* [#2666](https://github.com/input-output-hk/ouroboros-network/pull/2666)
* [#2679](https://github.com/input-output-hk/ouroboros-network/pull/2679)

The steps are fairly straightforward. We have put things in place (in consensus
and in the ledger) when adding the Allegra and Mary eras that should now make it
much easier to add a new additional era.

There are two main driving changes: adding the new era to
`Ouroboros.Consensus.Shelley.Eras` and including that era in
`Ouroboros.Consensus.Cardano.Block`. The compiler should point the rest of the
way. It's mostly a matter of following the existing patterns by imitating the
previous Shelley-based case. Be sure to run both the Shelley
(`ouroboros-consensus-shelley-test`) and the Cardano
(`ouroboros-consensus-cardano-test`) tests.

For exhaustiveness, we give an overview of the changes needed. The new era we'll
be adding is the Alonzo era, which comes after the Mary era.

## Preparation

* Locate the new tag in the ledger, e.g., `AlonzoEra`. This is an empty data
  type that is used at the type level to indicate the era. The ledger should
  have an instance of the `ShelleyBasedEra` class (the class defined in
  `Cardano.Ledger.Shelley.API`, do not confuse it with the class with the same name
  in consensus) for this era. This class should provide all the instances
  consensus integration will rely on.

* Note that both the affected consensus libraries and test suites are parametric
  in the Shelley-based era or in the general era. The test suite relies on the
  necessary `Arbitrary`, `Show`, `Eq`, ... instances to be available for all of
  the Cardano eras. This means that a new era can only be added to the Cardano
  eras when the ledger provides all the necessary instances for the new era,
  *including the instances for the test suite* (e.g., the serialisation
  generators). Note that because the test suite is parametric/generic in the
  eras, it is not easy to temporarily omit a single era from the test suite
  until the required instances have been added. This means that we can't extend
  the library with the new era without extending the test suite too. In the
  past, this has been the main blocker: the library was ready, but the required
  instances in the ledger were missing for our test suite(s).

* Add the required dependencies on the ledger package (and its test package) to
  the `cabal.project` file. You will have to add it to some other cabal files
  too, let the compiler tell you which ones.

## `ouroboros-consensus-shelley`

* Define `StandardAlonzo` in `Ouroboros.Consensus.Shelley.Eras` and add any
  missing instances to that module, update the export list appropriately.

* In `Ouroboros.Consensus.Shelley.Node`, define `ProtocolParamsAlonzo` just like
  the existing ones. In case the era in question needs extra parameters (e.g., a
  new genesis config?), they can be added here.

## `ouroboros-consensus-shelley-test`

* In `Test.Consensus.Shelley.Examples`, define an `examplesAlonzo` similar to
  how `examplesMary` is defined. If necessary, i.e., when new type families have
  been created in the ledger (e.g., `TxOut`), the `examples` function might have
  to take more arguments. This is where the golden test examples are defined,
  but only the Shelley ones are tested as part of this test suite. The others
  are only tested as part of the `ouroboros-consensus-cardano-test` test suite.

## `ouroboros-consensus-cardano`

* In `Ouroboros.Consensus.Cardano.Block`, include `AlonzoEra` in `CardanoEras`
  and `ShelleyBasedEras`. Update all the pattern synonyms in the module with the
  new era. Don't forget to update the comments, the `COMPLETE` pragmas, and the
  export lists. It's easy to forget a case and the compiler will likely not warn
  you, you'll notice it when trying to use the pattern synonyms.

* In `Ouroboros.Consensus.Cardano.CanHardFork`, update
  `CardanoHardForkConstraints`, add additional translations to the `CanHardFork`
  instance.

* In `Ouroboros.Consensus.Cardano.Node`, update the `SerialiseHFC` instance by
  following the existing patterns. Add a new `CardanoNodeToNodeVersion` and
  `CardanoNodeToClientVersion` that enable the `AlonzoEra`, update the existing
  ones so that they disable the new era. Be sure to include the new versions in
  the two methods of the `SupportedNetworkProtocolVersion` instance. Extend
  `protocolInfoCardano` with the new era by following the type errors and adding
  the missing parameters (including `ProtocolParamsTransition`). Don't forget to
  derive `maxMajorProtVer` from the new final era. Update
  `protocolClientInfoCardano` too.

* In `Ouroboros.Consensus.Cardano`, update the `ProtocolCardano` type synonym,
  add the extra arguments needed for `protocolInfoCardano` to the
  `ProtocolCardano` constructor. Update `protocolInfo` accordingly.

## `ouroboros-consensus-cardano-test`

* In `Test.Consensus.Cardano.Generators`, update `arbitraryNodeToNode`,
  `arbitraryNodeToClient`. Try to understand the logic of these two, you will
  also have to add a new case to these. It would be nice if we could write these
  two in a generic way so that they won't have to be updated with each era, but
  it's not so simple. Update the other functions/instances. Be careful, you
  won't get warnings for missing cases in `Arbitrary` instances, so go over all
  of them and add the missing cases.

* In `Test.Consensus.Cardano.ByronCompatibility`, update `toCardanoCodecConfig`.

* In `Test.Consensus.Cardano.Serialisation`, update `testCodecCfg` and
  `dictNestedHdr`.

* In `Test.Consensus.Cardano.Examples`, update `eraExamples`, `combineEras`, and
  the rest.

* In `Test.Consensus.Cardano.Golden`, update the `ToGoldenDirectory` instances.

* Create a `Test.ThreadNet.MaryAlonzo` module similar to
  `Test.ThreadNet.AllegraMary`, to test the transition from Mary to Alonzo.
  Don't forget to include it in the `Main` of the test suite. You will have to
  create a `Test.ThreadNet.TxGen.Alonzo` module similar to
  `Test.ThreadNet.TxGen.Mary`.

* Run the golden tests of `ouroboros-consensus-cardano-test`. Golden test
  results should have been created for the new Cardano versions. Don't forget to
  commit those files, otherwise they will be recreated on each run in CI and not
  compared against the previous results, rendering them useless.

* Extend `Test.ThreadNet.Cardano` with the new era. At the time of writing this
  hasn't been done yet for Allegra or Mary, though.
