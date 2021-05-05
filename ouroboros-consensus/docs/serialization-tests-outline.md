# Context

```
$ git lg -1
aad909cfb - (HEAD -> alonzo-prerelease-1, origin/nfrisby/CAD-1720, origin/alonzo-prerelease-1, nfrisby/CAD-1720) WIP (3 days ago) <Nicolas Frisby>

$ git grep -E -e '^test' -- 'ouroboros-consensus*cabal'
ouroboros-consensus-byron-test/ouroboros-consensus-byron-test.cabal:test-suite test
ouroboros-consensus-cardano-test/ouroboros-consensus-cardano-test.cabal:test-suite test
ouroboros-consensus-mock-test/ouroboros-consensus-mock-test.cabal:test-suite test
ouroboros-consensus-shelley-test/ouroboros-consensus-shelley-test.cabal:test-suite test
ouroboros-consensus-test/ouroboros-consensus-test.cabal:test-suite test-consensus
ouroboros-consensus-test/ouroboros-consensus-test.cabal:test-suite test-storage
ouroboros-consensus-test/ouroboros-consensus-test.cabal:test-suite test-infra
```

# Reordered by semantics

```
ouroboros-consensus-test/ouroboros-consensus-test.cabal:test-suite test-storage
ouroboros-consensus-test/ouroboros-consensus-test.cabal:test-suite test-infra
ouroboros-consensus-test/ouroboros-consensus-test.cabal:test-suite test-consensus
ouroboros-consensus-mock-test/ouroboros-consensus-mock-test.cabal:test-suite test
ouroboros-consensus-byron-test/ouroboros-consensus-byron-test.cabal:test-suite test
ouroboros-consensus-shelley-test/ouroboros-consensus-shelley-test.cabal:test-suite test
ouroboros-consensus-cardano-test/ouroboros-consensus-cardano-test.cabal:test-suite test
```

# Top-level definitions

`ouroboros-consensus-test:exe:test-storage:Main`

```
  testGroup "ouroboros-storage"
  [ Test.Ouroboros.Storage.tests tmpDir
  ]
```

`ouroboros-consensus-test:exe:test-infra:Main`

```
  testGroup "test-infra"
  [ Test.ThreadNet.Util.Tests.tests
  , Test.Util.Split.Tests.tests
  ]
```

`ouroboros-consensus-test:exe:test-consensus:Main`

```
  testGroup "ouroboros-consensus"
  [ Test.Consensus.BlockchainTime.Simple.tests
  , Test.Consensus.MiniProtocol.ChainSync.Client.tests
  , Test.Consensus.MiniProtocol.LocalStateQuery.Server.tests
  , Test.Consensus.Mempool.tests
  , Test.Consensus.Node.tests
  , Test.Consensus.ResourceRegistry.tests
  , Test.Consensus.Util.MonadSTM.RAWLock.tests
  , Test.Consensus.Util.Versioned.tests           -- combinators for migration during deserialization (used by storage layer) (TODO only by storage layer?)
  , testGroup "HardFork" [
        testGroup "History" [
            Test.Consensus.HardFork.Summary.tests
          , Test.Consensus.HardFork.History.tests
          ]
      , testGroup "Combinator" [
            Test.Consensus.HardFork.Forecast.tests
          , Test.Consensus.HardFork.Combinator.tests
          ]
      ]
  ]
```

`ouroboros-consensus-mock-test:Main`

```
  testGroup "ouroboros-consensus"
  [ Test.Consensus.Ledger.Mock.tests      -- serialization tests for our mock hashing types
  , Test.ThreadNet.BFT.tests
  , Test.ThreadNet.LeaderSchedule.tests
  , Test.ThreadNet.PBFT.tests
  , Test.ThreadNet.Praos.tests
  ]
```

`ouroboros-consensus-byron-test:Main`

```
  testGroup "byron"
  [ Test.Consensus.Byron.Golden.tests         -- TODO goldenTest_all, "examples"
  , Test.Consensus.Byron.Serialisation.tests  -- TODO roundtrip_all, BinaryBlockInfo sanity-check, corruption detection
  , Test.ThreadNet.Byron.tests
  , Test.ThreadNet.DualByron.tests
  ]
```

`ouroboros-consensus-shelley-test:Main`

```
  testGroup "shelley"
  [ Test.Consensus.Shelley.Golden.tests         -- TODO goldenTest_all, "examples"
  , Test.Consensus.Shelley.Serialisation.tests  -- TODO roundtrip_all, BinaryBlockInfo sanity-check, corruption detection
  , Test.ThreadNet.Shelley.tests
  ]
```

`ouroboros-consensus-cardano-test:Main`

```
  testGroup "cardano"
  [ Test.Consensus.Cardano.ByronCompatibility.tests  -- TODO B2C and C2B roundtrips
  , Test.Consensus.Cardano.Golden.tests              -- TODO goldenTest_all, "examples"
  , Test.Consensus.Cardano.Serialisation.tests       -- TODO roundtrip_all, BinaryBlockInfo sanity-check
  , Test.ThreadNet.Cardano.tests
  , Test.ThreadNet.ShelleyAllegra.tests
  , Test.ThreadNet.AllegraMary.tests
  ]
```

# Discussion

Above, I've added comments for the modules related to serialization. Morever,
for the modules that we have been recently discussing as candidates for
upstreaming to `cardano-ledger-specs`, I've also written `TODO` in that
comment.

Historically, these modules exist in this repository because Consensus decided
they'd be useful (we still think that), but now the fact that they live in our
repository are making it difficult for us to update the `cardano-ledger-specs`
dependency for new eras etc -- it'd be more appropriate for them to live
upstream and easier to maintain them there.

The current task at head is for Consensus to better understand:

  * What exactly these modules are each meant to test.

  * Which Consensus code/concepts they _need_ to depend on. (eg does
    `*-cardano-*` need HFC?)

  * Whatever _additional_ Consensus code/concepts they currently depend on
    beyond that. (eg `Test.Util.Serialisation.Golden`)

Only then can we consider (in cooperation with Ledger) how best to relocate the
tests, how much work it will be, what we can dovetail that with, who can/should
do it, etc.

In particular, we had decided that the relocation itself might also be a good
time to establish a new "interface" package shared between Ledger and Consensus
(eg in `cardano-base`?) and perhaps even relocating significant portions of the
HFC to it.
