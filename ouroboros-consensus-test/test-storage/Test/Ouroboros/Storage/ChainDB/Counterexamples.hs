module Test.Ouroboros.Storage.ChainDB.Counterexamples where

import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Cardano.Slotting.Slot as Slot
import           Control.Monad.State
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainType (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Network.Block (BlockNo (..), ChainHash (..),
                     ChainUpdate (RollBack))
import qualified Ouroboros.Network.Block as Network
import           Ouroboros.Network.Point (blockPointHash, blockPointSlot)
import qualified Ouroboros.Network.Point as Network
import           Test.Ouroboros.Storage.ChainDB.StateMachine hiding (BlockNo)
import           Test.Ouroboros.Storage.TestBlock
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic (monadicIO)
import           Test.StateMachine.Types (Command (..), Commands (..),
                     Reference (..), Symbolic (..), Var (..))
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.ChunkInfo (SmallChunkInfo (..))


tests :: TestTree
tests = testGroup "ChainDB QSM-derived unit tests" [
  testProperty "small chain" prop_bogus
  ]

prop_bogus :: Property
prop_bogus = monadicIO $ do
  (_, prop) <- QC.run $ executeCommands maxClockSkew (SmallChunkInfo chunkInfo) commands
  pure prop
  where maxClockSkew = MaxClockSkew 100000
        chunkInfo = UniformChunkSize (ChunkSize {chunkCanContainEBB = True, numRegularBlocks = 11})


-- commands' = do
--   let genesis = undefined
--   a1 <- addBlock genesis
--   a2 <- addBlock a1


-- Requirements
-- - Comparison of hashed (because tie breaking depends on the hash)
-- -

tree :: [TestBlock]
tree =
  let fork i = TestBody { tbForkNo = i, tbIsValid = True }
      b1 = firstEBB (const True) $ fork 0
      b2 = mkNextBlock b1 0 $ fork 0
      b3 = mkNextBlock b2 1 $ fork 0
      b4 = mkNextBlock b2 1 $ fork 1
      b5 = mkNextBlock b4 2 $ fork 1
      b6 = mkNextBlock b3 2 $ fork 0
  in [b1, b2, b3, b4, b5, b6]

addBlock :: blk -> At Cmd blk m r
addBlock b = At (AddBlock b)

mkModel :: ChunkInfo -> MaxClockSkew -> Model TestBlock m r
mkModel chunkInfo maxClockSkew =
  let topLevelConfig = mkTestCfg chunkInfo
        in initModel topLevelConfig testInitExtLedger maxClockSkew

mkChunkInfo :: ChunkInfo
mkChunkInfo = UniformChunkSize (ChunkSize {chunkCanContainEBB = True, numRegularBlocks = 11})

mkMaxClockSkew :: MaxClockSkew
mkMaxClockSkew = MaxClockSkew 100000

mkCommands
  :: Model blk m Symbolic
  -> [At Cmd blk m Symbolic]
  -> State (Model blk m Symbolic) (Commands (At Cmd blk m) (At Resp blk m))
mkCommands model cmds = undefined
  where
    go acc []           = acc
    go acc (cmd : rest) = undefined

{--
let (resp, counter') = runGenSym (mock model next) counter
        put (transition model next resp)
        go (size - 1) counter' (Command next resp (getUsedVars resp) : cmds)
--}

try :: IO ()
try = do
  let model :: Model TestBlock IO Symbolic
      model = mkModel mkChunkInfo mkMaxClockSkew
      resp = step model (addBlock $ head tree)
  print resp
  pure ()


commands :: Commands (At Cmd TestBlock IO) (At Resp TestBlock IO)
commands =
  Commands
    { unCommands =
        [ Command
            At
              { unAt =
                  AddBlock
                    TestBlock
                      { testHeader =
                          TestHeader
                            { thHash = TestHeaderHash (-1684377399790728946),
                              thPrevHash = GenesisHash,
                              thBodyHash = TestBodyHash 590681868797176966,
                              thSlotNo = SlotNo 0,
                              thBlockNo = BlockNo 0,
                              thChainLength = ChainLength 1,
                              thIsEBB = EBB (EpochNo 0)
                            },
                        testBody = TestBody {tbForkNo = 2, tbIsValid = True}
                      }
              }
            At
              { unAt =
                  Resp
                    { getResp =
                        Right
                          ( Point
                              ( Network.Point
                                  ( Slot.At
                                        Network.Block
                                          { blockPointSlot = SlotNo 0,
                                            blockPointHash = TestHeaderHash (-1684377399790728946)
                                          }
                                  )
                              )
                          )
                    }
              }
            [],
          Command
            At
              { unAt =
                  AddBlock
                    TestBlock
                      { testHeader =
                          TestHeader
                            { thHash = TestHeaderHash 2036412332245722423,
                              thPrevHash = BlockHash (TestHeaderHash (-1684377399790728946)),
                              thBodyHash = TestBodyHash 590682968308805179,
                              thSlotNo = SlotNo 0,
                              thBlockNo = BlockNo 1,
                              thChainLength = ChainLength 2,
                              thIsEBB = RegularBlock
                            },
                        testBody = TestBody {tbForkNo = 1, tbIsValid = True}
                      }
              }
            At
              { unAt =
                  Resp
                    { getResp =
                        Right
                          ( Point
                              ( Network.Point
                                  (  Slot.At
                                        Network.Block
                                          { blockPointSlot = SlotNo 0,
                                            blockPointHash = TestHeaderHash 2036412332245722423
                                          }
                                  )
                              )
                          )
                    }
              }
            [],
          Command
            At
              { unAt =
                  AddBlock
                    TestBlock
                      { testHeader =
                          TestHeader
                            { thHash = TestHeaderHash (-8335835222620935469),
                              thPrevHash = BlockHash (TestHeaderHash 2036412332245722423),
                              thBodyHash = TestBodyHash 590682968308805179,
                              thSlotNo = SlotNo 1,
                              thBlockNo = BlockNo 2,
                              thChainLength = ChainLength 3,
                              thIsEBB = RegularBlock
                            },
                        testBody = TestBody {tbForkNo = 1, tbIsValid = True}
                      }
              }
            At
              { unAt =
                  Resp
                    { getResp =
                        Right
                          ( Point
                              ( Network.Point
                                  ( Slot.At
                                        Network.Block
                                          { blockPointSlot = SlotNo 1,
                                            blockPointHash = TestHeaderHash (-8335835222620935469)
                                          }

                                  )
                              )
                          )
                    }
              }
            [],
          Command
            At
              { unAt =
                  AddBlock
                    TestBlock
                      { testHeader =
                          TestHeader
                            { thHash = TestHeaderHash 7029085255816357683,
                              thPrevHash = BlockHash (TestHeaderHash 2036412332245722423),
                              thBodyHash = TestBodyHash 590680769285548757,
                              thSlotNo = SlotNo 3,
                              thBlockNo = BlockNo 2,
                              thChainLength = ChainLength 3,
                              thIsEBB = RegularBlock
                            },
                        testBody = TestBody {tbForkNo = 3, tbIsValid = True}
                      }
              }
            At
              { unAt =
                  Resp
                    { getResp =
                        Right
                          ( Point
                              ( Network.Point
                                  ( Slot.At
                                        Network.Block
                                          { blockPointSlot = SlotNo 3,
                                            blockPointHash = TestHeaderHash 7029085255816357683
                                          }
                                  )
                              )
                          )
                    }
              }
            [],
          Command
            At {unAt = NewFollower TentativeChain}
            At
              { unAt =
                  Resp {getResp = Right (Flr (Reference (Symbolic (Var 0))))}
              }
            [Var 0],
          Command
            At
              { unAt =
                  FollowerForward
                    (Reference (Symbolic (Var 0)))
                    [ Network.Point
                        ( Slot.At
                            Network.Block
                              { blockPointSlot = SlotNo 0,
                                blockPointHash = TestHeaderHash (-1684377399790728946)
                              }
                        )
                    ]
              }
            At
              { unAt =
                  Resp
                    { getResp =
                        Right
                          ( MbPoint
                              ( Just
                                  ( Network.Point
                                      ( Slot.At
                                            Network.Block
                                              { blockPointSlot = SlotNo 0,
                                                blockPointHash = TestHeaderHash (-1684377399790728946)
                                              }

                                      )
                                  )
                              )
                          )
                    }
              }
            [],
          Command
            At
              { unAt =
                  AddBlock
                    TestBlock
                      { testHeader =
                          TestHeader
                            { thHash = TestHeaderHash (-8583948168383975628),
                              thPrevHash = BlockHash (TestHeaderHash 7029085255816357683),
                              thBodyHash = TestBodyHash 590682968308805179,
                              thSlotNo = SlotNo 4,
                              thBlockNo = BlockNo 3,
                              thChainLength = ChainLength 4,
                              thIsEBB = RegularBlock
                            },
                        testBody = TestBody {tbForkNo = 1, tbIsValid = True}
                      }
              }
            At
              { unAt =
                  Resp
                    { getResp =
                        Right
                          ( Point
                              ( Network.Point
                                  ( Slot.At
                                      Network.Block
                                        { blockPointSlot = SlotNo 4,
                                          blockPointHash = TestHeaderHash (-8583948168383975628)
                                        }
                                  )
                              )
                          )
                    }
              }
            [],
          Command
            At {unAt = PersistBlks}
            At {unAt = Resp {getResp = Right (Unit ())}}
            [],
          Command
            At
              { unAt =
                  AddBlock
                    TestBlock
                      { testHeader =
                          TestHeader
                            { thHash = TestHeaderHash 5812820193101916369,
                              thPrevHash = BlockHash (TestHeaderHash (-8335835222620935469)),
                              thBodyHash = TestBodyHash 590682968308805179,
                              thSlotNo = SlotNo 3,
                              thBlockNo = BlockNo 3,
                              thChainLength = ChainLength 4,
                              thIsEBB = RegularBlock
                            },
                        testBody = TestBody {tbForkNo = 1, tbIsValid = True}
                      }
              }
            At
              { unAt =
                  Resp
                    { getResp =
                        Right
                          ( Point
                              ( Network.Point
                                  ( Slot.At
                                        Network.Block
                                          { blockPointSlot = SlotNo 3,
                                            blockPointHash = TestHeaderHash 5812820193101916369
                                          }

                                  )
                              )
                          )
                    }
              }
            [],
          Command
            At {unAt = FollowerInstruction (Reference (Symbolic (Var 0)))}
            At
              { unAt =
                  Resp
                    { getResp =
                        Right
                          ( MbChainUpdate
                              ( Just
                                  ( RollBack
                                      ( Network.Point
                                          ( Slot.At
                                                Network.Block
                                                  { blockPointSlot = SlotNo 0,
                                                    blockPointHash = TestHeaderHash (-1684377399790728946)
                                                  }
                                          )
                                      )
                                  )
                              )
                          )
                    }
              }
            []
        ]
    }
