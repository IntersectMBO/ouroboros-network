module Test.Ouroboros.Storage.ChainDB.Counterexamples where

import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Cardano.Slotting.Slot as Slot
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainType (..))
import           Ouroboros.Network.Block (BlockNo (..), ChainHash (..),
                     ChainUpdate (RollBack))
import qualified Ouroboros.Network.Block as Network
import           Ouroboros.Network.Point (blockPointHash, blockPointSlot)
import qualified Ouroboros.Network.Point as Network
import           Test.Ouroboros.Storage.ChainDB.StateMachine hiding (BlockNo)
import           Test.Ouroboros.Storage.TestBlock
import           Test.QuickCheck (Property)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO)
import           Test.StateMachine.Types (Command (..), Commands (..), Concrete,
                     History, Reason, Reference (..), StateMachine,
                     Symbolic (..), Var (..))



prop_bogus :: Property
prop_bogus = monadicIO $ runCommands counterexample sm
  where sm = undefined

runCommands
  :: Monad m
  => Commands cmd resp
  -> StateMachine model cmd m resp
  -> PropertyM m (History cmd resp, model Concrete, Reason)
runCommands sm cmds = do
  (hist, model, res) <- runCommands sm cmds
  return (hist, model, res)


counterexample :: Commands (At Cmd TestBlock IO) (At Resp TestBlock IO)
counterexample =
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
                                  ( ( Slot.At
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
                                  ( ( Slot.At
                                        Network.Block
                                          { blockPointSlot = SlotNo 0,
                                            blockPointHash = TestHeaderHash 2036412332245722423
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
                                  ( ( Slot.At
                                        Network.Block
                                          { blockPointSlot = SlotNo 1,
                                            blockPointHash = TestHeaderHash (-8335835222620935469)
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
                                  ( ( Slot.At
                                        Network.Block
                                          { blockPointSlot = SlotNo 3,
                                            blockPointHash = TestHeaderHash 7029085255816357683
                                          }
                                    )
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
                                      ( ( Slot.At
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
                                  ( ( Slot.At
                                        Network.Block
                                          { blockPointSlot = SlotNo 3,
                                            blockPointHash = TestHeaderHash 5812820193101916369
                                          }
                                    )
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
                                          ( ( Slot.At
                                                Network.Block
                                                  { blockPointSlot = SlotNo 0,
                                                    blockPointHash = TestHeaderHash (-1684377399790728946)
                                                  }
                                            )
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
