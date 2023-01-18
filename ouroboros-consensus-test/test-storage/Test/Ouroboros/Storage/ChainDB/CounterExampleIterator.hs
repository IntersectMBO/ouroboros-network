module Test.Ouroboros.Storage.ChainDB.CounterExampleIterator where

import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Cardano.Slotting.Slot as Slot
import           Control.Monad.State
import           Data.ByteString.Lazy.Char8 (pack)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainType (..))
import           Ouroboros.Consensus.Storage.Common
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

cmds :: Commands (At Cmd TestBlock IO) (At Resp TestBlock IO)
cmds = Commands {
  unCommands =
        [ Command
            At
              { unAt =
                  AddBlock
                    TestBlock
                      { testHeader =
                          TestHeader
                            { thHash = TestHeaderHash 7029910109426438170,
                              thPrevHash = GenesisHash,
                              thBodyHash = TestBodyHash 590682968308805178,
                              thSlotNo = SlotNo 1,
                              thBlockNo = BlockNo 0,
                              thChainLength = ChainLength 1,
                              thIsEBB = RegularBlock
                            },
                        testBody = TestBody {tbForkNo = 1, tbIsValid = False}
                      }
              }
            At {unAt = Resp {getResp = Right (Point (Network.Point Origin))}}
            [],

          Command
            At
              { unAt =
                  AddBlock
                    TestBlock
                      { testHeader =
                          TestHeader
                            { thHash = TestHeaderHash 2308540464147526198,
                              thPrevHash = GenesisHash,
                              thBodyHash = TestBodyHash 590681868797176966,
                              thSlotNo = SlotNo 1,
                              thBlockNo = BlockNo 0,
                              thChainLength = ChainLength 1,
                              thIsEBB = RegularBlock
                            },
                        testBody = TestBody {tbForkNo = 2, tbIsValid = True}
                      }
              }
            At
              { unAt =
                  Resp
                    { getResp =
                        Right (
                        Point
                          ( Network.Point
                              ( Slot.At
                                  Network.Block
                                    { blockPointSlot = SlotNo 1,
                                      blockPointHash = TestHeaderHash 2308540464147526198
                                    }
                              )
                          ))
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
                            { thHash = TestHeaderHash 8656999388501844387,
                              thPrevHash = BlockHash (TestHeaderHash 2308540464147526198),
                              thBodyHash = TestBodyHash 590680769285548757,
                              thSlotNo = SlotNo 2,
                              thBlockNo = BlockNo 1,
                              thChainLength = ChainLength 2,
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
                          (Network.Point
                              ( Slot.At
                                  Network.Block
                                    { blockPointSlot = SlotNo 2,
                                      blockPointHash = TestHeaderHash 8656999388501844387
                                    }
                              )
                          ))
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
                            { thHash = TestHeaderHash (-340232222464533890),
                              thPrevHash = BlockHash (TestHeaderHash 8656999388501844387),
                              thBodyHash = TestBodyHash 590680769285548757,
                              thSlotNo = SlotNo 4,
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
                                    { blockPointSlot = SlotNo 4,
                                      blockPointHash = TestHeaderHash (-340232222464533890)
                                    }
                              )
                          ))
                    }
              }
            [],

          Command
            At
              { unAt =
                  Stream
                    ( StreamFromInclusive
                        (RealPoint (SlotNo 1) (TestHeaderHash 2308540464147526198))
                    )
                    ( StreamToInclusive
                        (RealPoint (SlotNo 4) (TestHeaderHash (-340232222464533890)))
                    )
              }
            At
              { unAt =
                  Resp {getResp = Right (Iter (Reference (Symbolic (Var 0))))}
              }
            [Var 0],
          Command
            At
              { unAt =
                  AddBlock
                    TestBlock
                      { testHeader =
                          TestHeader
                            { thHash = TestHeaderHash 5928148677101900795,
                              thPrevHash = BlockHash (TestHeaderHash 5227245844658471275),
                              thBodyHash = TestBodyHash 590682968308805179,
                              thSlotNo = SlotNo 5,
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
                                    { blockPointSlot = SlotNo 4,
                                      blockPointHash = TestHeaderHash (-340232222464533890)
                                    }
                              )
                          ))
                    }
              }
            []

        ]}

-- commands :: Commands (At Cmd TestBlock IO) (At Resp TestBlock IO)
-- commands =
--   Commands
--     { unCommands =
--         [ Command
--             At
--               { unAt =
--                   AddBlock
--                     TestBlock
--                       { testHeader =
--                           TestHeader
--                             { thHash = TestHeaderHash 7029910109426438170,
--                               thPrevHash = GenesisHash,
--                               thBodyHash = TestBodyHash 590682968308805178,
--                               thSlotNo = SlotNo 1,
--                               thBlockNo = BlockNo 0,
--                               thChainLength = ChainLength 1,
--                               thIsEBB = RegularBlock
--                             },
--                         testBody = TestBody {tbForkNo = 1, tbIsValid = False}
--                       }
--               }
--             At {unAt = Resp {getResp = Right (Point (Network.Point Origin))}}
--             [],
--           Command
--             At
--               { unAt =
--                   AddBlock
--                     TestBlock
--                       { testHeader =
--                           TestHeader
--                             { thHash = TestHeaderHash 2308540464147526198,
--                               thPrevHash = GenesisHash,
--                               thBodyHash = TestBodyHash 590681868797176966,
--                               thSlotNo = SlotNo 1,
--                               thBlockNo = BlockNo 0,
--                               thChainLength = ChainLength 1,
--                               thIsEBB = RegularBlock
--                             },
--                         testBody = TestBody {tbForkNo = 2, tbIsValid = True}
--                       }
--               }
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right (
--                         Point
--                           ( Network.Point
--                               ( Slot.At
--                                   Network.Block
--                                     { blockPointSlot = SlotNo 1,
--                                       blockPointHash = TestHeaderHash 2308540464147526198
--                                     }
--                               )
--                           ))
--                     }
--               }
--             [],
--           Command
--             At
--               { unAt =
--                   AddBlock
--                     TestBlock
--                       { testHeader =
--                           TestHeader
--                             { thHash = TestHeaderHash 8656999388501844387,
--                               thPrevHash = BlockHash (TestHeaderHash 2308540464147526198),
--                               thBodyHash = TestBodyHash 590680769285548757,
--                               thSlotNo = SlotNo 2,
--                               thBlockNo = BlockNo 1,
--                               thChainLength = ChainLength 2,
--                               thIsEBB = RegularBlock
--                             },
--                         testBody = TestBody {tbForkNo = 3, tbIsValid = True}
--                       }
--               }
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right
--                           ( Point
--                           (Network.Point
--                               ( Slot.At
--                                   Network.Block
--                                     { blockPointSlot = SlotNo 2,
--                                       blockPointHash = TestHeaderHash 8656999388501844387
--                                     }
--                               )
--                           ))
--                     }
--               }
--             [],
--           Command
--             At
--               { unAt =
--                   AddBlock
--                     TestBlock
--                       { testHeader =
--                           TestHeader
--                             { thHash = TestHeaderHash (-340232222464533890),
--                               thPrevHash = BlockHash (TestHeaderHash 8656999388501844387),
--                               thBodyHash = TestBodyHash 590680769285548757,
--                               thSlotNo = SlotNo 4,
--                               thBlockNo = BlockNo 2,
--                               thChainLength = ChainLength 3,
--                               thIsEBB = RegularBlock
--                             },
--                         testBody = TestBody {tbForkNo = 3, tbIsValid = True}
--                       }
--               }
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right
--                     ( Point
--                           ( Network.Point
--                               ( Slot.At
--                                   Network.Block
--                                     { blockPointSlot = SlotNo 4,
--                                       blockPointHash = TestHeaderHash (-340232222464533890)
--                                     }
--                               )
--                           ))
--                     }
--               }
--             [],
--           Command
--             At
--               { unAt =
--                   Stream
--                     ( StreamFromInclusive
--                         (RealPoint (SlotNo 1) (TestHeaderHash 2308540464147526198))
--                     )
--                     ( StreamToInclusive
--                         (RealPoint (SlotNo 4) (TestHeaderHash (-340232222464533890)))
--                     )
--               }
--             At
--               { unAt =
--                   Resp {getResp = Right (Iter (Reference (Symbolic (Var 0))))}
--               }
--             [Var 0],
--           Command
--             At
--               { unAt =
--                   AddBlock
--                     TestBlock
--                       { testHeader =
--                           TestHeader
--                             { thHash = TestHeaderHash 5928148677101900795,
--                               thPrevHash = BlockHash (TestHeaderHash 5227245844658471275),
--                               thBodyHash = TestBodyHash 590682968308805179,
--                               thSlotNo = SlotNo 5,
--                               thBlockNo = BlockNo 2,
--                               thChainLength = ChainLength 3,
--                               thIsEBB = RegularBlock
--                             },
--                         testBody = TestBody {tbForkNo = 1, tbIsValid = True}
--                       }
--               }
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right
--                           ( Point
--                           ( Network.Point
--                               ( Slot.At
--                                   Network.Block
--                                     { blockPointSlot = SlotNo 4,
--                                       blockPointHash = TestHeaderHash (-340232222464533890)
--                                     }
--                               )
--                           ))
--                     }
--               }
--             [],
--           Command
--             At
--               { unAt =
--                   AddBlock
--                     TestBlock
--                       { testHeader =
--                           TestHeader
--                             { thHash = TestHeaderHash 5227245844658471275,
--                               thPrevHash = BlockHash (TestHeaderHash 2308540464147526198),
--                               thBodyHash = TestBodyHash 590682968308805179,
--                               thSlotNo = SlotNo 4,
--                               thBlockNo = BlockNo 1,
--                               thChainLength = ChainLength 2,
--                               thIsEBB = RegularBlock
--                             },
--                         testBody = TestBody {tbForkNo = 1, tbIsValid = True}
--                       }
--               }
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right
--                           ( Point
--                           ( Network.Point
--                               ( Slot.At
--                                   Network.Block
--                                     { blockPointSlot = SlotNo 5,
--                                       blockPointHash = TestHeaderHash 5928148677101900795
--                                     }
--                               )
--                           ))
--                     }
--               }
--             [],
--           Command
--             At {unAt = IteratorNext (Reference (Symbolic (Var 0)))}
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right
--                           ( IterResult
--                               ( IteratorResult
--                                   ( TestBlock
--                                       { testHeader =
--                                           TestHeader
--                                             { thHash = TestHeaderHash 2308540464147526198,
--                                               thPrevHash = GenesisHash,
--                                               thBodyHash = TestBodyHash 590681868797176966,
--                                               thSlotNo = SlotNo 1,
--                                               thBlockNo = BlockNo 0,
--                                               thChainLength = ChainLength 1,
--                                               thIsEBB = RegularBlock
--                                             },
--                                         testBody = TestBody {tbForkNo = 2, tbIsValid = True}
--                                       },
--                                     TestBlock
--                                       { testHeader =
--                                         TestHeader' (
--                                           TestHeader
--                                             { thHash = TestHeaderHash 2308540464147526198,
--                                               thPrevHash = GenesisHash,
--                                               thBodyHash = TestBodyHash 590681868797176966,
--                                               thSlotNo = SlotNo 1,
--                                               thBlockNo = BlockNo 0,
--                                               thChainLength = ChainLength 1,
--                                               thIsEBB = RegularBlock
--                                             }),
--                                         testBody = TestBody {tbForkNo = 2, tbIsValid = True}
--                                       },
--                                     TestHeader' (TestHeader
--                                       { thHash = TestHeaderHash 2308540464147526198,
--                                         thPrevHash = GenesisHash,
--                                         thBodyHash = TestBodyHash 590681868797176966,
--                                         thSlotNo = SlotNo 1,
--                                         thBlockNo = BlockNo 0,
--                                         thChainLength = ChainLength 1,
--                                         thIsEBB = RegularBlock
--                                       }),
--                                     pack "\131\NUL\136\NUL\ESC \t\149R+ \242\&6\128\ESC\b2\134\a\180\235l\134\SOH\NUL\SOH\129\SOH\131\NUL\STX\245",
--                                     pack "\136\NUL\ESC \t\149R+ \242\&6\128\ESC\b2\134\a\180\235l\134\SOH\NUL\SOH\129\SOH",
--                                     TestHeaderHash 2308540464147526198,
--                                     SlotNo 1,
--                                     IsNotEBB,
--                                     32,
--                                     26,
--                                     SomeSecond NestedCtxt {flipNestedCtxt = CtxtTestBlock}
--                                   )
--                               )
--                           )
--                     }
--               }
--             [],
--           Command
--             At
--               { unAt =
--                   AddBlock
--                     TestBlock
--                       { testHeader =
--                           TestHeader' (TestHeader
--                             { thHash = TestHeaderHash 3200521468649028381,
--                               thPrevHash = BlockHash (TestHeaderHash 5928148677101900795),
--                               thBodyHash = TestBodyHash 590681868797176966,
--                               thSlotNo = SlotNo 8,
--                               thBlockNo = BlockNo 3,
--                               thChainLength = ChainLength 4,
--                               thIsEBB = RegularBlock
--                             }),
--                         testBody = TestBody {tbForkNo = 2, tbIsValid = True}
--                       }
--               }
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right
--                           ( Point
--                           ( Network.Point
--                               ( Slot.At
--                                   Network.Block
--                                     { blockPointSlot = SlotNo 8,
--                                       blockPointHash = TestHeaderHash 3200521468649028381
--                                     }
--                               )
--                           ))
--                     }
--               }
--             [],
--           Command
--             At
--               { unAt =
--                   AddBlock
--                     TestBlock
--                       { testHeader =
--                           TestHeader' (TestHeader
--                             { thHash = TestHeaderHash (-5616847762568615097),
--                               thPrevHash = BlockHash (TestHeaderHash 3200521468649028381),
--                               thBodyHash = TestBodyHash 590680769285548757,
--                               thSlotNo = SlotNo 9,
--                               thBlockNo = BlockNo 4,
--                               thChainLength = ChainLength 5,
--                               thIsEBB = RegularBlock
--                             }),
--                         testBody = TestBody {tbForkNo = 3, tbIsValid = True}
--                       }
--               }
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right
--                           ( Point
--                           ( Network.Point
--                               ( Slot.At
--                                   Network.Block
--                                     { blockPointSlot = SlotNo 9,
--                                       blockPointHash = TestHeaderHash (-5616847762568615097)
--                                     }
--                               )
--                           ))
--                     }
--               }
--             [],
--           Command
--             At {unAt = PersistBlksThenGC}
--             At {unAt = Resp {getResp = Right (Unit ())}}
--             [],
--           Command
--             At {unAt = IteratorNextGCed (Reference (Symbolic (Var 0)))}
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right
--                           ( IterResultGCed
--                               IteratorResultGCed
--                                 { real = False,
--                                   iterResult =
--                                     IteratorResult
--                                       ( TestBlock
--                                           { testHeader =
--                                               TestHeader' (TestHeader
--                                                 { thHash = TestHeaderHash 8656999388501844387,
--                                                   thPrevHash =
--                                                     BlockHash (TestHeaderHash 2308540464147526198),
--                                                   thBodyHash = TestBodyHash 590680769285548757,
--                                                   thSlotNo = SlotNo 2,
--                                                   thBlockNo = BlockNo 1,
--                                                   thChainLength = ChainLength 2,
--                                                   thIsEBB = RegularBlock
--                                                 }),
--                                             testBody = TestBody {tbForkNo = 3, tbIsValid = True}
--                                           },
--                                         TestBlock
--                                           { testHeader =
--                                               TestHeader' (TestHeader
--                                                 { thHash = TestHeaderHash 8656999388501844387,
--                                                   thPrevHash =
--                                                     BlockHash (TestHeaderHash 2308540464147526198),
--                                                   thBodyHash = TestBodyHash 590680769285548757,
--                                                   thSlotNo = SlotNo 2,
--                                                   thBlockNo = BlockNo 1,
--                                                   thChainLength = ChainLength 2,
--                                                   thIsEBB = RegularBlock
--                                                 }),
--                                             testBody = TestBody {tbForkNo = 3, tbIsValid = True}
--                                           },
--                                         TestHeader
--                                           { thHash = TestHeaderHash 8656999388501844387,
--                                             thPrevHash = BlockHash (TestHeaderHash 2308540464147526198),
--                                             thBodyHash = TestBodyHash 590680769285548757,
--                                             thSlotNo = SlotNo 2,
--                                             thBlockNo = BlockNo 1,
--                                             thChainLength = ChainLength 2,
--                                             thIsEBB = RegularBlock
--                                           },
--                                         "\131\NUL\136\NUL\ESCx#\215\DC3\188m!\163\129\ESC \t\149R+ \242\&6\ESC\b2\133\a\180\235j\213\STX\SOH\STX\129\SOH\131\NUL\ETX\245",
--                                         "\136\NUL\ESCx#\215\DC3\188m!\163\129\ESC \t\149R+ \242\&6\ESC\b2\133\a\180\235j\213\STX\SOH\STX\129\SOH",
--                                         TestHeaderHash 8656999388501844387,
--                                         SlotNo 2,
--                                         IsNotEBB,
--                                         41,
--                                         35,
--                                         SomeSecond NestedCtxt {flipNestedCtxt = CtxtTestBlock}
--                                       )
--                                 }
--                           )
--                     }
--               }
--             [],
--           Command
--             At {unAt = IteratorNextGCed (Reference (Symbolic (Var 0)))}
--             At
--               { unAt =
--                   Resp
--                     { getResp =
--                         Right
--                           ( IterResultGCed
--                               IteratorResultGCed
--                                 { real = False,
--                                   iterResult =
--                                     IteratorResult
--                                       ( TestBlock
--                                           { testHeader =
--                                               TestHeader' (TestHeader
--                                                 { thHash = TestHeaderHash (-340232222464533890),
--                                                   thPrevHash =
--                                                     BlockHash (TestHeaderHash 8656999388501844387),
--                                                   thBodyHash = TestBodyHash 590680769285548757,
--                                                   thSlotNo = SlotNo 4,
--                                                   thBlockNo = BlockNo 2,
--                                                   thChainLength = ChainLength 3,
--                                                   thIsEBB = RegularBlock
--                                                 }),
--                                             testBody = TestBody {tbForkNo = 3, tbIsValid = True}
--                                           },
--                                         TestBlock
--                                           { testHeader =
--                                               TestHeader' (TestHeader
--                                                 { thHash = TestHeaderHash (-340232222464533890),
--                                                   thPrevHash =
--                                                     BlockHash (TestHeaderHash 8656999388501844387),
--                                                   thBodyHash = TestBodyHash 590680769285548757,
--                                                   thSlotNo = SlotNo 4,
--                                                   thBlockNo = BlockNo 2,
--                                                   thChainLength = ChainLength 3,
--                                                   thIsEBB = RegularBlock
--                                                 }),
--                                             testBody = TestBody {tbForkNo = 3, tbIsValid = True}
--                                           },
--                                         TestHeader' (TestHeader
--                                           { thHash = TestHeaderHash (-340232222464533890),
--                                             thPrevHash = BlockHash (TestHeaderHash 8656999388501844387),
--                                             thBodyHash = TestBodyHash 590680769285548757,
--                                             thSlotNo = SlotNo 4,
--                                             thBlockNo = BlockNo 2,
--                                             thChainLength = ChainLength 3,
--                                             thIsEBB = RegularBlock
--                                           }),
--                                         pack "\131\NUL\136\NUL;\EOT\184\191gY#\229\129\129\ESCx#\215\DC3\188m!\163\ESC\b2\133\a\180\235j\213\EOT\STX\ETX\129\SOH\131\NUL\ETX\245",
--                                         pack "\136\NUL;\EOT\184\191gY#\229\129\129\ESCx#\215\DC3\188m!\163\ESC\b2\133\a\180\235j\213\EOT\STX\ETX\129\SOH",
--                                         TestHeaderHash (-340232222464533890),
--                                         SlotNo 4,
--                                         IsNotEBB,
--                                         41,
--                                         35,
--                                         SomeSecond NestedCtxt {flipNestedCtxt = CtxtTestBlock}
--                                       )
--                                 }
--                           )
--                     }
--               }
--             []
--         ]
--     }
