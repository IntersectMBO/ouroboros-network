{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
module Test.Consensus.Shelley.Ledger.Golden (
    tests
  , mkDummyHash
  ) where

import           Codec.CBOR.FlatTerm (TermToken (..))
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))

import           Cardano.Binary (toCBOR)

import           Ouroboros.Network.Block (SlotNo (..), blockHash, genesisPoint)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract

import qualified Cardano.Ledger.Shelley.Crypto as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Ledger as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS
import qualified Test.Shelley.Spec.Ledger.Examples.Examples as Examples
import qualified Test.Shelley.Spec.Ledger.Utils as SL (testGlobals)

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.Util.Golden
import           Test.Util.Orphans.Arbitrary ()

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)

tests :: TestTree
tests = testGroup "Golden tests"
    [ testCase "Block"          test_golden_Block
    , testCase "Header"         test_golden_Header
    , testCase "HeaderHash"     test_golden_HeaderHash
    , testCase "GenTx"          test_golden_GenTx
    , testCase "GenTxId"        test_golden_GenTxId
    , testCase "ApplyTxErr"     test_golden_ApplyTxErr
    , testCase "ConsensusState" test_golden_ConsensusState
    , testCase "LedgerState"    test_golden_LedgerState
      -- TODO Query and result
    ]

type Block = ShelleyBlock TPraosMockCrypto

exampleBlock :: Block
exampleBlock = mkShelleyBlock Examples.blockEx3B

exampleGenTx :: GenTx Block
exampleGenTx = mkShelleyTx Examples.txEx2A

test_golden_Block :: Assertion
test_golden_Block = goldenTestCBOR
    toCBOR
    exampleBlock
    [ TkListLen 19
    , TkBytes "R]\STX\193"
    , TkInt 1677861428
    , TkInt 1239952560
    , TkInt 20
    , TkListLen 2
    , TkInt 2
    , TkListLen 2
    , TkInt 1239952560
    , TkInteger 41946005475178663182620722476359969013
    , TkListLen 2
    , TkInt 0
    , TkListLen 2
    , TkInt 1239952560
    , TkInteger 105192313543810009112138634467866559854
    , TkInt 194
    , TkInt 2
    , TkBytes "\159\SYN\n5"
    , TkInt 42768536
    , TkInt 0
    , TkInt 0
    , TkListLen 2
    , TkBytes "5\\\211K"
    , TkInt 1677861428
    , TkInt 0
    , TkInt 0
    , TkListLen 2
    , TkInteger 205434739841108429842581223023660727856
    , TkListLen 3
    , TkInt 42768536
    , TkInt 1
    , TkInt 10
    , TkListLen 1
    , TkMapLen 5
    , TkInt 0
    , TkTag 258
    , TkListLen 1
    , TkListLen 2
    , TkBytes "\t|W\190"
    , TkInt 0
    , TkInt 1
    , TkListLen 1
    , TkListLen 4
    , TkInt 0
    , TkBytes "\207\178\196\DC4"
    , TkBytes "\166DmC"
    , TkInt 9999999999999998
    , TkInt 2
    , TkInt 1
    , TkInt 3
    , TkInt 31
    , TkInt 6
    , TkListLen 2
    , TkMapLen 2
    , TkBytes "r\198Y\SO"
    , TkMapLen 2
    , TkInt 8
    , TkInt 200
    , TkInt 18
    , TkListLen 2
    , TkInt 1
    , TkBytes "e\138\176\224\245\253\239\216h\168\232\ENQ\209\SOH\169\140\195Z\181\ETB\233}\243\147q\130\218\fz\243\139="
    , TkBytes "\135\151\&0\144"
    , TkMapLen 2
    , TkInt 8
    , TkInt 200
    , TkInt 18
    , TkListLen 2
    , TkInt 1
    , TkBytes "e\138\176\224\245\253\239\216h\168\232\ENQ\209\SOH\169\140\195Z\181\ETB\233}\243\147q\130\218\fz\243\139="
    , TkInt 0
    , TkListLen 1
    , TkMapLen 1
    , TkInt 0
    , TkListLen 3
    , TkListLen 2
    , TkInt 1677861428
    , TkListLen 2
    , TkBytes "X\246?\147"
    , TkInt 1677861428
    , TkListLen 2
    , TkInt 302809592
    , TkListLen 2
    , TkBytes "X\246?\147"
    , TkInt 302809592
    , TkListLen 2
    , TkInt 1733510303
    , TkListLen 2
    , TkBytes "X\246?\147"
    , TkInt 1733510303
    , TkMapLen 0
    ]

test_golden_Header :: Assertion
test_golden_Header = goldenTestCBOR
    toCBOR
    (getHeader exampleBlock)
    [ TkListLen 16
    , TkBytes "R]\STX\193"
    , TkInt 1677861428
    , TkInt 1239952560
    , TkInt 20
    , TkListLen 2
    , TkInt 2
    , TkListLen 2
    , TkInt 1239952560
    , TkInteger 41946005475178663182620722476359969013
    , TkListLen 2
    , TkInt 0
    , TkListLen 2
    , TkInt 1239952560
    , TkInteger 105192313543810009112138634467866559854
    , TkInt 194
    , TkInt 2
    , TkBytes "\159\SYN\n5"
    , TkInt 42768536
    , TkInt 0
    , TkInt 0
    , TkListLen 2
    , TkBytes "5\\\211K"
    , TkInt 1677861428
    , TkInt 0
    , TkInt 0
    , TkListLen 2
    , TkInteger 205434739841108429842581223023660727856
    , TkListLen 3
    , TkInt 42768536
    , TkInt 1
    , TkInt 10
    ]

test_golden_HeaderHash :: Assertion
test_golden_HeaderHash = goldenTestCBOR
    toCBOR
    (blockHash exampleBlock)
    [ TkBytes ")?eL"
    ]

test_golden_GenTx :: Assertion
test_golden_GenTx = goldenTestCBOR
    toCBOR
    exampleGenTx
    [ TkListLen 3
    , TkMapLen 6
    , TkInt 0
    , TkTag 258
    , TkListLen 1
    , TkListLen 2
    , TkBytes "\180\\H\145"
    , TkInt 0
    , TkInt 1
    , TkListLen 1
    , TkListLen 4
    , TkInt 0
    , TkBytes "\207\178\196\DC4"
    , TkBytes "\166DmC"
    , TkInt 9999999999999726
    , TkInt 2
    , TkInt 3
    , TkInt 3
    , TkInt 10
    , TkInt 4
    , TkListLen 5
    , TkListLen 2
    , TkInt 0
    , TkBytes "\166DmC"
    , TkListLen 2
    , TkInt 0
    , TkBytes "\SI\205{m"
    , TkListLen 2
    , TkInt 0
    , TkBytes "n\240L\239"
    , TkListLen 10
    , TkInt 6
    , TkBytes "\188\190\&9\NUL"
    , TkBytes "\177G\FS\150"
    , TkInt 1
    , TkInt 5
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 10
    , TkListLen 2
    , TkInt 0
    , TkBytes "\166DmC"
    , TkTag 258
    , TkListLen 1
    , TkBytes "\166DmC"
    , TkListLen 0
    , TkListLen 1
    , TkListLen 2
    , TkString "alice.pool"
    , TkBytes "{}"
    , TkListLen 2
    , TkInt 9
    , TkMapLen 2
    , TkListLen 2
    , TkInt 0
    , TkBytes "n\240L\239"
    , TkInt 110
    , TkListLen 2
    , TkInt 0
    , TkBytes "\197 {\182"
    , TkInt 99
    , TkInt 6
    , TkListLen 2
    , TkMapLen 1
    , TkBytes "\246\216G\149"
    , TkMapLen 1
    , TkInt 5
    , TkInt 255
    , TkInt 0
    , TkMapLen 1
    , TkInt 0
    , TkListLen 10
    , TkListLen 2
    , TkInt 1247842986
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 1247842986
    , TkListLen 2
    , TkInt 314670341
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 314670341
    , TkListLen 2
    , TkInt 1684081173
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 1684081173
    , TkListLen 2
    , TkInt 2947187726
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 2947187726
    , TkListLen 2
    , TkInt 4006405861
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 4006405861
    , TkListLen 2
    , TkInt 1752983663
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 1752983663
    , TkListLen 2
    , TkInt 1725970733
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 1725970733
    , TkListLen 2
    , TkInt 1733510303
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 1733510303
    , TkListLen 2
    , TkInt 1355872821
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 1355872821
    , TkListLen 2
    , TkInt 1246613107
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 1246613107
    , TkListLen 0
    ]

test_golden_GenTxId :: Assertion
test_golden_GenTxId = goldenTestCBOR
    toCBOR
    (txId exampleGenTx)
    [ TkBytes "\176b\130\227"
    ]

test_golden_ApplyTxErr :: Assertion
test_golden_ApplyTxErr = goldenTestCBOR
    toCBOR
    exampleApplyTxErr
    [ TkListBegin
    , TkListLen 2
    , TkInt 0
    , TkListLen 1
    , TkInt 0
    , TkBreak
    ]
  where
    -- TODO incomplete, this type has tons of constructors that can all
    -- change.
    exampleApplyTxErr :: ApplyTxErr Block
    exampleApplyTxErr =
        ApplyTxError
      $ pure
      $ STS.LedgerFailure
      $ STS.UtxowFailure
      $ STS.InvalidWitnessesUTXOW

test_golden_ConsensusState :: Assertion
test_golden_ConsensusState = goldenTestCBOR
    toCBOR
    exampleConsensusState
    [ TkListLen 2
    , TkInt 0
    , TkListLen 2
    , TkListLen 2
    , TkInt 1
    , TkInt 1
    , TkMapLen 2
    , TkListLen 2
    , TkInt 1
    , TkInt 1
    , TkListLen 6
    , TkMapLen 2
    , TkBytes "U\165@\b"
    , TkInt 1
    , TkBytes "\158h\140X"
    , TkInt 2
    , TkListLen 1
    , TkListLen 3
    , TkInt 0
    , TkInt 1
    , TkBytes "U\165@\b"
    , TkListLen 1
    , TkInt 0
    , TkListLen 2
    , TkInt 1
    , TkBytes "K\245\DC2/4ET\197;\222.\187\140\210\183\227\209`\n\214\&1\195\133\165\215\204\226<w\133E\154"
    , TkListLen 2
    , TkInt 1
    , TkBytes "\219\193\180\201\NUL\255\228\141W[]\165\198\&8\EOT\SOH%\246]\176\254>$IKv\234\152dW\217\134"
    , TkListLen 2
    , TkInt 1
    , TkBytes "\bO\237\b\185x\175M}\EMjtF\168kX\NUL\158cka\GS\177b\DC1\182Z\154\173\255)\197"
    , TkListLen 2
    , TkInt 1
    , TkInt 2
    , TkListLen 6
    , TkMapLen 2
    , TkBytes "U\165@\b"
    , TkInt 1
    , TkBytes "\158h\140X"
    , TkInt 2
    , TkListLen 1
    , TkListLen 3
    , TkInt 0
    , TkInt 2
    , TkBytes "U\165@\b"
    , TkListLen 1
    , TkInt 0
    , TkListLen 2
    , TkInt 1
    , TkBytes "K\245\DC2/4ET\197;\222.\187\140\210\183\227\209`\n\214\&1\195\133\165\215\204\226<w\133E\154"
    , TkListLen 2
    , TkInt 1
    , TkBytes "\219\193\180\201\NUL\255\228\141W[]\165\198\&8\EOT\SOH%\246]\176\254>$IKv\234\152dW\217\134"
    , TkListLen 2
    , TkInt 1
    , TkBytes "\bO\237\b\185x\175M}\EMjtF\168kX\NUL\158cka\GS\177b\DC1\182Z\154\173\255)\197"
    ]
  where
    exampleConsensusState :: ConsensusState (BlockProtocol Block)
    exampleConsensusState =
      TPraosState.append (mkPrtclState 2) $
      TPraosState.empty  (mkPrtclState 1)

    mkPrtclState :: SlotNo -> STS.PrtclState TPraosMockCrypto
    mkPrtclState slot = STS.PrtclState
      (Map.fromList
       [ (SL.DiscKeyHash (mkDummyHash (Proxy @TPraosMockCrypto) 1), 1)
       , (SL.DiscKeyHash (mkDummyHash (Proxy @TPraosMockCrypto) 2), 2)
       ])
      (At SL.LastAppliedBlock {
          labBlockNo = 0
        , labSlotNo  = slot
        , labHash    = SL.HashHeader (mkDummyHash (Proxy @TPraosMockCrypto) 1)
        })
      SL.NeutralNonce
      (SL.mkNonce 1)
      (SL.mkNonce 2)
      (SL.mkNonce 3)

test_golden_LedgerState :: Assertion
test_golden_LedgerState = goldenTestCBOR
    encodeShelleyLedgerState
    exampleLedgerState
    [ TkListLen 2
    , TkInt 0
    , TkListLen 3
    , TkListLen 2
    , TkInt 10
    , TkBytes "5\246\240\SUB"
    , TkListLen 2
    , TkListLen 1
    , TkInt 0
    , TkListLen 0
    , TkListLen 7
    , TkInt 0
    , TkMapLen 0
    , TkMapLen 0
    , TkListLen 6
    , TkListLen 2
    , TkInt 0
    , TkInt 34000000000000000
    , TkListLen 4
    , TkListLen 3
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkListLen 3
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkListLen 3
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkInt 0
    , TkListLen 2
    , TkListLen 4
    , TkMapLen 2
    , TkListLen 2
    , TkBytes "\176b\130\227"
    , TkInt 0
    , TkListLen 4
    , TkInt 0
    , TkBytes "\207\178\196\DC4"
    , TkBytes "\166DmC"
    , TkInt 9999999999999726
    , TkListLen 2
    , TkBytes "\180\\H\145"
    , TkInt 1
    , TkListLen 4
    , TkInt 0
    , TkBytes "\154\244nn"
    , TkBytes "\SI\205{m"
    , TkInt 1000000000000000
    , TkInt 271
    , TkInt 3
    , TkMapLen 1
    , TkBytes "\246\216G\149"
    , TkMapLen 1
    , TkInt 5
    , TkInt 255
    , TkListLen 2
    , TkListLen 7
    , TkMapLen 3
    , TkListLen 2
    , TkInt 0
    , TkBytes "\SI\205{m"
    , TkInt 10
    , TkListLen 2
    , TkInt 0
    , TkBytes "n\240L\239"
    , TkInt 10
    , TkListLen 2
    , TkInt 0
    , TkBytes "\166DmC"
    , TkInt 10
    , TkMapLen 3
    , TkListLen 2
    , TkInt 0
    , TkBytes "\SI\205{m"
    , TkInt 0
    , TkListLen 2
    , TkInt 0
    , TkBytes "n\240L\239"
    , TkInt 0
    , TkListLen 2
    , TkInt 0
    , TkBytes "\166DmC"
    , TkInt 0
    , TkMapLen 0
    , TkMapLen 3
    , TkListLen 3
    , TkInt 10
    , TkInt 0
    , TkInt 0
    , TkListLen 2
    , TkInt 0
    , TkBytes "\166DmC"
    , TkListLen 3
    , TkInt 10
    , TkInt 0
    , TkInt 1
    , TkListLen 2
    , TkInt 0
    , TkBytes "\SI\205{m"
    , TkListLen 3
    , TkInt 10
    , TkInt 0
    , TkInt 2
    , TkListLen 2
    , TkInt 0
    , TkBytes "n\240L\239"
    , TkMapLen 0
    , TkMapLen 7
    , TkBytes "\SO\152\236\221"
    , TkBytes "\188\207\205\182"
    , TkBytes "!Mlu"
    , TkBytes "F\205Q\241"
    , TkBytes "Sk\188V"
    , TkBytes "/\170\227Y"
    , TkBytes "r\198Y\SO"
    , TkBytes "I\DC3@\DC4"
    , TkBytes "\135\151\&0\144"
    , TkBytes "j\222\211F"
    , TkBytes "\195\232\167\212"
    , TkBytes "\155\226\225\176"
    , TkBytes "\246\216G\149"
    , TkBytes "7\FS~\227"
    , TkMapLen 2
    , TkListLen 2
    , TkInt 0
    , TkBytes "n\240L\239"
    , TkInt 110
    , TkListLen 2
    , TkInt 0
    , TkBytes "\197 {\182"
    , TkInt 99
    , TkListLen 3
    , TkMapLen 1
    , TkBytes "\188\190\&9\NUL"
    , TkInt 10
    , TkMapLen 1
    , TkBytes "\188\190\&9\NUL"
    , TkListLen 9
    , TkBytes "\188\190\&9\NUL"
    , TkBytes "\177G\FS\150"
    , TkInt 1
    , TkInt 5
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 10
    , TkListLen 2
    , TkInt 0
    , TkBytes "\166DmC"
    , TkTag 258
    , TkListLen 1
    , TkBytes "\166DmC"
    , TkListLen 0
    , TkListLen 1
    , TkListLen 2
    , TkString "alice.pool"
    , TkBytes "{}"
    , TkMapLen 0
    , TkListLen 20
    , TkInt 0
    , TkInt 0
    , TkInt 50000
    , TkInt 10000
    , TkInt 10000
    , TkInt 7
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 2
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 500
    , TkInt 250
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 2
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 1000
    , TkInt 10000
    , TkInt 100
    , TkTag 30
    , TkListLen 2
    , TkInt 0
    , TkInt 1
    , TkTag 30
    , TkListLen 2
    , TkInt 21
    , TkInt 10000
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 5
    , TkTag 30
    , TkListLen 2
    , TkInt 9
    , TkInt 10
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 2
    , TkListLen 1
    , TkInt 0
    , TkInt 0
    , TkInt 0
    , TkListLen 20
    , TkInt 0
    , TkInt 0
    , TkInt 50000
    , TkInt 10000
    , TkInt 10000
    , TkInt 7
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 2
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 500
    , TkInt 250
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 2
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 1000
    , TkInt 10000
    , TkInt 100
    , TkTag 30
    , TkListLen 2
    , TkInt 0
    , TkInt 1
    , TkTag 30
    , TkListLen 2
    , TkInt 21
    , TkInt 10000
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 5
    , TkTag 30
    , TkListLen 2
    , TkInt 9
    , TkInt 10
    , TkTag 30
    , TkListLen 2
    , TkInt 1
    , TkInt 2
    , TkListLen 1
    , TkInt 0
    , TkInt 0
    , TkInt 0
    , TkListLen 3
    , TkMapLen 0
    , TkInt 0
    , TkListLen 3
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkListLen 0
    , TkMapLen 0
    , TkMapLen 50
    , TkInt 0
    , TkListLen 1
    , TkBytes "\SO\152\236\221"
    , TkInt 2
    , TkListLen 1
    , TkBytes "!Mlu"
    , TkInt 4
    , TkListLen 1
    , TkBytes "Sk\188V"
    , TkInt 6
    , TkListLen 1
    , TkBytes "r\198Y\SO"
    , TkInt 8
    , TkListLen 1
    , TkBytes "\135\151\&0\144"
    , TkInt 10
    , TkListLen 1
    , TkBytes "\195\232\167\212"
    , TkInt 12
    , TkListLen 1
    , TkBytes "\246\216G\149"
    , TkInt 14
    , TkListLen 1
    , TkBytes "\SO\152\236\221"
    , TkInt 16
    , TkListLen 1
    , TkBytes "!Mlu"
    , TkInt 18
    , TkListLen 1
    , TkBytes "Sk\188V"
    , TkInt 20
    , TkListLen 1
    , TkBytes "r\198Y\SO"
    , TkInt 22
    , TkListLen 1
    , TkBytes "\135\151\&0\144"
    , TkInt 24
    , TkListLen 1
    , TkBytes "\195\232\167\212"
    , TkInt 26
    , TkListLen 1
    , TkBytes "\246\216G\149"
    , TkInt 28
    , TkListLen 1
    , TkBytes "\SO\152\236\221"
    , TkInt 30
    , TkListLen 1
    , TkBytes "!Mlu"
    , TkInt 32
    , TkListLen 1
    , TkBytes "Sk\188V"
    , TkInt 34
    , TkListLen 1
    , TkBytes "r\198Y\SO"
    , TkInt 36
    , TkListLen 1
    , TkBytes "\135\151\&0\144"
    , TkInt 38
    , TkListLen 1
    , TkBytes "\195\232\167\212"
    , TkInt 40
    , TkListLen 1
    , TkBytes "\246\216G\149"
    , TkInt 42
    , TkListLen 1
    , TkBytes "\SO\152\236\221"
    , TkInt 44
    , TkListLen 1
    , TkBytes "!Mlu"
    , TkInt 46
    , TkListLen 1
    , TkBytes "Sk\188V"
    , TkInt 48
    , TkListLen 1
    , TkBytes "r\198Y\SO"
    , TkInt 50
    , TkListLen 1
    , TkBytes "\135\151\&0\144"
    , TkInt 52
    , TkListLen 1
    , TkBytes "\195\232\167\212"
    , TkInt 54
    , TkListLen 1
    , TkBytes "\246\216G\149"
    , TkInt 56
    , TkListLen 1
    , TkBytes "\SO\152\236\221"
    , TkInt 58
    , TkListLen 1
    , TkBytes "!Mlu"
    , TkInt 60
    , TkListLen 1
    , TkBytes "Sk\188V"
    , TkInt 62
    , TkListLen 1
    , TkBytes "r\198Y\SO"
    , TkInt 64
    , TkListLen 1
    , TkBytes "\135\151\&0\144"
    , TkInt 66
    , TkListLen 1
    , TkBytes "\195\232\167\212"
    , TkInt 68
    , TkListLen 1
    , TkBytes "\246\216G\149"
    , TkInt 70
    , TkListLen 1
    , TkBytes "\SO\152\236\221"
    , TkInt 72
    , TkListLen 1
    , TkBytes "!Mlu"
    , TkInt 74
    , TkListLen 1
    , TkBytes "Sk\188V"
    , TkInt 76
    , TkListLen 1
    , TkBytes "r\198Y\SO"
    , TkInt 78
    , TkListLen 1
    , TkBytes "\135\151\&0\144"
    , TkInt 80
    , TkListLen 1
    , TkBytes "\195\232\167\212"
    , TkInt 82
    , TkListLen 1
    , TkBytes "\246\216G\149"
    , TkInt 84
    , TkListLen 1
    , TkBytes "\SO\152\236\221"
    , TkInt 86
    , TkListLen 1
    , TkBytes "!Mlu"
    , TkInt 88
    , TkListLen 1
    , TkBytes "Sk\188V"
    , TkInt 90
    , TkListLen 1
    , TkBytes "r\198Y\SO"
    , TkInt 92
    , TkListLen 1
    , TkBytes "\135\151\&0\144"
    , TkInt 94
    , TkListLen 1
    , TkBytes "\195\232\167\212"
    , TkInt 96
    , TkListLen 1
    , TkBytes "\246\216G\149"
    , TkInt 98
    , TkListLen 1
    , TkBytes "\SO\152\236\221"
    ]
  where
    Examples.CHAINExample { startState, newBlock } = Examples.ex2A

    exampleLedgerState :: LedgerState Block
    exampleLedgerState = reapplyLedgerBlock
      (ShelleyLedgerConfig SL.testGlobals)
      (mkShelleyBlock newBlock)
      (ShelleyLedgerState {
          ledgerTip    = genesisPoint
        , history      = History.empty
        , shelleyState = STS.chainNes startState
        })

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mkDummyHash :: forall c a. Crypto c => Proxy c -> Int -> SL.Hash (SL.HASH c) a
mkDummyHash _ = coerce . SL.hash @(SL.HASH c)
