{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Consensus.Byron.Ledger.Golden (tests) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.FlatTerm (FlatTerm, TermToken (..))
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Control.Monad.Except (runExcept)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy8
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set

import           Cardano.Binary (toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import           Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC
import qualified Cardano.Chain.Update.Validation.Endorsement as CC
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Chain.Update.Validation.Registration as CC
import qualified Cardano.Chain.UTxO as CC

import           Ouroboros.Network.Block (SlotNo)
import           Ouroboros.Network.Point (WithOrigin (At))

import           Ouroboros.Consensus.Block (BlockProtocol, IsEBB (..))
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Protocol.PBFT.State.HeaderHashBytes

import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.DelegationHistory as DH

import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.Util.Golden

import qualified Test.Cardano.Chain.Common.Example as CC
import qualified Test.Cardano.Chain.Genesis.Dummy as CC
import qualified Test.Cardano.Chain.Update.Example as CC
import qualified Test.Cardano.Chain.UTxO.Example as CC

tests :: TestTree
tests = testGroup "Golden tests"
    -- Note that for most Byron types, we simply wrap the en/decoders from
    -- cardano-ledger, which already has golden tests for them.
    [ testCase "ConsensusState" test_golden_ConsensusState
    , testCase "LedgerState"    test_golden_LedgerState
    , testCase "GenTxId"        test_golden_GenTxId
    , testCase "UPIState"       test_golden_UPIState
    , testCase "TxSizeLinear"   test_golden_TxSizeLinear
    , testCase "HeaderHash"     test_golden_HeaderHash
    , testCase "GenTx"          test_golden_GenTx
    , testCase "ApplyTxErr"     test_golden_ApplyTxErr
    , testCase "HeaderState"    test_golden_HeaderState
    , testCase "ExtLedgerState" test_golden_ExtLedgerState
    , testCase "Query"          test_golden_Query
    , testCase "Result"         test_golden_Result
    ]

-- | Note that we must use the same value for the 'SecurityParam' as for the
-- 'S.WindowSize', because 'decodeByronConsensusState' only takes the
-- 'SecurityParam' and uses it as the basis for the 'S.WindowSize'.
secParam :: SecurityParam
secParam = SecurityParam 2

windowSize :: S.WindowSize
windowSize = S.WindowSize 2

exampleConsensusState :: ConsensusState (BlockProtocol ByronBlock)
exampleConsensusState = withEBB
  where
    signers = map (`S.PBftSigner` CC.exampleKeyHash) [1..4]

    withoutEBB = S.fromList
      secParam
      windowSize
      (At 2, Seq.fromList signers, S.NothingEbbInfo)

    -- info about an arbitrary hypothetical EBB
    exampleEbbSlot            :: SlotNo
    exampleEbbHeaderHashBytes :: HeaderHashBytes
    exampleEbbSlot            = 6
    exampleEbbHeaderHashBytes = mkHeaderHashBytesForTestingOnly
                                  (Lazy8.pack "test_golden_ConsensusState6")

    withEBB = S.appendEBB secParam windowSize
                exampleEbbSlot exampleEbbHeaderHashBytes
                withoutEBB

test_golden_ConsensusState :: Assertion
test_golden_ConsensusState = goldenTestCBOR
    encodeByronConsensusState
    exampleConsensusState
    [ TkListLen 2
    , TkInt 0
    , TkListLen 3
    , TkListLen 1
    , TkInt 2
    , TkListLen 4
    , TkListLen 2
    , TkInt 1
    , TkBytes "\180\192\224\129\n\140\212\245\185$\156A\144\a\EOT\179^\209\134 \\p\138@\181\171\172\144"
    , TkListLen 2
    , TkInt 2
    , TkBytes "\180\192\224\129\n\140\212\245\185$\156A\144\a\EOT\179^\209\134 \\p\138@\181\171\172\144"
    , TkListLen 2
    , TkInt 3
    , TkBytes "\180\192\224\129\n\140\212\245\185$\156A\144\a\EOT\179^\209\134 \\p\138@\181\171\172\144"
    , TkListLen 2
    , TkInt 4
    , TkBytes "\180\192\224\129\n\140\212\245\185$\156A\144\a\EOT\179^\209\134 \\p\138@\181\171\172\144"
    , TkListLen 2
    , TkInt 1
    , TkListLen 4
    , TkInt 0
    , TkInt 6
    , TkBytes "test_golden_ConsensusState6"
    , TkListLen 2
    , TkInt 1
    , TkInt 4
    ]

exampleLedgerState :: LedgerState ByronBlock
exampleLedgerState = ByronLedgerState
    { byronLedgerState       = initState
    , byronDelegationHistory = DH.empty
    }
  where
    initState :: CC.Block.ChainValidationState
    Right initState = runExcept $
      CC.Block.initialChainValidationState CC.dummyConfig

test_golden_LedgerState :: Assertion
test_golden_LedgerState = goldenTestCBOR
    encodeByronLedgerState
    exampleLedgerState
    [ TkListLen 2
    , TkListLen 5
    , TkInt 0
    , TkListLen 2
    , TkInt 0
    , TkBytes "y\USBV\225Lg\185\ETX\\;\128\160\130j\223q\157\&66\193\142\239\SYN\201\139\132\184\&3r=Q"
    , TkMapLen 26
    , TkListLen 2
    , TkListLen 4
    , TkInt 145932809460399234
    , TkInt 2066773051833847901
    , TkInt 4014626457154473992
    , TkInt 5443928220376176620
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\242\247\ESC\232\165!\139#y\160n\203|\134\230'w=\DC2\254s\130\184\192\&4\245\153\EM\161\STXE\SUB\ETXO\160\&1\NUL\SUBWkx\168"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 638553421352263617
    , TkInt 1278979393409512636
    , TkInt 3206717528751391115
    , TkInt 942270070445544212
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\DC1\135\147\243\&4\165\210\228y\189\CANF\DLEw\198\158\160\US\157\238\172p\181\177V6G\205\161\STXE\SUB\ETXO\160\&1\STX\SUB\NAK\"\231\202"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 987322392125868598
    , TkInteger 18107770733663414954
    , TkInt 9080228708490772990
    , TkInteger 12566528882800227583
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\133PA\217`\247-\233f\176\186\233Y\188H$\242I{\158\183\150\128*\CAN\206\DC4\246\161\STXE\SUB\ETXO\160\&1\NUL\SUB\193Ya\184"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 1077219383633751352
    , TkInteger 13779938414064947295
    , TkInteger 13290892156411602714
    , TkInteger 15642540401506821110
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\133\185\200\221c\128\139\GS\190\179-\245\&7\n@0\EOT\197\236i\n\203\217x/\173k\EOT\161\STXE\SUB\ETXO\160\&1\STX\SUBU\130\199\&2"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 1140582490054540175
    , TkInteger 12671340449058009518
    , TkInt 9050193900121815118
    , TkInt 6749707747826545980
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FSR\247\&6\210\215yv\187p%\153;!\186\206\241\161\v\197\239|\135*$\187\231p\199\161\STXE\SUB\ETXO\160\&1\NUL\SUB\219\223\\\141"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 1746500173916227596
    , TkInteger 16207848531057018955
    , TkInteger 17748920106976756341
    , TkInt 8572314307988464705
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS;\DC3\160\&6\131~FG\190\&6+\US\EOTS;\242\GS\140\222d\169\148\&1y\ETB\192\241\134\161\STXE\SUB\ETXO\160\&1\NUL\SUB\218D|\176"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 3838391068189529133
    , TkInteger 10455339755664026743
    , TkInteger 12095308140428552401
    , TkInteger 11669651806149219437
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS{\226\212\163\197Q)@m\180f\221\168\128\DC4\240\175\192\215\174*#\242H\SUB`\162'\161\STXE\SUB\ETXO\160\&1\STX\SUB\132o=&"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 4542638322102565995
    , TkInteger 14323167351822697394
    , TkInteger 11156764329846795491
    , TkInt 1720061027411356432
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\243\207\198f\167\129\241\244;\128<y>\141\b\145\217\255\197'\187\DC2\239x\SI\153BZ\161\STXE\SUB\ETXO\160\&1\STX\SUB\156M-D"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 4632815190778387767
    , TkInteger 11906640867094937249
    , TkInteger 18310359702763415096
    , TkInt 2462545015939261677
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\159\247_!\SYN\tc\237\234\241\204Q\187]d\189N\169Y9\230\242G\171\&4;\141\&0\161\STXE\SUB\ETXO\160\&1\NUL\SUBD\fzZ"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 6132754149677084214
    , TkInteger 18053418585707400126
    , TkInt 5163878468623839394
    , TkInt 8564432760274978479
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FSe\143\DC1\238\229(\146\199\NUL\196w\191h\231\143\161\158\206\209\140\178v\159\230\153k\185\210\161\STXE\SUB\ETXO\160\&1\NUL\SUBo\218\224\148"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 6222137868530982613
    , TkInteger 13435282552215222510
    , TkInt 7561658303799227849
    , TkInteger 14062744093696836369
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\144\254l\211\DC4\217\224\135\174\227\248\161*)\147\185\SUBk\193\&2;\165\&3\151`q\169\187\161\STXE\SUB\ETXO\160\&1\NUL\SUB\ESC\ACK\DEL?"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 7979518298955643164
    , TkInteger 9809227854801451707
    , TkInteger 15480150435945157662
    , TkInt 2146110223735237964
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS&\137\182\228EK\209\254\253i\r\142\215l\240\174\251\193\NAK\245\251#\165Q\204\&7\ESC'\161\STXE\SUB\ETXO\160\&1\NUL\SUB\233v!\SOH"
    , TkInt 1484999999752500
    , TkListLen 2
    , TkListLen 4
    , TkInt 8263086324104474949
    , TkInt 8018597027991575130
    , TkInteger 17024485007769541090
    , TkInteger 11024084952928002273
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\182\230\"\SYNI.\179A.I{\216oX\170\224\207h7\141\198\194\195\147\244\224\173\200\161\STXE\SUB\ETXO\160\&1\STX\SUB0\255\&3\194"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 8442650079267732540
    , TkInteger 10810541564712081323
    , TkInt 6025286487921769361
    , TkInt 6993488686648862048
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FShP\237\197\239\252\210\r|\a\n}\196\ESCJ16:(\"\132=r\210`c\169\EOT\161\STXE\SUB\ETXO\160\&1\NUL\SUB\DC2v\215o"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 11167735569370480516
    , TkInt 4597837650897650785
    , TkInt 7672439953764322743
    , TkInt 7611921438180931751
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\tZ\200\208\ETB\132\196\244\SOH-\156&\213B\234\247\202@4\229I\136\196d\249\"\STX\175\161\STXE\SUB\ETXO\160\&1\STX\SUB\242g\ETX\166"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInteger 11572861025515785825
    , TkInteger 14626238934551034050
    , TkInt 4527828982957692009
    , TkInt 8525277721551289281
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\131\FS\252\USw\239\205C\179|\a\187&\236\172\163b\206\222\175(tU\167\144\RS\166\135\161\STXE\SUB\ETXO\160\&1\STX\SUB\191\173\&4\140"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInteger 12138289793200168837
    , TkInteger 16897700957097401000
    , TkInt 7426733002984601805
    , TkInt 6659088720030705546
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FSd\142\a\143Q\224qX\160\156\246\200\141\140fSIY\155\147Z\SYN\183\252\NUL\129\138\EOT\161\STXE\SUB\ETXO\160\&1\NUL\SUB\200M\164\158"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 12519624700035879376
    , TkInt 4133236515225696707
    , TkInteger 9750044942473957653
    , TkInt 3631360888127509049
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\137\171M$\SUB\EMS\224Fk\206\198G\143;+z\170)\244o!\134/\SUB\213!\DC4\161\STXE\SUB\ETXO\160\&1\STX\SUBul\145\207"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInteger 14049461374821561010
    , TkInteger 15049041127247864172
    , TkInteger 13423312326842282974
    , TkInteger 10779087238818319226
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\144\&4\236\167\254\SUB.\213\FS\222P\193\235\132\239\210}\198/\255h\197b\176=\136{\184\161\STXE\SUB\ETXO\160\&1\NUL\SUB\162\145\247T"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 14064056026919452258
    , TkInt 4774118957873457035
    , TkInt 6724040529474867229
    , TkInteger 9996415388281714729
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\156\190\150T\a\DC2\214}\135\&0\141\176k\DC3\245\158~\ETX\NAK1\DEL\223\136\ETX\154\128\&5\143\161\STXE\SUB\ETXO\160\&1\NUL\SUB\243\242\253\167"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 14091197760055523581
    , TkInteger 16749904622920749241
    , TkInt 4572623388132145937
    , TkInteger 16339124336446351562
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS.%~\DC1:|\SO.\\\137\156$\202\210\160rD7<\190\DEL\220\v\206e\177\227:\161\STXE\SUB\ETXO\160\&1\NUL\SUBU\183\ESC\200"
    , TkInt 1484999999752500
    , TkListLen 2
    , TkListLen 4
    , TkInteger 14661851656095516300
    , TkInt 7212920377225478636
    , TkInt 848885576222546425
    , TkInteger 17637186979907084323
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\139D7~\"\193\GS\214\130X\a\205\DC26oA\133xiLpH\193S\139y\200.\161\STXE\SUB\ETXO\160\&1\NUL\SUB\212~\223~"
    , TkInt 1484999999752500
    , TkListLen 2
    , TkListLen 4
    , TkInteger 16190632671347901473
    , TkInt 1774991130580694905
    , TkInteger 9756871929249673052
    , TkInteger 11956763899831506646
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\185\253\136\145\179^\139\227\254\130d\169A\SO\245\DELO\231\186\b\185\136^\DC2\DELi\159\&6\161\STXE\SUB\ETXO\160\&1\STX\SUBw\223\173\203"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInteger 17138939940637387584
    , TkInteger 9460999825282989633
    , TkInteger 12388859200407555021
    , TkInt 2442977770505928115
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\215\251\215}Q\FSDWX]\223\204\142\181\DLE\US\156\DC2M\253\186\191q@\194\250\185\SI\161\STXE\SUB\ETXO\160\&1\NUL\SUB\128\172X\228"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 17830920586845443660
    , TkInt 1638043712563223825
    , TkInt 6774544498463254325
    , TkInteger 15659936038236310002
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\be\196\ETXjT\134\190q\248l\155\\\197\151\210\145\237\201q\248 \139v3\195vs\161\STXE\SUB\ETXO\160\&1\NUL\SUB\DC3d:\149"
    , TkInt 1484999999752500
    , TkListLen 2
    , TkListLen 4
    , TkInteger 18242269704432320112
    , TkInteger 13970827755514032820
    , TkInt 8901293958884611136
    , TkInteger 15092937231456236479
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\NAK\DC3\181\171\192\DC3#\NAK,\204f\181\141\172\248*c/\183C\rOEi\144\252\174\212\161\STXE\SUB\ETXO\160\&1\STX\SUB\255\DEL\ESC/"
    , TkInt 100000
    , TkListLen 11
    , TkInt 0
    , TkListLen 3
    , TkInt 0
    , TkInt 0
    , TkInt 0
    , TkListLen 14
    , TkInt 0
    , TkInt 7000
    , TkInt 2000000
    , TkInt 2000000
    , TkInt 8192
    , TkInt 700
    , TkInt 10000000000000
    , TkInt 5000000000000
    , TkInt 1000000000000
    , TkInt 100000000000000
    , TkInt 10
    , TkListLen 3
    , TkInt 900000000000000
    , TkInt 600000000000000
    , TkInt 50000000000000
    , TkListLen 2
    , TkInt 0
    , TkTag 24
      -- This is the serialised form of exampleTxSizeLinear below
    , TkBytes "\130\ESC\NUL\NUL\141QuOR\NUL\ESC\NUL\NUL\NUL\n;b\190\128"
    , TkInteger 18446744073709551615
    , TkListBegin
    , TkBreak
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkTag 258
    , TkListLen 0
    , TkMapLen 0
    , TkListLen 2
    , TkListLen 2
    , TkListBegin
    , TkBreak
    , TkTag 258
    , TkListLen 4
    , TkListLen 2
    , TkInt 0
    , TkBytes "\SUB\US\247\ETXQ\ETX\216\169\225\229jNN\GS\NUL1\251?1\166\169\149\&17\223\229Lg"
    , TkListLen 2
    , TkInt 0
    , TkBytes "(\RSZ\233\227W\151\n\r\208\a\220\155!gZ\176}\227\214~\200%\215\&66!\249"
    , TkListLen 2
    , TkInt 0
    , TkBytes "D(<\229\196Nn\NUL\132a\144\173_\180p\197\219\220gL\"\223*>\145\\Mq"
    , TkListLen 2
    , TkInt 0
    , TkBytes "_S\224\RS\DC3f\174\218\136\DC1\194\166\&0\240\224\&7\az{e\DLE\147\210\189\196\239r\NUL"
    , TkListLen 2
    , TkListBegin
    , TkListLen 2
    , TkBytes "\SUB\US\247\ETXQ\ETX\216\169\225\229jNN\GS\NUL1\251?1\166\169\149\&17\223\229Lg"
    , TkBytes "\179\f>\t\FS\209\143v*\151RJlV$\195\251\FS\224\t;P*\n0\DC2\248\242"
    , TkListLen 2
    , TkBytes "(\RSZ\233\227W\151\n\r\208\a\220\155!gZ\176}\227\214~\200%\215\&66!\249"
    , TkBytes "\223I\SOH\232\246\171\217j\139\208W\159$\ETXj\139\155$\193\223K\237\133\161@-\176\155"
    , TkListLen 2
    , TkBytes "D(<\229\196Nn\NUL\132a\144\173_\180p\197\219\220gL\"\223*>\145\\Mq"
    , TkBytes "\r\145eg\249kje\210\EOT\150nj\171_\189$.V\195!\131?\139\165\214\a\218"
    , TkListLen 2
    , TkBytes "_S\224\RS\DC3f\174\218\136\DC1\194\166\&0\240\224\&7\az{e\DLE\147\210\189\196\239r\NUL"
    , TkBytes "\248>\158\STXp\171\204tK&W\v\ACK\159f\219Z\131\205\235\228\190\219|\212\&5\196C"
    , TkBreak
    , TkMapLen 4
    , TkBytes "\SUB\US\247\ETXQ\ETX\216\169\225\229jNN\GS\NUL1\251?1\166\169\149\&17\223\229Lg"
    , TkInt 0
    , TkBytes "(\RSZ\233\227W\151\n\r\208\a\220\155!gZ\176}\227\214~\200%\215\&66!\249"
    , TkInt 0
    , TkBytes "D(<\229\196Nn\NUL\132a\144\173_\180p\197\219\220gL\"\223*>\145\\Mq"
    , TkInt 0
    , TkBytes "_S\224\RS\DC3f\174\218\136\DC1\194\166\&0\240\224\&7\az{e\DLE\147\210\189\196\239r\NUL"
    , TkInt 0
    , TkListLen 2
    , TkListLen 1
    , TkInt 0
    , TkListLen 0
    ]

test_golden_HeaderState :: Assertion
test_golden_HeaderState = goldenTestCBOR
    encodeByronHeaderState
    exampleHeaderState
    [ TkListLen 3
    , TkListLen 2
    , TkInt 0
    , TkListLen 3
    , TkListLen 0
    , TkListLen 0
    , TkListLen 1
    , TkInt 0
    , TkListBegin
    , TkListLen 4
    , TkInt 0
    , TkBytes "y\USBV\225Lg\185\ETX\\;\128\160\130j\223q\157\&66\193\142\239\SYN\201\139\132\184\&3r=Q"
    , TkInt 0
    , TkBool False
    , TkBreak
    , TkListLen 0
    ]

exampleHeaderState :: HeaderState ByronBlock
exampleHeaderState = (genesisHeaderState S.empty)
    { headerStateTips = Seq.singleton annTip }
  where
    annTip = AnnTip {
        annTipSlotNo  = 0
      , annTipHash    = exampleHeaderHash
      , annTipBlockNo = 0
      , annTipInfo    = IsNotEBB
      }

exampleHeaderHash :: ByronHash
exampleHeaderHash = ByronHash $ CC.Genesis.configGenesisHeaderHash CC.dummyConfig

test_golden_ExtLedgerState :: Assertion
test_golden_ExtLedgerState = goldenTestCBOR
    encodeByronExtLedgerState
    exampleExtLedgerState
    [ TkListLen 2
    , TkListLen 5
    , TkInt 0
    , TkListLen 2
    , TkInt 0
    , TkBytes "y\USBV\225Lg\185\ETX\\;\128\160\130j\223q\157\&66\193\142\239\SYN\201\139\132\184\&3r=Q"
    , TkMapLen 26
    , TkListLen 2
    , TkListLen 4
    , TkInt 145932809460399234
    , TkInt 2066773051833847901
    , TkInt 4014626457154473992
    , TkInt 5443928220376176620
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\242\247\ESC\232\165!\139#y\160n\203|\134\230'w=\DC2\254s\130\184\192\&4\245\153\EM\161\STXE\SUB\ETXO\160\&1\NUL\SUBWkx\168"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 638553421352263617
    , TkInt 1278979393409512636
    , TkInt 3206717528751391115
    , TkInt 942270070445544212
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\DC1\135\147\243\&4\165\210\228y\189\CANF\DLEw\198\158\160\US\157\238\172p\181\177V6G\205\161\STXE\SUB\ETXO\160\&1\STX\SUB\NAK\"\231\202"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 987322392125868598
    , TkInteger 18107770733663414954
    , TkInt 9080228708490772990
    , TkInteger 12566528882800227583
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\133PA\217`\247-\233f\176\186\233Y\188H$\242I{\158\183\150\128*\CAN\206\DC4\246\161\STXE\SUB\ETXO\160\&1\NUL\SUB\193Ya\184"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 1077219383633751352
    , TkInteger 13779938414064947295
    , TkInteger 13290892156411602714
    , TkInteger 15642540401506821110
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\133\185\200\221c\128\139\GS\190\179-\245\&7\n@0\EOT\197\236i\n\203\217x/\173k\EOT\161\STXE\SUB\ETXO\160\&1\STX\SUBU\130\199\&2"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 1140582490054540175
    , TkInteger 12671340449058009518
    , TkInt 9050193900121815118
    , TkInt 6749707747826545980
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FSR\247\&6\210\215yv\187p%\153;!\186\206\241\161\v\197\239|\135*$\187\231p\199\161\STXE\SUB\ETXO\160\&1\NUL\SUB\219\223\\\141"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 1746500173916227596
    , TkInteger 16207848531057018955
    , TkInteger 17748920106976756341
    , TkInt 8572314307988464705
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS;\DC3\160\&6\131~FG\190\&6+\US\EOTS;\242\GS\140\222d\169\148\&1y\ETB\192\241\134\161\STXE\SUB\ETXO\160\&1\NUL\SUB\218D|\176"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 3838391068189529133
    , TkInteger 10455339755664026743
    , TkInteger 12095308140428552401
    , TkInteger 11669651806149219437
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS{\226\212\163\197Q)@m\180f\221\168\128\DC4\240\175\192\215\174*#\242H\SUB`\162'\161\STXE\SUB\ETXO\160\&1\STX\SUB\132o=&"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 4542638322102565995
    , TkInteger 14323167351822697394
    , TkInteger 11156764329846795491
    , TkInt 1720061027411356432
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\243\207\198f\167\129\241\244;\128<y>\141\b\145\217\255\197'\187\DC2\239x\SI\153BZ\161\STXE\SUB\ETXO\160\&1\STX\SUB\156M-D"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 4632815190778387767
    , TkInteger 11906640867094937249
    , TkInteger 18310359702763415096
    , TkInt 2462545015939261677
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\159\247_!\SYN\tc\237\234\241\204Q\187]d\189N\169Y9\230\242G\171\&4;\141\&0\161\STXE\SUB\ETXO\160\&1\NUL\SUBD\fzZ"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 6132754149677084214
    , TkInteger 18053418585707400126
    , TkInt 5163878468623839394
    , TkInt 8564432760274978479
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FSe\143\DC1\238\229(\146\199\NUL\196w\191h\231\143\161\158\206\209\140\178v\159\230\153k\185\210\161\STXE\SUB\ETXO\160\&1\NUL\SUBo\218\224\148"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 6222137868530982613
    , TkInteger 13435282552215222510
    , TkInt 7561658303799227849
    , TkInteger 14062744093696836369
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\144\254l\211\DC4\217\224\135\174\227\248\161*)\147\185\SUBk\193\&2;\165\&3\151`q\169\187\161\STXE\SUB\ETXO\160\&1\NUL\SUB\ESC\ACK\DEL?"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInt 7979518298955643164
    , TkInteger 9809227854801451707
    , TkInteger 15480150435945157662
    , TkInt 2146110223735237964
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS&\137\182\228EK\209\254\253i\r\142\215l\240\174\251\193\NAK\245\251#\165Q\204\&7\ESC'\161\STXE\SUB\ETXO\160\&1\NUL\SUB\233v!\SOH"
    , TkInt 1484999999752500
    , TkListLen 2
    , TkListLen 4
    , TkInt 8263086324104474949
    , TkInt 8018597027991575130
    , TkInteger 17024485007769541090
    , TkInteger 11024084952928002273
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\182\230\"\SYNI.\179A.I{\216oX\170\224\207h7\141\198\194\195\147\244\224\173\200\161\STXE\SUB\ETXO\160\&1\STX\SUB0\255\&3\194"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInt 8442650079267732540
    , TkInteger 10810541564712081323
    , TkInt 6025286487921769361
    , TkInt 6993488686648862048
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FShP\237\197\239\252\210\r|\a\n}\196\ESCJ16:(\"\132=r\210`c\169\EOT\161\STXE\SUB\ETXO\160\&1\NUL\SUB\DC2v\215o"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 11167735569370480516
    , TkInt 4597837650897650785
    , TkInt 7672439953764322743
    , TkInt 7611921438180931751
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\tZ\200\208\ETB\132\196\244\SOH-\156&\213B\234\247\202@4\229I\136\196d\249\"\STX\175\161\STXE\SUB\ETXO\160\&1\STX\SUB\242g\ETX\166"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInteger 11572861025515785825
    , TkInteger 14626238934551034050
    , TkInt 4527828982957692009
    , TkInt 8525277721551289281
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\131\FS\252\USw\239\205C\179|\a\187&\236\172\163b\206\222\175(tU\167\144\RS\166\135\161\STXE\SUB\ETXO\160\&1\STX\SUB\191\173\&4\140"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInteger 12138289793200168837
    , TkInteger 16897700957097401000
    , TkInt 7426733002984601805
    , TkInt 6659088720030705546
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FSd\142\a\143Q\224qX\160\156\246\200\141\140fSIY\155\147Z\SYN\183\252\NUL\129\138\EOT\161\STXE\SUB\ETXO\160\&1\NUL\SUB\200M\164\158"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 12519624700035879376
    , TkInt 4133236515225696707
    , TkInteger 9750044942473957653
    , TkInt 3631360888127509049
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\137\171M$\SUB\EMS\224Fk\206\198G\143;+z\170)\244o!\134/\SUB\213!\DC4\161\STXE\SUB\ETXO\160\&1\STX\SUBul\145\207"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInteger 14049461374821561010
    , TkInteger 15049041127247864172
    , TkInteger 13423312326842282974
    , TkInteger 10779087238818319226
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\144\&4\236\167\254\SUB.\213\FS\222P\193\235\132\239\210}\198/\255h\197b\176=\136{\184\161\STXE\SUB\ETXO\160\&1\NUL\SUB\162\145\247T"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 14064056026919452258
    , TkInt 4774118957873457035
    , TkInt 6724040529474867229
    , TkInteger 9996415388281714729
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\156\190\150T\a\DC2\214}\135\&0\141\176k\DC3\245\158~\ETX\NAK1\DEL\223\136\ETX\154\128\&5\143\161\STXE\SUB\ETXO\160\&1\NUL\SUB\243\242\253\167"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 14091197760055523581
    , TkInteger 16749904622920749241
    , TkInt 4572623388132145937
    , TkInteger 16339124336446351562
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS.%~\DC1:|\SO.\\\137\156$\202\210\160rD7<\190\DEL\220\v\206e\177\227:\161\STXE\SUB\ETXO\160\&1\NUL\SUBU\183\ESC\200"
    , TkInt 1484999999752500
    , TkListLen 2
    , TkListLen 4
    , TkInteger 14661851656095516300
    , TkInt 7212920377225478636
    , TkInt 848885576222546425
    , TkInteger 17637186979907084323
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\139D7~\"\193\GS\214\130X\a\205\DC26oA\133xiLpH\193S\139y\200.\161\STXE\SUB\ETXO\160\&1\NUL\SUB\212~\223~"
    , TkInt 1484999999752500
    , TkListLen 2
    , TkListLen 4
    , TkInteger 16190632671347901473
    , TkInt 1774991130580694905
    , TkInteger 9756871929249673052
    , TkInteger 11956763899831506646
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\185\253\136\145\179^\139\227\254\130d\169A\SO\245\DELO\231\186\b\185\136^\DC2\DELi\159\&6\161\STXE\SUB\ETXO\160\&1\STX\SUBw\223\173\203"
    , TkInt 100000
    , TkListLen 2
    , TkListLen 4
    , TkInteger 17138939940637387584
    , TkInteger 9460999825282989633
    , TkInteger 12388859200407555021
    , TkInt 2442977770505928115
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\215\251\215}Q\FSDWX]\223\204\142\181\DLE\US\156\DC2M\253\186\191q@\194\250\185\SI\161\STXE\SUB\ETXO\160\&1\NUL\SUB\128\172X\228"
    , TkInt 4999999999166
    , TkListLen 2
    , TkListLen 4
    , TkInteger 17830920586845443660
    , TkInt 1638043712563223825
    , TkInt 6774544498463254325
    , TkInteger 15659936038236310002
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\be\196\ETXjT\134\190q\248l\155\\\197\151\210\145\237\201q\248 \139v3\195vs\161\STXE\SUB\ETXO\160\&1\NUL\SUB\DC3d:\149"
    , TkInt 1484999999752500
    , TkListLen 2
    , TkListLen 4
    , TkInteger 18242269704432320112
    , TkInteger 13970827755514032820
    , TkInt 8901293958884611136
    , TkInteger 15092937231456236479
    , TkInt 0
    , TkListLen 2
    , TkBytes "\130\216\CANX(\131X\FS\NAK\DC3\181\171\192\DC3#\NAK,\204f\181\141\172\248*c/\183C\rOEi\144\252\174\212\161\STXE\SUB\ETXO\160\&1\STX\SUB\255\DEL\ESC/"
    , TkInt 100000
    , TkListLen 11
    , TkInt 0
    , TkListLen 3
    , TkInt 0
    , TkInt 0
    , TkInt 0
    , TkListLen 14
    , TkInt 0
    , TkInt 7000
    , TkInt 2000000
    , TkInt 2000000
    , TkInt 8192
    , TkInt 700
    , TkInt 10000000000000
    , TkInt 5000000000000
    , TkInt 1000000000000
    , TkInt 100000000000000
    , TkInt 10
    , TkListLen 3
    , TkInt 900000000000000
    , TkInt 600000000000000
    , TkInt 50000000000000
    , TkListLen 2
    , TkInt 0
    , TkTag 24
    , TkBytes "\130\ESC\NUL\NUL\141QuOR\NUL\ESC\NUL\NUL\NUL\n;b\190\128"
    , TkInteger 18446744073709551615
    , TkListBegin
    , TkBreak
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkTag 258
    , TkListLen 0
    , TkMapLen 0
    , TkListLen 2
    , TkListLen 2
    , TkListBegin
    , TkBreak
    , TkTag 258
    , TkListLen 4
    , TkListLen 2
    , TkInt 0
    , TkBytes "\SUB\US\247\ETXQ\ETX\216\169\225\229jNN\GS\NUL1\251?1\166\169\149\&17\223\229Lg"
    , TkListLen 2
    , TkInt 0
    , TkBytes "(\RSZ\233\227W\151\n\r\208\a\220\155!gZ\176}\227\214~\200%\215\&66!\249"
    , TkListLen 2
    , TkInt 0
    , TkBytes "D(<\229\196Nn\NUL\132a\144\173_\180p\197\219\220gL\"\223*>\145\\Mq"
    , TkListLen 2
    , TkInt 0
    , TkBytes "_S\224\RS\DC3f\174\218\136\DC1\194\166\&0\240\224\&7\az{e\DLE\147\210\189\196\239r\NUL"
    , TkListLen 2
    , TkListBegin
    , TkListLen 2
    , TkBytes "\SUB\US\247\ETXQ\ETX\216\169\225\229jNN\GS\NUL1\251?1\166\169\149\&17\223\229Lg"
    , TkBytes "\179\f>\t\FS\209\143v*\151RJlV$\195\251\FS\224\t;P*\n0\DC2\248\242"
    , TkListLen 2
    , TkBytes "(\RSZ\233\227W\151\n\r\208\a\220\155!gZ\176}\227\214~\200%\215\&66!\249"
    , TkBytes "\223I\SOH\232\246\171\217j\139\208W\159$\ETXj\139\155$\193\223K\237\133\161@-\176\155"
    , TkListLen 2
    , TkBytes "D(<\229\196Nn\NUL\132a\144\173_\180p\197\219\220gL\"\223*>\145\\Mq"
    , TkBytes "\r\145eg\249kje\210\EOT\150nj\171_\189$.V\195!\131?\139\165\214\a\218"
    , TkListLen 2
    , TkBytes "_S\224\RS\DC3f\174\218\136\DC1\194\166\&0\240\224\&7\az{e\DLE\147\210\189\196\239r\NUL"
    , TkBytes "\248>\158\STXp\171\204tK&W\v\ACK\159f\219Z\131\205\235\228\190\219|\212\&5\196C"
    , TkBreak
    , TkMapLen 4
    , TkBytes "\SUB\US\247\ETXQ\ETX\216\169\225\229jNN\GS\NUL1\251?1\166\169\149\&17\223\229Lg"
    , TkInt 0
    , TkBytes "(\RSZ\233\227W\151\n\r\208\a\220\155!gZ\176}\227\214~\200%\215\&66!\249"
    , TkInt 0
    , TkBytes "D(<\229\196Nn\NUL\132a\144\173_\180p\197\219\220gL\"\223*>\145\\Mq"
    , TkInt 0
    , TkBytes "_S\224\RS\DC3f\174\218\136\DC1\194\166\&0\240\224\&7\az{e\DLE\147\210\189\196\239r\NUL"
    , TkInt 0
    , TkListLen 2
    , TkListLen 1
    , TkInt 0
    , TkListLen 0
    , TkListLen 3
    , TkListLen 2
    , TkInt 0
    , TkListLen 3
    , TkListLen 0
    , TkListLen 0
    , TkListLen 1
    , TkInt 0
    , TkListBegin
    , TkListLen 4
    , TkInt 0
    , TkBytes "y\USBV\225Lg\185\ETX\\;\128\160\130j\223q\157\&66\193\142\239\SYN\201\139\132\184\&3r=Q"
    , TkInt 0
    , TkBool False
    , TkBreak
    , TkListLen 0
    ]
  where
    exampleExtLedgerState = ExtLedgerState
      { ledgerState = exampleLedgerState
      , headerState = exampleHeaderState
      }

test_golden_GenTxId :: Assertion
test_golden_GenTxId = goldenTestCBOR
    encodeByronGenTxId
    exampleGenTxId
    [ TkListLen 2
    , TkInt 0
    , TkBytes "K\168\&9\196 \179\210\189C\149\&0\248\145\202\233\165\212\196\216\DC2\EOTF0\218\199.\142\tb\254\238\204"
    ]
  where
    exampleGenTxId = ByronTxId CC.exampleTxId

test_golden_UPIState :: Assertion
test_golden_UPIState = goldenTestCBOR
    toCBOR
    exampleUPIState
    [ TkListLen 11
    , TkInt 0
    , TkListLen 3
    , TkInt 0
    , TkInt 0
    , TkInt 0
    , TkListLen 14
    , TkInt 0
    , TkInt 7000
    , TkInt 2000000
    , TkInt 2000000
    , TkInt 8192
    , TkInt 700
    , TkInt 10000000000000
    , TkInt 5000000000000
    , TkInt 1000000000000
    , TkInt 100000000000000
    , TkInt 10
    , TkListLen 3
    , TkInt 900000000000000
    , TkInt 600000000000000
    , TkInt 50000000000000
    , TkListLen 2
    , TkInt 0
    , TkTag 24
      -- This is the serialised form of exampleTxSizeLinear below
    , TkBytes "\130\ESC\NUL\NUL\141QuOR\NUL\ESC\NUL\NUL\NUL\n;b\190\128"
    , TkInteger 18446744073709551615
    , TkListBegin
    , TkBreak
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkMapLen 0
    , TkTag 258
    , TkListLen 0
    , TkMapLen 0
    ]
  where
    exampleUPIState = CC.UPI.initialState CC.dummyConfig

-- This gets embedded above, but it unclear because of the double serialisation.
test_golden_TxSizeLinear :: Assertion
test_golden_TxSizeLinear = goldenTestCBOR
    toCBOR
    exampleTxSizeLinear
    [ TkListLen 2
    , TkInt 155381000000000
    , TkInt 43946000000
    ]
  where
    exampleTxSizeLinear :: CC.TxSizeLinear
    exampleTxSizeLinear = CC.TxSizeLinear (CC.mkKnownLovelace @155381)
                                          (43.946 :: Rational)

test_golden_HeaderHash :: Assertion
test_golden_HeaderHash = goldenTestCBOR
    encodeByronHeaderHash
    exampleHeaderHash
    [ TkBytes "y\USBV\225Lg\185\ETX\\;\128\160\130j\223q\157\&66\193\142\239\SYN\201\139\132\184\&3r=Q"
    ]

test_golden_GenTx :: Assertion
test_golden_GenTx = goldenTestCBOR
    encodeByronGenTx
    getTx
    [ TkListLen 2
    , TkInt 0
    , TkListLen 2
    , TkListLen 3
    , TkListBegin
    , TkListLen 2
    , TkInt 0
    , TkTag 24
    , TkBytes "\130X K\168\&9\196 \179\210\189C\149\&0\248\145\202\233\165\212\196\216\DC2\EOTF0\218\199.\142\tb\254\238\204\CAN/"
    , TkBreak
    , TkListBegin
    , TkListLen 2
    , TkListLen 2
    , TkTag 24
    , TkBytes "\131X\FS\170Sr\tZ\170h\r\EM\212\202Ii\131\161Ep\156;\225\139\rL\131\203{\220^\160\NUL"
    , TkInt 853317774
    , TkInt 47
    , TkBreak
    , TkMapLen 0
    , TkListLen 1
    , TkListLen 2
    , TkInt 0
    , TkTag 24
    , TkBytes "\130X@Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kX@h\138\173\133{\199\255\&0\252hb\218\ESC\226\129\244 \198Rq\183j\177\151\130\255@\226\149Z\248\136\EM\195\142\\y\DC3\143(\a:\186\225R\200\130%\139D \160\193\201\253\210l\152\129&\151\252>\NUL"
    ]
  where
    getTx :: GenTx ByronBlock
    getTx = ByronTx CC.exampleTxId (CC.annotateTxAux CC.exampleTxAux)

test_golden_ApplyTxErr :: Assertion
test_golden_ApplyTxErr = goldenTestCBOR
    encodeByronApplyTxError
    exampleApplyTxErr
    [ TkListLen 2
    , TkInt 0
    , TkListLen 2
    , TkInt 0
    , TkListLen 3
    , TkInt 0
    , TkString "a"
    , TkListLen 2
    , TkInt 0
    , TkInt 0
    ]
  where
    exampleApplyTxErr =
        CC.MempoolTxErr
      $ CC.UTxOValidationTxValidationError
      $ CC.TxValidationLovelaceError "a"
      $ CC.LovelaceOverflow 0

test_golden_Query :: Assertion
test_golden_Query = goldenTestCBOR
    encodeByronQuery
    GetUpdateInterfaceState
    [ TkInt 0
    ]

test_golden_Result :: Assertion
test_golden_Result = goldenTestCBOR
    (encodeByronResult GetUpdateInterfaceState)
    state
    flatTerm
  where
    state :: CC.UPI.State
    state = CC.UPI.State
      { CC.UPI.currentEpoch                      = 0
      , CC.UPI.adoptedProtocolVersion            = CC.exampleProtocolVersion
      , CC.UPI.adoptedProtocolParameters         = CC.dummyProtocolParameters
      , CC.UPI.candidateProtocolUpdates          = [candidateProtocolUpdate]
      , CC.UPI.appVersions                       = applicationVersions
      , CC.UPI.registeredProtocolUpdateProposals = protocolUpdateProposals
      , CC.UPI.registeredSoftwareUpdateProposals = softwareUpdateProposals
      , CC.UPI.confirmedProposals                = Map.singleton CC.exampleUpId 0
      , CC.UPI.proposalVotes                     = Map.singleton CC.exampleUpId Set.empty
      , CC.UPI.registeredEndorsements            = Set.singleton endorsement
      , CC.UPI.proposalRegistrationSlot          = Map.singleton CC.exampleUpId 0
      }

    candidateProtocolUpdate :: CC.CandidateProtocolUpdate
    candidateProtocolUpdate = CC.CandidateProtocolUpdate
      { CC.cpuSlot               = 0
      , CC.cpuProtocolVersion    = CC.exampleProtocolVersion
      , CC.cpuProtocolParameters = CC.dummyProtocolParameters
      }

    applicationVersions :: CC.ApplicationVersions
    applicationVersions = Map.singleton
      (CC.ApplicationName "Golden-Test")
      (0, 0, Map.empty)

    protocolUpdateProposals :: CC.ProtocolUpdateProposals
    protocolUpdateProposals = Map.singleton
      CC.exampleUpId
      (CC.exampleProtocolVersion, CC.exampleProtocolParameters)

    softwareUpdateProposals :: CC.SoftwareUpdateProposals
    softwareUpdateProposals = Map.singleton
      CC.exampleUpId
      ( CC.exampleSoftwareVersion
      , Map.singleton CC.exampleSystemTag CC.exampleInstallerHash
      )

    endorsement :: CC.Endorsement
    endorsement = CC.Endorsement
      { CC.endorsementProtocolVersion = CC.exampleProtocolVersion
      , CC.endorsementKeyHash         = CC.exampleKeyHash
      }

    flatTerm :: FlatTerm
    flatTerm =
      [ TkListLen 11
      , TkInt 0
      , TkListLen 3
      , TkInt 1
      , TkInt 1
      , TkInt 1
      , TkListLen 14
      , TkInt 0
      , TkInt 7000
      , TkInt 2000000
      , TkInt 2000000
      , TkInt 8192
      , TkInt 700
      , TkInt 10000000000000
      , TkInt 5000000000000
      , TkInt 1000000000000
      , TkInt 100000000000000
      , TkInt 10
      , TkListLen 3
      , TkInt 900000000000000
      , TkInt 600000000000000
      , TkInt 50000000000000
      , TkListLen 2
      , TkInt 0
      , TkTag 24
      , TkBytes "\130\ESC\NUL\NUL\141QuOR\NUL\ESC\NUL\NUL\NUL\n;b\190\128"
      , TkInteger 18446744073709551615
      , TkListBegin
      , TkListLen 3
      , TkInt 0
      , TkListLen 3
      , TkInt 1
      , TkInt 1
      , TkInt 1
      , TkListLen 14
      , TkInt 0
      , TkInt 7000
      , TkInt 2000000
      , TkInt 2000000
      , TkInt 8192
      , TkInt 700
      , TkInt 10000000000000
      , TkInt 5000000000000
      , TkInt 1000000000000
      , TkInt 100000000000000
      , TkInt 10
      , TkListLen 3
      , TkInt 900000000000000
      , TkInt 600000000000000
      , TkInt 50000000000000
      , TkListLen 2
      , TkInt 0
      , TkTag 24
      , TkBytes "\130\ESC\NUL\NUL\141QuOR\NUL\ESC\NUL\NUL\NUL\n;b\190\128"
      , TkInteger 18446744073709551615
      , TkBreak
      , TkMapLen 1
      , TkString "Golden-Test"
      , TkListLen 3
      , TkInt 0
      , TkInt 0
      , TkMapLen 0
      , TkMapLen 1
      , TkBytes "r#\ESC\141\CAN\213_f7Y\FS-q\252G\164'\156\180f\250PY\210\235\188|\138\DC2 \250\236"
      , TkListLen 2
      , TkListLen 3
      , TkInt 1
      , TkInt 1
      , TkInt 1
      , TkListLen 14
      , TkInt 999
      , TkInt 999
      , TkInt 999
      , TkInt 999
      , TkInt 999
      , TkInt 999
      , TkInt 99
      , TkInt 99
      , TkInt 99
      , TkInt 99
      , TkInt 99
      , TkListLen 3
      , TkInt 99
      , TkInt 99
      , TkInt 99
      , TkListLen 2
      , TkInt 0
      , TkTag 24
      , TkBytes "\130\ESC\NUL\NUL\NUL\232\153\nF\NUL\ESC\NUL\NUL\NUL\DC1\237\142\194\NUL"
      , TkInt 99
      , TkMapLen 1
      , TkBytes "r#\ESC\141\CAN\213_f7Y\FS-q\252G\164'\156\180f\250PY\210\235\188|\138\DC2 \250\236"
      , TkListLen 2
      , TkListLen 2
      , TkString "Golden"
      , TkInt 99
      , TkMapLen 1
      , TkString "Kmyw4lDSE5S4fSH6"
      , TkListLen 4
      , TkBytes "\ETX\ETB\n.u\151\183\183\227\216L\ENQ9\GS\DC3\154b\177W\231\135\134\216\192\130\242\157\207L\DC1\DC3\DC4"
      , TkBytes "\206\192+A\165\194\&6\ACK\156\179\253?\166X\160\DC3\226\&0\135< \DC3\247\131\192P@7\220\133\149\135"
      , TkBytes "\ETX\ETB\n.u\151\183\183\227\216L\ENQ9\GS\DC3\154b\177W\231\135\134\216\192\130\242\157\207L\DC1\DC3\DC4"
      , TkBytes "\ETX\ETB\n.u\151\183\183\227\216L\ENQ9\GS\DC3\154b\177W\231\135\134\216\192\130\242\157\207L\DC1\DC3\DC4"
      , TkMapLen 1
      , TkBytes "r#\ESC\141\CAN\213_f7Y\FS-q\252G\164'\156\180f\250PY\210\235\188|\138\DC2 \250\236"
      , TkInt 0
      , TkMapLen 1
      , TkBytes "r#\ESC\141\CAN\213_f7Y\FS-q\252G\164'\156\180f\250PY\210\235\188|\138\DC2 \250\236"
      , TkTag 258
      , TkListLen 0
      , TkTag 258
      , TkListLen 1
      , TkListLen 2
      , TkListLen 3
      , TkInt 1
      , TkInt 1
      , TkInt 1
      , TkBytes "\180\192\224\129\n\140\212\245\185$\156A\144\a\EOT\179^\209\134 \\p\138@\181\171\172\144"
      , TkMapLen 1
      , TkBytes "r#\ESC\141\CAN\213_f7Y\FS-q\252G\164'\156\180f\250PY\210\235\188|\138\DC2 \250\236"
      , TkInt 0
      ]

-- | Check whether we can successfully decode the contents of the given file.
-- This file will typically contain an older serialisation format.
_goldenTestCBORBackwardsCompat
  :: (Eq a, Show a)
  => (forall s. Decoder s a)
  -> a
  -> FilePath
  -> Assertion
_goldenTestCBORBackwardsCompat dec a path = do
    bytes <- Lazy.readFile path
    case deserialiseFromBytes dec bytes of
      Left failure
        -> assertFailure (show failure)
      Right (leftover, a')
        | Lazy.null leftover
        -> a' @?= a
        | otherwise
        -> assertFailure $ "Left-over bytes: " <> show leftover
