{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
module Test.Consensus.Shelley.Ledger.Golden (
    tests
  , mkDummyHash
  ) where

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
    "2nidM76Ny8pviiZezND3D7MZ6y37Fx4j4bQetNkXysWGxrahrTaxxSnrgg4tkQfMLggA4gfLXd\
    \hB6zeiK4xPrkaa3yDFQFEfgvayoyAEZmLKsJYeLqoM2iHMnFysTYUVDL1qSNjfReV2mFYzcrs3\
    \46L2B47QG1x5g4Hk52PZA39SBPcvqXkHiw6DgtcYmdXAQW8JpW5zJqwKKnMTamfwvvKVTTqR1x\
    \shgn49xHV1vEMQ5ubGUZYYi6RF1XmaTsGcFPfT5D69Gdx91DbCzLLjD8JhPZivTghC6iwDJMhp\
    \dkC8bjRLWdP7xx52s4u44BDNiRcYrTVjXRVbB4bpjyr5GvtZKt8Muz5rfNrv3S3TynSZvQ3jhc\
    \FbdwK2uaJw2cYbSqRLm2SNdfxJir3Df5LwHnZ1Prqov254fRcCPRqnvAW5xeLzKEo"

test_golden_Header :: Assertion
test_golden_Header = goldenTestCBOR
    toCBOR
    (getHeader exampleBlock)
    "3FrQDCt2rUAPbDFJ66wa7NCrgaZARaZg9mcvdgwzS7XnVgHNGvcUdrrdEnPMeziiVVkZTvhtcE\
    \bPUVnAyq2dReKLtqgntvCR5Xf4p7d2BwwzLjtsBYmx8yuCgwVa6NZXCzdzB5nEoVTGR6hFTTGn\
    \7jYwG7qZSSbgZwtyacPyZj"

test_golden_HeaderHash :: Assertion
test_golden_HeaderHash = goldenTestCBOR
    toCBOR
    (blockHash exampleBlock)
    "8h2JEJf"

test_golden_GenTx :: Assertion
test_golden_GenTx = goldenTestCBOR
    toCBOR
    exampleGenTx
    "4Wnj4aL4oxhnRJbGmrWeygZGFARTVMN8QFaqSTu4JPMVdVLAQ2BojPHwt9yUSrUePYhHBMdv5x\
    \uWwMyVUBQYc1cu7Db57huJngYVaTDuiiSX2f1pQ3e4EUVzzR3BzdkWV58kyMMwEeukNSqguq6s\
    \iF3bawYuB2FJKtowVAjc8WMtLkhZLZsxRpbxsrpdA7wCXWQ1Ce33x6Acad54Yj5GcheEdBSohd\
    \hLDno58rGTwt2U87u2NrXZjPCbALWb5M5mUrHA3eAuqKxGjPb1k6stj5XPdTB8185qHsEhNG9i\
    \2yJR4MDHUbPkiA1QonqhU3R5W3hjxisiRQboxPD7nNNv5MssAdrrVTDgCrDhFmLL1Y2dW377gr\
    \BUD4DsRMMHJnWZcgN2GJ8GVaTRM5GUY74fTJKQa24FENPUu7ugf94th1uTmeZT15X23HZPgoh"

test_golden_GenTxId :: Assertion
test_golden_GenTxId = goldenTestCBOR
    toCBOR
    (txId exampleGenTx)
    "8kUeMTQ"

test_golden_ApplyTxErr :: Assertion
test_golden_ApplyTxErr = goldenTestCBOR
    toCBOR
    exampleApplyTxErr
    "2NRxKASh4"
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
    "Ymgf4xjnZfRRx8eyR1z1CCciUJPLPZwySJhVFwrELPsNLfjNWacKeaEKyFE49BLwjkbwVM4thd\
    \xNFi2mBhkLgxAX3xwMqzakSyEYuNbQ7wDA2D9q6vyE8RwM3eGhz1jS7fWv3MwVN9fwCdrUkKMs\
    \qbYHLynDwuuT95Nb4FLL4i7utVV1HQBH7dq7Qn51npCVsXQwJwsTC2M6RD7j6VyhzHpCNDTWo4\
    \wCgKd9ezH234wRxbQ32uB4FFXnz5iK46b9MhqHokspn2qLFF7evyWqocGN748j5GmhNpbvJf61\
    \1J9skUcPNFkkibNjPsKoivcwtrzXF4YKLiFKGH7CZugxJ3TqZKmMW64FxhrNmKxpehkwEeWtA4\
    \UftFwGN9qGp"
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
    "BdWSwUUCJeRwZ385owJZcogJVLPA36X5DAi5vV9wwrULTURpd16N7AHfCvkEseTPgKL6xndfa4\
    \ocw188C3FamenBggc15HuYfSj4cz3FWgYWaYvRz56TabWzT8of8mNr8pWEDeSsSfR4Q1YMwZEQ\
    \vkMD58zMoTsvVBJpKqJbvNA768w1HPVjhALNt1gy6tY47jnSLKomNm7oBMEP5Fa4q7QkQPrhw8\
    \VkpfKcukMZU8rEMP7FmjM4hpEhY4wyfiHeivSrPSZPvQjQF9a6EaxaZ2rnTdT7cm1DDo3q7KHq\
    \boRLBVh9dA6ahcMQHdEJug9gEdaC2jTvWfe6d8iPG3H6cXme9e7oYrFrMyLxCMHUJqQu8L6LrE\
    \wEGLChhY8N1b11SEBd1vRDpXHKEGWqpQPEG62nbR9QE4ggSJD7h1cxhW4Razj3UTVUPox1mrH3\
    \JLAP2cpjVGjJaoXsWgrXMjVKcXKTBGVKwNCfZg9eZPadfYgX6WnbueQLMKVpFHtkMpswjYteVG\
    \E6EtPaCaTrsq1mHa1wEbEvKCE3B3UWYBy5cUA2Bg2AbH9kKj5YsUdK3NfrwKRwKMPhh4CsgpLt\
    \nuEHDqKnj3uK2nKoMQtsxZ3BU9dMMP33rFwLbSyPciybJ5tiystSqzFz9fXZ4aZYcwFyvJefxZ\
    \t1YhXF8xA9fCXypAWeZu3WDHPnazYdbWsoZxf3Q21tWk4WYGy8TEoiXziYCisk9J9P2SqXLbkJ\
    \9ztRbrJN9NXoEC5PG9gswjgyqfJ4N5n48WXVZPRrdzRk4G1eNErkur3Ya3T9RQetfxiQfuRbDD\
    \AibN5ZAUw3bAtuttNUS82gikKjvtz812Apg3UU5iJmtv3nrHrBTSmsdwjYGz4LDX4SRWsMDxCK\
    \KAHrAG8qkcgsTr8xykyWiebEQvVxAgW2ch3rj9ixJsC3Mevf9zhCW9p5LweKmWV8pnG7fWQ3jX\
    \nh7P31GhZQtFmyqditLPjXUVXo64G6V2f1WThSMG7s8byAQKfn3SW67kWktGj1BgkQAiYo7z2q\
    \W2XrAWY8YgFRa6AoL4LmnHn635gn8xf1ut5bnPm38QX3B1unDKKFeiwwGcnXXdRR2k6VtBZpoL\
    \7iyeMhhtufn2AtzNKCqXaQjT8hXzmyVfPCEkoJ7gKWb14yLFpWpPvdWRh39HZNn7WCyToTzwCW\
    \cfT8hMuVsthMbqPS9SvGafVdP5sbSgzX5RGcEiJabPd9M8w5hmztXmvYnDi"
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
