module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import           Cardano.Tools.DBSynthesizer.Types

import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import qualified Cardano.Tools.DBAnalyser.Run as DBAnalyser
import           Cardano.Tools.DBAnalyser.Types


nodeConfig, chainDB :: FilePath
nodeConfig  = "test/disk/config/config.json"
chainDB     = "test/disk/chaindb"


testSynthOptionsCreate :: DBSynthesizerOptions
testSynthOptionsCreate =
    DBSynthesizerOptions {
        synthLimit          = ForgeLimitEpoch 1
      , synthOpenMode       = OpenCreateForce
    }

testSynthOptionsAppend :: DBSynthesizerOptions
testSynthOptionsAppend =
    DBSynthesizerOptions {
        synthLimit          = ForgeLimitSlot 8192
      , synthOpenMode       = OpenAppend
    }

testNodeFilePaths :: NodeFilePaths
testNodeFilePaths =
    NodeFilePaths {
        nfpConfig   = nodeConfig
      , nfpChainDB  = chainDB
    }

testNodeCredentials :: NodeCredentials
testNodeCredentials =
    NodeCredentials {
        credCertFile  = Nothing
      , credVRFFile   = Nothing
      , credKESFile   = Nothing
      , credBulkFile  = Just "test/disk/config/bulk-creds-k2.json"
    }

testAnalyserConfig :: DBAnalyserConfig
testAnalyserConfig =
  DBAnalyserConfig {
      dbDir       = chainDB
    , verbose     = False
    , selectDB    = SelectChainDB
    , validation  = Just ValidateAllBlocks
    , blockType   = CardanoBlock (Cardano.CardanoBlockArgs nodeConfig Nothing)
    , analysis    = CountBlocks
    , cfgLimit    = Nothing
    , bsSelector  = MEM
    }

-- | A multi-step test including synthesis and analaysis 'SomeConsensusProtocol' using the Cardano instance.
--
-- 1. step: synthesize a ChainDB from scratch and count the amount of blocks forged.
-- 2. step: append to the previous ChainDB and coutn the amount of blocks forged.
-- 3. step: analyze the ChainDB resulting from previous steps and confirm the total block count.

--
blockCountTest :: (String -> IO ()) -> Assertion
blockCountTest logStep = do
    logStep "running synthesis - create"
    (options, protocol) <- either assertFailure pure
        =<< DBSynthesizer.initialize
          testNodeFilePaths
          testNodeCredentials
          testSynthOptionsCreate
    resultCreate <- DBSynthesizer.synthesize options protocol
    let blockCountCreate = resultForged resultCreate
    blockCountCreate > 0 @? "no blocks have been forged during create step"

    logStep "running synthesis - append"
    resultAppend <- DBSynthesizer.synthesize options {confOptions = testSynthOptionsAppend} protocol
    let blockCountAppend = resultForged resultAppend
    blockCountAppend > 0 @? "no blocks have been forged during append step"

    logStep "running analysis"
    resultAnalysis <- case blockType testAnalyserConfig of
        CardanoBlock b  -> DBAnalyser.analyse testAnalyserConfig b
        _               -> assertFailure "expexcting test case for Cardano block type"

    let blockCount = blockCountCreate + blockCountAppend
    resultAnalysis == Just (ResultCountBlock blockCount) @?
        "wrong number of blocks encountered during analysis \
        \ (counted: " ++ show resultAnalysis ++ "; expected: " ++ show blockCount ++ ")"

tests :: TestTree
tests =
    testGroup "cardano-tools"
      [ testCaseSteps "synthesize and analyse: blockCount\n" blockCountTest
      ]

main :: IO ()
main = defaultMain tests

-- Counter to address the zfs copy bug on Hydra
-- ```
-- cannot execute binary file: Exec format error
-- ```
--
-- 3
