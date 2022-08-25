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


testSynthOptions :: DBSynthesizerOptions
testSynthOptions =
    DBSynthesizerOptions {
        synthLimit          = ForgeLimitEpoch 1
      , synthForceDBRemoval = True
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
    , confLimit   = Unlimited
    }

-- | A multi-step test including synthesis and analaysis 'SomeConsensusProtocol' using the Cardano instance.
--
-- 1. step: synthesize a ChainDB and counts the amount of blocks forged in the proces.
-- 2. step: analyze the ChainDB from previous step and confirm the block count.

--
blockCountTest :: (String -> IO ()) -> Assertion
blockCountTest logStep = do
    logStep "intializing synthesis"
    (protocol, options) <- either assertFailure pure
        =<< DBSynthesizer.initialize
          testNodeFilePaths
          testNodeCredentials
          testSynthOptions

    logStep "running synthesis"
    resultSynth <- DBSynthesizer.synthesize protocol options
    let blockCount = resultForged resultSynth
    blockCount > 0 @? "no blocks have been forged"

    logStep "running analysis"
    resultAnalysis <- case blockType testAnalyserConfig of
        CardanoBlock b  -> DBAnalyser.analyse testAnalyserConfig b
        _               -> assertFailure "expexcting test case for Cardano block type"

    resultAnalysis == Just (ResultCountBlock blockCount) @?
        "wrong number of blocks encountered during analysis \
        \ (counted: " ++ show resultAnalysis ++ "; expected: " ++ show blockCount ++ ")"

tests :: TestTree
tests =
    testGroup "cardano-tools"
      [ testCaseSteps "synthesize and analyse: blockCount" blockCountTest
      ]

main :: IO ()
main = defaultMain tests

-- Counter to address the zfs copy bug on Hydra
-- ```
-- cannot execute binary file: Exec format error
-- ```
--
-- 0
