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

multiStepTest :: (String -> IO ()) -> Assertion
multiStepTest logStep = do
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

    resultAnalysis == Just (ResultCountBlock blockCount) @? "wrong number of blocks counted during analysis"

tests :: TestTree
tests =
    testGroup "cardano-tools"
      [ testCaseSteps "synthesize and analyse" multiStepTest
      ]

main :: IO ()
main = defaultMain tests
