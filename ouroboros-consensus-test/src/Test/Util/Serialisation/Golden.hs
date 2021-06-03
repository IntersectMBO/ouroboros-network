{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Serialisation.Golden (
    Examples (..)
  , Labelled
  , ToGoldenDirectory (..)
  , combineExamples
  , goldenTest_SerialiseDisk
  , goldenTest_SerialiseNodeToClient
  , goldenTest_SerialiseNodeToNode
  , goldenTest_all
  , labelled
  , mapExamples
  , prefixExamples
  , unlabelled
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.FlatTerm (FlatTerm, TermToken (..))
import qualified Codec.CBOR.FlatTerm as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (encode)
import           Control.Exception (SomeException, evaluate, try)
import           Data.Bifunctor (first)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as BS.UTF8
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.TreeDiff (Expr (..), ToExpr (..), ansiWlEditExpr, ediff)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory, (</>))

import           Cardano.Prelude (forceElemsToWHNF)

import           Ouroboros.Network.Block (Serialised)

import           Ouroboros.Consensus.Block (BlockProtocol, CodecConfig, Header,
                     HeaderHash, SomeSecond)
import           Ouroboros.Consensus.HeaderValidation (AnnTip)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState,
                     encodeExtLedgerState)
import           Ouroboros.Consensus.Ledger.Query (BlockQuery, QueryVersion,
                     nodeToClientVersionToQueryVersion)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (HasNetworkProtocolVersion (..),
                     SupportedNetworkProtocolVersion (..))
import           Ouroboros.Consensus.Node.Run (SerialiseDiskConstraints,
                     SerialiseNodeToClientConstraints,
                     SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseNodeToNode (..),
                     SerialiseResult (..))
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.Storage.Serialisation (EncodeDisk (..),
                     SerialisedHeader)
import           Ouroboros.Consensus.Util.CBOR (decodeAsFlatTerm)
import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Test.Tasty
import           Test.Tasty.Golden.Advanced (goldenTest)

import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

{-------------------------------------------------------------------------------
  Golden test
-------------------------------------------------------------------------------}

-- | Golden test for CBOR output. When the output doesn't match the golden
-- one, we show an 'ediff' of the 'FlatTerm' output of both.
--
-- Exceptions: when forcing an encoding throws an exception, we 'show' the
-- exception and use that as the output.
goldenTestCBOR ::
     TestName
  -> a
  -> (a -> Encoding)
  -> FilePath -- ^ Path to the file containing the golden output
  -> TestTree
goldenTestCBOR testName example enc goldenFile =
    goldenTest
      testName
      (Strict.readFile goldenFile)
      (either exceptionToByteString id <$> try (evaluate actualValue))
      diff
      updateGoldenFile
  where
    -- Copied from tasty-golden because it isn't exported
    updateGoldenFile :: Strict.ByteString -> IO ()
    updateGoldenFile bytes = do
        let dir = takeDirectory goldenFile
        createDirectoryIfMissing True dir
        Strict.writeFile goldenFile bytes

    actualValue :: Strict.ByteString
    actualValue = CBOR.toStrictByteString (enc example)

    exceptionToByteString :: SomeException -> Strict.ByteString
    exceptionToByteString = BS.UTF8.fromString . show

    -- | Use 'ediff' ('ToExpr') to diff the 'FlatTerm' representation.
    diff :: Strict.ByteString -> Strict.ByteString -> IO (Maybe String)
    diff golden actual = do
        actualRes <- fmap (first (\(e :: SomeException) -> e))
                   . try
                   . evaluate
                   . forceElemsToWHNF
                   . CBOR.toFlatTerm
                   . enc
                   $ example
        return $ case (actualRes, decodeAsFlatTerm golden) of
          (Left e, Right goldenFlatTerm)
              -- Encoder threw an exception and the golden output was valid
              -- CBOR. However, sometimes the 'show'n exception is also valid
              -- CBOR. So if the exception and the golden output match, the test
              -- passes.
            | exceptionToByteString e == golden -> Nothing
            | otherwise -> Just $ unlines [
                "Exception thrown by encoder doesn't match the golden CBOR output"
              , "Exception:"
              , show e
              , "Golden term:"
              , condense goldenFlatTerm
              ]

          (Left e, Left _)
              -- Encoder threw an exception. The golden output was not valid
              -- CBOR and the bytestrings match: we expected the exception
            | exceptionToByteString e == golden -> Nothing
            | otherwise -> Just $ unlines [
                "Exception thrown by encoder doesn't match the golden output"
              , "Exception:"
              , show e
              , "Golden output:"
              , BS.UTF8.toString golden
              ]

          (Right actualFlatTerm, Right goldenFlatTerm)
            | actual == golden -> Nothing
            | otherwise -> Just $ unlines [
                "Golden term /= actual term, diff golden actual:"
              , showFlatTermDiff goldenFlatTerm actualFlatTerm
              ]

          (Right actualFlatTerm, Left _) -> Just $ unlines [
                "Golden output /= actual term:"
              , "Golden output is not valid CBOR:"
              , BS.UTF8.toString golden
              , "Actual term:"
              , condense actualFlatTerm
              ]

goldenTests ::
     HasCallStack
  => TestName
  -> Labelled a
  -> (a -> Encoding)
  -> FilePath  -- ^ Folder containing the golden files
  -> TestTree
goldenTests testName examples enc goldenFolder
  | nub labels /= labels
  = error $ "Examples with the same label for " <> testName
  | [(Nothing, example)] <- examples
    -- If there's just a single unlabelled example, no need for grouping,
    -- which makes the output more verbose.
  = goldenTestCBOR testName example enc (goldenFolder </> testName)
  | otherwise
  = testGroup testName [
        goldenTestCBOR testName' example enc (goldenFolder </> testName')
      | (mbLabel, example) <- examples
      , let testName' = case mbLabel of
              Nothing    -> testName
              Just label -> testName <> "_" <> label
      ]
  where
    labels :: [Maybe String]
    labels = map fst examples

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

type Labelled a = [(Maybe String, a)]

unlabelled :: a -> Labelled a
unlabelled x = [(Nothing, x)]

labelled :: [(String, a)] -> Labelled a
labelled = map (first Just)

data Examples blk = Examples {
      exampleBlock            :: Labelled blk
    , exampleSerialisedBlock  :: Labelled (Serialised blk)
    , exampleHeader           :: Labelled (Header blk)
    , exampleSerialisedHeader :: Labelled (SerialisedHeader blk)
    , exampleHeaderHash       :: Labelled (HeaderHash blk)
    , exampleGenTx            :: Labelled (GenTx blk)
    , exampleGenTxId          :: Labelled (GenTxId blk)
    , exampleApplyTxErr       :: Labelled (ApplyTxErr blk)
    , exampleQuery            :: Labelled (SomeSecond BlockQuery blk)
    , exampleResult           :: Labelled (SomeResult blk)
    , exampleAnnTip           :: Labelled (AnnTip blk)
    , exampleLedgerState      :: Labelled (LedgerState blk)
    , exampleChainDepState    :: Labelled (ChainDepState (BlockProtocol blk))
    , exampleExtLedgerState   :: Labelled (ExtLedgerState blk)
    }

emptyExamples :: Examples blk
emptyExamples = Examples {
      exampleBlock            = mempty
    , exampleSerialisedBlock  = mempty
    , exampleHeader           = mempty
    , exampleSerialisedHeader = mempty
    , exampleHeaderHash       = mempty
    , exampleGenTx            = mempty
    , exampleGenTxId          = mempty
    , exampleApplyTxErr       = mempty
    , exampleQuery            = mempty
    , exampleResult           = mempty
    , exampleAnnTip           = mempty
    , exampleLedgerState      = mempty
    , exampleChainDepState    = mempty
    , exampleExtLedgerState   = mempty
    }

combineExamples ::
     forall blk.
     (forall a. Labelled a -> Labelled a -> Labelled a)
  -> Examples blk
  -> Examples blk
  -> Examples blk
combineExamples f e1 e2 = Examples {
      exampleBlock            = combine exampleBlock
    , exampleSerialisedBlock  = combine exampleSerialisedBlock
    , exampleHeader           = combine exampleHeader
    , exampleSerialisedHeader = combine exampleSerialisedHeader
    , exampleHeaderHash       = combine exampleHeaderHash
    , exampleGenTx            = combine exampleGenTx
    , exampleGenTxId          = combine exampleGenTxId
    , exampleApplyTxErr       = combine exampleApplyTxErr
    , exampleQuery            = combine exampleQuery
    , exampleResult           = combine exampleResult
    , exampleAnnTip           = combine exampleAnnTip
    , exampleLedgerState      = combine exampleLedgerState
    , exampleChainDepState    = combine exampleChainDepState
    , exampleExtLedgerState   = combine exampleExtLedgerState
    }
  where
    combine :: (Examples blk -> Labelled a) -> Labelled a
    combine getField = f (getField e1) (getField e2)

instance Semigroup (Examples blk) where
  (<>) = combineExamples (<>)

instance Monoid (Examples blk) where
  mempty  = emptyExamples
  mappend = (<>)

mapExamples ::
     forall blk.
     (forall a. Labelled a -> Labelled a)
  -> Examples blk
  -> Examples blk
mapExamples f = combineExamples (const f) mempty

-- | Add the given prefix to each labelled example.
--
-- When a label is empty, the prefix is used as the label. If the label is not
-- empty, the prefix and @_@ are prepended.
prefixExamples :: String -> Examples blk -> Examples blk
prefixExamples prefix = mapExamples addPrefix
  where
    addPrefix :: Labelled a -> Labelled a
    addPrefix l = [
          (Just label, x)
        | (mbLabel, x) <- l
        , let label = case mbLabel of
                Nothing  -> prefix
                Just lbl -> prefix <> "_" <> lbl
        ]

{-------------------------------------------------------------------------------
  Skeletons
-------------------------------------------------------------------------------}

-- | Convert 'a' to a 'FilePath' that can be used as the directory containing
-- golden output files.
--
-- This class allows overriding the 'Show' in cases where that output is not
-- suitable to be used as a directory.
--
-- For example, the 'Show' output for a hard fork enabled 'NodeToNodeVersion'
-- will contain colons, asterisks, spaces, parentheses, ... and other
-- characters that we don't want to use for a directory name. For instance
-- colons cannot be used in Windows file/folder names.
class ToGoldenDirectory a where
  toGoldenDirectory :: a -> FilePath

  default toGoldenDirectory :: Show a => a -> FilePath
  toGoldenDirectory = show

-- | Golden tests for all things we serialise to disk and send across the
-- network.
--
-- Exceptions: when an encoder throws an exception, which can happen when
-- serialising a Shelley header in combination with
-- 'CardanoNodeToNodeVersion1', we 'show' the exception and use that as the
-- output.
goldenTest_all ::
     ( SerialiseDiskConstraints         blk
     , SerialiseNodeToNodeConstraints   blk
     , SerialiseNodeToClientConstraints blk
     , SupportedNetworkProtocolVersion  blk

     , ToGoldenDirectory (BlockNodeToNodeVersion   blk)
     , ToGoldenDirectory (QueryVersion, BlockNodeToClientVersion blk)

     , HasCallStack
     )
  => CodecConfig blk
  -> FilePath
     -- ^ Path relative to the root of the repository that contains the golden
     -- files
  -> Examples blk
  -> TestTree
goldenTest_all codecConfig goldenDir examples =
    testGroup "Golden tests" [
        goldenTest_SerialiseDisk         codecConfig goldenDir examples
      , goldenTest_SerialiseNodeToNode   codecConfig goldenDir examples
      , goldenTest_SerialiseNodeToClient codecConfig goldenDir examples
      ]

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseDiskConstraints'?
goldenTest_SerialiseDisk ::
     forall blk. (SerialiseDiskConstraints blk, HasCallStack)
  => CodecConfig blk
  -> FilePath
  -> Examples blk
  -> TestTree
goldenTest_SerialiseDisk codecConfig goldenDir Examples {..} =
    testGroup "SerialiseDisk" [
        test "Block"          exampleBlock         (encodeDisk codecConfig)
      , test "HeaderHash"     exampleHeaderHash     encode
      , test "LedgerState"    exampleLedgerState   (encodeDisk codecConfig)
      , test "AnnTip"         exampleAnnTip        (encodeDisk codecConfig)
      , test "ChainDepState"  exampleChainDepState (encodeDisk codecConfig)
      , test "ExtLedgerState" exampleExtLedgerState encodeExt
      ]
  where
    test :: TestName -> Labelled a -> (a -> Encoding) -> TestTree
    test testName exampleValues enc =
        goldenTests
          testName
          exampleValues
          enc
          (goldenDir </> "disk")

    encodeExt =
        encodeExtLedgerState
          (encodeDisk codecConfig)
          (encodeDisk codecConfig)
          (encodeDisk codecConfig)

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseNodeToNodeConstraints'?
goldenTest_SerialiseNodeToNode ::
     forall blk.
     ( SerialiseNodeToNodeConstraints blk
     , SupportedNetworkProtocolVersion blk
     , ToGoldenDirectory (BlockNodeToNodeVersion blk)
     , HasCallStack
     )
  => CodecConfig blk
  -> FilePath
  -> Examples blk
  -> TestTree
goldenTest_SerialiseNodeToNode codecConfig goldenDir Examples {..} =
    testGroup "SerialiseNodeToNode" [
        testVersion version
      | version <- nub $ Map.elems $ supportedNodeToNodeVersions $ Proxy @blk
      ]
  where
    testVersion :: BlockNodeToNodeVersion blk -> TestTree
    testVersion version = testGroup (toGoldenDirectory version) [
          test "Block"            exampleBlock
        , test "Header"           exampleHeader
        , test "SerialisedBlock"  exampleSerialisedBlock
        , test "SerialisedHeader" exampleSerialisedHeader
        , test "GenTx"            exampleGenTx
        , test "GenTxId"          exampleGenTxId
        ]
      where
        test :: SerialiseNodeToNode blk a => TestName -> Labelled a -> TestTree
        test testName exampleValues =
            goldenTests
              testName
              exampleValues
              (encodeNodeToNode codecConfig version)
              (goldenDir </> toGoldenDirectory version)

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseNodeToClientConstraints'?
goldenTest_SerialiseNodeToClient ::
     forall blk.
     ( SerialiseNodeToClientConstraints blk
     , SupportedNetworkProtocolVersion blk
     , ToGoldenDirectory (QueryVersion, BlockNodeToClientVersion blk)
     , HasCallStack
     )
  => CodecConfig blk
  -> FilePath
  -> Examples blk
  -> TestTree
goldenTest_SerialiseNodeToClient codecConfig goldenDir Examples {..} =
    testGroup "SerialiseNodeToClient" [
        testVersion (nodeToClientVersionToQueryVersion version, blockVersion)
      | (version, blockVersion) <- nub $ Map.toList $ supportedNodeToClientVersions $ Proxy @blk
      ]
  where
    testVersion :: (QueryVersion, BlockNodeToClientVersion blk) -> TestTree
    testVersion versions@(_, blockVersion) = testGroup (toGoldenDirectory versions) [
          test "Block"           exampleBlock           enc'
        , test "SerialisedBlock" exampleSerialisedBlock enc'
        , test "GenTx"           exampleGenTx           enc'
        , test "ApplyTxErr"      exampleApplyTxErr      enc'
        , test "Query"           exampleQuery           enc'
        , test "Result"          exampleResult          encRes
        ]
      where

        enc' :: SerialiseNodeToClient blk a => a -> Encoding
        enc' = encodeNodeToClient codecConfig blockVersion

        encRes :: SomeResult blk -> Encoding
        encRes (SomeResult q r) = encodeResult codecConfig blockVersion q r

        test :: TestName -> Labelled a -> (a -> Encoding) -> TestTree
        test testName exampleValues enc =
            goldenTests
              testName
              exampleValues
              enc
              (goldenDir </> toGoldenDirectory versions)

{-------------------------------------------------------------------------------
  FlatTerm
-------------------------------------------------------------------------------}

deriving instance Generic TermToken
deriving instance ToExpr  TermToken

instance Condense TermToken where
  condense = show

{-------------------------------------------------------------------------------
  Diffing Cbor
-------------------------------------------------------------------------------}

data CborTree =
    FlatTerm   TermToken
  | NestedTerm CborForest

type CborForest = [CborTree]

instance ToExpr CborTree where
  toExpr (FlatTerm term)     = toExpr term
  toExpr (NestedTerm forest) = App "CBOR-in-CBOR" [Lst (map toExpr forest)]

flatTermToCborForest :: FlatTerm -> Either CBOR.DeserialiseFailure CborForest
flatTermToCborForest = go
  where
    go :: FlatTerm -> Either CBOR.DeserialiseFailure CborForest
    go []                              = return []
    go (TkTag 24 : TkBytes bytes : ts) = do
        nestedCBOR <- decodeAsFlatTerm bytes
        t' <- go nestedCBOR
        (NestedTerm t' :) <$> go ts
    go (t : ts) = (FlatTerm t :) <$> go ts

-- | Shows the diff between two 'FlatTerm's as a 'String' using
-- "Data.TreeDiff".
--
-- Handles CBOR-in-CBOR.
showFlatTermDiff :: FlatTerm -> FlatTerm -> String
showFlatTermDiff a b
    | Right a' <- flatTermToCborForest a
    , Right b' <- flatTermToCborForest b
    = diff a' b'
    | otherwise
    = diff a b
  where
    diff :: ToExpr a => a -> a -> String
    diff x y = show (ansiWlEditExpr (ediff x y))
