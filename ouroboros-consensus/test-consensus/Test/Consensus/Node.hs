{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Consensus.Node (tests) where

import qualified Codec.CBOR.Write as CBOR
import           Data.Bifunctor (second)
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map

import           Control.Monad.IOSim (runSimOrThrow)

import           Cardano.Binary (toCBOR)
import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Consensus.Node (ChainDbCheckError (..),
                     checkProtocolMagicId, protocolMagicIdFile)

import           Ouroboros.Storage.FS.API.Types
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.Util.FS.Sim.FsTree (FsTree (..))
import           Test.Util.FS.Sim.MockFS (Files)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (runSimFS)

tests :: TestTree
tests = testGroup "Node"
    [ testGroup "checkProtocolMagicId"
      [ testCase "match"        test_checkProtocolMagicId_match
      , testCase "mismatch"     test_checkProtocolMagicId_mismatch
      , testCase "empty folder" test_checkProtocolMagicId_empty_folder
      , testCase "missing"      test_checkProtocolMagicId_missing
      , testCase "corrupt"      test_checkProtocolMagicId_corrupt
      , testCase "empty"        test_checkProtocolMagicId_empty
      ]
    ]

{-------------------------------------------------------------------------------
  checkProtocolMagicId
-------------------------------------------------------------------------------}

expectedProtocolMagicId :: ProtocolMagicId
expectedProtocolMagicId = ProtocolMagicId 1910

mountPoint :: MountPoint
mountPoint = MountPoint "root"

fullPath :: FilePath
fullPath = fsToFilePath
    mountPoint (fsPathFromList [protocolMagicIdFile])

runCheck :: Files -> (Either ChainDbCheckError (), Files)
runCheck files = runSimOrThrow $ do
    fmap (second Mock.mockFiles) $
      runSimFS EH.monadCatch Mock.empty { Mock.mockFiles = files } $ \hasFS ->
        checkProtocolMagicId hasFS mountPoint expectedProtocolMagicId

protocolMagicIdToBS :: ProtocolMagicId -> ByteString
protocolMagicIdToBS = CBOR.toStrictByteString . toCBOR

test_checkProtocolMagicId_match :: Assertion
test_checkProtocolMagicId_match = res @?= Right ()
  where
    fs = Folder $ Map.fromList
      [ (protocolMagicIdFile, File $ protocolMagicIdToBS expectedProtocolMagicId)
      , ("immutable",         Folder mempty)
      , ("ledger",            Folder mempty)
      , ("volatile",          Folder mempty)
      ]
    (res, _) = runCheck fs

test_checkProtocolMagicId_mismatch :: Assertion
test_checkProtocolMagicId_mismatch = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ (protocolMagicIdFile, File $ protocolMagicIdToBS actual)
      , ("immutable",         Folder mempty)
      , ("ledger",            Folder mempty)
      , ("volatile",          Folder mempty)
      ]
    (res, _) = runCheck fs
    actual = ProtocolMagicId 10
    e = ProtocolMagicIdMismatch
      fullPath
      actual
      expectedProtocolMagicId

test_checkProtocolMagicId_empty_folder :: Assertion
test_checkProtocolMagicId_empty_folder = do
    res @?= Right ()
    expectedFs' @?= fs'
  where
    fs = Folder mempty
    (res, fs') = runCheck fs
    expectedFs' = Folder $ Map.fromList
      [ (protocolMagicIdFile, File $ protocolMagicIdToBS expectedProtocolMagicId) ]

test_checkProtocolMagicId_missing :: Assertion
test_checkProtocolMagicId_missing = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ ("passwords.txt", File "qwerty\n123456\n")
      ]
    (res, _) = runCheck fs
    e = NoProtocolMagicIdAndNotEmpty fullPath

test_checkProtocolMagicId_corrupt :: Assertion
test_checkProtocolMagicId_corrupt = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ (protocolMagicIdFile, File "garbage")
      , ("immutable",         Folder mempty)
      , ("ledger",            Folder mempty)
      , ("volatile",          Folder mempty)
      ]
    (res, _) = runCheck fs
    e = CorruptProtocolMagicId fullPath

test_checkProtocolMagicId_empty :: Assertion
test_checkProtocolMagicId_empty = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ (protocolMagicIdFile, File "")
      , ("immutable",         Folder mempty)
      , ("ledger",            Folder mempty)
      , ("volatile",          Folder mempty)
      ]
    (res, _) = runCheck fs
    e = CorruptProtocolMagicId fullPath
