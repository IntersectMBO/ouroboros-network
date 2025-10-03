{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT (..), runExceptT)

import Codec.CBOR.Read qualified as CBOR
-- import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as CBOR
-- import Codec.CBOR.Write qualified as CBOR
-- import Codec.Serialise.Class (Serialise)
-- import Codec.Serialise.Class qualified as Serialise
import Codec.Serialise.Decoding qualified as CBOR
-- import Codec.Serialise.Encoding qualified as CBOR

import Data.Bool (bool)
import Data.ByteString.Base16.Lazy as BL.Base16
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL.Char8

import System.Directory (doesDirectoryExist)
import System.Environment (setEnv)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.Process.ByteString.Lazy

import Cardano.KESAgent.Protocols.StandardCrypto (StandardCrypto)

import DMQ.Protocol.SigSubmission.Codec
import DMQ.Protocol.SigSubmission.Type

-- import Test.QuickCheck hiding (Result (..))
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (QuickCheckMaxSize (..))

main :: IO ()
main = do
  cddlSpecs <- readCDDLSpecs
  defaultMain (tests cddlSpecs)

tests :: CDDLSpecs -> TestTree
tests CDDLSpecs { cddlSig
                } =
  adjustOption (const $ QuickCheckMaxSize 10) $
  testGroup "cddl"
    [ testGroup "decoding"
      -- validate decoder by generating messages from the specification
      [ testCase "Sig" (unit_decodeSig cddlSig)
      ]
    ]

newtype CDDLSpec ps = CDDLSpec BL.ByteString

type AnnSigRawWithSignedBytes = BL.ByteString -> SigRawWithSignedBytes StandardCrypto

data CDDLSpecs = CDDLSpecs {
    cddlSig :: CDDLSpec AnnSigRawWithSignedBytes
  }


unit_decodeSig :: CDDLSpec AnnSigRawWithSignedBytes
               -> Assertion
unit_decodeSig spec = validateDecoder spec decodeSig 100


--
-- utils
--
-- TODO: move the utils function to a common library to be shared with other
-- cddl tests.  They need to be slightly generalised to do so.
--

-- | Use `cddlc` to resolve module directives (`;# include` and `;# import`).
--
-- The `CDDL_INCLUDE_PATH` environment variable must be set.
cddlc :: FilePath -> IO BL.ByteString
cddlc path = do
  (_, cddl, _) <- readProcessWithExitCode "cddlc" ["-u", "-2", "-t", "cddl", path] mempty
  return cddl


readCDDLSpecs :: IO CDDLSpecs
readCDDLSpecs = do
    dir <- bool (               "cddl" </> "specs") -- False
                ("dmq-node" </> "cddl" </> "specs") -- True
       <$> doesDirectoryExist "dmq-node"
    setEnv "CDDL_INCLUDE_PATH" (dir <> ":")

    sigSpec <- cddlc (dir </> "sig.cddl")

    return CDDLSpecs {
        cddlSig = CDDLSpec sigSpec
      }


validateDecoder :: CDDLSpec a
                -> (forall s. CBOR.Decoder s a)
                -> Int
                -> Assertion
validateDecoder (CDDLSpec spec) decoder rounds = do
  eterms <- runExceptT $ generateCBORFromSpec spec rounds
  case eterms of
    Left err -> assertFailure err
    Right terms ->
      forM_ terms $ \(generated_term, encoded_term) -> do
        let Right (_, decoded_term) = CBOR.deserialiseFromBytes CBOR.decodeTerm encoded_term
            res = CBOR.deserialiseFromBytes decoder encoded_term
        case res of
          Left err -> assertFailure $ concat
            [ "decoding failure:\n"
            , show err
            , "\nwhile decoding:\n"
            , show decoded_term
            , "\ngenerated term:\n"
            , BL.Char8.unpack generated_term
            , "\nencoded term:\n"
            , BL.Char8.unpack (BL.Base16.encode encoded_term)
            ]
          Right _ -> return ()


generateCBORFromSpec :: BL.ByteString
                     -> Int
                     -> ExceptT String IO [(BL.ByteString, BL.ByteString)]
generateCBORFromSpec spec rounds = do
    terms <-
      ExceptT $ withTemporaryFile spec $ \filePath ->
        unpackResult $
          readProcessWithExitCode
            "cddl"
            [filePath, "generate", show rounds]
            BL.empty
    traverse (\bs -> (bs,) <$> diagToBytes bs) (BL.Char8.lines terms)
  where
    diagToBytes :: BL.ByteString -> ExceptT String IO BL.ByteString
    diagToBytes = ExceptT
                . unpackResult
                . readProcessWithExitCode "diag2cbor.rb" ["-"]


    unpackResult :: IO (ExitCode, BL.ByteString, BL.ByteString)
                 -> IO (Either String BL.ByteString)
    unpackResult r = r >>= \case
        (ExitFailure _, _, err) -> return (Left $ BL.Char8.unpack err)
        (ExitSuccess, bytes, _) -> return (Right bytes)


    withTemporaryFile :: BL.ByteString
                      -> (FilePath -> IO a) -> IO a
    withTemporaryFile bs k =
        withTempFile "." "tmp" $
          \fileName h -> BL.hPut h bs
                      >> hClose h
                      >> k fileName
