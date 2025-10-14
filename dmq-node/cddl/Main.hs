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
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad (forM, forM_, unless)
import Control.Monad.Except (ExceptT (..), runExceptT)

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
-- import Codec.Serialise.Class (Serialise)
-- import Codec.Serialise.Class qualified as Serialise
import Codec.Serialise.Decoding qualified as CBOR
-- import Codec.Serialise.Encoding qualified as CBOR

import Data.Bool (bool)
import Data.ByteString.Base16.Lazy qualified as BL.Base16
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL.Char8
import Text.Printf

import System.Directory (doesDirectoryExist)
import System.Environment (setEnv)
import System.Exit (ExitCode (..), die)
import System.FilePath
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.Process.ByteString.Lazy

import Network.TypedProtocol.Codec

import Cardano.KESAgent.Protocols.StandardCrypto (StandardCrypto)

import DMQ.Protocol.LocalMsgNotification.Codec
import DMQ.Protocol.LocalMsgNotification.Type as LocalMsgNotification
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
tests CDDLSpecs { cddlSig,
                  cddlLocalMsgNotification
                } =
  adjustOption (const $ QuickCheckMaxSize 10) $
  testGroup "cddl"
    [ testGroup "decoding"
      -- validate decoder by generating messages from the specification
      [ testCase "Sig" (unit_decodeSig cddlSig)
      , testCase "LocalMsgNotification" (unit_decodeLocalMsgNotification cddlLocalMsgNotification)
      ]
      -- TODO: validate `LocalMsgNotification` encoder
    ]

newtype CDDLSpec ps = CDDLSpec BL.ByteString

type AnnSigRawWithSignedBytes = BL.ByteString -> SigRawWithSignedBytes StandardCrypto

data CDDLSpecs = CDDLSpecs {
    cddlSig                  :: CDDLSpec AnnSigRawWithSignedBytes,
    cddlLocalMsgNotification :: CDDLSpec (LocalMsgNotification (Sig StandardCrypto))
  }


unit_decodeSig :: CDDLSpec AnnSigRawWithSignedBytes
               -> Assertion
unit_decodeSig spec = validateDecoder spec decodeSig 100

unit_decodeLocalMsgNotification :: CDDLSpec (LocalMsgNotification (Sig StandardCrypto))
                                -> Assertion
unit_decodeLocalMsgNotification spec =
    validateAnnotatedDecoder
      (Just fix)
      spec
      codecLocalMsgNotification
      [ SomeAgency   LocalMsgNotification.SingIdle
      , SomeAgency $ LocalMsgNotification.SingBusy LocalMsgNotification.SingBlocking
      , SomeAgency $ LocalMsgNotification.SingBusy LocalMsgNotification.SingNonBlocking
      ]
      100
  where
    -- | The cddl spec cannot differentiate between fix-length list encoding and
    -- infinite-length encoding.  The cddl tool always generates fix-length
    -- encoding but tx-submission codec is accepting only indefinite-length
    -- encoding.
    --
    fix :: CBOR.Term -> CBOR.Term
    fix = \case
            TList (TInt tag : TList l : as)
                 | tag == 1 || tag == 2
                 -> TList (TInt tag : TListI l : as)
            term -> term


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
  (exitCode, cddl, _) <- readProcessWithExitCode "cddlc" ["-u", "-2", "-t", "cddl", path] mempty
  unless (exitCode == ExitSuccess) $
    die $ printf "cddlc failed on \"%s\" with %s " path (show exitCode)
  return cddl


readCDDLSpecs :: IO CDDLSpecs
readCDDLSpecs = do
    dir <- bool (               "cddl" </> "specs") -- False
                ("dmq-node" </> "cddl" </> "specs") -- True
       <$> doesDirectoryExist "dmq-node"
    setEnv "CDDL_INCLUDE_PATH" (dir <> ":")

    sigSpec <- cddlc (dir </> "sig.cddl")
    localMessageNotificationSpec <- cddlc (dir </> "local-msg-notification.cddl")

    return CDDLSpecs {
        cddlSig = CDDLSpec sigSpec,
        cddlLocalMsgNotification = CDDLSpec localMessageNotificationSpec
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
            [ printf "decoding failure:\n"
            , show err
            , "\nwhile decoding:\n"
            , show decoded_term
            , "\ngenerated term:\n"
            , BL.Char8.unpack generated_term
            , "\nencoded term:\n"
            , BL.Char8.unpack (BL.Base16.encode encoded_term)
            ]
          Right _ -> return ()


data SomeAgency ps where
    SomeAgency :: ( ActiveState st
                  , Show (StateToken st)
                  )
               => StateToken (st :: ps)
               -> SomeAgency ps

deriving instance Show (SomeAgency ps)


-- | Generate valid encoded messages from a specification using `cddl generate`
-- (and encoded with `diag2cbor.rb`) and check that we can decode it at one of
-- the given agencies.
--
validateAnnotatedDecoder
  :: forall ps.
     Maybe (CBOR.Term -> CBOR.Term)
  -- ^ transform a generated term
  -> CDDLSpec ps
  -> AnnotatedCodec ps CBOR.DeserialiseFailure IO BL.ByteString
  -> [SomeAgency ps]
  -> Int
  -> Assertion
validateAnnotatedDecoder transform (CDDLSpec spec) codec stoks rounds = do
    eterms <- runExceptT $ generateCBORFromSpec spec rounds
    case eterms of
      Left err -> assertFailure err
      Right terms ->
        forM_ terms $ \(generated_term, encoded_term) -> do
          let encoded_term' = case transform of
                 Nothing -> encoded_term
                 Just tr -> case CBOR.deserialiseFromBytes CBOR.decodeTerm encoded_term of
                   Right (rest, term)  | BL.null rest
                                      -> CBOR.toLazyByteString (CBOR.encodeTerm (tr term))
                   Right _            -> error   "validateDecoder: trailing bytes"
                   Left err           -> error $ "validateDecoder: decoding error: "
                                              ++ show err

              Right (_, decoded_term) =
                CBOR.deserialiseFromBytes CBOR.decodeTerm encoded_term'
          res <- decodeMsg encoded_term'
          case res of
            Just errs -> assertFailure $ concat
              [ printf "decoding failures:\n"
              , unlines ((\(SomeAgency a, e) -> printf "%-25s: %s" (show a) (show e)) <$>errs)
              , "while decoding:\n"
              , show decoded_term
              , "\ngenerated term (not rewritten):\n"
              , BL.Char8.unpack generated_term
              , "\nencoded term:\n"
              , BL.Char8.unpack (BL.Base16.encode encoded_term')
              ]
            Nothing -> return ()
  where
    -- | Try decode at all given agencies.  If one succeeds return
    -- 'Nothing' otherwise return all 'DeserialiseFailure's.
    --
    decodeMsg :: BL.ByteString
              -> IO (Maybe [(SomeAgency ps, CBOR.DeserialiseFailure)])
    decodeMsg bs =
        -- sequence [Nothing, ...] = Nothing
        fmap sequence $
        forM stoks $ \(a@(SomeAgency (stok :: StateToken st))) -> do
            decoder <- decode codec stok
            res <- runDecoder [bs] decoder
            return $ case res of
              Left err -> Just (a, err)
              Right {} -> Nothing


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
