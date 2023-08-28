{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Processes.ControlClient
import Cardano.KESAgent.Protocols.Service.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Serialization.CBOR
import Cardano.KESAgent.Serialization.TextEnvelope
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.Libsodium (sodiumInit)
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Libsodium.MLockedSeed (mlockedSeedNewRandom, mlockedSeedFinalize)
import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Control.Monad ( (>=>) )
import Control.Monad.Class.MonadThrow (bracket, throwIO, catch, SomeException)
import Control.Tracer
import qualified Data.Aeson as JSON
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text.IO as Text
import Network.Socket
import Options.Applicative
import System.Environment
import System.IO (hPutStrLn, stdout, stderr, hFlush)
import System.IOManager

data NewKeyOptions =
  NewKeyOptions
    { nkoControlPath :: Maybe String
    , nkoKESVerificationKeyFile :: Maybe FilePath
    , nkoOpCertFile :: Maybe FilePath
    }
    deriving (Show, Eq)

instance Semigroup NewKeyOptions where
  NewKeyOptions c1 vk1 oc1 <> NewKeyOptions c2 vk2 oc2 =
    NewKeyOptions (c1 <|> c2) (vk1 <|> vk2) (oc1 <|> oc2)

defNewKeyOptions :: NewKeyOptions =
  NewKeyOptions
    { nkoControlPath = Just "/tmp/kes-agent-control.socket"
    , nkoKESVerificationKeyFile = Just "kes.vkey"
    , nkoOpCertFile = Just "ocert.cert"
    }

nkoFromEnv :: IO NewKeyOptions
nkoFromEnv = do
  controlPath <- lookupEnv "KES_AGENT_CONTROL_PATH"
  return NewKeyOptions
    { nkoControlPath = controlPath
    , nkoKESVerificationKeyFile = Nothing
    , nkoOpCertFile = Nothing
    }


pNewKeyOptions =
  NewKeyOptions
    <$> option (Just <$> str)
          (  long "control-address"
          <> short 'c'
          <> value Nothing
          <> metavar "ADDR"
          <> help "Socket address for 'control' connections to a running kes-agent process"
          )
    <*> option (Just <$> str)
          (  long "kes-verification-key-file"
          <> value Nothing
          <> metavar "FILE"
          <> help "File to write KES verification key to"
          )
    <*> option (Just <$> str)
          (  long "opcert-file"
          <> value Nothing
          <> metavar "FILE"
          <> help "File to read OpCert from"
          )

data ProgramOptions
  = RunNewKey NewKeyOptions
  deriving (Show, Eq)

pProgramOptions = subparser
  (  command "new-key" (info (RunNewKey <$> pNewKeyOptions) idm)
  )

eitherError :: Either String a -> IO a
eitherError (Left err) = error err
eitherError (Right x) = return x

humanFriendlyControlTracer :: Tracer IO ControlClientTrace
humanFriendlyControlTracer = Tracer $ \case
  ControlClientKeyAccepted -> putStrLn "Key accepted."
  ControlClientKeyRejected reason -> putStrLn $ "Key rejected: " ++ formatReason reason
  _ -> return ()

formatReason :: RecvResult -> String
formatReason RecvOK = "no error"
formatReason RecvErrorInvalidOpCert = "OpCert validation failed"
formatReason RecvErrorKeyOutdated = "KES key outdated"
formatReason RecvErrorUnknown = "unknown error"

runNewKey :: NewKeyOptions -> IO ()
runNewKey nko' = withIOManager $ \ioManager -> do
  nkoEnv <- nkoFromEnv
  let nko = nko' <> nkoEnv <> defNewKeyOptions
  verKeyFilename <- maybe (error "Missing KES VerKey file") return (nkoKESVerificationKeyFile nko)
  ocertFilename <- maybe (error "Missing OpCert file") return (nkoOpCertFile nko)
  controlPath <- maybe (error "No control address") return (nkoControlPath nko)
  let controlClientOptions =
        ControlClientOptions
          { controlClientSnocket = socketSnocket ioManager
          , controlClientAddress = SockAddrUnix controlPath
          , controlClientLocalAddress = Nothing
          }
  sk <- genKeyKESIO
  ( do
      vkKES :: VerKeyKES (KES StandardCrypto) <- deriveVerKeyKES sk

      encodeTextEnvelopeFile verKeyFilename (KESVerKey vkKES)
      putStrLn $ "KES VerKey written to " ++ verKeyFilename
      putStrLn $ "OpCert will be read from " ++ ocertFilename
      putStrLn "Press ENTER to continue..."
      hFlush stdout
      _ <- Text.getLine
      oc <- eitherError =<< decodeTextEnvelopeFile ocertFilename
      withNewCRef
        (forgetSignKeyKES . skWithoutPeriodKES)
        (SignKeyWithPeriodKES sk 0) $ \skpVar -> do
          runControlClient1
            (Proxy @StandardCrypto)
            makeSocketRawBearer
            controlClientOptions
            skpVar (opCert oc)
            humanFriendlyControlTracer
    ) `catch` (\(e :: SomeException) -> forgetSignKeyKES sk >> throwIO e)

genKeyKESIO :: KESAlgorithm k => IO (SignKeyKES k)
genKeyKESIO =
  bracket
    mlockedSeedNewRandom
    mlockedSeedFinalize
    genKeyKES

programDesc = fullDesc

main :: IO ()
main = do
  sodiumInit
  programOptions <- execParser (info (pProgramOptions <**> helper) programDesc)
  case programOptions of
    RunNewKey nko' -> runNewKey nko'
