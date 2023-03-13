{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- DUPLICATE -- adapted from: cardano-node/src/Cardano/Node/Protocol/Shelley.hs

module Cardano.Node.Protocol.Shelley (
    mkSomeConsensusProtocolShelley
    -- * Errors
  , GenesisReadError (..)
  , GenesisValidationError (..)
  , PraosLeaderCredentialsError (..)
  , ShelleyProtocolInstantiationError (..)
    -- * Reusable parts
  , genesisHashToPraosNonce
  , readGenesis
  , readGenesisAny
  , readLeaderCredentials
  , validateGenesis
  ) where

import           Cardano.Api.Any hiding (FileError (..))
import qualified Cardano.Api.Any as Api (FileError (..))
import           Cardano.Api.Key
import           Cardano.Api.KeysPraos as Praos
import           Cardano.Api.KeysShelley
import           Cardano.Api.OperationalCertificate
import qualified Cardano.Api.Protocol.Types as Protocol
import           Cardano.Api.SerialiseTextEnvelope
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (coerceKeyRole)
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Types
import           Cardano.Prelude
import           Control.Monad.Trans.Except.Extra (firstExceptT,
                     handleIOExceptT, hoistEither, left, newExceptT)
import qualified Data.Aeson as Aeson (FromJSON (..), eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (PraosCanBeLeader (..))
import           Ouroboros.Consensus.Shelley.Node (Nonce (..),
                     ProtocolParamsShelley (..),
                     ProtocolParamsShelleyBased (..), ShelleyGenesis (..),
                     ShelleyLeaderCredentials (..))
import           Prelude (String, id)


------------------------------------------------------------------------------
-- Shelley protocol
--

-- | Make 'SomeConsensusProtocol' using the Shelley instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
mkSomeConsensusProtocolShelley
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolShelley NodeShelleyProtocolConfiguration {
                                  npcShelleyGenesisFile,
                                  npcShelleyGenesisFileHash
                                }
                          files = do
    (genesis, genesisHash) <- firstExceptT GenesisReadError $
                              readGenesis npcShelleyGenesisFile
                                          npcShelleyGenesisFileHash
    firstExceptT GenesisValidationError $ validateGenesis genesis
    leaderCredentials <- firstExceptT PraosLeaderCredentialsError $
                         readLeaderCredentials files

    return $ SomeConsensusProtocol Protocol.ShelleyBlockType $ Protocol.ProtocolInfoArgsShelley
      Consensus.ProtocolParamsShelleyBased {
        shelleyBasedGenesis = genesis,
        shelleyBasedInitialNonce = genesisHashToPraosNonce genesisHash,
        shelleyBasedLeaderCredentials =
            leaderCredentials
      }
      Consensus.ProtocolParamsShelley {
        shelleyProtVer =
          ProtVer (natVersion @2) 0,
        shelleyMaxTxCapacityOverrides =
          Mempool.mkOverrides Mempool.noOverridesMeasure
      }

genesisHashToPraosNonce :: GenesisHash -> Nonce
genesisHashToPraosNonce (GenesisHash h) = Nonce (Crypto.castHash h)

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (ShelleyGenesis StandardCrypto, GenesisHash)
readGenesis = readGenesisAny

readGenesisAny :: Aeson.FromJSON genesis
               => GenesisFile
               -> Maybe GenesisHash
               -> ExceptT GenesisReadError IO (genesis, GenesisHash)
readGenesisAny (GenesisFile file) mbExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadFileError file) $
                 BS.readFile file
    let genesisHash = GenesisHash (Crypto.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (GenesisDecodeError file) $ hoistEither $
                 Aeson.eitherDecodeStrict' content
    return (genesis, genesisHash)
  where
    checkExpectedGenesisHash :: GenesisHash
                             -> ExceptT GenesisReadError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected
          -> throwError (GenesisHashMismatch actual expected)
        _ -> return ()

validateGenesis :: ShelleyGenesis StandardCrypto
                -> ExceptT GenesisValidationError IO ()
validateGenesis genesis =
    firstExceptT GenesisValidationErrors . hoistEither $
      Shelley.validateGenesis genesis

readLeaderCredentials
  :: Maybe ProtocolFilepaths
  -> ExceptT PraosLeaderCredentialsError IO [ShelleyLeaderCredentials StandardCrypto]
readLeaderCredentials Nothing = return []
readLeaderCredentials (Just pfp) =
  -- The set of credentials is a sum total of what comes from the CLI,
  -- as well as what's in the bulk credentials file.
  (<>) <$> readLeaderCredentialsSingleton pfp
       <*> readLeaderCredentialsBulk      pfp

readLeaderCredentialsSingleton ::
     ProtocolFilepaths ->
     ExceptT PraosLeaderCredentialsError IO
             [ShelleyLeaderCredentials StandardCrypto]
-- It's OK to supply none of the files on the CLI
readLeaderCredentialsSingleton
   ProtocolFilepaths
     { shelleyCertFile      = Nothing,
       shelleyVRFFile       = Nothing,
       shelleyKESFile       = Nothing
     } = pure []
-- Or to supply all of the files
readLeaderCredentialsSingleton
  ProtocolFilepaths { shelleyCertFile = Just opCertFile,
                      shelleyVRFFile = Just vrfFile,
                      shelleyKESFile = Just kesFile
                    } = do
    vrfSKey <-
      firstExceptT FileError (newExceptT $ readFileTextEnvelope (AsSigningKey AsVrfKey) vrfFile)

    (opCert, kesSKey) <- opCertKesKeyCheck kesFile opCertFile

    return [mkPraosLeaderCredentials opCert vrfSKey kesSKey]

-- But not OK to supply some of the files without the others.
readLeaderCredentialsSingleton ProtocolFilepaths {shelleyCertFile = Nothing} =
     left OCertNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths {shelleyVRFFile = Nothing} =
     left VRFKeyNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths {shelleyKESFile = Nothing} =
     left KESKeyNotSpecified

opCertKesKeyCheck
  :: FilePath
  -- ^ KES key
  -> FilePath
  -- ^ Operational certificate
  -> ExceptT PraosLeaderCredentialsError IO (OperationalCertificate, SigningKey KesKey)
opCertKesKeyCheck kesFile certFile = do
  opCert <-
    firstExceptT FileError (newExceptT $ readFileTextEnvelope AsOperationalCertificate certFile)
  kesSKey <-
    firstExceptT FileError (newExceptT $ readFileTextEnvelope (AsSigningKey AsKesKey) kesFile)
  let opCertSpecifiedKesKeyhash = verificationKeyHash $ getHotKey opCert
      suppliedKesKeyHash = verificationKeyHash $ getVerificationKey kesSKey
  -- Specified KES key in operational certificate should match the one
  -- supplied to the node.
  if suppliedKesKeyHash /= opCertSpecifiedKesKeyhash
  then left $ MismatchedKesKey kesFile certFile
  else return (opCert, kesSKey)

data ShelleyCredentials
  = ShelleyCredentials
    { scCert :: (TextEnvelope, FilePath)
    , scVrf  :: (TextEnvelope, FilePath)
    , scKes  :: (TextEnvelope, FilePath)
    }

readLeaderCredentialsBulk
  :: ProtocolFilepaths
  -> ExceptT PraosLeaderCredentialsError IO [ShelleyLeaderCredentials StandardCrypto]
readLeaderCredentialsBulk ProtocolFilepaths { shelleyBulkCredsFile = mfp } =
  mapM parseShelleyCredentials =<< readBulkFile mfp
 where
   parseShelleyCredentials
     :: ShelleyCredentials
     -> ExceptT PraosLeaderCredentialsError IO (ShelleyLeaderCredentials StandardCrypto)
   parseShelleyCredentials ShelleyCredentials { scCert, scVrf, scKes } = do
     mkPraosLeaderCredentials
       <$> parseEnvelope AsOperationalCertificate scCert
       <*> parseEnvelope (AsSigningKey AsVrfKey) scVrf
       <*> parseEnvelope (AsSigningKey AsKesKey) scKes

   readBulkFile
     :: Maybe FilePath
     -> ExceptT PraosLeaderCredentialsError IO [ShelleyCredentials]
   readBulkFile Nothing = pure []
   readBulkFile (Just fp) = do
     content <- handleIOExceptT (CredentialsReadError fp) $
                  BS.readFile fp
     envelopes <- firstExceptT (EnvelopeParseError fp) $ hoistEither $
                    Aeson.eitherDecodeStrict' content
     pure $ uncurry mkCredentials <$> zip [0..] envelopes
    where
      mkCredentials :: Int -> (TextEnvelope, TextEnvelope, TextEnvelope)
                    -> ShelleyCredentials
      mkCredentials ix (teCert, teVrf, teKes) =
       let loc ty = fp <> "." <> show ix <> ty
       in ShelleyCredentials (teCert, loc "cert")
                             (teVrf,  loc "vrf")
                             (teKes,  loc "kes")

mkPraosLeaderCredentials ::
     OperationalCertificate
  -> SigningKey VrfKey
  -> SigningKey KesKey
  -> ShelleyLeaderCredentials StandardCrypto
mkPraosLeaderCredentials
    (OperationalCertificate opcert (StakePoolVerificationKey vkey))
    (VrfSigningKey vrfKey)
    (KesSigningKey kesKey) =
    ShelleyLeaderCredentials
    { shelleyLeaderCredentialsCanBeLeader =
        PraosCanBeLeader {
        praosCanBeLeaderOpCert     = opcert,
          praosCanBeLeaderColdVerKey = coerceKeyRole vkey,
          praosCanBeLeaderSignKeyVRF = vrfKey
        },
      shelleyLeaderCredentialsInitSignKey = kesKey,
      shelleyLeaderCredentialsLabel = "Shelley"
    }

parseEnvelope ::
     HasTextEnvelope a
  => AsType a
  -> (TextEnvelope, String)
  -> ExceptT PraosLeaderCredentialsError IO a
parseEnvelope as (te, loc) =
  firstExceptT (FileError . Api.FileError loc) . hoistEither $
    deserialiseFromTextEnvelope as te


------------------------------------------------------------------------------
-- Errors
--

data ShelleyProtocolInstantiationError =
       GenesisReadError GenesisReadError
     | GenesisValidationError GenesisValidationError
     | PraosLeaderCredentialsError PraosLeaderCredentialsError
  deriving Show

instance Error ShelleyProtocolInstantiationError where
  displayError (GenesisReadError err)            = displayError err
  displayError (GenesisValidationError err)      = displayError err
  displayError (PraosLeaderCredentialsError err) = displayError err


data GenesisReadError =
       GenesisReadFileError !FilePath !IOException
     | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
     | GenesisDecodeError !FilePath !String
  deriving Show

instance Error GenesisReadError where
  displayError (GenesisReadFileError fp err) =
        "There was an error reading the genesis file: "
     <> toS fp <> " Error: " <> show err

  displayError (GenesisHashMismatch actual expected) =
        "Wrong genesis file: the actual hash is " <> show actual
     <> ", but the expected genesis hash given in the node "
     <> "configuration file is " <> show expected

  displayError (GenesisDecodeError fp err) =
        "There was an error parsing the genesis file: "
     <> toS fp <> " Error: " <> show err


newtype GenesisValidationError = GenesisValidationErrors [Shelley.ValidationErr]
  deriving Show

instance Error GenesisValidationError where
  displayError (GenesisValidationErrors vErrs) =
    T.unpack (unlines (map Shelley.describeValidationErr vErrs))


data PraosLeaderCredentialsError =
       CredentialsReadError !FilePath !IOException
     | EnvelopeParseError !FilePath !String
     | FileError !(Api.FileError TextEnvelopeError)
     | OCertNotSpecified
     | VRFKeyNotSpecified
     | KESKeyNotSpecified
     | MismatchedKesKey
         FilePath
         -- KES signing key
         FilePath
         -- Operational certificate
  deriving Show

instance Error PraosLeaderCredentialsError where
  displayError (CredentialsReadError fp err) =
        "There was an error reading a credentials file: "
     <> toS fp <> " Error: " <> show err

  displayError (EnvelopeParseError fp err) =
        "There was an error parsing a credentials envelope: "
     <> toS fp <> " Error: " <> show err

  displayError (FileError fileErr) = displayError fileErr
  displayError (MismatchedKesKey kesFp certFp) =
       "The KES key provided at: " <> show kesFp
    <> " does not match the KES key specified in the operational certificate at: " <> show certFp
  displayError OCertNotSpecified  = missingFlagMessage "shelley-operational-certificate"
  displayError VRFKeyNotSpecified = missingFlagMessage "shelley-vrf-key"
  displayError KESKeyNotSpecified = missingFlagMessage "shelley-kes-key"

missingFlagMessage :: String -> String
missingFlagMessage flag =
  "To create blocks, the --" <> flag <> " must also be specified"
