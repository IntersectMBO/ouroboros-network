{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBSynthesizer.Run (
    initialize
  , synthesize
  ) where

import           Cardano.Api.Any (displayError)
import           Cardano.Api.Protocol.Types (protocolInfo)
import           Cardano.Node.Protocol
import           Cardano.Node.Types
import           Cardano.Tools.DBSynthesizer.Forging
import           Cardano.Tools.DBSynthesizer.Orphans ()
import           Cardano.Tools.DBSynthesizer.Types
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT,
                     handleIOExceptT, hoistEither, runExceptT)
import           Control.Tracer (nullTracer)
import           Data.Aeson as Aeson (FromJSON, Result (..), Value,
                     eitherDecodeFileStrict', eitherDecodeStrict', fromJSON)
import           Data.Bool (bool)
import           Data.ByteString as BS (ByteString, readFile)
import           Ouroboros.Consensus.Config (configSecurityParam, configStorage)
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture (dontCheck)
import qualified Ouroboros.Consensus.Node as Node (mkChainDbArgs,
                     stdMkChainDbHasFS)
import qualified Ouroboros.Consensus.Node.InitStorage as Node
                     (nodeImmutableDbChunkInfo)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..),
                     validateGenesis)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB (defaultArgs,
                     getTipPoint)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB (cdbTracer,
                     withDB)
import           Ouroboros.Consensus.Storage.LedgerDB (SnapshotInterval (..),
                     defaultDiskPolicy)
import           Ouroboros.Consensus.Util.IOLike (atomically)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.Block
import           Ouroboros.Network.Point (WithOrigin (..))
import           System.Directory
import           System.FilePath (takeDirectory, (</>))


initialize
    :: NodeFilePaths
    -> NodeCredentials
    -> DBSynthesizerOptions
    -> IO (Either String (DBSynthesizerConfig, SomeConsensusProtocol))
initialize NodeFilePaths{nfpConfig, nfpChainDB} creds synthOptions = do
    relativeToConfig :: (FilePath -> FilePath) <-
        (</>) . takeDirectory <$> makeAbsolute nfpConfig
    runExceptT $ do
        conf    <- initConf relativeToConfig
        proto   <- initProtocol relativeToConfig conf
        pure    (conf, proto)
  where
    initConf :: (FilePath -> FilePath) -> ExceptT String IO DBSynthesizerConfig
    initConf relativeToConfig = do
        inp             <- handleIOExceptT show (BS.readFile nfpConfig)
        configStub      <- adjustFilePaths relativeToConfig <$> readJson inp
        shelleyGenesis  <- readFileJson $ ncsShelleyGenesisFile configStub
        _               <- hoistEither $ validateGenesis shelleyGenesis
        let
            protocolCredentials = ProtocolFilepaths {
              byronCertFile         = Nothing
            , byronKeyFile          = Nothing
            , shelleyKESFile        = credKESFile creds
            , shelleyVRFFile        = credVRFFile creds
            , shelleyCertFile       = credCertFile creds
            , shelleyBulkCredsFile  = credBulkFile creds
            }
        pure DBSynthesizerConfig {
              confConfigStub            = configStub
            , confOptions               = synthOptions
            , confProtocolCredentials   = protocolCredentials
            , confShelleyGenesis        = shelleyGenesis
            , confDbDir                 = nfpChainDB
            }

    initProtocol :: (FilePath -> FilePath) -> DBSynthesizerConfig -> ExceptT String IO SomeConsensusProtocol
    initProtocol relativeToConfig DBSynthesizerConfig{confConfigStub, confProtocolCredentials} = do
        hfConfig :: NodeHardForkProtocolConfiguration <-
            hoistEither hfConfig_
        byronConfig :: NodeByronProtocolConfiguration <-
            adjustFilePaths relativeToConfig <$> hoistEither byConfig_

        let
            cardanoConfig = NodeProtocolConfigurationCardano byronConfig shelleyConfig alonzoConfig conwayConfig hfConfig
        firstExceptT displayError $
            mkConsensusProtocol
                cardanoConfig
                (Just confProtocolCredentials)
      where
        shelleyConfig   = NodeShelleyProtocolConfiguration (GenesisFile $ ncsShelleyGenesisFile confConfigStub) Nothing
        alonzoConfig    = NodeAlonzoProtocolConfiguration (GenesisFile $ ncsAlonzoGenesisFile confConfigStub) Nothing
        conwayConfig    = NodeConwayProtocolConfiguration (GenesisFile $ ncsConwayGenesisFile confConfigStub) Nothing
        hfConfig_       = eitherParseJson $ ncsNodeConfig confConfigStub
        byConfig_       = eitherParseJson $ ncsNodeConfig confConfigStub

readJson :: (Monad m, FromJSON a) => ByteString -> ExceptT String m a
readJson = hoistEither . eitherDecodeStrict'

readFileJson :: FromJSON a => FilePath -> ExceptT String IO a
readFileJson f = handleIOExceptT show (eitherDecodeFileStrict' f) >>= hoistEither

eitherParseJson :: FromJSON a => Aeson.Value -> Either String a
eitherParseJson v = case fromJSON v of
    Error err -> Left err
    Success a -> Right a

synthesize :: DBSynthesizerConfig -> SomeConsensusProtocol -> IO ForgeResult
synthesize DBSynthesizerConfig{confOptions, confShelleyGenesis, confDbDir} (SomeConsensusProtocol _ runP) =
    withRegistry $ \registry -> do
        let
            epochSize   = sgEpochLength confShelleyGenesis
            chunkInfo   = Node.nodeImmutableDbChunkInfo (configStorage pInfoConfig)
            k           = configSecurityParam pInfoConfig
            diskPolicy  = defaultDiskPolicy k DefaultSnapshotInterval
            dbArgs      = Node.mkChainDbArgs
                registry InFuture.dontCheck pInfoConfig pInfoInitLedger chunkInfo $
                    ChainDB.defaultArgs (Node.stdMkChainDbHasFS confDbDir) diskPolicy

        forgers <- pInfoBlockForging
        let fCount = length forgers
        putStrLn $ "--> forger count: " ++ show fCount
        if fCount > 0
            then do
                putStrLn $ "--> opening ChainDB on file system with mode: " ++ show synthOpenMode
                preOpenChainDB synthOpenMode confDbDir
                let dbTracer = nullTracer
                ChainDB.withDB dbArgs {ChainDB.cdbTracer = dbTracer} $ \chainDB -> do
                    slotNo <- do
                        tip <- atomically (ChainDB.getTipPoint chainDB)
                        pure $ case pointSlot tip of
                            Origin -> 0
                            At s   -> succ s

                    putStrLn $ "--> starting at: " ++ show slotNo
                    runForge epochSize slotNo synthLimit chainDB forgers pInfoConfig
            else do
                putStrLn "--> no forgers found; leaving possibly existing ChainDB untouched"
                pure $ ForgeResult 0
  where
    DBSynthesizerOptions
        { synthOpenMode
        , synthLimit
        } = confOptions
    ProtocolInfo
        { pInfoConfig
        , pInfoBlockForging
        , pInfoInitLedger
        } = protocolInfo runP

preOpenChainDB :: DBSynthesizerOpenMode -> FilePath -> IO ()
preOpenChainDB mode db =
    doesDirectoryExist db >>= bool create checkMode
  where
    checkIsDB ls    = length ls <= 3 && all (`elem` ["immutable", "ledger", "volatile"]) ls
    loc             = "preOpenChainDB: '" ++ db ++ "'"
    create          = createDirectoryIfMissing True db
    checkMode = do
        isChainDB <- checkIsDB <$> listDirectory db
        case mode of
            OpenCreate ->
                fail $ loc ++ " already exists. Use -f to overwrite or -a to append."
            OpenAppend | isChainDB ->
                pure ()
            OpenCreateForce | isChainDB ->
                removePathForcibly db >> create
            _ ->
                fail $ loc ++ " is non-empty and does not look like a ChainDB. Aborting."
