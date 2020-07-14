{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Database validation tool.
module Main (main) where

import           Control.Monad.Except (runExceptT)
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import qualified Options.Applicative as Options
import           Options.Generic
import qualified System.FilePath as FilePath ((</>))

import           Control.Tracer (contramap, debugTracer, nullTracer)

import qualified Cardano.Binary as CB
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update
import           Cardano.Crypto (Hash, ProtocolMagicId (..),
                     RequiresNetworkMagic (..), decodeAbstractHash)

import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import qualified Ouroboros.Consensus.Node as Node
import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Util.IOLike (atomically)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (fromChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Byron.Node

import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node (Nonce (..), ShelleyGenesis,
                     protocolInfoShelley)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..),
                     protocolInfoCardano)

instance ParseField UTCTime

instance ParseFields UTCTime

instance ParseRecord UTCTime where
  parseRecord = fmap getOnly parseRecord

instance ParseField (Hash CB.Raw) where
  readField = Options.eitherReader (first Text.unpack . decodeAbstractHash . Text.pack)

instance ParseFields (Hash CB.Raw)

instance ParseRecord (Hash CB.Raw) where
  parseRecord = fmap getOnly parseRecord

instance ParseField  BlockType
instance ParseRecord BlockType
instance ParseFields BlockType

data Args w = Args {
    dbDir                :: w ::: FilePath        <?> "Path to the new database directory"
  , configFile           :: w ::: [FilePath]      <?> "Configuration file (e.g. mainnet-genesis.json)"
  , requiresNetworkMagic :: w ::: Bool            <?> "Expecto patronum?"
  , genesisHash          :: w ::: Hash CB.Raw     <?> "Expected genesis hash"
  , blockType            :: w ::: Maybe BlockType <?> "type of network"
  , verbose              :: w ::: Bool            <?> "Enable verbose logging"
  , onlyImmDB            :: w ::: Bool            <?> "Validate only the immutable DB (e.g. do not do ledger validation)"
  } deriving (Generic)

instance ParseRecord (Args Wrapped)

deriving instance Show (Args Unwrapped)

data BlockType = Byron | Shelley | Cardano
  deriving (Generic, Show, Read)

main :: IO ()
main = do
    cmd <- unwrapRecord "DB validator"
    let Args{ dbDir, blockType } = cmd
    case blockType of
      Just Byron   -> validate cmd (Proxy @ByronBlock)
      Just Shelley -> validate cmd (Proxy @(ShelleyBlock TPraosStandardCrypto))
      Just Cardano -> validate cmd (Proxy @(CardanoBlock TPraosStandardCrypto))
      Nothing -> do
        -- check the dbmarker of the db if the block type is not specified.
        protocolMagicId <- readDBMarker dbDir
        case unProtocolMagicId protocolMagicId of
          764824073  -> validate cmd (Proxy @ByronBlock)
          1097911063 -> validate cmd (Proxy @ByronBlock)
          42         -> validate cmd (Proxy @(ShelleyBlock TPraosStandardCrypto))
          _          -> error $ "unsupported protocolMagicId: " ++ show protocolMagicId

readDBMarker :: FilePath -> IO ProtocolMagicId
readDBMarker dbPath = do
    bs <- BS.readFile markerPath
    protocolMagicId <- runExceptT $ dbMarkerParse markerPath bs
    either
      (\err -> error $
        "failed to parse protocolMagicId from db Marker file. Error " ++ show err)
      return
      protocolMagicId
  where
    markerPath :: String
    markerPath = dbPath FilePath.</> Text.unpack dbMarkerFile

validate :: forall blk. (HasProtocolInfo blk, Node.RunNode blk, Show (Header blk))
         => Args Unwrapped -> Proxy blk ->  IO ()
validate Args {..} _ = do
    protocolInfo <- mkProtocolInfo @blk configFile genesisHash requiresNetworkMagic
    validateChainDb dbDir protocolInfo onlyImmDB verbose

validateChainDb
  :: (Node.RunNode blk, Show (Header blk))
  => FilePath -- ^ DB directory
  -> ProtocolInfo IO blk
  -> Bool -- Immutable DB only?
  -> Bool -- Verbose
  -> IO ()
validateChainDb dbDir protocolInfo onlyImmDB verbose =
    withRegistry $ \registry -> do
      let chainDbArgs = mkChainDbArgs registry InFuture.dontCheck
          (immDbArgs, _, _, _) = fromChainDbArgs chainDbArgs
      if onlyImmDB then
        ImmDB.withImmDB immDbArgs $ \immDB -> do
          immDbTipPoint <- ImmDB.getPointAtTip immDB
          putStrLn $ "DB tip: " ++ show immDbTipPoint
      else
        ChainDB.withDB chainDbArgs $ \chainDB -> do
          chainDbTipPoint <- atomically $ ChainDB.getTipPoint chainDB
          putStrLn $ "DB tip: " ++ show chainDbTipPoint
  where
    ProtocolInfo { pInfoInitLedger = initLedger, pInfoConfig = cfg } =
      protocolInfo

    tracer
      | verbose   = contramap show debugTracer
      | otherwise = nullTracer

    chunkInfo  = Node.nodeImmDbChunkInfo cfg

    mkChainDbArgs registry btime =
      let args = Node.mkChainDbArgs tracer registry btime
                   dbDir cfg initLedger chunkInfo
      in args {
          ChainDB.cdbImmValidation = ImmDB.ValidateAllChunks
        }

class HasProtocolInfo blk where
  mkProtocolInfo :: [FilePath]
                 -> Hash CB.Raw
                 -> Bool  -- is it mainnet?
                 -> IO (ProtocolInfo IO blk)

{-------------------------------------------------------------------------------
  ByronBlock instance
-------------------------------------------------------------------------------}

instance HasProtocolInfo ByronBlock where
  mkProtocolInfo [configFile] genesisHash requiresNetworkMagic = do
    config <- openGenesisByron configFile genesisHash requiresNetworkMagic
    return $ mkProtocolInfoByron config
  mkProtocolInfo ls _ _ =
     error $
       "a single genesis file is needed for pure Byron. Given " ++ show ls

openGenesisByron :: FilePath -> Hash CB.Raw -> Bool -> IO CC.Genesis.Config
openGenesisByron configFile genesisHash requiresNetworkMagic = do
    econfig <-
      runExceptT $
        CC.Genesis.mkConfigFromFile
          (if requiresNetworkMagic then RequiresMagic else RequiresNoMagic)
          configFile
          genesisHash
    either (error . show) return econfig

mkProtocolInfoByron :: CC.Genesis.Config -> ProtocolInfo IO ByronBlock
mkProtocolInfoByron genesisConfig = protocolInfoByron
    genesisConfig
    (Just $ PBftSignatureThreshold 0.22) -- PBFT signature threshold
    (CC.Update.ProtocolVersion 1 0 0)
    (CC.Update.SoftwareVersion (CC.Update.ApplicationName "Cardano SL") 2)
    Nothing

{-------------------------------------------------------------------------------
  ShelleyBlock instance
-------------------------------------------------------------------------------}

instance TPraosCrypto c => HasProtocolInfo (ShelleyBlock c) where
  mkProtocolInfo [shelleyGenesis] _ _ = do
    config <- either (error . show) return =<< Aeson.eitherDecodeFileStrict' shelleyGenesis
    return $ mkShelleyProtocolInfo config
  mkProtocolInfo ls _ _ =
     error $
       "a single genesis file is needed for pure Shelley. Given " ++ show ls

mkShelleyProtocolInfo :: forall c. TPraosCrypto c
                      => ShelleyGenesis c
                      -> ProtocolInfo IO (ShelleyBlock c)
mkShelleyProtocolInfo genesis =
    protocolInfoShelley
      genesis
      NeutralNonce -- TODO
      2000
      (SL.ProtVer 0 0)
      Nothing

{-------------------------------------------------------------------------------
  CardanoBlock instance
-------------------------------------------------------------------------------}

instance TPraosCrypto c => HasProtocolInfo (CardanoBlock c) where
  mkProtocolInfo [byronGenesis, shelleyGenesis] genesisHash requiresNetworkMagic = do
    byronConfig <- openGenesisByron byronGenesis genesisHash requiresNetworkMagic
    shelleyConfig <- either (error . show) return =<< Aeson.eitherDecodeFileStrict' shelleyGenesis
    return $ mkCardanoProtocolInfo byronConfig shelleyConfig
  mkProtocolInfo ls _ _ =
    error $ "Two genesis files are needed for Cardano. Given " ++ show ls

mkCardanoProtocolInfo :: forall c. TPraosCrypto c
                      => CC.Genesis.Config
                      -> ShelleyGenesis c
                      -> ProtocolInfo IO (CardanoBlock c)
mkCardanoProtocolInfo byronConfig shelleyConfig =
    protocolInfoCardano
      byronConfig
      (Just $ PBftSignatureThreshold 0.22)
      (CC.Update.ProtocolVersion 1 0 0)
      (CC.Update.SoftwareVersion (CC.Update.ApplicationName "db-validator") 2)
      Nothing
      shelleyConfig
      NeutralNonce -- TODO
      (SL.ProtVer 2 0)
      2000
      Nothing
      Nothing
      (TriggerHardForkAtVersion 2)
