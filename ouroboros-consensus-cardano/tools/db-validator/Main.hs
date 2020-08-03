{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Database validation tool.
module Main (main) where

import           Control.Monad.Except (runExceptT)
import qualified Data.Aeson as Aeson
import           Data.Foldable (asum)
import           Options.Applicative

import           Control.Tracer (contramap, debugTracer, nullTracer)

import qualified Cardano.Binary as CB
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update
import           Cardano.Crypto (Hash, RequiresNetworkMagic (..))

import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import qualified Ouroboros.Consensus.Node as Node
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

data CmdLine = CmdLine {
    dbDir     :: FilePath
  , verbose   :: Bool
  , onlyImmDB :: Bool
  , blockType :: BlockType
  }

parseCmdLine :: Parser CmdLine
parseCmdLine = CmdLine
    <$> strOption (mconcat [
            long "db"
          , help "Path to the chain DB"
          , metavar "PATH"
          ])
    <*> flag False True (mconcat [
            long "verbose"
          , help "Enable verbose logging"
          ])
    <*> flag False True (mconcat [
            long "onlyImmDB"
          , help "Validate only the immutable DB (e.g. do not do ledger validation)"
          ])
    <*> blockTypeParser

data BlockType =
    ByronBlock   ByronBlockArgs
  | ShelleyBlock ShelleyBlockArgs
  | CardanoBlock CardanoBlockArgs

blockTypeParser :: Parser BlockType
blockTypeParser = subparser $ mconcat
  [ command "byron"   (info (parseByronType   <**> helper) (progDesc "byron command"  ))
  , command "shelley" (info (parseShelleyType <**> helper) (progDesc "shelley command"))
  , command "cardano" (info (parseCardanoType <**> helper) (progDesc "cardano command"))
  ]

data ByronBlockArgs = ByronBlockArgs {
    configFileByron      :: FilePath
  , requiresNetworkMagic :: Bool
  , genesisHash          :: Hash CB.Raw
  , threshold            :: Maybe PBftSignatureThreshold
  }

parseByronType :: Parser BlockType
parseByronType = ByronBlock <$> parseByronArgs

parseByronArgs :: Parser ByronBlockArgs
parseByronArgs = ByronBlockArgs
    <$> strOption (mconcat [
            long "configByron"
          , help "Path to config file"
          , metavar "PATH"
          ])
    <*> flag False True (mconcat [
            long "testnet"
          , help "The DB contains blocks from testnet rather than mainnet"
          ])
    <*> option auto (mconcat [
            long "genesisHash"
          , help "Expected genesis hash"
          , metavar "PATH"
          ])
    <*> asum [ Just . PBftSignatureThreshold <$> thresholdParser
             , pure Nothing
             ]
  where
    thresholdParser = option auto (mconcat [
            long "threshold"
          , help "PBftSignatureThreshold"
          , metavar "THRESHOLD"
          ])

data ShelleyBlockArgs = ShelleyBlockArgs {
    configFileShelley :: FilePath
  , initialNonce      :: Nonce
  } deriving Show

parseShelleyType :: Parser BlockType
parseShelleyType = ShelleyBlock <$> parseShelleyArgs

parseShelleyArgs :: Parser ShelleyBlockArgs
parseShelleyArgs = ShelleyBlockArgs
    <$> strOption (mconcat [
            long "configShelley"
          , help "Path to config file."
          , metavar "PATH"
          ])
    <*> asum [ Nonce  <$> parseNonce
             , pure NeutralNonce]
  where
    parseNonce = strOption (mconcat [
            long "nonce"
          , help "initial nonce"
          , metavar "NONCE"
          ])

parseCardanoType :: Parser BlockType
parseCardanoType = CardanoBlock <$> parseCardanoArgs

parseCardanoArgs :: Parser CardanoBlockArgs
parseCardanoArgs = CardanoBlockArgs
    <$> parseByronArgs
    <*> parseShelleyArgs

data CardanoBlockArgs = CardanoBlockArgs {
    byronArgs   :: ByronBlockArgs
  , shelleyArgs :: ShelleyBlockArgs
  }

main :: IO ()
main = do
    cmdLine <- getCmdLine
    case blockType cmdLine of
      ByronBlock args   -> validate cmdLine args
      ShelleyBlock args -> validate cmdLine args
      CardanoBlock args -> validate cmdLine args

getCmdLine :: IO CmdLine
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework used to validate a Chain DB"
        ])

validate :: forall blk. (HasProtocolInfo blk, Node.RunNode blk, Show (Header blk))
         => CmdLine -> Args blk -> IO ()
validate CmdLine {..} args = do
    protocolInfo <- mkProtocolInfo @blk args
    validateChainDb @blk dbDir protocolInfo onlyImmDB verbose

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
  type Args blk = args | args -> blk
  mkProtocolInfo :: Args blk -> IO (ProtocolInfo IO blk)

{-------------------------------------------------------------------------------
  ByronBlock instance
-------------------------------------------------------------------------------}

instance HasProtocolInfo ByronBlock where
  type Args ByronBlock = ByronBlockArgs
  mkProtocolInfo ByronBlockArgs {..} = do
    config <- openGenesisByron configFileByron genesisHash requiresNetworkMagic
    return $ mkProtocolInfoByron config threshold

openGenesisByron :: FilePath -> Hash CB.Raw -> Bool -> IO CC.Genesis.Config
openGenesisByron configFile genesisHash requiresNetworkMagic = do
    econfig <-
      runExceptT $
        CC.Genesis.mkConfigFromFile
          (if requiresNetworkMagic then RequiresMagic else RequiresNoMagic)
          configFile
          genesisHash
    either (error . show) return econfig

mkProtocolInfoByron :: CC.Genesis.Config
                    -> Maybe PBftSignatureThreshold
                    -> ProtocolInfo IO ByronBlock
mkProtocolInfoByron genesisConfig signatureThreshold = protocolInfoByron
    genesisConfig
    signatureThreshold
    (CC.Update.ProtocolVersion 1 0 0)
    (CC.Update.SoftwareVersion (CC.Update.ApplicationName "Cardano SL") 2)
    Nothing

{-------------------------------------------------------------------------------
  ShelleyBlock instance
-------------------------------------------------------------------------------}

instance HasProtocolInfo (ShelleyBlock TPraosStandardCrypto) where
  type Args (ShelleyBlock TPraosStandardCrypto) = ShelleyBlockArgs
  mkProtocolInfo ShelleyBlockArgs {..}  = do
    config <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileShelley
    return $ mkShelleyProtocolInfo config initialNonce

mkShelleyProtocolInfo :: forall c. TPraosCrypto c
                      => ShelleyGenesis c
                      -> Nonce
                      -> ProtocolInfo IO (ShelleyBlock c)
mkShelleyProtocolInfo genesis nonce =
    protocolInfoShelley
      genesis
      nonce
      2000
      (SL.ProtVer 0 0)
      Nothing

{-------------------------------------------------------------------------------
  CardanoBlock instance
-------------------------------------------------------------------------------}

instance HasProtocolInfo (CardanoBlock TPraosStandardCrypto) where
  type Args (CardanoBlock TPraosStandardCrypto) = CardanoBlockArgs
  mkProtocolInfo CardanoBlockArgs {..} = do
    let ByronBlockArgs {..} = byronArgs
    let ShelleyBlockArgs {..} = shelleyArgs
    byronConfig <- openGenesisByron configFileByron genesisHash requiresNetworkMagic
    shelleyConfig <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileShelley
    return $ mkCardanoProtocolInfo byronConfig shelleyConfig threshold initialNonce

mkCardanoProtocolInfo :: forall c. TPraosCrypto c
                      => CC.Genesis.Config
                      -> ShelleyGenesis c
                      -> Maybe PBftSignatureThreshold
                      -> Nonce
                      -> ProtocolInfo IO (CardanoBlock c)
mkCardanoProtocolInfo byronConfig shelleyConfig signatureThreshold nonce =
    protocolInfoCardano
      byronConfig
      signatureThreshold
      (CC.Update.ProtocolVersion 1 0 0)
      (CC.Update.SoftwareVersion (CC.Update.ApplicationName "db-validator") 2)
      Nothing
      shelleyConfig
      nonce
      (SL.ProtVer 2 0)
      2000
      Nothing
      Nothing
      (TriggerHardForkAtVersion 2)
