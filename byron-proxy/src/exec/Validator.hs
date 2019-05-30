{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Codec.SerialiseTerm (CodecCBORTerm (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Tracer (Tracer (..), contramap, traceWith)
import qualified Data.Text.Lazy.Builder as Text (Builder, fromString)
import qualified Options.Applicative as Opt

import qualified Network.Socket as Socket

import Cardano.BM.Data.Severity (Severity (Info))
import qualified Cardano.Binary as Binary (unAnnotated)
import Cardano.Chain.Block (ChainValidationState (..))
import qualified Cardano.Chain.Block as Block
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Slotting (SlotNumber(..))
import Cardano.Chain.ValidationMode (fromBlockValidationMode)
import Cardano.Crypto (RequiresNetworkMagic(..), decodeAbstractHash)

import Cardano.Shell.Constants.Types (CardanoConfiguration (..), Core (..), Genesis (..))
import Cardano.Shell.Presets (mainnetConfiguration)

import qualified Ouroboros.Byron.Proxy.ChainSync.Client as Client
import Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync (Block)

import Ouroboros.Network.Socket
import Ouroboros.Byron.Proxy.Network.Protocol

-- For orphan instances
import qualified Control.Monad.Class.MonadThrow as NonStandard
import qualified Control.Monad.Catch as Standard

import qualified Logging

-- | Assumes there will never be a roll back. Validates each block that
-- comes via roll-forward. Never attempts to improve the read pointer (will
-- start from genesis).
--
-- If the stop condition gives `Just`, then the client stops _after_ validating
-- that block.
--
-- Can't give a return value other than (), by constraint of the mux interface.
clientFold
  :: Tracer IO Text.Builder
  -> Genesis.Config
  -> (Block -> IO (Maybe t)) -- ^ Stop condition
  -> ChainValidationState
  -> Client.Fold (ResourceT IO) () -- Either ChainValidationError (t, ChainValidationState))
clientFold tracer genesisConfig stopCondition cvs = Client.Fold $ pure $ Client.Continue
  (\block _ -> Client.Fold $ do
    lift $ traceWith tracer $ case Binary.unAnnotated block of
      Block.ABOBBlock    blk -> mconcat
        [ "Validating block\n"
        , Block.renderBlock (Genesis.configEpochSlots genesisConfig) (fmap (const ()) blk)
        ]
      Block.ABOBBoundary _   -> "Validating boundary block\n"
    let validationMode = fromBlockValidationMode Block.BlockValidation
    outcome <- lift $ (`runReaderT` validationMode) $ runExceptT
      (Block.updateChainBlockOrBoundary genesisConfig cvs (Binary.unAnnotated block))
    case outcome of
      Left err   -> do
        let msg = mconcat ["Validation failed: ", Text.fromString (show err)]
        lift $ traceWith tracer msg
        pure $ Client.Stop ()
      Right cvs' -> do
        maybeStop <- lift $ stopCondition block
        case maybeStop of
          Just t -> pure $ Client.Stop ()
          Nothing -> Client.runFold $ clientFold tracer genesisConfig stopCondition cvs'
  )
  (\_ _ -> error "got rollback")

-- TODO option for genesis config provenance.
-- currently mainnet is hard-coded.
data Options = Options
  { loggerConfigPath     :: !(Maybe FilePath)
  , serverHost           :: !Socket.HostName
  , serverPort           :: !Socket.ServiceName
  , overrideGenesisJson  :: !(Maybe FilePath)
  , requiresNetworkMagic :: !RequiresNetworkMagic
  }

cliParser :: Opt.Parser Options
cliParser = Options
  <$> cliLoggerConfigPath
  <*> cliServerHost
  <*> cliServerPort
  <*> cliOverrideGenesisJson
  <*> cliRequiresNetworkMagic

  where

  cliLoggerConfigPath = Opt.optional $ Opt.strOption $
    Opt.long "logger-config" <>
    Opt.metavar "FILEPATH"   <>
    Opt.help "Path to logger config file"

  cliServerHost = Opt.strOption $
    Opt.long "server-host" <>
    Opt.metavar "HOST"     <>
    Opt.help "Host of chain sync server"

  cliServerPort = Opt.strOption $
    Opt.long "server-port" <>
    Opt.metavar "PORT"     <>
    Opt.help "Port of chain sync server"

  cliOverrideGenesisJson = Opt.optional $ Opt.strOption $
    Opt.long "override-genesis-json" <>
    Opt.metavar "FILEPATH"           <>
    Opt.help "Path to genesis JSON file"

  cliRequiresNetworkMagic = Opt.flag RequiresNoMagic RequiresMagic $
    Opt.long "requires-network-magic" <>
    Opt.help "Flag to require network magic"

cliParserInfo :: Opt.ParserInfo Options
cliParserInfo = Opt.info cliParser infoMod
  where
  infoMod :: Opt.InfoMod Options
  infoMod =
       Opt.header "Validator"
    <> Opt.progDesc "Download and validate a chain"
    <> Opt.fullDesc

main :: IO ()
main = do
  opts <- Opt.execParser cliParserInfo
  Logging.withLogging (loggerConfigPath opts) "validator" $ \trace_ -> do
    let trace = Logging.convertTrace' trace_
        -- Hard-code to mainnet configuration.
        cc = mainnetConfiguration
        mainnetGenFilepath = case overrideGenesisJson opts of
          Nothing -> geSrc . coGenesis $ ccCore cc
          Just fp -> fp
        rnm = requiresNetworkMagic opts
    -- Copied from validate-mainnet in cardano-ledger.
    let (Right genHash) = decodeAbstractHash
                          . geGenesisHash
                          . coGenesis
                          . ccCore
                          $ cc
    Right genesisConfig <-
      runExceptT (Genesis.mkConfigFromFile rnm mainnetGenFilepath genHash)
    Right cvs <- runExceptT $ Block.initialChainValidationState genesisConfig
    genesisConfig `seq` cvs `seq` pure ()
    addrInfoLocal  : _ <- Socket.getAddrInfo
      (Just Socket.defaultHints)
      (Just "127.0.0.1")
      (Just "0")
    addrInfoRemote : _ <- Socket.getAddrInfo
      (Just Socket.defaultHints)
      (Just (serverHost opts))
      (Just (serverPort opts))
    let epochSlots = Genesis.configEpochSlots genesisConfig
        stopCondition :: Block -> IO (Maybe t)
        stopCondition = const (pure Nothing)
        tracer = contramap (\tbuilder -> ("", Info, tbuilder)) trace
        client  = Client.chainSyncClient (clientFold tracer genesisConfig stopCondition cvs)
    connectToNode
      encodeTerm
      decodeTerm
      (initiatorVersions epochSlots client)
      (Just addrInfoLocal)
      addrInfoRemote

-- Orphan, forced upon me because of the IO sim stuff.
-- Required because we use ResourceT in the chain sync server, and `runPeer`
-- demands this non-standard `MonadThrow`. That could be fixed by returning
-- the failure reason rather than throwing it...

instance NonStandard.MonadThrow (ResourceT IO) where
  throwM = Standard.throwM
  -- There's a default definition fo this which requires
  -- NonStandard.MonadCatch (ResourceT IO). To avoid having to give those,
  -- we'll just use the standard definition.
  -- NB: it's weird huh? This implementation uses the _standard_ MonadMask
  -- constraint, but the non-standard one is not defined.
  bracket = Standard.bracket
