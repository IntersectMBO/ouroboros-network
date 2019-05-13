{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket)
import Control.Monad.Trans.Except (runExceptT)
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.Functor.Contravariant (contramap)
import Data.Text (Text, pack)
import qualified Options.Applicative as Opt

import qualified Network.Socket as Socket
import Network.TypedProtocol.Driver (runPeer)

import Cardano.BM.Data.Severity (Severity (Info))
import qualified Cardano.Binary as Binary (unAnnotated)
import Cardano.Chain.Block (ChainValidationError, ChainValidationState (..))
import qualified Cardano.Chain.Block as Block
import Cardano.Chain.Common (parseReqNetworkMag)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Slotting (FlatSlotId(..))

import Cardano.Shell.Constants.Types (CardanoConfiguration (..), Core (..), Genesis (..))
import Cardano.Shell.Presets (mainnetConfiguration)

import qualified Ouroboros.Byron.Proxy.ChainSync.Client as Client
import Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync (Block, codec)
import Ouroboros.Network.Channel (socketAsChannel)
import qualified Ouroboros.Network.Protocol.ChainSync.Client as Client

import qualified Logging

-- | Assumes there will never be a roll back. Validates each block that
-- comes via roll-forward. Never attempts to improve the read pointer (will
-- start from genesis).
--
-- If the stop condition gives `Just`, then the client stops _after_ validating
-- that block.
clientFold
  :: Tracer IO Text
  -> Genesis.Config
  -> (Block -> IO (Maybe t)) -- ^ Stop condition
  -> ChainValidationState
  -> Client.Fold IO (Either ChainValidationError (t, ChainValidationState))
clientFold tracer genesisConfig stopCondition cvs = Client.Fold $ pure $ Client.Continue
  (\block _ -> Client.Fold $ do
    outcome <- runExceptT (Block.updateChainBlockOrBoundary genesisConfig cvs (Binary.unAnnotated block))
    case outcome of
      Left err   -> do
        let msg = pack $ mconcat ["Validation failed: ", show err]
        traceWith tracer msg
        pure $ Client.Stop $ Left err
      Right cvs' -> do
        let msg = pack $ mconcat ["Validated block at slot ", show (unFlatSlotId $ cvsLastSlot cvs')]
        traceWith tracer msg
        maybeStop <- stopCondition block
        case maybeStop of
          Just t -> pure $ Client.Stop $ Right (t, cvs')
          Nothing -> Client.runFold $ clientFold tracer genesisConfig stopCondition cvs'
  )
  (\_ _ -> error "got rollback")

-- TODO option for genesis config provenance.
-- currently mainnet is hard-coded.
data Options = Options
  { loggerConfigPath :: !(Maybe FilePath)
  , serverHost       :: !Socket.HostName
  , serverPort       :: !Socket.ServiceName
  }

cliParser :: Opt.Parser Options
cliParser = Options
  <$> cliLoggerConfigPath
  <*> cliServerHost
  <*> cliServerPort

  where

  cliLoggerConfigPath = Opt.optional $ Opt.strOption $
    Opt.long "logger-config" <>
    Opt.metavar "FILEPATH"   <>
    Opt.help "Path to logger config file."

  cliServerHost = Opt.strOption $
    Opt.long "server-host" <>
    Opt.metavar "HOST"     <>
    Opt.help "Host of chain sync server"

  cliServerPort = Opt.strOption $
    Opt.long "server-port" <>
    Opt.metavar "PORT"     <>
    Opt.help "Port of chain sync server"

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
    let trace = Logging.convertTrace trace_
        -- Hard-code to mainnet configuration.
        -- mainnet-genesis.json file is assumed to be there.
        cc = mainnetConfiguration
        mainnetGenFilepath = geSrc . coGenesis $ ccCore cc
        reqNetworkMagic = parseReqNetworkMag . coRequiresNetworkMagic $ ccCore cc
    -- Copied from validate-mainnet in cardano-ledger.
    Right genesisConfig <-
      runExceptT (Genesis.mkConfigFromFile reqNetworkMagic mainnetGenFilepath Nothing)
    Right cvs <- runExceptT $ Block.initialChainValidationState genesisConfig
    genesisConfig `seq` cvs `seq` pure ()
    addrInfo : _ <- Socket.getAddrInfo
      (Just Socket.defaultHints)
      (Just (serverHost opts))
      (Just (serverPort opts))
    let epochSlots = Genesis.configEpochSlots genesisConfig
        stopCondition :: Block -> IO (Maybe t)
        stopCondition = const (pure Nothing)
        tracer = contramap (\txt -> ("", Info, txt)) trace
        client  = Client.chainSyncClient (clientFold tracer genesisConfig stopCondition cvs)
        peer    = Client.chainSyncClientPeer client
        codec   = ChainSync.codec epochSlots
        action  = \socket -> runPeer nullTracer codec (socketAsChannel socket) peer
        mkSocket = Socket.socket
          (Socket.addrFamily addrInfo)
          (Socket.addrSocketType addrInfo)
          (Socket.addrProtocol addrInfo)
    outcome <- bracket mkSocket Socket.close $ \socket -> do
      _ <- Socket.connect socket (Socket.addrAddress addrInfo)
      action socket
    -- TODO print the outcome?
    pure ()
