
module DBSynthesizer.Parsers (parseCommandLine) where

import           Cardano.Tools.DBSynthesizer.Types
import           Data.Word (Word64)
import           Options.Applicative as Opt
import           Ouroboros.Consensus.Block.Abstract (SlotNo (..))


parseCommandLine :: IO (NodeFilePaths, NodeCredentials, DBSynthesizerOptions)
parseCommandLine =
    Opt.customExecParser p opts
  where
    p     = Opt.prefs Opt.showHelpOnEmpty
    opts  = Opt.info parserCommandLine mempty

parserCommandLine :: Parser (NodeFilePaths, NodeCredentials, DBSynthesizerOptions)
parserCommandLine =
  (,,)
    <$> parseNodeFilePaths
    <*> parseNodeCredentials
    <*> parseDBSynthesizerOptions

parseNodeFilePaths :: Parser NodeFilePaths
parseNodeFilePaths =
  NodeFilePaths
    <$> parseNodeConfigFilePath
    <*> parseChainDBFilePath

parseNodeCredentials :: Parser NodeCredentials
parseNodeCredentials =
  NodeCredentials
    <$> optional parseOperationalCertFilePath
    <*> optional parseVrfKeyFilePath
    <*> optional parseKesKeyFilePath
    <*> optional parseBulkFilePath

parseDBSynthesizerOptions :: Parser DBSynthesizerOptions
parseDBSynthesizerOptions =
  DBSynthesizerOptions
    <$> parseForgeOptions
    <*> parseOpenMode

parseForgeOptions :: Parser ForgeLimit
parseForgeOptions =
      ForgeLimitSlot <$> parseSlotLimit
  <|> ForgeLimitBlock <$> parseBlockLimit
  <|> ForgeLimitEpoch <$> parseEpochLimit

parseChainDBFilePath :: Parser FilePath
parseChainDBFilePath =
  strOption
    ( long "db"
        <> metavar "PATH"
        <> help "Path to the Chain DB"
        <> completer (bashCompleter "directory")
    )

parseNodeConfigFilePath :: Parser FilePath
parseNodeConfigFilePath =
  strOption
    ( long "config"
        <> metavar "FILE"
        <> help "Path to the node's config.json"
        <> completer (bashCompleter "file")
    )

parseOperationalCertFilePath :: Parser FilePath
parseOperationalCertFilePath =
  strOption
    ( long "shelley-operational-certificate"
        <> metavar "FILE"
        <> help "Path to the delegation certificate"
        <> completer (bashCompleter "file")
    )

parseKesKeyFilePath :: Parser FilePath
parseKesKeyFilePath =
  strOption
    ( long "shelley-kes-key"
        <> metavar "FILE"
        <> help "Path to the KES signing key"
        <> completer (bashCompleter "file")
    )

parseVrfKeyFilePath :: Parser FilePath
parseVrfKeyFilePath =
  strOption
    ( long "shelley-vrf-key"
        <> metavar "FILE"
        <> help "Path to the VRF signing key"
        <> completer (bashCompleter "file")
    )

parseBulkFilePath :: Parser FilePath
parseBulkFilePath =
  strOption
    ( long "bulk-credentials-file"
        <> metavar "FILE"
        <> help "Path to the bulk credentials file"
        <> completer (bashCompleter "file")
    )

parseSlotLimit :: Parser SlotNo
parseSlotLimit =
  SlotNo <$> option auto
    (     short 's'
       <> long "slots"
       <> metavar "NUMBER"
       <> help "Amount of slots to process"
    )

parseBlockLimit :: Parser Word64
parseBlockLimit =
  option auto
    (     short 'b'
       <> long "blocks"
       <> metavar "NUMBER"
       <> help "Amount of blocks to forge"
    )

parseEpochLimit :: Parser Word64
parseEpochLimit =
  option auto
    (     short 'e'
       <> long "epochs"
       <> metavar "NUMBER"
       <> help "Amount of epochs to process"
    )

parseForce :: Parser Bool
parseForce =
  switch
    (     short 'f'
      <>  help "Force overwrite an existing Chain DB"
    )

parseAppend :: Parser Bool
parseAppend =
  switch
    (     short 'a'
      <>  help "Append to an existing Chain DB"
    )

parseOpenMode :: Parser DBSynthesizerOpenMode
parseOpenMode =
      (parseForce *> pure OpenCreateForce)
  <|> (parseAppend *> pure OpenAppend)
  <|> pure OpenCreate
