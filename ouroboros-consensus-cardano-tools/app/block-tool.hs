{-# LANGUAGE NamedFieldPuns #-}

import Cardano.Tools.Block (BlockOptions (..), readChainPoint, run)
import Control.Concurrent.MVar (newMVar)
import Options.Applicative (
    Parser,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    option,
    optional,
    progDesc,
    strOption,
    (<**>),
    (<|>),
 )
import Ouroboros.Consensus.Storage.FS.API.Types (Handle (..), MountPoint (..), mkFsPath)
import Ouroboros.Consensus.Storage.FS.Handle (HandleOS (..))
import Ouroboros.Consensus.Storage.FS.IO (HandleIO, ioHasFS)
import System.Posix (Fd, stdInput, stdOutput)

main :: IO ()
main = do
    stdinIO <- mkHandle "<stdin>" stdInput
    stdoutIO <- mkHandle "<stdout>" stdOutput
    parseOptions >>= run (ioHasFS $ MountPoint ".") stdinIO stdoutIO

mkHandle :: [Char] -> Fd -> IO (Handle HandleIO)
mkHandle filePath fd = do
    handle <- newMVar (Just fd)
    pure $
        Handle
            { handlePath = mkFsPath [filePath]
            , handleRaw =
                HandleOS
                    { filePath
                    , handle
                    }
            }

parseOptions :: IO BlockOptions
parseOptions = execParser opts
  where
    opts =
        info
            (parseBlockOptions <**> helper)
            ( mconcat
                [ fullDesc
                , progDesc "Dump JSON representation of a CBOR-encoded block."
                ]
            )

parseBlockOptions :: Parser BlockOptions
parseBlockOptions =
    viewBlockOptions <|> extractBlockOptions

viewBlockOptions =
    ViewBlock
        <$> optional
            ( mkFsPath . (: [])
                <$> strOption
                    ( long "file-in"
                        <> metavar "FILE"
                        <> help "Path to file containing hex-encoded CBOR-encoded block"
                    )
            )

extractBlockOptions =
    ExtractBlock
        <$> strOption
            ( long "db-directory"
                <> metavar "DIR"
                <> help "Path to the directory where cardano DB lives "
            )
            <*> strOption
                ( long "config"
                    <> metavar "FILE"
                    <> help "Path to cardano node configuration file"
                )
            <*> option
                (maybeReader readChainPoint)
                ( long "point"
                    <> metavar "SLOT.HEADER_HASH"
                    <> help
                        "The id of the block we want to start observing the chain from. \
                        \If not given, uses the chain tip at startup. Composed by the slot \
                        \number, a separator ('.') and the hash of the block header. \
                        \For example: 52970883.d36a9936ae7a07f5f4bdc9ad0b23761cb7b14f35007e54947e27a1510f897f04."
                )
