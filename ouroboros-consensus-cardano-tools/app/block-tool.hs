import Cardano.Tools.Block (BlockOptions (..), run)
import Control.Concurrent.MVar (newMVar)
import Options.Applicative (
    Parser,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    optional,
    progDesc,
    strOption,
    (<**>),
 )
import Ouroboros.Consensus.Storage.FS.API.Types (Handle (..), MountPoint (..), mkFsPath)
import Ouroboros.Consensus.Storage.FS.Handle (HandleOS (..))
import Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import System.Posix (stdInput, stdOutput)

main :: IO ()
main = do
    mvarIn <- newMVar (Just stdInput)
    mvarOut <- newMVar (Just stdOutput)
    let stdinIO =
            Handle
                { handlePath = mkFsPath ["<stdin>"]
                , handleRaw =
                    HandleOS
                        { filePath = "<stdin>"
                        , handle = mvarIn
                        }
                }
    let stdoutIO =
            Handle
                { handlePath = mkFsPath ["<stdout>"]
                , handleRaw =
                    HandleOS
                        { filePath = "<stdout>"
                        , handle = mvarOut
                        }
                }
    parseOptions >>= run (ioHasFS $ MountPoint ".") stdinIO stdoutIO

parseOptions :: IO BlockOptions
parseOptions = execParser opts
  where
    opts =
        info
            (parseBlockOptions <**> helper)
            ( mconcat
                [ fullDesc
                , progDesc "Dump representation of a CBOR-encoded block in JSON"
                ]
            )

parseBlockOptions :: Parser BlockOptions
parseBlockOptions =
    ViewBlock
        <$> optional
            ( mkFsPath . (: [])
                <$> strOption
                    ( long "file-in"
                        <> metavar "FILE"
                        <> help "Path to file containing hex-encoded CBOR-encoded block"
                    )
            )
        <*> ( mkFsPath . (: [])
                <$> strOption
                    ( long "config"
                        <> metavar "FILE"
                        <> help "Path to cardano node configuration file"
                    )
            )
