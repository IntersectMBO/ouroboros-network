{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Void
import qualified Network.Socket as Socket
import           Options.Applicative

import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))

import           Cardano.Tools.DBAnalyser.Block.Cardano (Args (..))
import           Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Cardano.Tools.ImmDBServer.Diffusion as ImmDBServer

main :: IO ()
main = do
    Opts {..} <- execParser optsParser
    let sockAddr = Socket.SockAddrInet port hostAddr
          where
            -- could also be passed in
            hostAddr = Socket.tupleToHostAddress (127, 0, 0, 1)
        args = CardanoBlockArgs configFile Nothing
    ProtocolInfo{pInfoConfig} <- mkProtocolInfo args
    absurd <$> ImmDBServer.run immDBDir sockAddr pInfoConfig

data Opts = Opts {
    immDBDir   :: FilePath
  , port       :: Socket.PortNumber
  , configFile :: FilePath
  }

optsParser :: ParserInfo Opts
optsParser =
    info (helper <*> parse) $ fullDesc <> progDesc desc
  where
    desc = "Serve an ImmutableDB via ChainSync and BlockFetch"

    parse = do
      immDBDir <- strOption $ mconcat
        [ long "db"
        , help "Path to the ImmutableDB"
        , metavar "PATH"
        ]
      port <- option auto $ mconcat
        [ long "port"
        , help "Port to serve on"
        , value 3001
        ]
      configFile <- strOption $ mconcat
        [ long "config"
        , help "Path to config file"
        , metavar "PATH"
        ]
      pure Opts {..}
