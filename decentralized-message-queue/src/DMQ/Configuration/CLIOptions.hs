{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module DMQ.Configuration.CLIOptions where

import Data.IP (IPv4)
import Network.Socket (PortNumber)
import Options.Applicative

data CLIOptions =
  CLIOptions {
    ipv4         :: IPv4
  , port         :: PortNumber
  , configFile   :: FilePath
  , topologyFile :: FilePath
  }

parseCLIOptions :: Parser CLIOptions
parseCLIOptions =
  CLIOptions
    <$> option auto
        ( long "address"
       <> short 'a'
       <> value (read "0.0.0.0")
       <> showDefault
       <> metavar "IPv4"
       <> help "IPv4 that the node will bind to" )
    <*> option auto
        ( long "port"
       <> short 'p'
       <> value (read "1234")
       <> showDefault
       <> metavar "Port Number"
       <> help "Port Number that the node will bind to" )
    <*> strOption
        ( long "configuration-file"
       <> short 'c'
       <> metavar "FILENAME"
       <> help "Configuration file for DMQ Node" )
    <*> strOption
        ( long "topology-file"
       <> short 't'
       <> metavar "FILENAME"
       <> help "Topology file for DMQ Node" )


