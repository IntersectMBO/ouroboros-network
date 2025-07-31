{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module DMQ.Configuration.CLIOptions where

import Data.IP
import Network.Socket (PortNumber)
import Options.Applicative

data CLIOptions =
  CLIOptions {
    ipv4         :: Maybe IPv4
  , ipv6         :: Maybe IPv6
  , port         :: PortNumber
  , configFile   :: FilePath
  , topologyFile :: FilePath
  }

parseCLIOptions :: Parser CLIOptions
parseCLIOptions =
  CLIOptions
    <$> optional (
          option auto
          (  long "host-addr"
          <> metavar "IPv4"
          <> help "IPv4 that the node will bind to"
          )
       )
    <*> optional (
          option auto
           (  long "host-ipv6-addr"
           <> metavar "IPv6"
           <> help "IPv6 that the node will bind to"
           )
        )
    <*> option auto
        (  long "port"
        <> short 'p'
        <> value (read "1234")
        <> showDefault
        <> metavar "Port Number"
        <> help "Port Number that the node will bind to"
        )
    <*> strOption
        (  long "configuration-file"
        <> short 'c'
        <> metavar "FILENAME"
        <> help "Configuration file for DMQ Node"
        )
    <*> strOption
        (  long "topology-file"
        <> short 't'
        <> metavar "FILENAME"
        <> help "Topology file for DMQ Node"
        )


