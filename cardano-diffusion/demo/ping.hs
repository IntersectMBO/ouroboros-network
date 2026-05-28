module Main where

import Cardano.Network.Ping as Ping
import Options.Applicative

main :: IO ()
main = execParser (info (Ping.pingOptsParser <**> helper) fullDesc)
   >>= Ping.pingClients
