{-# LANGUAGE TypeApplications #-}

module Main
where

import Cardano.KESAgent.Agent
import Cardano.KESAgent.Protocol

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Control.Monad.Class.MonadThrow ( bracket )
import Control.Concurrent.Class.MonadMVar
import Control.Tracer
import Data.Proxy (Proxy (..))
import System.IOManager
import Network.Socket

main :: IO ()
main = withIOManager $ \ioManager -> do
  logLock <- newMVar ()
  let agentOptions =
        defAgentOptions
          { agentServiceAddr = SockAddrUnix "kes-agent-service.socket"
          , agentControlAddr = SockAddrUnix "kes-agent-control.socket"
          , agentSnocket = socketSnocket ioManager
          }
      mrb = makeSocketRawBearer
      tracer = Tracer $ \msg -> do
        () <- takeMVar logLock
        print msg
        putMVar logLock ()
  bracket
    (newAgent (Proxy @StandardCrypto) mrb agentOptions tracer)
    finalizeAgent
    runAgent
