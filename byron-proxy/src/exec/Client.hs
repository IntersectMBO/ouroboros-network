import Control.Concurrent.Async (withAsync)
import Control.Tracer (stdoutTracer)
import qualified Network.Socket as Socket (tupleToHostAddress)

import IPC (runVersionedClient)

main :: IO ()
main = do
  let -- TODO get from CLI
      host = Socket.tupleToHostAddress (127, 0, 0, 1)
      port = 7777
      shelleyThread = runVersionedClient host port stdoutTracer
      userInterrupt = getChar
  withAsync shelleyThread $ \_ ->
    userInterrupt
  pure ()
