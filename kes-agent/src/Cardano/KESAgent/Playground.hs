module Cardano.KESAgent.Playground
where

import Data.Proxy
import Cardano.KESAgent.Serialization.Spec
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.Protocols.Control.Protocol
import Cardano.KESAgent.Protocols.Control.Driver
import GHC.Generics
import Cardano.Crypto.KES.Class

runPlayground :: IO ()
runPlayground = do
  let fi = info (Proxy :: Proxy (AgentInfo StandardCrypto))
  printSpec fi
