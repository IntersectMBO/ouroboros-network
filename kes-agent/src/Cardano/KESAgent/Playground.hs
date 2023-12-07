{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Cardano.KESAgent.Playground
where


import Data.Proxy
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Classes
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionHandshake.Driver
import Cardano.KESAgent.Protocols.Control.Protocol
import Cardano.KESAgent.Protocols.Control.Driver
import Cardano.KESAgent.Protocols.Service.Protocol
import Cardano.KESAgent.Protocols.Service.Driver
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Serialization.DirectCodec

import GHC.Generics
import Cardano.Crypto.KES.Class
import Text.Printf
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import Data.SerDoc.Class
import Network.TypedProtocol.Core
import Network.TypedProtocol.Documentation
import Network.TypedProtocol.Documentation.Html

allDocs =
  [ $(describeProtocol ''VersionHandshakeProtocol [] ''DirectCodec [''IO])
  , $(describeProtocol ''ServiceProtocol [''IO, ''StandardCrypto] ''DirectCodec [''IO])
  , $(describeProtocol ''ControlProtocol [''IO, ''StandardCrypto] ''DirectCodec [''IO])
  ]
  
runPlayground :: IO ()
runPlayground = do
  writeFile "spec.html" (Pretty.renderHtml . wrapDocument . renderProtocolDescriptions $ allDocs)
