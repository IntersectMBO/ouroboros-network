{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Playground
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Classes
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.V0.Driver
import Cardano.KESAgent.Protocols.Control.V0.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Service.V0.Driver
import Cardano.KESAgent.Protocols.Service.V0.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.VersionHandshake.Driver
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Serialization.DirectCodec
import Data.Proxy

import Cardano.Crypto.KES.Class
import Data.SerDoc.Class
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import GHC.Generics
import Network.TypedProtocol.Core
-- import Network.TypedProtocol.Documentation
-- import Network.TypedProtocol.Documentation.Html
-- import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
-- import Text.Blaze.Html.Renderer.Text (renderHtml)
-- import Text.Printf

-- allDocs =
--   [ $(describeProtocol ''VersionHandshakeProtocol [] ''DirectCodec [''IO])
--   , $(describeProtocol ''ServiceProtocol [''IO, ''StandardCrypto] ''DirectCodec [''IO])
--   , $(describeProtocol ''ControlProtocol [''IO, ''StandardCrypto] ''DirectCodec [''IO])
--   ]

runPlayground :: IO ()
runPlayground = do
  putStrLn "Hello world"
  -- writeFile "spec.html" (Pretty.renderHtml . wrapDocument . renderProtocolDescriptions $ allDocs)
