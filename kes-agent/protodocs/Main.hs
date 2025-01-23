{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Classes
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.V1.Driver
import Cardano.KESAgent.Protocols.Control.V1.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Service.V1.Driver
import Cardano.KESAgent.Protocols.Service.V1.Protocol
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
import Network.TypedProtocol.Documentation
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Printf

allDocs =
  [ $(describeProtocol ''VersionHandshakeProtocol [] ''DirectCodec [''IO])
  , $(describeProtocol ''ServiceProtocol [''IO] ''DirectCodec [''IO])
  , $(describeProtocol ''ControlProtocol [''IO] ''DirectCodec [''IO])
  ]

main :: IO ()
main = defaultMain allDocs
