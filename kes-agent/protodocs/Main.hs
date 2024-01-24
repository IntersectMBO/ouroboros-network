{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main
where

import Data.Proxy
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Classes
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionHandshake.Driver
import Cardano.KESAgent.Protocols.Control.V1.Protocol
import Cardano.KESAgent.Protocols.Control.V1.Driver
import Cardano.KESAgent.Protocols.Service.V1.Protocol
import Cardano.KESAgent.Protocols.Service.V1.Driver
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

allDocs =
  [ $(describeProtocol ''VersionHandshakeProtocol [] ''DirectCodec [''IO])
  , $(describeProtocol ''ServiceProtocol [''IO] ''DirectCodec [''IO])
  , $(describeProtocol ''ControlProtocol [''IO] ''DirectCodec [''IO])
  ]
  
main :: IO ()
main = defaultMain allDocs
