{-# LANGUAGE TemplateHaskell #-}

module Cardano.KESAgent.Util.Version
where

import Cardano.KESAgent.Util.GetVersion (getProgramVersion)
import Language.Haskell.TH

libraryVersion :: String
libraryVersion = $(litE =<< (stringL <$> runIO getProgramVersion))
