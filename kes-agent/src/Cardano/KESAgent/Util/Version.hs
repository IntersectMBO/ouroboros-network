{-# LANGUAGE TemplateHaskell #-}

module Cardano.KESAgent.Util.Version
where

import Language.Haskell.TH
import Cardano.KESAgent.Util.GetVersion (getProgramVersion)

libraryVersion :: String
libraryVersion = $( litE =<< (stringL <$> runIO getProgramVersion) )
