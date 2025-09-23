{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.KESAgent.Util.Version
where

#if !defined(wasm32_HOST_ARCH)
import Cardano.KESAgent.Util.GetVersion (getProgramVersion)
import Language.Haskell.TH
#endif

libraryVersion :: String
#if !defined(wasm32_HOST_ARCH)
libraryVersion = $(litE =<< (stringL <$> runIO getProgramVersion))
#else
libraryVersion = "unknown"
#endif
