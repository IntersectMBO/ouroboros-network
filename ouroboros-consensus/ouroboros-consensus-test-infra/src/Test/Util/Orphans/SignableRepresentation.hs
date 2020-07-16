{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.SignableRepresentation () where

import           Cardano.Crypto.Util

instance SignableRepresentation () where
  getSignableRepresentation () = ""
