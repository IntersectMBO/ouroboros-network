{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

#if defined(mingw32_HOST_OS)
import           Test.Tasty

import qualified Test.Async
import qualified Test.NamedPipes
import qualified Test.Event

main :: IO ()
main = defaultMain $ testGroup "Win32"
  [ Test.Async.tests
  , Test.NamedPipes.tests
  , Test.Event.tests
  ]
#else
main :: IO ()
main = pure ()
#endif
