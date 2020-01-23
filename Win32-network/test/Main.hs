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

import qualified Test.Generators
import qualified Test.Async.Handle
import qualified Test.Async.Socket

main :: IO ()
main = defaultMain $ testGroup "Win32"
  [ Test.Generators.tests
  , Test.Async.Handle.tests
  , Test.Async.Socket.tests
  ]
#else
main :: IO ()
main = pure ()
#endif
