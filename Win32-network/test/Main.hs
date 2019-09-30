{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import           Test.Tasty

import qualified Test.NamedPipes

main :: IO ()
main = defaultMain $ testGroup "Win32"
  [ Test.NamedPipes.tests
  ]
   
