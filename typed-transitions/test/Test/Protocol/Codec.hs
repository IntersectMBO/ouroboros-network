{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Test.Protocol.Codec where

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Test.Protocol.Codec.PingPong as PingPong (tests)

tests :: TestTree
tests = testGroup "Codec"
  [ PingPong.tests
  ]
