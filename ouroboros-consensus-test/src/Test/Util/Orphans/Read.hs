{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.Read () where

import qualified System.Random.Internal as SR
import qualified System.Random.SplitMix as SM

newtype StdGen = StdGen SM.SMGen
  deriving (Read)

instance Read SR.StdGen where
  readsPrec d s =
      [ (SR.StdGen x, s')
      | (StdGen x, s') <- readsPrec d s
      ]
