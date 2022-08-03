{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneKindSignatures   #-}

module Ouroboros.Consensus.HardFork.Combinator.Util.Functors (
    Flip (..)
  , Flip2 (..)
  , Product2 (..)
  ) where

import           Data.Kind
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

data Product2 f g x y = Pair2 (f x y) (g x y)
  deriving (Eq, Generic, Show)

newtype Flip f x y = Flip {unFlip :: f y x}
  deriving (Eq, Generic, NoThunks, Show)

type Flip2 :: (blk -> wt -> mk -> Type) -> wt -> mk -> blk -> Type
newtype Flip2 f x y z = Flip2 {unFlip2 :: f z x y}
  deriving (Eq, Generic, NoThunks, Show)
