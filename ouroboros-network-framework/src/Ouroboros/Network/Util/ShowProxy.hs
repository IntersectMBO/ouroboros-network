{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Ouroboros.Network.Util.ShowProxy
  ( ShowProxy (..)
  , Proxy (..)
  ) where

import Data.Proxy (Proxy (..))

class ShowProxy p where
    showProxy :: Proxy p -> String
