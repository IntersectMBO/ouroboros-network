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
import Data.Typeable

class ShowProxy p where
    showProxy :: Proxy p -> String

    default showProxy :: Typeable p => Proxy p -> String
    showProxy p = showsTypeRep (typeRep p) ""
