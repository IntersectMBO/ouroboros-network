{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoStarIsType #-}

module Cardano.KESAgent.Serialization.Spec.Class
where

import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Serialization.Spec.Types

import Ouroboros.Network.RawBearer ( RawBearer (..) )

import Data.Proxy

-- * Typeclasses

class HasSerInfo a where
  info :: Proxy a -> FieldInfo
  infoOf :: a -> FieldInfo
  infoOf _ = info (Proxy @a)

class HasSerInfo a => IsSerItem m a where
  sendItem :: RawBearer m -> a -> m ()
  receiveItem :: RawBearer m -> ReadResultT m a
