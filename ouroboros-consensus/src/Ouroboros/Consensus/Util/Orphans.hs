{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Util.Orphans () where

import           Codec.Serialise (Serialise(..))
import           Control.Monad.Identity
import           Control.Monad.Trans
import           Crypto.Random

import           Cardano.Crypto.Hash (Hash)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HeaderHash, HasHeader, SlotNo (..),
                                          TBlockPoint (..), TPoint (..))
import           Ouroboros.Network.Chain (Chain (..))

import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance Condense SlotNo where
  condense (SlotNo n) = condense n

instance Condense t => Condense (TPoint t) where
    condense Origin = "(Origin)"
    condense (Point p) = "(Point " <> condense p <> ")"

instance (Condense slot, Condense hash) => Condense (TBlockPoint slot hash) where
  condense p = condense (pointSlot p) <> ", " <> condense (pointHash p)

instance Condense block => Condense (Chain block) where
    condense Genesis   = "Genesis"
    condense (cs :> b) = condense cs <> " :> " <> condense b

instance (Condense block, HasHeader block, Condense (HeaderHash block))
    => Condense (AnchoredFragment block) where
    condense (AF.Empty pt) = "EmptyAnchor " <> condense pt
    condense (cs AF.:> b)  = condense cs <> " :> " <> condense b

{-------------------------------------------------------------------------------
  MonadRandom
-------------------------------------------------------------------------------}

instance MonadRandom m => MonadRandom (IdentityT m) where
     getRandomBytes = lift . getRandomBytes

{-------------------------------------------------------------------------------
  Serialise
-------------------------------------------------------------------------------}

instance Serialise (Hash h a) where
