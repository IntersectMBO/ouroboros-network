{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.HardFork.Combinator.Common (
    -- Shared header fields
    Hash
  , HeaderFields(..)
  ) where

import           Codec.Serialise
import qualified Data.ByteString as Strict
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

{-------------------------------------------------------------------------------
  Types shared by A and B
-------------------------------------------------------------------------------}

type Hash = Strict.ByteString

data HeaderFields b = HeaderFields {
       headerFieldHash     :: HeaderHash b
     , headerFieldPrevHash :: ChainHash b
     , headerFieldSlot     :: SlotNo
     , headerFieldNo       :: BlockNo
     }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

deriving instance Serialise (HeaderHash b) => Serialise (HeaderFields b)
