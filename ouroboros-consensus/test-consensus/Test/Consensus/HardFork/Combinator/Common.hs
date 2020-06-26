{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Consensus.HardFork.Combinator.Common (
    -- Shared header fields
    Hash
  , HeaderFields(..)
  , zeroHeaderFields
  ) where

import           Codec.Serialise
import qualified Data.ByteString as Strict
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))

import           Ouroboros.Consensus.Block

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
  deriving (Show, Eq, Generic)
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "HeaderFields" (HeaderFields b)

deriving instance Serialise (HeaderHash b) => Serialise (HeaderFields b)

-- | Value for 'HeaderFields' when the values of the fields is irrelevant
zeroHeaderFields :: HeaderHash b ~ Hash => HeaderFields b
zeroHeaderFields = HeaderFields {
      headerFieldHash     = Strict.empty
    , headerFieldPrevHash = GenesisHash
    , headerFieldSlot     = SlotNo  0
    , headerFieldNo       = BlockNo 0
    }
