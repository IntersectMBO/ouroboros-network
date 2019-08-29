{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Ouroboros.Storage.EpochInfo.API (
    EpochInfo(..)
  , epochInfoRange
    -- * Utility
  , hoistEpochInfo
  , generalizeEpochInfo
  ) where

import           Control.Monad.Morph (generalize)
import           Data.Functor.Classes (showsUnaryWith)
import           Data.Functor.Identity

import           Cardano.Prelude (DontCheckForThunks (..), NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Storage.Common

-- | Information about epochs
--
-- Epochs may have different sizes at different times during the lifetime of the
-- blockchain. This information is encapsulated by 'EpochInfo'; it is
-- parameterized over a monad @m@ because the information about how long each
-- epoch is may depend on information derived from the blockchain itself, and
-- hence requires access to state.
--
-- The other functions provide some derived information from epoch sizes. In the
-- default implementation all of these functions query and update an internal
-- cache maintaining cumulative epoch sizes; for that reason, all of these
-- functions live in a monad @m@.
data EpochInfo m = EpochInfo {
      -- | Return the size of the given epoch as a number of slots
      --
      -- Note that the number of slots does /not/ bound the number of blocks,
      -- since the EBB and a regular block share a slot number.
      epochInfoSize  :: EpochNo -> m EpochSize

       -- | First slot in the specified epoch
       --
       -- See also 'epochInfoRange'
    , epochInfoFirst :: EpochNo -> m SlotNo

      -- | Epoch containing the given slot
      --
      -- We should have the property that
      --
      -- > s `inRange` epochInfoRange (epochInfoEpoch s)
    , epochInfoEpoch :: SlotNo -> m EpochNo
    }
  deriving NoUnexpectedThunks via DontCheckForThunks (EpochInfo m)

-- | Show instance only for non-stateful instances
instance Show (EpochInfo Identity) where
    showsPrec p EpochInfo{..} =
        showsUnaryWith showsPrec "fixedSizeEpochInfo" p $
          runIdentity $ epochInfoSize (EpochNo 0)

epochInfoRange :: Monad m => EpochInfo m ->  EpochNo -> m (SlotNo, SlotNo)
epochInfoRange epochInfo epochNo =
    aux <$> epochInfoFirst epochInfo epochNo
        <*> epochInfoSize  epochInfo epochNo
  where
    aux :: SlotNo -> EpochSize -> (SlotNo, SlotNo)
    aux (SlotNo s) (EpochSize sz) = (SlotNo s, SlotNo (s + sz - 1))

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

hoistEpochInfo :: (forall a. m a -> n a) -> EpochInfo m -> EpochInfo n
hoistEpochInfo f EpochInfo{..} = EpochInfo{
      epochInfoSize  = f . epochInfoSize
    , epochInfoFirst = f . epochInfoFirst
    , epochInfoEpoch = f . epochInfoEpoch
    }

generalizeEpochInfo :: Monad m => EpochInfo Identity -> EpochInfo m
generalizeEpochInfo = hoistEpochInfo generalize
