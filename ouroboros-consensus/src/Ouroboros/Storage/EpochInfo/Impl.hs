{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Storage.EpochInfo.Impl (
      newEpochInfo
    , fixedSizeEpochInfo
    ) where

import           Control.Monad.Trans.State.Strict (runStateT)
import           Data.List (maximumBy)
import           Data.Ord (comparing)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.Util.MonadSTM.NormalForm

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo.API
import           Ouroboros.Storage.EpochInfo.CumulEpochSizes as CES

newEpochInfo :: forall m. MonadSTM m
             => (EpochNo -> m EpochSize) -> m (EpochInfo m)
newEpochInfo getSize = do
    firstEpochSize <- getSize 0
    cesVar         <- atomically $ newTVar $ CES.singleton firstEpochSize
    return EpochInfo {
        epochInfoSize  = wrap cesVar . CES.epochSize
      , epochInfoFirst = wrap cesVar . CES.firstSlotOf
      , epochInfoEpoch = wrap cesVar . CES.slotToEpoch
      }
  where
    wrap :: StrictTVar m CumulEpochSizes
         -> (CumulEpochSizes -> Maybe a)
         -> m a
    wrap cesVar f = do
        ces       <- atomically $ readTVar cesVar
        (a, ces') <- runStateT (CES.getNewEpochSizesUntilM f getSize) ces
        atomically $ do
          -- It is possible that a concurrent thread also updated the CES.
          -- It's no disaster when this happens, since this is merely a summary
          -- of otherwise non-mutable information. We just pick the larger one.
          ces'' <- readTVar cesVar
          writeTVar cesVar $ maximumBy (comparing CES.lastEpoch) [ces', ces'']
        return a

fixedSizeEpochInfo :: Monad m => EpochSize -> EpochInfo m
fixedSizeEpochInfo (EpochSize size) = EpochInfo {
      epochInfoSize = \_ ->
        return $ EpochSize size
    , epochInfoFirst = \(EpochNo epochNo) ->
        return $ SlotNo (epochNo * size)
    , epochInfoEpoch = \(SlotNo slot) ->
        return $ EpochNo (slot `div` size)
    }
