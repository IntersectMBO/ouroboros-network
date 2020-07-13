{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'TipRegistry' keeps chains of tips of warm peers and allows to extract
-- which peers offer headers from our chain the earliest.
--
module Ouroboros.Network.TipSample.TipRegistry
  ( TipRegistryArguments (..)
  , TipRegistry (..)
  , TipFragmentVar (..)
  , NumberOfWins (..)
  , NumberOfPeers (..)
  , makeTipRegistry
  , TipRegistryTrace (..)
  ) where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime (Time)

import           Control.Tracer (Tracer, traceWith)

import           Data.Bifunctor (bimap)
import           Data.Functor (($>))
import           Data.List (sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down (..))

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))

import           Ouroboros.Network.Block ( HasHeader
                                         , ChainHash (BlockHash)
                                         , blockHash
                                         , blockNo
                                         , blockSlot
                                         , getTipSlotNo
                                         , getTipBlockNo
                                         , getTipPoint
                                         , pointHash )
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.ChainFragment as CF

import           Ouroboros.Network.TipSample.TipFragment ( TipFragment
                                                         , TipFragment' (Empty)
                                                         , Timed (..) )
import qualified Ouroboros.Network.TipSample.TipFragment as TF


-- TipFragmentVar
newtype TipFragmentVar peerAddr m header =
    TipFragmentVar
      -- modifyTipFragment
      { modifyTipFragment
          :: forall a.
             (TipFragment header -> (a, TipFragment header))
          -> STM m a
      }


newtype NumberOfWins = NumberOfWins Int
  deriving stock (Show, Eq)
  deriving Ord  via (Down Int)
  deriving Num  via Int
  deriving Enum via Int


newtype NumberOfPeers = NumberOfPeers Int
  deriving stock (Show, Eq)
  deriving Ord  via (Down Int)
  deriving Num  via Int
  deriving Enum via Int


data TipRegistry peerAddr header m = TipRegistry {
    -- | Returns an ordered list of peers which offered the most tips earliest.
    -- The 'NumberOfPeers' is the overall number of all peers that the peer won
    -- against across all slots.
    --
    getPeerResults :: m [(NumberOfWins, NumberOfPeers, peerAddr)],

    -- | Returns an action which allows to modify 'TipFragment' for the given
    -- peer in an 'STM' transaction.
    --
    registerPeer   :: peerAddr -> m (TipFragmentVar peerAddr m header),

    -- | Removes a peer from 'TipRegistry' internal state.
    --
    unregisterPeer :: peerAddr -> m ()
  }


-- | Internal state of 'TipRegistry'; we keep each peer 'TipFragment' in
-- a separate 'StrictTVar' to avoid synchronisation between threads.
--
newtype TipRegistryState peerAddr header m = TipRegistryState {
    tipFragmentsVar :: StrictTVar m (Map peerAddr (StrictTVar m (TipFragment header)))
  }

data TipRegistryArguments peerAddr header m = TipRegistryArguments {
      -- | Offset from the tip of the current chain.  This allows us to ignore
      -- a few most recent headers.
      traChainOffset     :: Int,

      -- | Get current chain.
      --
      traGetCurrentChain :: STM m (AnchoredFragment header),

      -- | Tracer.
      traTracer          :: Tracer m (TipRegistryTrace peerAddr header)
    }


-- | 'TipRegistry'; it keeps track of the 'TipFragment's of registered peers
-- and allows to compute results for the current chain.  The results are not
-- cached as the chain might change (we are using the volatile part of the
-- chain).
--
makeTipRegistry :: forall peerAddr header m.
                   ( MonadSTM m
                   , HasHeader header
                   , Ord peerAddr
                   )
                => TipRegistryArguments peerAddr header m
                -> m (TipRegistry       peerAddr header m)
makeTipRegistry TipRegistryArguments { traChainOffset
                                     , traGetCurrentChain
                                     , traTracer } =
    (makeTipRegistryImpl . TipRegistryState) <$> newTVarIO Map.empty
  where
    makeTipRegistryImpl :: TipRegistryState peerAddr header m
                        -> TipRegistry      peerAddr header m
    makeTipRegistryImpl trs = TipRegistry {
        getPeerResults = getPeerResultsImpl trs,
        registerPeer   = registerPeerImpl   trs,
        unregisterPeer = unregisterPeerImpl trs
      }

    -- summarise results; we ignore all the slots in which there was only one
    -- peer.
    summariseResults :: Map SlotNo (NumberOfPeers, peerAddr)
                     -> [(NumberOfWins, NumberOfPeers, peerAddr)]

    summariseResults =
          sort
        . map (\(a, (b, c)) -> (b, c, a))
        . Map.assocs
        . Map.foldl' go Map.empty
      where
        go :: Map peerAddr (NumberOfWins, NumberOfPeers)
           -> (NumberOfPeers, peerAddr)
           -> Map peerAddr (NumberOfWins, NumberOfPeers)
        go acc (numberOfPeers, peerAddr)
          | numberOfPeers <= NumberOfPeers 1
          = acc
          | otherwise
          = Map.alter (Just . maybe (NumberOfWins 1, numberOfPeers)
                                    (bimap succ (+ numberOfPeers)))
                      peerAddr acc


    getPeerResultsImpl :: TipRegistryState peerAddr header m
                       -> m [(NumberOfWins, NumberOfPeers, peerAddr)]

    getPeerResultsImpl TipRegistryState { tipFragmentsVar } = do
        currentChain <- AF.dropNewest traChainOffset <$> atomically traGetCurrentChain
        atomically $ do
          (peersMap :: Map peerAddr (TipFragment header)) <-
            readTVar tipFragmentsVar >>= traverse readTVar

          pure $! summariseResults (winningPeers currentChain peersMap)


    -- create and register peer's `TipFragment`'s `TVar`.
    registerPeerImpl :: TipRegistryState peerAddr header m
                     -> peerAddr
                     -> m (TipFragmentVar peerAddr m header)

    registerPeerImpl TipRegistryState { tipFragmentsVar } peerAddr = do
      k <- atomically $ do
        tf <- newTVar Empty
        modifyTVar tipFragmentsVar (Map.insert peerAddr tf)
        pure $ TipFragmentVar $ \f -> do
          (a, x) <- f <$> readTVar tf
          writeTVar tf x $> a
      traceWith traTracer (TipRegistryRegisteredPeer peerAddr)
      pure k


    -- remove peer's `TipFragment`'s `TVar` from the registry
    unregisterPeerImpl :: TipRegistryState peerAddr header m
                       -> peerAddr -> m ()

    unregisterPeerImpl TipRegistryState { tipFragmentsVar } peerAddr = do
      atomically $ modifyTVar tipFragmentsVar (Map.delete peerAddr)
      traceWith traTracer (TipRegistryUnregisteredPeer peerAddr)


--
-- Operations needed by the client application.
--


-- | Find winning peers. Traverse the current chain and for each header find
-- a winning peer; accumulate results.
--
-- This relies on the monotonicity of 'SlotNo's in 'TipFragment's, which is
-- guarateed by 'tipSampleClient'. 
--
winningPeers
    :: forall header peerAddr.
       HasHeader header
    => AnchoredFragment header
    -- ^ current chain
    -> Map peerAddr (TipFragment header)
    -- ^ peer 'TipFragment'.  It may contain non valid 'Tip's.
    -- use points!
    -> Map SlotNo (NumberOfPeers, peerAddr)
winningPeers currentChain0 tips0 =
    fst $ CF.foldChainFragment go (Map.empty, tips0)
                                  (AF.unanchorFragment currentChain0)
  where
    -- The outer loop traversing the chain; we thread peers map through the
    -- computation: we truncate peers 'TipFragment's as we go.  Note:
    -- 'CF.foldChainFragment' is a left fold, so we traverse the chain from
    -- left (old) to right (new) headers, as we go we truncate the
    -- 'TipFragment's (which improves computational complexity).
    go :: (Map SlotNo (NumberOfPeers, peerAddr), Map peerAddr (TipFragment header))
       -> header
       -> (Map SlotNo (NumberOfPeers, peerAddr), Map peerAddr (TipFragment header))
    go (acc, peersMap) header =
      case takeRow header peersMap of
        ( Just (_, !noPeers, !peerAddr), peersMap' ) ->
          ( Map.insert (blockSlot header) (noPeers, peerAddr) acc
          , peersMap'
          )

        ( Nothing, peersMap' ) -> (acc, peersMap')

    -- The innter loop which is traversing all 'TipFragment's; it finds the
    -- winning peer in a given slot and returns the winner and truncated
    -- 'TipFragment's.  It only accounts valid 'Tip's.
    takeRow :: header
            -> Map peerAddr (TipFragment header)
            -> ( Maybe (Time, NumberOfPeers, peerAddr)
               , Map peerAddr (TipFragment header)
               )
    takeRow header = Map.mapAccumWithKey goTakeRow Nothing
      where
        blockSlotNo = blockSlot header
        goTakeRow :: Maybe (Time, NumberOfPeers, peerAddr)
                  -> peerAddr
                  -> TipFragment header
                  -> (Maybe (Time, NumberOfPeers, peerAddr), TipFragment header)
        goTakeRow mr peerAddr tf =
          -- we find the newest most block with the given 'SlotNo'
          case TF.lookupBySlotNo (At blockSlotNo) tf of
            Just (Timed t tip, tf')
              -- validate the tip
              | getTipSlotNo  tip == At blockSlotNo
              , getTipBlockNo tip == At (blockNo header)
              , pointHash (getTipPoint tip) == BlockHash (blockHash header)
              -> case mr of
                  Just (t', !noPeers', peerAddr')
                    | t' <= t
                    -> (Just (t', succ noPeers',   peerAddr'), tf')

                    | otherwise
                    -> (Just (t,  succ noPeers',   peerAddr),  tf')

                  Nothing
                    -> (Just (t,  NumberOfPeers 1, peerAddr),  tf')

              -- there might be multiple headers with the same slot, if the
              -- `Tip` has greater `blockNo` than the 'header', preserve it.
              | getTipSlotNo tip == At blockSlotNo
              , getTipBlockNo tip > At (blockNo header)
              -> (mr, tf)

              | otherwise
              -> (mr, tf')

            Nothing -> (mr, tf)


--
-- Trace
--


data TipRegistryTrace peerAddr header =
    TipRegistryRegisteredPeer   !peerAddr
  | TipRegistryUnregisteredPeer !peerAddr
  deriving (Eq, Show)

