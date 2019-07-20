{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Ouroboros.Consensus.Protocol.HardFork (
    CanHardFork (..)
  , Forked (..)
  , forked
  , unsafeBeforeFork
  , unsafeAfterFork
  , HardForkSupportedHeader (..)
  , BeforeMaybeT (..)
  , runBeforeMaybeT
  , AfterMaybeT (..)
  , runAfterMaybeT
  , AfterForkChainState (..)
  , encodeAfterForkChainState
  , HardForksTo
  , NodeConfig (..)
  ) where

import           Control.Monad (join)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Except (throwE, withExcept)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Crypto.Random (MonadRandom (..))
import           Data.Bifunctor (Bifunctor (..))
import           Data.Kind (Type)
import           Data.Maybe (fromJust, isJust)
import           GHC.Stack (HasCallStack)

import           Cardano.Binary (Encoding, ToCBOR (..), encodeListLen)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import           Ouroboros.Network.Point (at)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense

-- These should move to another module with the specific CanHardFork definition for PBFT and Praos
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.Praos
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT hiding (pbftParams)
import           Ouroboros.Consensus.Protocol.Praos

import Debug.Trace

-- | Protocols implementing this class will give up control to a new protocol
--   when the 'shouldHardFork' becomes true
class (OuroborosTag p1, OuroborosTag p2) => CanHardFork p1 p2 where

  hardForksAtView :: LedgerView p1 -> Maybe SlotNo

  -- | Convert the 'NodeState' from the old protocol to the new at the boundary
  translateNodeStateForHardFork
    :: NodeConfig (p1 `HardForksTo` p2)
    -> NodeState p1
    -> NodeState p2

  -- | Convert the 'ChainState' from the old protocol to the new at the boundary
  translateChainStateForHardFork :: ChainState p1 -> ChainState p2

  -- | Convert the 'LedgerView' from the old protocol to the new at the boundary
  translateLedgerViewForHardFork :: LedgerView p1 -> LedgerView p2

  -- translateChainHash ::

  -- | A custom 'preferCandidate' function to handle transition between
  --   protocols. We use this because it is tricky to define a generic version
  --   for any two protocols.
  hardForkPreferCandidate
    :: HasHeader hdr
    => NodeConfig (p1 `HardForksTo` p2)
    -> AnchoredFragment hdr
    -- ^ Our chain
    -> AnchoredFragment hdr
    -- ^ Candidate
    -> Bool

  -- | A custom 'compareCandidates' function to handle transition between
  --   protocols. We use this because it is tricky to define a generic version
  --   for any two protocols.
  hardForkCompareCandidates
    :: HasHeader hdr
    => NodeConfig (p1 `HardForksTo` p2)
    -> AnchoredFragment hdr
    -> AnchoredFragment hdr
    -> Ordering


-- | A sum type to represent values before and after a hard fork
data Forked a b = BeforeFork a | AfterFork b
  deriving (Eq, Ord, Show)

instance Bifunctor Forked where
  bimap f g = forked (BeforeFork . f) (AfterFork . g)

instance (Condense a, Condense b) => Condense (Forked a b) where
  condense (BeforeFork a) = condense a
  condense (AfterFork b) = condense b

forked :: (a -> c) -> (b -> c) -> Forked a b -> c
forked f g = \case
  BeforeFork a -> f a
  AfterFork b -> g b

unsafeBeforeFork :: HasCallStack => Forked a b -> a
unsafeBeforeFork (BeforeFork a) = a
unsafeBeforeFork _ = error "unsafeBeforeFork: Got AfterFork"

unsafeAfterFork :: HasCallStack => Forked a b -> b
unsafeAfterFork (AfterFork b) = b
unsafeAfterFork _ = error "unsafeAfterFork: Got BeforeFork"


class
  ( SupportedHeader p1 (ForkedBefore hdr)
  , SupportedHeader p2 (ForkedAfter hdr)
  , HasHeader (ForkedAfter hdr)
  )
  => HardForkSupportedHeader p1 p2 hdr where

  type ForkedBefore hdr :: Type

  type ForkedAfter hdr :: Type

  getForkedHeader :: hdr -> Forked (ForkedBefore hdr) (ForkedAfter hdr)


-- | This newtype around 'MaybeT' is used to get around the functional
--   dependency required by 'HasNodeState'
newtype BeforeMaybeT m a
  = BeforeMaybeT (MaybeT m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance MonadRandom m => MonadRandom (BeforeMaybeT m) where
  getRandomBytes = lift . getRandomBytes

runBeforeMaybeT :: BeforeMaybeT m a -> m (Maybe a)
runBeforeMaybeT (BeforeMaybeT m) = runMaybeT m

instance
  HasNodeState_ (Forked p1 p2) m
  => HasNodeState_ p1 (BeforeMaybeT m) where

  getNodeState = lift getNodeState >>= \case
    BeforeFork x -> return x
    AfterFork _ -> BeforeMaybeT (MaybeT (pure Nothing))

  putNodeState = lift . putNodeState . BeforeFork


-- | This newtype around 'MaybeT' is used to get around the functional
--   dependency required by 'HasNodeState'
newtype AfterMaybeT m a
  = AfterMaybeT (MaybeT m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance MonadRandom m => MonadRandom (AfterMaybeT m) where
  getRandomBytes = lift . getRandomBytes

runAfterMaybeT :: AfterMaybeT m a -> m (Maybe a)
runAfterMaybeT (AfterMaybeT m) = runMaybeT m

instance
  HasNodeState_ (Forked p1 p2) m
  => HasNodeState_ p2 (AfterMaybeT m) where

  getNodeState = lift getNodeState >>= \case
    BeforeFork _ -> AfterMaybeT (MaybeT (pure Nothing))
    AfterFork x -> return x

  putNodeState = lift . putNodeState . AfterFork


-- | After the fork we need to keep track of when we forked and keep a snapshot
--   of the old era 'ChainState' for k blocks
data AfterForkChainState p1 p2
  = AfterForkChainState
      { forkSlotNo :: SlotNo
      -- ^ The slot that we hard-forked at
      , snapshotAtFork :: Maybe (ChainState p1)
      -- ^ A snapshot of the old era chain state taken at the fork. It is
      -- 'Nothing' after k blocks, because we can no longer roll back far enough
      -- to need it
      , afterForkChainState :: ChainState p2
      -- ^ The 'ChainState' of the new era protocol
      }

deriving instance
  (Show (ChainState p1), Show (ChainState p2))
  => Show (AfterForkChainState p1 p2)

encodeAfterForkChainState
  :: (ChainState p1 -> Encoding)
  -> (ChainState p2 -> Encoding)
  -> AfterForkChainState p1 p2
  -> Encoding
encodeAfterForkChainState _encodeBeforeFork encodeAfterFork chainState =
  encodeListLen 3 <>
    toCBOR (forkSlotNo chainState) <>
    undefined <>
    encodeAfterFork (afterForkChainState chainState)


data ForkedValidationErr p1 p2
  = StateMismatch
  | NewHeaderBeforeFork
  | OldHeaderAfterFork
  | BeforeForkValidationErr (ValidationErr p1)
  | AfterForkValidationErr (ValidationErr p2)

deriving instance
  (Show (ValidationErr p1), Show (ValidationErr p2))
  => Show (ForkedValidationErr p1 p2)


-- | A protocol that acts as @p1@ until a hard fork switches to @p2@
data HardForksTo p1 p2

instance CanHardFork p1 p2 => OuroborosTag (p1 `HardForksTo` p2) where

  -- Store configuration for both protocols
  data NodeConfig (p1 `HardForksTo` p2)
    = ForkedNodeConfig
        { nodeConfigBeforeFork :: NodeConfig p1
        , nodeConfigAfterFork :: NodeConfig p2
        }

  type NodeState (p1 `HardForksTo` p2) = Forked (NodeState p1) (NodeState p2)

  -- Before the fork store @p1@ state, but after we require extra state
  type ChainState (p1 `HardForksTo` p2)
    = Forked (ChainState p1) (AfterForkChainState p1 p2)

  type IsLeader (p1 `HardForksTo` p2) = Forked (IsLeader p1) (IsLeader p2)

  type LedgerView (p1 `HardForksTo` p2) = Forked (LedgerView p1) (LedgerView p2)

  type ValidationErr (p1 `HardForksTo` p2) = ForkedValidationErr p1 p2

  type SupportedHeader (p1 `HardForksTo` p2) = HardForkSupportedHeader p1 p2

  -- Use the custom implementation for this pair of protocols
  preferCandidate = hardForkPreferCandidate

  -- Use the custom implementation for this pair of protocols
  compareCandidates = hardForkCompareCandidates

  -- Call into the 'checkIsLeader' functions for the two underlying protocols
  checkIsLeader nodeConfig slotNo forkedLedgerView forkedChainState =
    case (forkedLedgerView, forkedChainState) of
      -- Before the fork simply act as @p1@
      (BeforeFork ledgerView, BeforeFork chainState) -> do
        let hardForkSlot = hardForksAtView @p1 @p2 ledgerView
        if isJust hardForkSlot && Just slotNo >= hardForkSlot
        then do
          traceM "Acting as after hard fork, but haven't forked state"
          getNodeState >>= \case
            BeforeFork nodeState -> putNodeState . AfterFork $
             translateNodeStateForHardFork nodeConfig nodeState
            AfterFork _ -> pure () -- TODO: Error?
          checkIsLeader
            nodeConfig
            slotNo
            (AfterFork $ translateLedgerViewForHardFork @p1 @p2 ledgerView)
            ( AfterFork $ AfterForkChainState
              { forkSlotNo = fromJust hardForkSlot -- TODO: Remove fromJust?
              , snapshotAtFork = Just chainState
              , afterForkChainState = translateChainStateForHardFork @p1 @p2 chainState
              }
            )
        else
          fmap (fmap BeforeFork . join) . runBeforeMaybeT $
            checkIsLeader
              (nodeConfigBeforeFork nodeConfig)
              slotNo
              ledgerView
              chainState
      -- After the fork simply act as @p2@
      (AfterFork ledgerView, AfterFork chainState) ->
        fmap (fmap AfterFork . join) . runAfterMaybeT $
          checkIsLeader
            (nodeConfigAfterFork nodeConfig)
            slotNo
            ledgerView
            (afterForkChainState chainState)
      _ -> pure Nothing -- TODO: Is it okay to silently fail here or should we error?

  -- Apply a header to the chain state.
  --
  -- Before or after the fork we act as the corresponding protocol.
  --
  -- During the fork, we take a snapshot of the chain state, which needs to be
  -- kept for k blocks.
  applyChainState nodeConfig forkedLedgerView forkedHeader forkedChainState =
    case ( forkedLedgerView
         , getForkedHeader @p1 @p2 forkedHeader
         , forkedChainState
         ) of
      -- Before the fork simply act as @p1@
      (BeforeFork ledgerView, BeforeFork header, BeforeFork chainState) ->
        withExcept BeforeForkValidationErr . fmap BeforeFork $
          applyChainState
            (nodeConfigBeforeFork nodeConfig)
            ledgerView
            header
            chainState
      -- We have received a new era 'LedgerView' and 'Header', but an old era
      -- 'ChainState'. This means we should be in the process of forking and
      -- should translate the 'ChainState', however we should make sure this is
      -- the case.
      (AfterFork ledgerView, AfterFork header, BeforeFork chainState) -> do
        chainState' <- withExcept AfterForkValidationErr $
          applyChainState
            (nodeConfigAfterFork nodeConfig)
            ledgerView
            header
            (translateChainStateForHardFork @p1 @p2 chainState)
        return . AfterFork $ AfterForkChainState
          { forkSlotNo = blockSlot header
          , snapshotAtFork = Just chainState
          , afterForkChainState = chainState'
          }
      -- After the fork simply act as @p2@
      -- TODO: Figure out when to delete the snapshot from the chain state
      (AfterFork ledgerView, AfterFork header, AfterFork chainState) -> do
        chainState' <- withExcept AfterForkValidationErr $
          applyChainState
            (nodeConfigAfterFork nodeConfig)
            ledgerView
            header
            (afterForkChainState chainState)
        return . AfterFork $ chainState { afterForkChainState = chainState' }
      (BeforeFork _ledgerView, BeforeFork _, AfterFork _chainState) ->
        throwE StateMismatch
      (BeforeFork _ledgerView, AfterFork _header, _) ->
        throwE NewHeaderBeforeFork
      (AfterFork _ledgerView, BeforeFork _header, _) ->
        throwE OldHeaderAfterFork

  -- TODO: This will have to change signature to take into account the fork
  protocolSecurityParam nodeConfig = protocolSecurityParam (nodeConfigBeforeFork nodeConfig)

  rewindChainState
    ForkedNodeConfig {nodeConfigBeforeFork, nodeConfigAfterFork}
    forkedChainState
    originOrSlotNo =
      case forkedChainState of
        -- Before the fork simply act as @p1@
        BeforeFork chainState ->
          BeforeFork <$>
            rewindChainState nodeConfigBeforeFork chainState originOrSlotNo
        -- After the fork, check if we cross the fork boundary
        AfterFork chainState -> if at (forkSlotNo chainState) < originOrSlotNo
          -- If we don't cross the boundary, act as @p2@
          then do
            chainState' <-
              rewindChainState
                nodeConfigAfterFork
                (afterForkChainState chainState)
                originOrSlotNo
            return . AfterFork $ chainState { afterForkChainState = chainState' }
          -- If we do cross the boundary, then restore the snapshot and act as
          -- @p1@.
          --
          -- Note: If the snapshot is gone, then we must be k past the boundary
          -- and can never rewind to @p1@
          else case snapshotAtFork chainState of
            Nothing -> Nothing
            Just chainStateSnapshot ->
              BeforeFork <$>
                rewindChainState nodeConfigBeforeFork chainStateSnapshot originOrSlotNo


instance
  CanHardFork
    (ExtNodeConfig (PBftLedgerView PBftMockCrypto) (PBft PBftMockCrypto))
    (ExtNodeConfig AddrDist (Praos PraosMockCrypto)) where

  hardForksAtView _ = Just $ SlotNo 5

  translateNodeStateForHardFork ForkedNodeConfig {nodeConfigAfterFork} _ =
    pInfoInitState (protocolInfoPraos (NumCoreNodes 7) (CoreNodeId nid) params)
    where
      CoreId nid = praosNodeId $ encNodeConfigP nodeConfigAfterFork
      params = praosParams $ encNodeConfigP nodeConfigAfterFork


  translateChainStateForHardFork _ = []

  translateLedgerViewForHardFork = undefined

  -- TODO: Take into account the fork
  hardForkPreferCandidate nodeConfig =
    preferCandidate
      @(ExtNodeConfig (PBftLedgerView PBftMockCrypto) (PBft PBftMockCrypto))
      (nodeConfigBeforeFork nodeConfig)

  -- TODO: Take into account the fork
  hardForkCompareCandidates nodeConfig =
    compareCandidates
      @(ExtNodeConfig (PBftLedgerView PBftMockCrypto) (PBft PBftMockCrypto))
      (nodeConfigBeforeFork nodeConfig)
