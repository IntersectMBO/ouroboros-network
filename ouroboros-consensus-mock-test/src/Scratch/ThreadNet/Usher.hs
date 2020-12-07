{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeFamilies               #-}

module Scratch.ThreadNet.Usher (
  -- *
  Usher (..),
  UsherBlockConfig (..),
  UsherEraConfig (..),
  UsherEvent (..),
  UsherNetConfig (..),
  WhetherIsByron (..),
  newUsher,
  wrapForgeBlock,
  -- *
  PseudoEraNo (..),
  EraSize (..),
  -- *
  UsherChainState (..),
  UsherCurrentEraState (..),
  UsherNetState (..),
  -- *
  UniversalBlockHash (..),
  -- *
  ForgeBlockType,
  ) where

import           Control.Monad (when)
import           Control.Tracer (Tracer, traceWith)
import           Data.Functor.Identity (Identity (..))
import           Data.ByteString.Short (ShortByteString)
import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word16)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import qualified Cardano.Slotting.EpochInfo as EI
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

import           Ouroboros.Consensus.Block (ChainHash (..), {- HasHeader, -} HeaderHash, blockHash)
import           Ouroboros.Consensus.HardFork.History.Util (addEpochs, addSlots)
import           Ouroboros.Consensus.Ledger.Abstract (ApplyBlock, reapplyLedgerBlock)
import           Ouroboros.Consensus.Ledger.Basics ({- GetTip, -} LedgerConfig, LedgerState, TickedLedgerState, getTipHash, getTipSlot)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike

import           Test.Util.Slots (NumSlots (..))

data Usher m = Usher
  { getUsherChainState :: !(UniversalBlockHash -> m UsherChainState)
  , putUsherChainState :: !(UniversalBlockHash -> UsherChainState -> m ())
-- TODO
--  , trimUsherChainState :: !(UniversalBlockHash -> m ())
--    -- ^ delete-all-except, for use by flusher
  }

newUsher :: IOLike.IOLike m => m (Usher m)
newUsher = do
    var <- IOLike.uncheckedNewTVarM Map.empty

    let
      getUsherChainState h = do
          sts <- IOLike.atomically $ IOLike.readTVar var
          case Map.lookup h sts of
            Nothing -> error "impossible!"
            Just x  -> pure x

      putUsherChainState h st =
          IOLike.atomically $ IOLike.modifyTVar var $ Map.insert h st

    pure Usher
      { getUsherChainState
      , putUsherChainState
      }

-- | The block hash as bytes
--
-- NOTE This is not necessarily the serialization of the hash. It's the hash
-- itself as raw bytes. The intention is for " compatible " nodes running
-- different versions to agree on these bytes, even if they disagree on the
-- block type.
newtype UniversalBlockHash = UniversalBlockHash ShortByteString
  deriving (Eq, Ord, Show)

-- | An 'UsherEraConfig' per scheduled era
data UsherNetConfig = UsherNetConfig (NE.NonEmpty UsherEraConfig)

-- | The stability window and desired size of an era
--
-- The era would /ideally/ indeed have the specified size, but it might have to
-- end later if the "Scratch.ThreadNet.Throttler" suppresses all block
-- production during the interval necessary to for the era to end promptly.

-- TODO should any of this move to 'UsherBlockConfig'?
data UsherEraConfig = UsherEraConfig
  { desiredEraSize  :: !EraSize
  , stabilityWindow :: !NumSlots
  , whetherIsByron  :: !WhetherIsByron
  }

lookupUsherEraConfig :: UsherNetConfig -> PseudoEraNo -> UsherEraConfig
lookupUsherEraConfig cfg (PseudoEraNo i) =
    eraCfgs NE.!! fromIntegral i
  where
    UsherNetConfig eraCfgs = cfg

finalEraUsherEraConfig :: UsherNetConfig -> PseudoEraNo
finalEraUsherEraConfig cfg =
    PseudoEraNo $ fromIntegral $ length eraCfgs
  where
    UsherNetConfig eraCfgs = cfg

-- | An 'UsherChainState' for /at/ /least/ each block in the network that may
-- yet be extended (ie at least every block selected by some node that can
-- lead)
--
-- NOTE For example, the usher can safely replace this state with a singleton
-- after each reset the "Scratch.ThreadNet.Throttler" injects, since there is
-- only one relevant chain immediately after a reset.
--
-- NOTE This state intentionally duplicates information that should be
-- available in the ledger state (including the hard fork combinator (HFC)
-- wrapper). We require that the ThreadNet tests fail (and especially do not
-- diverge!) even despite bugs in those components. So we deem this small
-- amount of duplication to be necessary.
newtype UsherNetState = UsherNetState (Map UniversalBlockHash UsherChainState)

data UsherChainState = UsherChainState
  { currentEra      :: !PseudoEraNo
  , currentEraState :: !UsherCurrentEraState
  }
  deriving (Show)

-- | An index into the scheduled sequence of eras
--
-- EG On mainnet, Byron is 0, Shelley is 1, Mary is 2, etc
--
-- In the tests, we also include an additional era after the last one in the
-- HFC sequence; that's why this is a pseudo-index. So if there are @n@
-- elements in the 'UsherNetConfig' sequences, then @n@ is the index of the
-- final pseudo-era.
--
-- INVARIANT This number is (TODO one less than?) the major component of the
-- protocol version during the era.
newtype PseudoEraNo = PseudoEraNo {unPseudoEraNo :: Word16}
  deriving (Enum, Eq, Generic, Ord)
  deriving Show via Quiet PseudoEraNo

-- | The number of epochs in an era
newtype EraSize = EraSize {unEraSize :: Word16}
  deriving (Enum, Eq, Generic, Ord)
  deriving Show via Quiet PseudoEraNo

-- | The state on some chain of the proposal that will end its current era
data UsherCurrentEraState
  = ByronConfirmedIn !SlotNo
    -- ^ Its current era uses the Byron ledger, and the threshold-satisfying
    -- vote for the era-ending proposal is in a transaction in a block in this
    -- slot.
    --
    -- Once enough later blocks endorse it, the proposal will become
    -- 'LockedInFor' either the epoch of the threshold-satisfying endorsement
    -- or the subsequent epoch.
    --
    -- NOTE We leave this state by forging a block at the right time.
  | LockedInFor !SlotNo !EpochNo
    -- ^ The proposal has finished advancing: a bug-free ledger will certainly
    -- adopt the era-ending proposal at the end of the given epoch.
    --
    -- NOTE We leave this state when enough time passes.
  | NotYetProposed !EpochNo
    -- ^ No progress has been made regarding ending the current era. The
    -- 'UsherNetConfig' requests that it end in the given epoch.
    --
    -- NOTE We leave this state by forging a block at the right time.
  | Done
    -- ^ We've entered the last pseudo-era. There will be no more
    -- era-transitions.
  deriving (Show)

-- | Tick the state up to the given epoch
--
-- Recall that ticking is only sound if there are no intervening blocks.
advanceStateForTimePassing ::
  UsherNetConfig -> EpochNo -> UsherChainState -> UsherChainState
advanceStateForTimePassing cfg epo ucSt = case currentEraState of
    ByronConfirmedIn{}   -> ucSt
    Done                 -> ucSt
    LockedInFor _ finalEpo ->
        if epo <= finalEpo then ucSt else UsherChainState
          { currentEra      = succ currentEra
          , currentEraState =
              case finalEraUsherEraConfig cfg `compare` succ currentEra of
                LT -> error "impossible!"
                EQ -> Done
                GT ->
                    NotYetProposed $
                    addEpochs
                       (fromIntegral $ unEraSize desiredEraSize)
                      finalEpo
          }
    NotYetProposed{}     -> ucSt
  where
    UsherChainState
      { currentEra
      , currentEraState
      } = ucSt
    UsherEraConfig
      { desiredEraSize
      } = lookupUsherEraConfig cfg currentEra

-- | The usher decides whether to inject the era-ending proposal (and votes)
-- into each block
data WhetherToInjectTransactions
  = DoInjectTransactions !EpochNo
    -- ^ the first epoch of the next era (ie what to put in the transactions)
  | DoNotInjectTransactions

-- | Byron has a different proposal evolution, so we need to know to flag it
data WhetherIsByron = IsByron | IsNotByron

-- | Compute the new state as of immediately after a block that's about to be
-- forged, and determine whether that new block should contain the era-ending
-- proposal
--
-- NOTE The caller, after forging the new block, should validate that its
-- actual era matches the expected era from the returned new state.
advanceStateForForgedBlock ::
     UsherNetConfig
  -> SlotNo
     -- ^ the slot of the block being forged
  -> EI.EpochInfo Identity
     -- ^ info for that slot's epoch
  -> UsherChainState
     -- ^ any state /after/ the block that is being extended (ie after the new
     -- block's predecessor)
     --
     -- NOTE This function ticks it up to the current slot.
  -> (UsherChainState, WhetherToInjectTransactions, UsherChainState)
advanceStateForForgedBlock cfg slot epochInfo ucSt =
    uncurry wrap $
    case currentEraState of
      ByronConfirmedIn confirmationSlot ->
          (,) DoNotInjectTransactions $
          -- TODO off by one?
          if slot < addSlots (unNumSlots stabilityWindow) confirmationSlot
          then currentEraState
          else   -- TODO comment on and also ensure Byron quorum = 1
          LockedInFor slot $ firstEpochAfter twoStabilityWindows

      Done{} -> noChange

      LockedInFor{} -> noChange

      NotYetProposed finalEpo ->
          -- Inject the proposal as soon as we're sure it can't succeed too
          -- soon.
          case whetherIsByron of
            IsByron    ->
                 -- Byron special case
                 if earliestPossibleEpo < finalEpo then noChange else 
                 ( DoInjectTransactions dummy
                 , ByronConfirmedIn slot
                 )
              where
                earliestPossibleEpo :: EpochNo
                earliestPossibleEpo =
                    -- TODO off by one?
                    (firstEpochAfter . NumSlots) $
                    1 + 3 * unNumSlots stabilityWindow

                dummy :: EpochNo
                dummy = currentEpo   -- not used by Byron

            IsNotByron ->
                if lockedInEpo < finalEpo then noChange else
                ( DoInjectTransactions lockedInEpo
                , LockedInFor slot lockedInEpo
                )
              where
                lockedInEpo :: EpochNo
                lockedInEpo = firstEpochAfter twoStabilityWindows
  where
    currentUcSt@UsherChainState
      { currentEra
      , currentEraState
      } = advanceStateForTimePassing cfg currentEpo ucSt
    UsherEraConfig
      { stabilityWindow
      , whetherIsByron
      } = lookupUsherEraConfig cfg currentEra

    -- only ticking advances the era
    wrap whetherToInject eraSt' =
        ( currentUcSt
        , whetherToInject
        , UsherChainState
          { currentEra
          , currentEraState = eraSt'
          }
        )

    twoStabilityWindows :: NumSlots
    twoStabilityWindows = NumSlots $ (* 2) $ unNumSlots stabilityWindow

    currentEpo :: EpochNo
    currentEpo = runIdentity $ EI.epochInfoEpoch epochInfo slot

    firstSlotNextEpo :: SlotNo
    firstSlotNextEpo =
        addSlots
          (unEpochSize $ runIdentity $ EI.epochInfoSize epochInfo currentEpo)
          (runIdentity $ EI.epochInfoFirst epochInfo currentEpo)

    noChange :: (WhetherToInjectTransactions, UsherCurrentEraState)
    noChange = (DoNotInjectTransactions, currentEraState)

    -- a proposal finalized (Byron: endorsed, Shelley: forged) in this slot
    -- will be adopted at the first epoch transition that happens at least two
    -- `stabilityWindow`-many slots later, _no matter what_
    firstEpochAfter :: NumSlots -> EpochNo
    firstEpochAfter x =
        if addSlots (unNumSlots x) slot < firstSlotNextEpo
        then currentEpo
        else succ currentEpo

type ForgeBlockType m blk cfg bno isLeader =
     cfg
  -> bno
  -> SlotNo
  -> TickedLedgerState blk
  -> [GenTx blk]
  -> isLeader
  -> m blk

-- TODO should this record be altered to instead simply contain an 'PseudoEraNo'?
data UsherBlockConfig blk txin = UsherBlockConfig
  { blockEra                :: blk -> PseudoEraNo
    -- ^ TODO cf
    -- 'Ouroboros.Consensus.HardFork.Combinator.AcrossEras.OneEraHeader'
    --
    -- NOTE For all but the final era, 'blockEra' and 'ledgerEra' should
    -- increment at the same time, specifically when our 'UsherCurrentEraState'
    -- says it should. For the final era, 'blockEra' won't increment.
  , getEpochInfo            :: TickedLedgerState blk -> EI.EpochInfo Identity
  , mkEraEndingTransactions ::
      PseudoEraNo -> EpochNo -> txin -> NE.NonEmpty (GenTx blk)
    -- ^ The 'PseudoEraNo' determines the new protocol version number.
  , tickedLedgerEra         :: TickedLedgerState blk -> PseudoEraNo
  , toUniversalBlockHash    :: HeaderHash blk -> UniversalBlockHash
  , toUsherTxIn             :: TickedLedgerState blk -> txin
    -- ^ We trust the ledger state's UTxO, since bugs there would not cause
    -- non-termination.
    --
    -- The @txin@ type should be 'Shelley.Spec.Ledger.API.TxIn', for example.
    -- It can be 'Data.Void.Void' if the transactions do require inputs (eg
    -- Byron). No existing type family abstracts this. If the ledger has no
    -- update mechanism, eg the mock ledger, then @wrapForgeBlock@ should not
    -- even be applied.
    --
    -- NOTE For example, this function scans the UTxOs to find one that the
    -- usher can consume in the new Shelley transaction.
  }

wrapForgeBlock ::
     ( {- GetTip (TickedLedgerState blk)
     , HasHeader blk -}
       ApplyBlock (LedgerState blk) blk
     , Monad m
     , Show (TickedLedgerState blk)
     )
  => LedgerConfig blk
  -> UsherBlockConfig blk txin
  -> Tracer m (UsherEvent (TickedLedgerState blk) (LedgerState blk))
  -> UsherNetConfig
  -> Usher m
  -> ForgeBlockType m blk cfg bno isLeader
  -> ForgeBlockType m blk cfg bno isLeader
wrapForgeBlock lcfg blkCfg tracer netCfg usher underlying cfg bno slot tlSt txs proof = do

    let epochInfo  = getEpochInfo tlSt
        currentEpo = runIdentity $ EI.epochInfoEpoch epochInfo slot
          
    -- We do rely on the ledger state to correctly identify the previous block.
    -- That'd be such a fundamental error that it should be caught elsewhere.
    -- Morever, these tests would still terminate.
    stateIn <- case getTipHash tlSt of
      BlockHash h -> get $ toUniversalBlockHash h
      GenesisHash -> do
          let UsherEraConfig
                { desiredEraSize
                } = lookupUsherEraConfig netCfg firstEra
          pure UsherChainState
            { currentEra      = firstEra
            , currentEraState =
                NotYetProposed $
                EpochNo $ fromIntegral $ pred $ unEraSize desiredEraSize
            }

    let (stateIn', whetherToInject, stateOut) =
            advanceStateForForgedBlock
              netCfg
              slot
              epochInfo
              -- TODO do not trust the EpochInfo from the TickedLedgerState.
              -- The logic there is comparatively subtle, so we should test it.
              -- EG We can separately maintain an EpochInfo in the
              -- UsherChainState.
              stateIn
        UsherChainState
          { currentEra
          } = stateIn'
        txs' = case whetherToInject of
            DoNotInjectTransactions          -> txs
            DoInjectTransactions lockedInEpo ->
                toList $
                mkEraEndingTransactions currentEra lockedInEpo (toUsherTxIn tlSt)
                -- TODO include largest txs prefix that also fits in the block
                -- alongside the injected txs
                --
                -- TODO any reason to report the txs suffix we ignore?
 
    when (finalEra == currentEra) $ traceWith tracer UsherForgedInFinalEra

    () <- do
      let expected = currentEra
          actual   = tickedLedgerEra tlSt
      when (actual /= expected) $
        let tipSlot = getTipSlot tlSt in
        error $
          "the ledger state ticked to " <> show slot <> " is in era "
          <> show actual <> " instead of the expected " <> show expected <> "\n"
          <> show currentEpo <> "\n"
          <> show (tipSlot, fmap (runIdentity . EI.epochInfoEpoch epochInfo) $ tipSlot) <> "\n"
          <> show tlSt <> "\n"
          <> show stateIn <> show stateIn' <> show stateOut

    blk <- underlying cfg bno slot tlSt txs' proof
    let hsh  = blockHash blk
        uhsh = toUniversalBlockHash hsh
    put uhsh stateOut

    traceWith tracer $ UsherForged
      currentEra
      currentEpo
      slot
      uhsh
      (Just tlSt)
      (Just (reapplyLedgerBlock lcfg blk tlSt))

    case whetherToInject of
      DoNotInjectTransactions{}        -> pure ()
      DoInjectTransactions lockedInEpo ->
          traceWith tracer $
            UsherInjectedTxs currentEra currentEpo slot uhsh lockedInEpo

    () <- do
      let expected = if currentEra == finalEra then pred currentEra else currentEra
          actual   = blockEra blk
      when (actual /= expected) $
        error $
          "forged the block " <> show hsh <> " with era tag "
          <> show actual <> " instead of the expected " <> show expected

    traceWith tracer UsherDone

    pure blk

  where
    UsherNetConfig eraCfgs = netCfg
    UsherBlockConfig
      { blockEra
      , getEpochInfo
      , mkEraEndingTransactions
      , tickedLedgerEra
      , toUniversalBlockHash
      , toUsherTxIn
      } = blkCfg
    Usher get put = usher

    -- We allow for the first block to skip over some eras. We assume the
    -- initial ledger state has been accordingly advanced.
    firstEra :: PseudoEraNo
    firstEra =
        (PseudoEraNo . fromIntegral) $
        length $ takeWhile isEmpty $ toList eraCfgs
      where
        isEmpty = (== EraSize 0) . desiredEraSize

    -- This is the era that isn't an actual era, it's merely an additional
    -- protocol version increment beyond the previous era.
    finalEra :: PseudoEraNo
    finalEra = finalEraUsherEraConfig netCfg

data UsherEvent tlst lst
  = UsherForgedInFinalEra
  | UsherDone
  | UsherInjectedTxs PseudoEraNo EpochNo SlotNo UniversalBlockHash EpochNo
  | UsherForged PseudoEraNo EpochNo SlotNo UniversalBlockHash (Maybe tlst) (Maybe lst)
  deriving (Show)
