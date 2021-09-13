{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}
module Ouroboros.Consensus.HardFork.Combinator.Embed.Nary (
    Inject (..)
  , inject'
    -- * Defaults
  , injectHardForkState
  , injectNestedCtxt_
  , injectQuery
    -- * Initial 'ExtLedgerState'
  , injectInitialExtLedgerState
  ) where

import           Data.Bifunctor (first)
import           Data.Coerce (Coercible, coerce)
import           Data.SOP.Dict (Dict (..))
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation (AnnTip, HeaderState (..),
                     genesisHeaderState)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Counting (Exactly (..))
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs

{-------------------------------------------------------------------------------
  Injection for a single block into a HardForkBlock
-------------------------------------------------------------------------------}

class Inject f where
  inject ::
       forall x xs. CanHardFork xs
    => Exactly xs History.Bound
       -- ^ Start bound of each era
    -> Index xs x
    -> f x
    -> f (HardForkBlock xs)

inject' ::
     forall f a b x xs.
     ( Inject f
     , CanHardFork xs
     , Coercible a (f x)
     , Coercible b (f (HardForkBlock xs))
     )
  => Proxy f -> Exactly xs History.Bound -> Index xs x -> a -> b
inject' _ startBounds idx = coerce . inject @f startBounds idx . coerce

{-------------------------------------------------------------------------------
  Defaults (to ease implementation)
-------------------------------------------------------------------------------}

injectNestedCtxt_ ::
     forall f x xs a.
     Index xs x
  -> NestedCtxt_ x f a
  -> NestedCtxt_ (HardForkBlock xs) f a
injectNestedCtxt_ idx nc = case idx of
    IZ      -> NCZ nc
    IS idx' -> NCS (injectNestedCtxt_ idx' nc)

injectQuery ::
     forall x xs result.
     Index xs x
  -> BlockQuery x result
  -> QueryIfCurrent xs result
injectQuery idx q = case idx of
    IZ      -> QZ q
    IS idx' -> QS (injectQuery idx' q)

injectHardForkState ::
     forall f x xs.
     Exactly xs History.Bound
     -- ^ Start bound of each era
  -> Index xs x
  -> f x
  -> HardForkState f xs
injectHardForkState startBounds idx x =
    HardForkState $ go startBounds idx
  where
    go ::
         Exactly xs' History.Bound
      -> Index xs' x
      -> Telescope (K State.Past) (State.Current f) xs'
    go (ExactlyCons start _) IZ =
        TZ (State.Current { currentStart = start, currentState = x })
    go (ExactlyCons start startBounds'@(ExactlyCons nextStart _)) (IS idx') =
        TS (K State.Past { pastStart = start, pastEnd = nextStart })
           (go startBounds' idx')
    go (ExactlyCons _ ExactlyNil) (IS idx') = case idx' of {}

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Inject I where
  inject _ = injectNS' (Proxy @I)

instance Inject Header where
  inject _ = injectNS' (Proxy @Header)

instance Inject SerialisedHeader where
  inject _ idx =
        serialisedHeaderFromPair
      . first (mapSomeNestedCtxt (injectNestedCtxt_ idx))
      . serialisedHeaderToPair

instance Inject WrapHeaderHash where
  inject _ (idx :: Index xs x) =
    case dictIndexAll (Proxy @SingleEraBlock) idx of
      Dict ->
          WrapHeaderHash
        . OneEraHash
        . toShortRawHash (Proxy @x)
        . unwrapHeaderHash

instance Inject GenTx where
  inject _ = injectNS' (Proxy @GenTx)

instance Inject WrapGenTxId where
  inject _ = injectNS' (Proxy @WrapGenTxId)

instance Inject WrapApplyTxErr where
  inject _ =
      (WrapApplyTxErr . HardForkApplyTxErrFromEra)
        .: injectNS' (Proxy @WrapApplyTxErr)

instance Inject (SomeSecond BlockQuery) where
  inject _ idx (SomeSecond q) = SomeSecond (QueryIfCurrent (injectQuery idx q))

instance Inject AnnTip where
  inject _ = undistribAnnTip .: injectNS' (Proxy @AnnTip)

instance Inject LedgerState where
  inject startBounds idx =
      HardForkLedgerState . injectHardForkState startBounds idx

instance Inject WrapChainDepState where
  inject startBounds idx =
      coerce . injectHardForkState startBounds idx

instance Inject HeaderState where
  inject startBounds idx HeaderState {..} = HeaderState {
        headerStateTip      = inject startBounds idx <$> headerStateTip
      , headerStateChainDep = unwrapChainDepState
                            $ inject startBounds idx
                            $ WrapChainDepState headerStateChainDep
      }

instance Inject ExtLedgerState where
  inject startBounds idx ExtLedgerState {..} = ExtLedgerState {
        ledgerState = inject startBounds idx ledgerState
      , headerState = inject startBounds idx headerState
      }

{-------------------------------------------------------------------------------
  Initial ExtLedgerState
-------------------------------------------------------------------------------}

-- | Inject the first era's initial 'ExtLedgerState' and trigger any
-- translations that should take place in the very first slot.
--
-- Performs any hard forks scheduled via 'TriggerHardForkAtEpoch'.
--
-- Note: we can translate across multiple eras when computing the initial ledger
-- state, but we do not support translation across multiple eras in general;
-- extending 'applyChainTick' to translate across more than one era is not
-- problematic, but extending 'ledgerViewForecastAt' is a lot more subtle; see
-- @forecastNotFinal@.
injectInitialExtLedgerState ::
     forall x xs. CanHardFork (x ': xs)
  => TopLevelConfig (HardForkBlock (x ': xs))
  -> ExtLedgerState x
  -> ExtLedgerState (HardForkBlock (x ': xs))
injectInitialExtLedgerState cfg extLedgerState0 =
    ExtLedgerState {
        ledgerState = targetEraLedgerState
      , headerState = targetEraHeaderState
      }
  where
    cfgs :: NP TopLevelConfig (x ': xs)
    cfgs =
        distribTopLevelConfig
          (State.epochInfoLedger
             (configLedger cfg)
             (hardForkLedgerStatePerEra targetEraLedgerState))
          cfg

    targetEraLedgerState :: LedgerState (HardForkBlock (x ': xs))
    targetEraLedgerState =
        HardForkLedgerState $
          -- We can immediately extend it to the right slot, executing any
          -- scheduled hard forks in the first slot
          State.extendToSlot
            (configLedger cfg)
            (SlotNo 0)
            (initHardForkState (ledgerState extLedgerState0))

    firstEraChainDepState :: HardForkChainDepState (x ': xs)
    firstEraChainDepState =
        initHardForkState $
          WrapChainDepState $
            headerStateChainDep $
              headerState extLedgerState0

    targetEraChainDepState :: HardForkChainDepState (x ': xs)
    targetEraChainDepState =
        -- Align the 'ChainDepState' with the ledger state of the target era.
        State.align
          (InPairs.requiringBoth
            (hmap (WrapConsensusConfig . configConsensus) cfgs)
            (translateChainDepState hardForkEraTranslation))
          (hpure (fn_2 (\_ st -> st)))
          (hardForkLedgerStatePerEra targetEraLedgerState)
          firstEraChainDepState

    targetEraHeaderState :: HeaderState (HardForkBlock (x ': xs))
    targetEraHeaderState = genesisHeaderState targetEraChainDepState
