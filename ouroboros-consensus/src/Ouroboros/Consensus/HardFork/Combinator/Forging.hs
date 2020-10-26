{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Ouroboros.Consensus.HardFork.Combinator.Forging (
    HardForkCannotForge
  , hardForkBlockForging
  , HardForkForgeStateInfo
  , HardForkForgeStateUpdateError
  ) where

import           Data.Functor.Product
import           Data.SOP.BasicFunctors
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger (Ticked (..))
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Protocol
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

-- | If we cannot forge, it's because the current era could not forge
type HardForkCannotForge xs = OneEraCannotForge xs

type instance CannotForge (HardForkBlock xs) = HardForkCannotForge xs

-- | For each era in which we want to forge blocks, we have a 'BlockForging',
-- and thus 'ForgeStateInfo'.
type HardForkForgeStateInfo xs = OneEraForgeStateInfo xs

type instance ForgeStateInfo (HardForkBlock xs) = HardForkForgeStateInfo xs

-- | For each era in which we want to forge blocks, we have a 'BlockForging',
-- and thus 'ForgeStateUpdateError'.
type HardForkForgeStateUpdateError xs = OneEraForgeStateUpdateError xs

type instance ForgeStateUpdateError (HardForkBlock xs) =
  HardForkForgeStateUpdateError xs

hardForkBlockForging ::
     forall m xs. (CanHardFork xs, Monad m)
  => NS (BlockForging m) xs
  -> BlockForging m (HardForkBlock xs)
hardForkBlockForging blockForging =
    BlockForging {
        forgeLabel       = hcollapse $ hmap (K . forgeLabel) blockForging
      , canBeLeader      = hardForkCanBeLeader               blockForging
      , updateForgeState = hardForkUpdateForgeState          blockForging
      , checkCanForge    = hardForkCheckCanForge             blockForging
      , forgeBlock       = hardForkForgeBlock                blockForging
      }

hardForkCanBeLeader ::
     CanHardFork xs
  => NS (BlockForging m) xs -> HardForkCanBeLeader xs
hardForkCanBeLeader =
    OneEraCanBeLeader . hmap (WrapCanBeLeader . canBeLeader)

-- | POSTCONDITION: the returned 'ForgeStateUpdateInfo' is from the same era as
-- the given 'NS' of 'BlockForging's.
hardForkUpdateForgeState ::
     forall m xs. (CanHardFork xs, Monad m)
  => NS (BlockForging m) xs
  -> SlotNo
  -> m (ForgeStateUpdateInfo (HardForkBlock xs))
hardForkUpdateForgeState blockForging curSlot =
    undistrib <$> htraverse' (flip updateForgeState curSlot) blockForging
  where
    undistrib ::
         NS ForgeStateUpdateInfo xs
      -> ForgeStateUpdateInfo (HardForkBlock xs)
    undistrib = hcollapse . hzipWith3 inj injections injections
      where
        inj :: forall blk.
               Injection WrapForgeStateInfo xs blk
            -> Injection WrapForgeStateUpdateError xs blk
            -> ForgeStateUpdateInfo blk
            -> K (ForgeStateUpdateInfo (HardForkBlock xs)) blk
        inj injInfo injUpdateError forgeStateUpdateInfo =
            K $ ForgeStateUpdateInfo $
              case getForgeStateUpdateInfo forgeStateUpdateInfo of
                Updated      info -> Updated      $ injInfo'        info
                Unchanged    info -> Unchanged    $ injInfo'        info
                UpdateFailed err  -> UpdateFailed $ injUpdateError' err
          where
            injInfo' ::
                 ForgeStateInfo blk
              -> OneEraForgeStateInfo xs
            injInfo' =
                  OneEraForgeStateInfo
                . unK
                . apFn injInfo
                . WrapForgeStateInfo

            injUpdateError' ::
                 ForgeStateUpdateError blk
              -> OneEraForgeStateUpdateError xs
            injUpdateError' =
                  OneEraForgeStateUpdateError
                . unK
                . apFn injUpdateError
                . WrapForgeStateUpdateError

-- | PRECONDITION: the 'NS' of 'BlockForging's, the ticked 'ChainDepState', the
-- 'HardForkIsLeader', and the 'HardForkStateInfo' are all from the same era.
--
-- This follows from the postconditions of 'check' and
-- 'hardForkUpdateForgeState'.
hardForkCheckCanForge ::
     forall m xs. CanHardFork xs
  => NS (BlockForging m) xs
  -> TopLevelConfig (HardForkBlock xs)
  -> SlotNo
  -> Ticked (HardForkChainDepState xs)
  -> HardForkIsLeader xs
  -> HardForkForgeStateInfo xs
  -> Either (HardForkCannotForge xs) ()
hardForkCheckCanForge blockForging
                      cfg
                      curSlot
                      (TickedHardForkChainDepState chainDepState ei)
                      isLeader
                      forgeStateInfo =
    distrib
      $ hzipWith
          checkOne
          (distribTopLevelConfig ei cfg)
        -- We know all three NSs must be from the same era, because they were
        -- all produced from the same 'BlockForging'. Unfortunately, we can't
        -- enforce it statically.
      $ Match.mustMatchNS "ForgeStateInfo"       (getOneEraForgeStateInfo forgeStateInfo)
      $ Match.mustMatchNS "IsLeader"             (getOneEraIsLeader isLeader)
      $ Match.mustMatchNS "Ticked ChainDepState" (State.tip chainDepState)
      $ blockForging
  where
    distrib ::
         NS (Maybe :.: WrapCannotForge) xs
      -> Either (HardForkCannotForge xs) ()
    distrib = maybe (Right ()) (Left . OneEraCannotForge) . hsequence'

    checkOne ::
         TopLevelConfig blk
      -> Product
           WrapForgeStateInfo
           (Product
             WrapIsLeader
             (Product
               (Ticked :.: WrapChainDepState)
               (BlockForging m)))
           blk
      -> (Maybe :.: WrapCannotForge) blk
         -- ^ We use @Maybe x@ instead of @Either x ()@ because the former can
         -- be partially applied.
    checkOne cfg'
             (Pair
               (WrapForgeStateInfo forgeStateInfo')
               (Pair
                 (WrapIsLeader isLeader')
                 (Pair
                   (Comp tickedChainDepState)
                   blockForging'))) =
        Comp $ either (Just . WrapCannotForge) (const Nothing) $
          checkCanForge
            blockForging'
            cfg'
            curSlot
            (unwrapTickedChainDepState tickedChainDepState)
            isLeader'
            forgeStateInfo'

-- | PRECONDITION: the 'NS' of 'BlockForging's, the ticked 'LedgerState' and
-- 'HardForkIsLeader' are from the same era.
--
-- This follows from the postcondition of 'check' and the fact that the ticked
-- 'ChainDepState' and ticked 'LedgerState' are from the same era.
hardForkForgeBlock ::
     forall m xs. (CanHardFork xs, Monad m)
  => NS (BlockForging m) xs
  -> TopLevelConfig (HardForkBlock xs)
  -> BlockNo
  -> SlotNo
  -> TickedLedgerState (HardForkBlock xs)
  -> [GenTx (HardForkBlock xs)]
  -> HardForkIsLeader xs
  -> m (HardForkBlock xs)
hardForkForgeBlock blockForging
                   cfg
                   bno
                   sno
                   (TickedHardForkLedgerState transition ledgerState)
                   txs
                   isLeader =
        fmap (HardForkBlock . OneEraBlock)
      $ hsequence
      $ hzipWith3
          forgeBlockOne
          (distribTopLevelConfig ei cfg)
          -- Although we get a list with transactions that each could be from a
          -- different era, we know they have been validated against the
          -- 'LedgerState', which means they __must__ be from the same era.
          (partition_NS (map (getOneEraGenTx . getHardForkGenTx) txs))
        -- We know both NSs must be from the same era, because they were all
        -- produced from the same 'BlockForging'. Unfortunately, we can't
        -- enforce it statically.
      $ Match.mustMatchNS "IsLeader"           (getOneEraIsLeader isLeader)
      $ Match.mustMatchNS "Ticked LedgerState" (State.tip ledgerState)
      $ blockForging
  where
    ei = State.epochInfoPrecomputedTransitionInfo
           (hardForkLedgerConfigShape (configLedger cfg))
           transition
           ledgerState

    -- | Unwraps all the layers needed for SOP and call 'forgeBlock'.
    forgeBlockOne ::
         TopLevelConfig blk
      -> ([] :.: GenTx) blk
      -> Product
           WrapIsLeader
             (Product
               (Ticked :.: LedgerState)
               (BlockForging m))
           blk
      -> m blk
    forgeBlockOne cfg'
                  (Comp txs')
                  (Pair
                    (WrapIsLeader isLeader')
                    (Pair
                      (Comp ledgerState')
                      blockForging')) =
        forgeBlock
          blockForging'
          cfg'
          bno
          sno
          ledgerState'
          txs'
          isLeader'
