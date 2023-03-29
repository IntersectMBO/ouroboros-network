{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Local representation for display purpose of cardano-ledger events.
-- Shamelessly stolen from db-sync.
module Cardano.Tools.DBAnalyser.LedgerEvents (
    ConvertLedgerEvent (..)
  , EventsConstraints
  , LedgerEvent (..)
  , convertAuxLedgerEvent
  , convertAuxLedgerEvent'
  , convertPoolRewards
  , ledgerEventName
  ) where

import           Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (KeyRole (StakePool))
import           Cardano.Ledger.Shelley.API (InstantaneousRewards (..), KeyHash)
import           Cardano.Ledger.Shelley.Core (EraCrypto)
import           Cardano.Ledger.Shelley.Rules (RupdEvent (..),
                     ShelleyEpochEvent (..), ShelleyMirEvent (..),
                     ShelleyNewEpochEvent (..), ShelleyPoolreapEvent (..),
                     ShelleyTickEvent (..))
import           Cardano.Prelude hiding (All)
import           Cardano.Slotting.Slot (EpochNo (..))
import           Control.State.Transition (Event)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (All, K (..), hcmap, hcollapse)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (HardForkBlock)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
                     (getOneEraLedgerEvent)
import           Ouroboros.Consensus.Ledger.Abstract (AuxLedgerEvent,
                     LedgerState)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyLedgerEvent (..))
import           Ouroboros.Consensus.TypeFamilyWrappers

data LedgerEvent
  = LedgerMirDist !(Map StakeCred (Set Reward))
  | LedgerPoolReap !EpochNo !Rewards
  | LedgerIncrementalRewards !EpochNo !Rewards
  | LedgerDeltaRewards !EpochNo !Rewards
  | LedgerRestrainedRewards !EpochNo !Rewards !(Set StakeCred)
  | LedgerTotalRewards !EpochNo !(Map StakeCred (Set (Ledger.Reward StandardCrypto)))
  | LedgerStartAtEpoch !EpochNo
  deriving (Eq, Show)

data Reward = Reward
  { rewardSource :: !RewardSource
  , rewardPool   :: !(Strict.Maybe (PoolKeyHash))
  , rewardAmount :: !Coin
  }
  deriving (Eq, Ord, Show)

-- The following must be in alphabetic order.
data RewardSource
  = RwdLeader
  | RwdMember
  | RwdReserves
  | RwdTreasury
  | RwdDepositRefund
  deriving (Bounded, Enum, Eq, Ord, Show)

type PoolKeyHash = KeyHash 'StakePool StandardCrypto

type StakeCred = Ledger.StakeCredential StandardCrypto

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Shelley/Allegra/Mary. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
newtype Rewards = Rewards
  { unRewards :: Map StakeCred (Set Reward)
  }
  deriving (Eq, Show)

instance Ord LedgerEvent where
  a <= b = toOrdering a <= toOrdering b

toOrdering :: LedgerEvent -> Int
toOrdering ev = case ev of
  LedgerMirDist {}            -> 0
  LedgerPoolReap {}           -> 1
  LedgerIncrementalRewards {} -> 2
  LedgerDeltaRewards {}       -> 3
  LedgerRestrainedRewards {}  -> 4
  LedgerTotalRewards {}       -> 5
  LedgerStartAtEpoch {}       -> 6

ledgerEventName :: LedgerEvent -> Text
ledgerEventName le =
  case le of
    LedgerMirDist {}            -> "LedgerMirDist"
    LedgerPoolReap {}           -> "LedgerPoolReap"
    LedgerIncrementalRewards {} -> "LedgerIncrementalRewards"
    LedgerDeltaRewards {}       -> "LedgerDeltaRewards"
    LedgerRestrainedRewards {}  -> "LedgerRestrainedRewards"
    LedgerTotalRewards {}       -> "LedgerTotalRewards"
    LedgerStartAtEpoch {}       -> "LedgerStartAtEpoch"

convertAuxLedgerEvent' :: forall xs blk . (All ConvertLedgerEvent xs, HardForkBlock xs ~ blk) => AuxLedgerEvent (LedgerState blk) -> Maybe LedgerEvent
convertAuxLedgerEvent' = toLedgerEvent . WrapLedgerEvent @blk

convertAuxLedgerEvent :: forall xs . (All ConvertLedgerEvent xs) => AuxLedgerEvent (LedgerState (HardForkBlock xs)) -> Maybe LedgerEvent
convertAuxLedgerEvent = toLedgerEvent . WrapLedgerEvent @(HardForkBlock xs)

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent =
    hcollapse
      . hcmap (Proxy @ ConvertLedgerEvent) (K . toLedgerEvent)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

type EventsConstraints era =
  ( Event (Ledger.EraRule "TICK" era) ~ ShelleyTickEvent era
  , Event (Ledger.EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  , Event (Ledger.EraRule "MIR" era) ~ ShelleyMirEvent era
  , Event (Ledger.EraRule "EPOCH" era) ~ ShelleyEpochEvent era
  , Event (Ledger.EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
  , Event (Ledger.EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
  )

instance
  ( EraCrypto ledgerera ~ StandardCrypto
  , EventsConstraints ledgerera
  ) =>
  ConvertLedgerEvent (ShelleyBlock p ledgerera)
  where
  toLedgerEvent evt =
    case unwrapLedgerEvent evt of
      LETotalRewards e m ->
        Just $ LedgerTotalRewards e m
      LERestraintRewards e m creds ->
        Just $ LedgerRestrainedRewards e (convertPoolRewards m) creds
      LEDeltaReward e m ->
        Just $ LedgerDeltaRewards e (convertPoolRewards m)
      LEIncrementalReward e m ->
        Just $ LedgerIncrementalRewards e (convertPoolRewards m)
      LEMirTransfer rp tp _rtt _ttr ->
        Just $ LedgerMirDist (convertMirRewards rp tp)
      LERetiredPools r _u en ->
        Just $ LedgerPoolReap en (convertPoolDepositRefunds r)
      ShelleyLedgerEventBBODY {} ->
        Nothing
      ShelleyLedgerEventTICK {} ->
        Nothing


--------------------------------------------------------------------------------

convertPoolDepositRefunds ::
  Map StakeCred (Map PoolKeyHash Coin) ->
  Rewards
convertPoolDepositRefunds rwds =
  Rewards $
    Map.map (Set.fromList . map convert . Map.toList) rwds
  where
    convert :: (PoolKeyHash, Coin) -> Reward
    convert (kh, coin) =
      Reward
        { rewardSource = RwdDepositRefund
        , rewardPool = Strict.Just kh
        , rewardAmount = coin
        }

convertMirRewards ::
  Map StakeCred Coin ->
  Map StakeCred Coin ->
  Map StakeCred (Set Reward)
convertMirRewards resPay trePay =
  Map.unionWith Set.union (convertResPay resPay) (convertTrePay trePay)
  where
    convertResPay :: Map StakeCred Coin -> Map StakeCred (Set Reward)
    convertResPay = Map.map (mkPayment RwdReserves)

    convertTrePay :: Map StakeCred Coin -> Map StakeCred (Set Reward)
    convertTrePay = Map.map (mkPayment RwdTreasury)

    mkPayment :: RewardSource -> Coin -> Set Reward
    mkPayment src coin =
      Set.singleton $
        Reward
          { rewardSource = src
          , rewardPool = Strict.Nothing
          , rewardAmount = coin
          }

convertPoolRewards ::
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  Rewards
convertPoolRewards rmap =
  Rewards $
    map (Set.map convertReward) rmap
  where
    convertReward :: Ledger.Reward StandardCrypto -> Reward
    convertReward sr =
      Reward
        { rewardSource = rewardTypeToSource $ Ledger.rewardType sr
        , rewardAmount = Ledger.rewardAmount sr
        , rewardPool = Strict.Just $ Ledger.rewardPool sr
        }

rewardTypeToSource :: Ledger.RewardType -> RewardSource
rewardTypeToSource rt =
  case rt of
    Ledger.LeaderReward -> RwdLeader
    Ledger.MemberReward -> RwdMember


--------------------------------------------------------------------------------
-- Patterns for event access. Why aren't these in ledger-specs?

pattern LERestraintRewards ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  ) =>
  EpochNo ->
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  Set StakeCred ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LERestraintRewards e m creds <-
  ShelleyLedgerEventTICK
    (TickNewEpochEvent (RestrainedRewards e m creds))

pattern LETotalRewards ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  ) =>
  EpochNo ->
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LETotalRewards e m <-
  ShelleyLedgerEventTICK
    (TickNewEpochEvent (TotalRewardEvent e m))

pattern LEDeltaReward ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent (EraCrypto ledgerera)
  ) =>
  EpochNo ->
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LEDeltaReward e m <-
  ShelleyLedgerEventTICK
    (TickNewEpochEvent (DeltaRewardEvent (RupdEvent e m)))

pattern LEIncrementalReward ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent (EraCrypto ledgerera)
  ) =>
  EpochNo ->
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LEIncrementalReward e m <-
  ShelleyLedgerEventTICK
    (TickRupdEvent (RupdEvent e m))

pattern LEMirTransfer ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  , Event (Ledger.EraRule "MIR" ledgerera) ~ ShelleyMirEvent ledgerera
  ) =>
  Map StakeCred Coin ->
  Map StakeCred Coin ->
  DeltaCoin ->
  DeltaCoin ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LEMirTransfer rp tp rtt ttr <-
  ShelleyLedgerEventTICK
    ( TickNewEpochEvent
        ( MirEvent
            ( MirTransfer
                (InstantaneousRewards rp tp rtt ttr)
              )
          )
      )

pattern LERetiredPools ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  , Event (Ledger.EraRule "EPOCH" ledgerera) ~ ShelleyEpochEvent ledgerera
  , Event (Ledger.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  ) =>
  Map StakeCred (Map PoolKeyHash Coin) ->
  Map StakeCred (Map PoolKeyHash Coin) ->
  EpochNo ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LERetiredPools r u e <-
  ShelleyLedgerEventTICK
    ( TickNewEpochEvent
        ( EpochEvent
            ( PoolReapEvent
                (RetiredPools r u e)
              )
          )
      )
