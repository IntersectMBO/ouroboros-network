{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

-- | Infrastructure shared by the various 'HardFork' tests
module Test.Consensus.HardFork.Infra (
    -- * Generic QuickCheck utilities
    checkGenerator
  , checkShrinker
  , checkInvariant
    -- * Generate HardFork shape
  , Era(..)
  , Eras(..)
  , chooseEras
  , erasMapStateM
    -- * Era-specified generators
  , genEraParams
  , genStartOfNextEra
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Word
import           Test.QuickCheck

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime.SlotLength
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Util.Counting

import           Test.Util.QuickCheck

{-------------------------------------------------------------------------------
  Generic QuickCheck utilities
-------------------------------------------------------------------------------}

-- | Test the generator
--
-- Uses explicit 'forAll' as we don't want to assume a correct shrinker.
checkGenerator :: (Arbitrary a, Show a) => (a -> Property) -> Property
checkGenerator p = forAll arbitrary $ p

-- | Test the shrinker
--
-- Uses explicit 'forAll': don't shrink when testing the shrinker
checkShrinker :: (Arbitrary a, Show a) => (a -> Property) -> Property
checkShrinker p = forAll arbitrary $ conjoin . map p . shrink

-- | Check invariant
checkInvariant :: (a -> Except String ()) -> (a -> Property)
checkInvariant f = expectRight () . runExcept . f

{-------------------------------------------------------------------------------
  Generate hard fork shape
-------------------------------------------------------------------------------}

data Era = Era {
       eraIndex  :: Word64
     , eraIsLast :: Bool
     }
  deriving (Show, Eq, Ord)

data Eras :: [*] -> * where
    -- We guarantee to have at least one era
    Eras :: Exactly (x ': xs) Era -> Eras (x ': xs)

deriving instance Show (Eras xs)

chooseEras :: (forall xs. Eras xs -> Gen r) -> Gen r
chooseEras k = do
    n <- choose (1, 4)
    exactlyReplicate n () $ k . renumber
  where
    renumber :: Exactly xs () -> Eras xs
    renumber ExactlyNil      = error "renumber: empty list of eras"
    renumber e@ExactlyCons{} = Eras $ go 0 e
      where
        go :: Word64 -> Exactly (x ': xs) () -> Exactly (x ': xs) Era
        go n (ExactlyCons () ExactlyNil) =
            ExactlyCons (Era n True)  ExactlyNil
        go n (ExactlyCons () e'@ExactlyCons{}) =
            ExactlyCons (Era n False) (go (n + 1) e')

erasMapStateM :: Monad m
              => (Era -> s -> m (a, s))
              -> Eras xs -> s -> m (Exactly xs a)
erasMapStateM f (Eras eras) = evalStateT (traverse (StateT . f) eras)

{-------------------------------------------------------------------------------
  Era-specific generators
-------------------------------------------------------------------------------}

-- | Generate era parameters
--
-- Need to know the start of the era to generate a valid 'HF.LowerBound'.
-- We do /not/ assume that the 'safeFromTip' must be less than an epoch.
genEraParams :: EpochNo -> Gen HF.EraParams
genEraParams startOfEra = do
    eraEpochSize  <- EpochSize         <$> choose (1, 10)
    eraSlotLength <- slotLengthFromSec <$> choose (1, 5)
    eraSafeZone   <- genSafeZone
    return HF.EraParams{..}
  where
    genSafeZone :: Gen HF.SafeZone
    genSafeZone = do
        safeFromTip     <- choose (1, 10)
        safeBeforeEpoch <- genSafeBeforeEpoch
        return HF.SafeZone{..}

    genSafeBeforeEpoch :: Gen HF.SafeBeforeEpoch
    genSafeBeforeEpoch = oneof [
          return HF.NoLowerBound
        , (\n -> HF.LowerBound (HF.addEpochs n startOfEra)) <$> choose (1, 5)
        ]

-- | Generate 'EpochNo' for the start of the next era
genStartOfNextEra :: EpochNo ->  HF.EraParams -> Gen EpochNo
genStartOfNextEra startOfEra HF.EraParams{..} =
    case HF.safeBeforeEpoch eraSafeZone of
       HF.LowerBound e -> (\n -> HF.addEpochs n e         ) <$> choose (0, 10)
       HF.NoLowerBound -> (\n -> HF.addEpochs n startOfEra) <$> choose (1, 10)
