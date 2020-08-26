{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
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
  , erasUnfoldAtMost
    -- * Era-specified generators
  , genEraParams
  , genStartOfNextEra
  ) where

import           Control.Monad.Except
import           Data.Kind (Type)
import           Data.SOP.Dict (Dict (..))
import           Data.SOP.Strict
import           Data.Word
import           Test.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.SOP

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

data Eras :: [Type] -> Type where
    -- We guarantee to have at least one era
    Eras :: Exactly (x ': xs) Era -> Eras (x ': xs)

instance Show (Eras xs) where
  show (Eras np) =
      npToSListI np $
        case allComposeShowK (Proxy @xs) (Proxy @Era) of
          Dict -> show np

chooseEras :: forall r. (forall xs. (SListI xs, IsNonEmpty xs) => Eras xs -> Gen r) -> Gen r
chooseEras k = do
    n <- choose (1, 4)
    exactlyReplicate n () $ renumber
  where
    renumber :: Exactly xs () -> Gen r
    renumber Nil        = error "renumber: empty list of eras"
    renumber e@(_ :* _) =
        npToSListI e $
          k (Eras $ go 0 e)
      where
        go :: Word64 -> Exactly (x ': xs) () -> Exactly (x ': xs) Era
        go n (K () :* Nil)         = K (Era n True)  :* Nil
        go n (K () :* e'@(_ :* _)) = K (Era n False) :* go (n + 1) e'

erasMapStateM :: forall m s a xs. Monad m
              => (Era -> s -> m (a, s))
              -> Eras xs -> s -> m (Exactly xs a)
erasMapStateM f (Eras eras) = go eras
  where
    go :: Exactly xs' Era -> s -> m (Exactly xs' a)
    go Nil         _ = return Nil
    go (K x :* xs) s = do
        (a, s') <- f x s
        (K a :*) <$> go xs s'

erasUnfoldAtMost :: forall m xs a. Monad m
                 => (Era -> HF.Bound -> m (a, HF.EraEnd))
                 -> Eras xs -> HF.Bound -> m (NonEmpty xs a)
erasUnfoldAtMost f (Eras eras) = go eras
  where
    go :: forall x xs'.
          Exactly (x ': xs') Era
       -> HF.Bound
       -> m (NonEmpty (x ': xs') a)
    go (K e :* es) s = do
        (a, ms) <- f e s
        case ms of
          HF.EraUnbounded -> return $ NonEmptyOne a
          HF.EraEnd s'    ->
            case es of
              _ :* _ -> NonEmptyCons a <$> go es s'
              Nil    -> return $ NonEmptyOne a

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
    genSafeZone = oneof [
          do safeFromTip <- choose (1, 10)
             safeBefore  <- genSafeBeforeEpoch
             return $ HF.StandardSafeZone safeFromTip safeBefore
        , return HF.UnsafeIndefiniteSafeZone
        ]

    genSafeBeforeEpoch :: Gen HF.SafeBeforeEpoch
    genSafeBeforeEpoch = oneof [
          return HF.NoLowerBound
        , (\n -> HF.LowerBound (HF.addEpochs n startOfEra)) <$> choose (1, 5)
        ]

-- | Generate 'EpochNo' for the start of the next era
genStartOfNextEra :: EpochNo ->  HF.EraParams -> Gen (Maybe EpochNo)
genStartOfNextEra startOfEra HF.EraParams{..} =
    case HF.safeBeforeEpoch eraSafeZone of
      Nothing     -> return Nothing
      Just mBound -> Just <$>
        case mBound of
          HF.LowerBound e -> (\n -> HF.addEpochs n e         ) <$> choose (0, 10)
          HF.NoLowerBound -> (\n -> HF.addEpochs n startOfEra) <$> choose (1, 10)
