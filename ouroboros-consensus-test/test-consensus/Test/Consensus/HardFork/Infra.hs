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
    -- * Generate HardFork shape
    Era (..)
  , Eras (..)
  , chooseEras
  , eraIndices
  , erasMapStateM
  , erasUnfoldAtMost
    -- * Era-specified generators
  , genEraParams
  , genShape
  , genStartOfNextEra
  , genSummary
  ) where

import           Data.Kind (Type)
import           Data.Maybe (fromMaybe)
import           Data.SOP.Strict
import           Data.Word
import           Test.QuickCheck hiding (elements)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.SOP

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

eraIndices :: Eras xs -> NP (K Era) xs
eraIndices (Eras eras) = getExactly eras

deriving instance Show (Eras xs)

chooseEras :: forall r. (forall xs. (SListI xs, IsNonEmpty xs) => Eras xs -> Gen r) -> Gen r
chooseEras k = do
    n <- choose (1, 4)
    exactlyReplicate n () $ renumber
  where
    renumber :: Exactly xs () -> Gen r
    renumber ExactlyNil           = error "renumber: empty list of eras"
    renumber e@(ExactlyCons _ _) =
        npToSListI (getExactly e) $
          k (Eras $ go 0 e)
      where
        go :: Word64 -> Exactly (x ': xs) () -> Exactly (x ': xs) Era
        go n (ExactlyCons () ExactlyNil)           = ExactlyCons (Era n True)  ExactlyNil
        go n (ExactlyCons () e'@(ExactlyCons _ _)) = ExactlyCons (Era n False) (go (n + 1) e')

erasMapStateM :: forall m s a xs. Monad m
              => (Era -> s -> m (a, s))
              -> Eras xs -> s -> m (Exactly xs a)
erasMapStateM f (Eras eras) = go eras
  where
    go :: Exactly xs' Era -> s -> m (Exactly xs' a)
    go ExactlyNil         _ = return ExactlyNil
    go (ExactlyCons x xs) s = do
        (a, s') <- f x s
        (ExactlyCons a) <$> go xs s'

erasUnfoldAtMost :: forall m xs a. Monad m
                 => (Era -> HF.Bound -> m (a, HF.EraEnd))
                 -> Eras xs -> HF.Bound -> m (NonEmpty xs a)
erasUnfoldAtMost f (Eras eras) = go eras
  where
    go :: forall x xs'.
          Exactly (x ': xs') Era
       -> HF.Bound
       -> m (NonEmpty (x ': xs') a)
    go (ExactlyCons e es) s = do
        (a, ms) <- f e s
        case ms of
          HF.EraUnbounded -> return $ NonEmptyOne a
          HF.EraEnd s'    ->
            case es of
              ExactlyCons _ _ -> NonEmptyCons a <$> go es s'
              ExactlyNil      -> return $ NonEmptyOne a

{-------------------------------------------------------------------------------
  Era-specific generators
-------------------------------------------------------------------------------}

-- | Generate era parameters
genEraParams :: Gen HF.EraParams
genEraParams = do
    eraEpochSize  <- EpochSize         <$> choose (1, 10)
    eraSlotLength <- slotLengthFromSec <$> choose (1, 5)
    eraSafeZone   <- genSafeZone
    return HF.EraParams{..}
  where
    genSafeZone :: Gen HF.SafeZone
    genSafeZone = oneof [
          HF.StandardSafeZone <$> choose (1, 10)
        , return HF.UnsafeIndefiniteSafeZone
        ]

-- | Generate 'EpochNo' for the start of the next era
genStartOfNextEra :: EpochNo ->  HF.EraParams -> Gen (Maybe EpochNo)
genStartOfNextEra startOfEra HF.EraParams{..} =
    case eraSafeZone of
      HF.UnsafeIndefiniteSafeZone -> return Nothing
      HF.StandardSafeZone _       ->
        Just . (\n -> HF.addEpochs n startOfEra) <$> choose (1, 10)

genShape :: Eras xs -> Gen (HF.Shape xs)
genShape eras = HF.Shape <$> erasMapStateM genParams eras (EpochNo 0)
  where
    genParams :: Era -> EpochNo -> Gen (HF.EraParams, EpochNo)
    genParams _era startOfThis = do
        params      <- genEraParams
        startOfNext <- genStartOfNextEra startOfThis params
        -- If startOfNext is 'Nothing', we used 'UnsafeIndefiniteSafeZone' for
        -- this era. This means we should not be generating any events for any
        -- succeeding eras, but to determine the /shape/ of the eras, and set
        -- subsequent lower bounds, we just need to make sure that we generate a
        -- valid shape: the next era must start after this one.
        return (params, fromMaybe (succ startOfThis) startOfNext)

genSummary :: Eras xs -> Gen (HF.Summary xs)
genSummary is =
    HF.Summary <$> erasUnfoldAtMost genEraSummary is HF.initBound
  where
    genEraSummary :: Era -> HF.Bound -> Gen (HF.EraSummary, HF.EraEnd)
    genEraSummary _era lo = do
        params <- genEraParams
        hi     <- genUpperBound lo params
        return (HF.EraSummary lo hi params, hi)

    genUpperBound :: HF.Bound -> HF.EraParams -> Gen HF.EraEnd
    genUpperBound lo params = do
        startOfNextEra <- genStartOfNextEra (HF.boundEpoch lo) params
        return $ HF.mkEraEnd params lo startOfNextEra
