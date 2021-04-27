{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

-- | Genesis config for the spec
--
-- Intended for qualified import
--
-- > import           Ouroboros.Consensus.ByronSpec.Ledger.Genesis (ByronSpecGenesis)
-- > import qualified Ouroboros.Consensus.ByronSpec.Ledger.Genesis as Genesis
module Ouroboros.Consensus.ByronSpec.Ledger.Genesis
  ( ByronSpecGenesis (..)
  , modFeeParams
  , modPBftThreshold
  , modPParams
  , modUtxo
  , modUtxoValues
    -- * Conversions
  , fromChainEnv
  , toChainEnv
  ) where

import           Codec.Serialise (decode, encode)
import           Data.Coerce (coerce)
import           Data.Set (Set)
import           NoThunks.Class (AllowThunk (..), NoThunks)
import           Numeric.Natural (Natural)

import           Cardano.Binary

import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.UTxO as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

import           Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Genesis config
-------------------------------------------------------------------------------}

-- | The equivalent of the genesis config for the abstract ledger
data ByronSpecGenesis = ByronSpecGenesis {
      byronSpecGenesisDelegators    :: Set Spec.VKeyGenesis
    , byronSpecGenesisInitUtxo      :: Spec.UTxO
    , byronSpecGenesisInitPParams   :: Spec.PParams
    , byronSpecGenesisSecurityParam :: Spec.BlockCount

      -- | Slot length
      --
      -- The Byron spec itself does not talk about slot length at all. Here we
      -- record it primarily to support the relation between the spec and the
      -- real implementation. For this reason we choose the same representation
      -- as the real PBFT does ('ppSlotDuration' in 'ProtocolParameters').
    , byronSpecGenesisSlotLength    :: Natural
    }
  deriving stock (Show)
  deriving NoThunks via AllowThunk ByronSpecGenesis





-- TODO serialisation tests!!!!!!




instance FromCBOR ByronSpecGenesis where
  fromCBOR = do
    enforceSize "ByronSpecGenesis" 5
    ByronSpecGenesis
      <$> fromCBOR @(Set Spec.VKeyGenesis)
      <*> decode @Spec.UTxO
      <*> decode @Spec.PParams
      <*> fromCBOR @Spec.BlockCount
      <*> fromCBOR @Natural

instance ToCBOR ByronSpecGenesis where
  toCBOR (ByronSpecGenesis delegators utxo pparams k slotLength) = mconcat
    [ encodeListLen 5
    , toCBOR delegators
    , encode utxo
    , encode pparams
    , toCBOR k
    , toCBOR slotLength
    ]

-- TODO remove instances when they have been merged upstream
deriving newtype instance FromCBOR Spec.BlockCount
deriving newtype instance ToCBOR Spec.BlockCount
deriving newtype instance FromCBOR Spec.VKeyGenesis
deriving newtype instance FromCBOR Spec.VKey
deriving newtype instance FromCBOR Spec.Owner

modPBftThreshold :: (Double -> Double)
                 -> ByronSpecGenesis -> ByronSpecGenesis
modPBftThreshold = modPParams . modPParamsPBftThreshold

-- | Modify the @a@ and @b@ fee parameters
modFeeParams :: ((Int, Int) -> (Int, Int))
             -> ByronSpecGenesis -> ByronSpecGenesis
modFeeParams = modPParams . modPParamsFeeParams

-- | Adjust all values in the initial UTxO equally
modUtxoValues :: (Integer -> Integer) -> ByronSpecGenesis -> ByronSpecGenesis
modUtxoValues = modUtxo . Spec.mapUTxOValues . coerce

modUtxo :: (Spec.UTxO -> Spec.UTxO) -> ByronSpecGenesis -> ByronSpecGenesis
modUtxo f genesis = genesis {
      byronSpecGenesisInitUtxo = f (byronSpecGenesisInitUtxo genesis)
    }

modPParams :: (Spec.PParams -> Spec.PParams)
           -> ByronSpecGenesis -> ByronSpecGenesis
modPParams f genesis = genesis {
      byronSpecGenesisInitPParams = f (byronSpecGenesisInitPParams genesis)
    }

{-------------------------------------------------------------------------------
  Internal: accessors for the protocol parameters
-------------------------------------------------------------------------------}

modPParamsPBftThreshold :: (Double -> Double)
                        -> Spec.PParams -> Spec.PParams
modPParamsPBftThreshold f pparams = pparams {
      Spec._bkSgnCntT = Spec.BkSgnCntT (f threshold)
    }
  where
    Spec.BkSgnCntT threshold = Spec._bkSgnCntT pparams

modPParamsFeeParams :: ((Int, Int) -> (Int, Int))
                    -> Spec.PParams -> Spec.PParams
modPParamsFeeParams f pparams = pparams {
      Spec._factorA = Spec.FactorA $ fst (f (a, b))
    , Spec._factorB = Spec.FactorB $ snd (f (a, b))
    }
  where
    Spec.FactorA a = Spec._factorA pparams
    Spec.FactorB b = Spec._factorB pparams

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Derive CHAIN rule environment
toChainEnv :: ByronSpecGenesis -> Spec.Environment Spec.CHAIN
toChainEnv ByronSpecGenesis{..} = disableConsensusChecks (
      Spec.Slot 0 -- current slot
    , byronSpecGenesisInitUtxo
    , byronSpecGenesisDelegators
    , byronSpecGenesisInitPParams
    , byronSpecGenesisSecurityParam
    )
  where
    -- We are only interested in updating the /ledger state/, not the /consensus
    -- chain state/. Unfortunately, the Byron spec does not make that
    -- distinction, and so when we call the CHAIN rule, we might get some errors
    -- here that the implementation does not report (because it would only find
    -- them when we update the chain state). There are at least two possible
    -- proper solutions for this:
    --
    -- 1. Modify the spec so that we /do/ have the separation. Note that if we
    --    did, we would not use the chain state part of the spec, since the
    --    chain state part of the dual ledger is determined entirely by the
    --    concrete Byron block.
    -- 2. Turn 'applyExtLedger' and related types into a type class of their
    --    own, so that we can override it specifically for the dual ledger.
    --
    -- Either way, we are only testing the /ledger/ part of the two blocks here,
    -- not the consensus part. For now we just override some parameters in the
    -- environment to work around the problem and make sure that none of the
    -- consensus checks in the spec can fail.
    disableConsensusChecks :: Spec.Environment Spec.CHAIN
                           -> Spec.Environment Spec.CHAIN
    disableConsensusChecks ( _currentSlot
                           , utx0
                           , delegators
                           , pparams
                           , k
                           ) = (
          -- Disable 'SlotInTheFuture' failure
          Spec.Slot maxBound
        , utx0
        , delegators
          -- Disable 'TooManyIssuedBlocks' failure
        , pparams { Spec._bkSgnCntT = Spec.BkSgnCntT 1 }
        , k
        )

-- | Construct genesis config from CHAIN environment
--
-- This doesn't make an awful lot of sense, but the abstract spec doesn't /have/
-- a concept of a genesis config, and instead the CHAIN environment fulfills
-- that role. In order to be able to reuse the test generators, we therefore
-- also define a translation in the opposite direction.
fromChainEnv :: Natural -> Spec.Environment Spec.CHAIN -> ByronSpecGenesis
fromChainEnv byronSpecGenesisSlotLength
             ( _currentSlot
             , byronSpecGenesisInitUtxo
             , byronSpecGenesisDelegators
             , byronSpecGenesisInitPParams
             , byronSpecGenesisSecurityParam
             ) = ByronSpecGenesis{..}
