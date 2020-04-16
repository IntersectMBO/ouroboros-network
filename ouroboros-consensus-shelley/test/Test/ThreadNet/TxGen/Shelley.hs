{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Shelley
  ( ShelleyTxGenExtra(..)
  ) where

import           Control.Monad.Except (runExcept)
import           Crypto.Number.Generate (generateBetween, generateMax)
import           Crypto.Random (MonadRandom)
import qualified Data.Sequence.Strict as Seq

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract

import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.STS.Ledger as STS
import qualified Shelley.Spec.Ledger.Tx as SL

import           Ouroboros.Consensus.Shelley.Ledger

import           Test.QuickCheck.Gen (Gen (..), suchThat)
import           Test.QuickCheck.Random (mkQCGen)

import           Test.ThreadNet.TxGen (TxGen (..))

import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as CSL
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Utxo as Gen

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)

data ShelleyTxGenExtra = ShelleyTxGenExtra
  { -- | Generator environment.
    stgeGenEnv :: Gen.GenEnv
  }

instance TxGen (ShelleyBlock TPraosMockCrypto) where

  type TxGenExtra (ShelleyBlock TPraosMockCrypto) = ShelleyTxGenExtra

  -- TODO #1823
  testGenTxs _numCoreNodes curSlotNo cfg (ShelleyTxGenExtra genEnv) lst = do
      n <- generateBetween 0 20
      go [] n $ applyChainTick (configLedger cfg) curSlotNo lst
    where
      go :: MonadRandom m
         => [GenTx (ShelleyBlock TPraosMockCrypto)]  -- ^ Accumulator
         -> Integer  -- ^ Number of txs to still produce
         -> TickedLedgerState (ShelleyBlock TPraosMockCrypto)
         -> m [GenTx (ShelleyBlock TPraosMockCrypto)]
      go acc 0 _  = return (reverse acc)
      go acc n st = do
        tx <- quickCheckAdapter $ genTx cfg st genEnv
        case runExcept $ applyTx (configLedger cfg) tx st of
          -- We don't mind generating invalid transactions
          Left  _   -> go (tx:acc) (n - 1) st
          Right st' -> go (tx:acc) (n - 1) st'

genTx
  :: TopLevelConfig (ShelleyBlock TPraosMockCrypto)
  -> TickedLedgerState (ShelleyBlock TPraosMockCrypto)
  -> Gen.GenEnv
  -> Gen (GenTx (ShelleyBlock TPraosMockCrypto))
genTx _cfg TickedLedgerState { tickedSlotNo, tickedLedgerState } genEnv =
    mkShelleyTx <$> Gen.genTx
      genEnv
      ledgerEnv
      (utxoSt, dpState) `suchThat` isSimpleTx
  where
    -- Filter (for the moment) to "simple" transactions - in particular, we
    -- filter all transactions which have certificates. Testing with
    -- certificates requires additional handling in the testing framework,
    -- because, for example, they may transfer block issuance rights from one
    -- node to another, and we must have the relevant nodes brought online at
    -- that point.
    isSimpleTx (SL._body -> txb) =
      (Seq.null $ SL._certs txb)
    ShelleyLedgerState { shelleyState } = tickedLedgerState

    epochState :: CSL.EpochState
    epochState = SL.nesEs shelleyState

    ledgerEnv :: STS.LedgerEnv
    ledgerEnv = STS.LedgerEnv {
        ledgerSlotNo   = tickedSlotNo
      , ledgerIx       = 0 -- TODO Ix
      , ledgerPp       = SL.esPp epochState
      , ledgerReserves =
            SL._reserves
          . SL.esAccountState
          $ epochState
      }

    utxoSt :: CSL.UTxOState
    utxoSt =
        SL._utxoState
      . SL.esLState
      $ epochState

    dpState :: CSL.DPState
    dpState =
        SL._delegationState
      . SL.esLState
      $ epochState

{-------------------------------------------------------------------------------
  QuickCheck to MonadRandom adapter
-------------------------------------------------------------------------------}

-- | Run the generator by producing a random seed
quickCheckAdapter :: MonadRandom m => Gen a -> m a
quickCheckAdapter (MkGen g) = do
    seed <- fromIntegral <$> generateMax (fromIntegral (maxBound :: Int))
    return $ g (mkQCGen seed) 30
