{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

-- | Transaction generator for testing
module Test.Dynamic.TxGen
  ( TxGen (..)
  ) where

import           Control.Monad (replicateM)
import           Control.Monad.Trans.Except (runExcept)
import           Crypto.Number.Generate (generateBetween)
import           Data.Bitraversable (bitraverse)
import           Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Block (SlotNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.HardFork
import           Ouroboros.Consensus.Ledger.Mock hiding (utxo)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.HardFork
import           Ouroboros.Consensus.Util.Random

{-------------------------------------------------------------------------------
  TxGen class
-------------------------------------------------------------------------------}

class TxGen blk where
  -- | Generate a transaction, valid or invalid, that can be submitted to a
  -- node's Mempool.
  testGenTx :: (HasCallStack, MonadRandom m)
            => NumCoreNodes
            -> NodeConfig (BlockProtocol blk)
            -> SlotNo
            -> LedgerState blk
            -> m (GenTx blk)

  -- | Generate a number of transactions, valid or invalid, that can be
  -- submitted to a node's Mempool.
  --
  -- This function (not 'testGenTx') will be called to generate transactions
  -- in consensus tests.
  testGenTxs :: MonadRandom m
             => NumCoreNodes
             -> NodeConfig (BlockProtocol blk)
             -> SlotNo
             -> LedgerState blk
             -> m [GenTx blk]
  testGenTxs numCoreNodes cfg curSlotNo ledger = do
    -- Currently 0 to 1 txs
    n <- generateBetween 0 1
    replicateM (fromIntegral n) $ testGenTx numCoreNodes cfg curSlotNo ledger

  {-# MINIMAL testGenTx #-}

{-------------------------------------------------------------------------------
  TxGen SimpleBlock
-------------------------------------------------------------------------------}

instance TxGen (SimpleBlock SimpleMockCrypto ext) where
  testGenTx (NumCoreNodes n) _ _ ledgerState =
      mkSimpleGenTx <$> genSimpleTx addrs utxo
    where
      addrs :: [Addr]
      addrs = Map.keys $ mkAddrDist n

      utxo :: Utxo
      utxo = mockUtxo $ simpleLedgerState ledgerState

genSimpleTx :: forall m. MonadRandom m => [Addr] -> Utxo -> m Tx
genSimpleTx addrs u = do
    let senders = Set.toList . Set.fromList . map fst . Map.elems $ u -- people with funds
    sender    <- genElt senders
    recipient <- genElt $ filter (/= sender) addrs
    let assets  = filter (\(_, (a, _)) -> a == sender) $ Map.toList u
        fortune = sum [c | (_, (_, c)) <- assets]
        ins     = Set.fromList $ map fst assets
    amount <- fromIntegral <$> generateBetween 1 (fromIntegral fortune)
    let outRecipient = (recipient, amount)
        outs         = if amount == fortune
            then [outRecipient]
            else [outRecipient, (sender, fortune - amount)]
    return $ Tx ins outs
  where
    genElt :: HasCallStack => [a] -> m a
    genElt xs = do
        m <- generateElement xs
        case m of
            Nothing -> error "expected non-empty list"
            Just x  -> return x

{-------------------------------------------------------------------------------
  TxGen ByronBlockOrEBB
-------------------------------------------------------------------------------}

instance TxGen (ByronBlockOrEBB ByronConfig) where
  testGenTx = error "TODO #855 testGenTx"
  -- 'testGenTxs' is used by the tests, not 'testGenTx'.
  testGenTxs _ _ _ _ = return []

{-------------------------------------------------------------------------------
  TxGen Forked
-------------------------------------------------------------------------------}

instance
  (TxGen blk1, TxGen blk2, UpdateLedger (Forked blk1 blk2))
  => TxGen (Forked blk1 blk2) where

  testGenTx numCoreNodes nodeConfig curSlotNo forkedLedgerState =
    ForkedGenTx <$>
      bitraverse
        (genTx (nodeConfigBeforeFork nodeConfig))
        (genTx (nodeConfigAfterFork nodeConfig))
        forkedLedgerState'
    where
      genTx cfg = testGenTx numCoreNodes cfg curSlotNo

      -- We 'applyChainTick' as a hard fork might be triggered during that call
      forkedLedgerState' :: Forked (LedgerState blk1) (LedgerState blk2)
      forkedLedgerState' =
        unForkedLedgerState .
          fromRight (error "applyChainTick failed while generating transaction") .
          runExcept .
          applyChainTick (ledgerConfigView nodeConfig) curSlotNo $
          forkedLedgerState
