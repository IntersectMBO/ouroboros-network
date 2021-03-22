{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Mock (

  ) where

import           Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Mock.Ledger

import           Test.QuickCheck hiding (elements)

import           Test.ThreadNet.TxGen
import           Test.Util.QuickCheck

{-------------------------------------------------------------------------------
  TxGen SimpleBlock
-------------------------------------------------------------------------------}

instance TxGen (SimpleBlock SimpleMockCrypto ext) where
  testGenTxs _coreNodeId numCoreNodes curSlotNo _cfg () ledgerState = do
      n <- choose (0, 20)
      -- We don't update the UTxO after each transaction, so some of the
      -- generated transactions could very well be invalid.
      replicateM n $
        mkSimpleGenTx <$> genSimpleTx curSlotNo addrs utxo
    where
      addrs :: [Addr]
      addrs = Map.keys $ mkAddrDist numCoreNodes

      utxo :: Utxo
      utxo = mockUtxo $ simpleLedgerState ledgerState

genSimpleTx :: SlotNo -> [Addr] -> Utxo -> Gen Tx
genSimpleTx curSlotNo addrs u = do
    let senders = Set.toList . Set.fromList . map fst . Map.elems $ u -- people with funds
    sender    <- elements senders
    recipient <- elements $ filter (/= sender) addrs
    let assets  = filter (\(_, (a, _)) -> a == sender) $ Map.toList u
        fortune = sum [c | (_, (_, c)) <- assets]
        ins     = Set.fromList $ map fst assets
    amount <- choose (1, fortune)
    let outRecipient = (recipient, amount)
        outs         = if amount == fortune
                       then [outRecipient]
                       else [outRecipient, (sender, fortune - amount)]
    -- generate transactions within several slots in the future or never
    expiry <- elements $ map mkExpiry $ Nothing : map Just [0 .. 10]
    return $ Tx expiry ins outs
  where
    mkExpiry :: Maybe SlotNo -> Expiry
    mkExpiry = \case
        Nothing    -> DoNotExpire
        Just delta -> ExpireAtOnsetOf $ curSlotNo + delta
