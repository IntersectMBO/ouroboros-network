{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Mock () where

import           Crypto.Number.Generate (generateBetween)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Util.Random

import           Test.ThreadNet.TxGen

{-------------------------------------------------------------------------------
  TxGen SimpleBlock
-------------------------------------------------------------------------------}

instance TxGen (SimpleBlock SimpleMockCrypto ext) where
  testGenTx numCoreNodes _curSlotNo _cfg ledgerState =
      mkSimpleGenTx <$> genSimpleTx addrs utxo
    where
      addrs :: [Addr]
      addrs = Map.keys $ mkAddrDist numCoreNodes

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
