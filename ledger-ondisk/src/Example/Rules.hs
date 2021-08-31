{-# LANGUAGE RankNTypes #-}
-- |

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Example.Rules where

import Example.Types
import LedgerOnDisk.KVHandle.Class
import LedgerOnDisk.Mapping.Class
import Prelude hiding (lookup)
import qualified Data.Set as Set

applyBlock :: LedgerMapping map => [Tx] -> LedgerState map -> Maybe (LedgerState map)
applyBlock []       st = Just st
applyBlock (tx:txs) st = applyTx tx st >>= applyBlock txs

applyTx :: LedgerMapping map => Tx -> LedgerState map -> Maybe (LedgerState map)
applyTx tx LedgerState{utxos, pparams} = do
    utxos' <- applyTx' tx utxos
    Just LedgerState {
           utxos = utxos',
           pparams
         }

-- Operations on a sub-component of the state

applyTx' :: LedgerMapping map => Tx -> UTxOState map -> Maybe (UTxOState map)
applyTx' tx = case tx of
  Tx txins txouts -> fmap (insertOutputs txid txouts) . deleteInputs txins
  LollyScramble txouts -> pure . insertOutputs txid txouts
  where
    txid = txId tx

deleteInputs :: LedgerMapping map => [TxIn] -> UTxOState map -> Maybe (UTxOState map)
deleteInputs []           st = Just st
deleteInputs (txin:txins) st = deleteInput txin st >>= deleteInputs txins

deleteInput :: LedgerMapping map => TxIn -> UTxOState map -> Maybe (UTxOState map)
deleteInput txin UTxOState {utxo, utxoagg} = do
    -- Verify that the input exists, and also update the utxo aggregate
    TxOut addr coin <- lookup txin utxo
    return UTxOState {
      utxo    = delete txin utxo,
      utxoagg = update addr (negate coin) utxoagg
    }

insertOutputs :: LedgerMapping map => TxId -> [TxOut] -> UTxOState map -> UTxOState map
insertOutputs txid txouts st =
    foldr (\(txix, txout) -> insertOutput (TxIn txid (TxIx txix)) txout)
          st (zip [0 ..] txouts)

insertOutput :: LedgerMapping map => TxIn -> TxOut -> UTxOState map -> UTxOState map
insertOutput txin txout@(TxOut addr coin) UTxOState {utxo, utxoagg} =
    -- also update the utxo aggregate
    UTxOState {
      utxo    = insert txin txout utxo,
      utxoagg = update addr coin utxoagg
    }

ledgerStateBlockKeys :: Block -> OnDiskMappings LedgerState Keys
ledgerStateBlockKeys block = LedgerStateMappings
  { sm_utxos = utxoStateKeys block
  }

utxoStateKeys :: Block -> OnDiskMappings UTxOState Keys
utxoStateKeys block = let
  go (Tx ins _outs) =
    ( Keys . Set.fromList $ ins
    -- , Keys . Set.fromList $ [addr | TxOut addr _ <- outs]
    )
  go LollyScramble {} = mempty
  sm_utxoagg = mempty -- we don't read this value while validating blocks
  sm_utxo = foldMap go block
  in UTxOStateMappings{..}
