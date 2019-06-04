{-# LANGUAGE RecordWildCards #-}

-- | Elaboration from our mock transactions into transactions on the real ledger
module Ouroboros.Consensus.Demo.Ledger.Byron.Elaborate (
    elaborateTx
  ) where

import           Data.Bifunctor (bimap)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as V

import           GHC.Stack (HasCallStack)

import           Cardano.Binary (Annotated (..), reAnnotate)
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.UTxO as CC.UTxO
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Consensus.Ledger.Byron
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig

import           Ouroboros.Consensus.Demo.Ledger.Byron.Config

-- | Elaborate a mock transaction to a real one
--
-- For now the only thing we support are transactions of the form
--
-- > Tx (Set.singleton (_hash, n)) [(addr, amount)]
--
-- We ignore the hash, and assume it refers to the initial balance of the @n@'th
-- rich actor. We then transfer it _to_ the @m@'s rich actor (with "a" being the
-- first rich actor), leaving any remaining balance simply as the transaction
-- fee.
--
-- This is adapted from 'Test.Cardano.Chain.Elaboration.UTxO.elaborateTxWits'
elaborateTx :: HasCallStack
            => NodeConfig ByronExtNodeConfig
            -> Mock.Tx -> GenTx (ByronBlock cfg)
elaborateTx cfg (Mock.Tx ins outs) =
    ByronTx $ CC.UTxO.ATxAux (annotate tx) (annotate witness)
  where
    annotate x = reAnnotate $ Annotated x ()
    -- mockInp and mockOut in [0 .. 3] (index of rich actor)
    [(_hash, mockInp)]    = Set.toList ins
    [(mockAddr, mockVal)] = outs

    mockOut :: HasCallStack => Int
    mockOut = case lookup mockAddr (zip ["a", "b", "c", "d"] [0..]) of
                Nothing -> error "supported addresses: 'a', 'b', 'c' or 'd'"
                Just i  -> i

    tx :: CC.UTxO.Tx
    tx = CC.UTxO.UnsafeTx {
          txInputs     = txIn  :| []
        , txOutputs    = txOut :| []
        , txAttributes = CC.Common.mkAttributes ()
        }

    txIn :: CC.UTxO.TxIn
    txIn = fst . fst $ initialUtxo Map.! mockInp

    txOut :: CC.UTxO.TxOut
    txOut = CC.UTxO.TxOut {
          txOutAddress = CC.UTxO.txOutAddress $ snd . fst $ initialUtxo Map.! mockOut
        , txOutValue   = assumeBound $
                           CC.Common.mkLovelace (fromIntegral (mockVal * 1000000))
        }

    witness :: CC.UTxO.TxWitness
    witness = V.fromList [
          CC.UTxO.VKWitness
            (Crypto.toVerification (snd $ initialUtxo Map.! mockInp))
            (Crypto.sign
              (Crypto.getProtocolMagicId . pbftProtocolMagic . encNodeConfigExt $ cfg)
              Crypto.SignTx
              (snd $ initialUtxo Map.! mockInp)
              (CC.UTxO.TxSigData (Crypto.hash tx))
              )
        ]

    -- UTxO in the genesis block for the rich men
    initialUtxo :: Map Int ((CC.UTxO.TxIn, CC.UTxO.TxOut), Crypto.SigningKey)
    initialUtxo =
          Map.fromList
        . mapMaybe (\(inp, out) -> mkEntry inp out <$> isRichman out)
        . fromCompactTxInTxOutList
        . Map.toList
        . CC.UTxO.unUTxO
        . CC.UTxO.genesisUtxo
        $ pbftGenesisConfig (encNodeConfigExt cfg)
      where
        mkEntry :: CC.UTxO.TxIn
                -> CC.UTxO.TxOut
                -> (Int, Crypto.SigningKey)
                -> (Int, ((CC.UTxO.TxIn, CC.UTxO.TxOut), Crypto.SigningKey))
        mkEntry inp out (richman, key) = (richman, ((inp, out), key))

    isRichman :: CC.UTxO.TxOut -> Maybe (Int, Crypto.SigningKey)
    isRichman out = listToMaybe $ filter (isValidKey . snd) richmen
      where
        isValidKey :: Crypto.SigningKey -> Bool
        isValidKey key =
            CC.Common.checkVerKeyAddress
              (Crypto.toVerification key)
              (CC.UTxO.txOutAddress out)

    richmen :: [(Int, Crypto.SigningKey)]
    richmen =
        zip [0..] $
          CC.Genesis.gsRichSecrets $ pbftSecrets (encNodeConfigExt cfg)

    fromCompactTxInTxOutList :: [(CC.UTxO.CompactTxIn, CC.UTxO.CompactTxOut)]
                             -> [(CC.UTxO.TxIn, CC.UTxO.TxOut)]
    fromCompactTxInTxOutList =
        map (bimap CC.UTxO.fromCompactTxIn CC.UTxO.fromCompactTxOut)

    assumeBound :: Either CC.Common.LovelaceError CC.Common.Lovelace
                -> CC.Common.Lovelace
    assumeBound (Left _err) = error "elaborateTx: too much"
    assumeBound (Right ll)  = ll
