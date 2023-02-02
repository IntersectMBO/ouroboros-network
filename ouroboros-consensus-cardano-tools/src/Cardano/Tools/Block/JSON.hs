{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tools.Block.JSON () where

import Cardano.Binary (
    ToCBOR (..),
    encodeListLen,
    encodeWord,
    serialize',
    serializeEncoding',
 )
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Alonzo (AlonzoAuxiliaryData, AlonzoEra, AlonzoScript, MaryValue)
import Cardano.Ledger.Alonzo.Data (binaryDataToData)
import qualified Cardano.Ledger.Alonzo.Data as Ledger.Alonzo
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), AlonzoTxBody)
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import Cardano.Ledger.Alonzo.TxSeq (txSeqTxns)
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger.Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.Babbage as Ledger
import Cardano.Ledger.Babbage.Scripts (AlonzoScript (..))
import qualified Cardano.Ledger.Babbage.Tx as Ledger.Babbage
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody, BabbageTxOut (..), Datum (..))
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import Cardano.Ledger.Block (bbody)
import qualified Cardano.Ledger.Block as Block
import Cardano.Ledger.Core (ScriptHash (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Mary.Value (AssetName (AssetName), MaryValue (..), PolicyID (PolicyID))
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger.Mary
import Data.Aeson (
    ToJSON (..),
    ToJSONKey,
    Value (..),
    object,
    toJSONKey,
    (.=),
 )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (
    Pair,
    toJSONKeyText,
 )
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base16 as SHex
import Data.ByteString.Short (fromShort)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..), isSJust)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as SE
import Data.Typeable (Typeable)
import Ouroboros.Consensus.Block (HeaderFields (..), getHeader, getHeaderFields, unBlockNo)
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..), Header)
import Ouroboros.Consensus.HardFork.Combinator (OneEraBlock (getOneEraBlock), OneEraHash, getHardForkBlock, getOneEraHash)
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Util.SOP (nsToIndex)

type CBlock = CardanoBlock StandardCrypto

type LedgerEra = Ledger.BabbageEra StandardCrypto

instance ToJSON CBlock where
    toJSON = \case
        block@(BlockBabbage (ShelleyBlock (txSeqTxns . bbody -> txs) _)) ->
            object
                [ "transactions" .= toList txs
                , "blockHash" .= getHeader block
                , "era" .= (nsToIndex . getOneEraBlock . getHardForkBlock $ block)
                ]
        block@(BlockAlonzo (ShelleyBlock (txSeqTxns . bbody -> txs) _)) ->
            object
                [ "transactions" .= toList txs
                , "blockHash" .= getHeader block
                , "era" .= (nsToIndex . getOneEraBlock . getHardForkBlock $ block)
                ]
        block ->
            object
                [ "blockHash" .= getHeader block
                , "era" .= (nsToIndex . getOneEraBlock . getHardForkBlock $ block)
                ]

-- | A very partial JSON instnace of the block header.
instance ToJSON (Header CBlock) where
    toJSON (getHeaderFields -> HeaderFields slotNo blockNo bhash) =
        object
            [ "slotNo" .= slotNo
            , "blockNo" .= unBlockNo blockNo
            , "blockHash" .= blockHashJson bhash
            ]

blockHashJson :: OneEraHash h -> Value
blockHashJson = String . SE.decodeUtf8 . SHex.encode . fromShort . getOneEraHash

--
-- AuxiliaryData
--

instance ToCBOR (AlonzoAuxiliaryData era) => ToJSON (AlonzoAuxiliaryData era) where
    toJSON = String . SE.decodeUtf8 . Base16.encode . serialize'

instance ToJSON (Ledger.AuxiliaryDataHash crypto) where
    toJSON =
        String
            . SE.decodeUtf8
            . Base16.encode
            . Crypto.hashToBytes
            . Ledger.extractHash
            . Ledger.unsafeAuxiliaryDataHash

--
-- Bootstrap Witness
--

instance Crypto crypto => ToJSON (Ledger.BootstrapWitness crypto) where
    toJSON = String . SE.decodeUtf8 . Base16.encode . serialize'

--
-- DCert
--
-- TODO: Delegation certificates can actually be represented as plain JSON
-- objects (it's a sum type), so we may want to revisit this interface later?

instance Crypto crypto => ToJSON (Ledger.DCert crypto) where
    toJSON = String . SE.decodeUtf8 . Base16.encode . serialize'

--
-- IsValid
--

instance ToJSON Ledger.Babbage.IsValid where
    toJSON (Ledger.Babbage.IsValid b) = toJSON b

--
-- Redeemers
--
-- TODO: Provide maybe better instances for redeemers from which we can actually
-- view them as a map from pointers to data?

instance ToCBOR (Ledger.Alonzo.Redeemers era) => ToJSON (Ledger.Alonzo.Redeemers era) where
    toJSON = String . SE.decodeUtf8 . Base16.encode . serialize'

--
-- RewardAcnt
--

-- NOTE: The Ledge derive generic ToJSONKey from 'RewardAcnt', which by default
-- turn them into an array of elements.
rewardAcntToText :: Ledger.RewardAcnt crypto -> Text
rewardAcntToText = SE.decodeUtf8 . Base16.encode . Ledger.serialiseRewardAcnt

--
-- SafeHash
--

instance (Crypto crypto) => ToJSON (Ledger.SafeHash crypto any) where
    toJSON = String . safeHashToText

instance (Crypto crypto) => ToJSONKey (Ledger.SafeHash crypto any) where
    toJSONKey = toJSONKeyText safeHashToText

safeHashToText ::
    Ledger.SafeHash crypto any ->
    Text
safeHashToText =
    SE.decodeUtf8 . Base16.encode . Crypto.hashToBytes . Ledger.extractHash

--
-- Script
--

instance ToJSON (AlonzoScript era) where
    toJSON (TimelockScript time) =
        object ["timelock" .= show time]
    toJSON (PlutusScript lan sbs) =
        object
            [ "language" .= show lan
            , "code" .= String (SE.decodeUtf8 $ Base16.encode $ fromShort sbs)
            ]

--
-- ScriptHash
--

instance Crypto crypto => ToJSONKey (Ledger.ScriptHash crypto) where
    toJSONKey = toJSONKeyText $ \(Ledger.ScriptHash h) ->
        SE.decodeUtf8 $ Base16.encode (Crypto.hashToBytes h)

--
-- Timelock
--

instance ToJSON (Ledger.Mary.Timelock StandardCrypto) where
    toJSON = String . SE.decodeUtf8 . Base16.encode . serialize'

--
-- TxBody
--

instance ToJSON (BabbageTxBody LedgerEra) where
    toJSON b =
        object $
            mconcat
                [ onlyIf (const True) "inputs" (Ledger.Babbage.inputs b)
                , onlyIf (not . null) "collateral" (Ledger.Babbage.collateral b)
                , onlyIf (not . null) "referenceInputs" (Ledger.Babbage.referenceInputs b)
                , onlyIf (const True) "outputs" (Ledger.Babbage.outputs' b)
                , onlyIf isSJust "collateralReturn" (Ledger.Babbage.collateralReturn' b)
                , onlyIf isSJust "totalCollateral" (Ledger.Babbage.totalCollateral b)
                , onlyIf (not . null) "certificates" (Ledger.Babbage.txcerts b)
                , onlyIf (not . null . Ledger.unWdrl) "withdrawals" (Ledger.Babbage.txwdrls b)
                , onlyIf (const True) "fees" (Ledger.Babbage.txfee b)
                , onlyIf (not . isOpenInterval) "validity" (Ledger.Babbage.txvldt b)
                , onlyIf (not . null) "requiredSignatures" (Ledger.Babbage.reqSignerHashes b)
                , onlyIf (/= mempty) "mint" (Ledger.Babbage.mint b)
                , onlyIf isSJust "scriptIntegrityHash" (Ledger.Babbage.scriptIntegrityHash b)
                , onlyIf isSJust "auxiliaryDataHash" (Ledger.Babbage.adHash b)
                , onlyIf isSJust "networkId" (Ledger.Babbage.txnetworkid b)
                ]

instance ToJSON (AlonzoTxBody (AlonzoEra StandardCrypto)) where
    toJSON b =
        object $
            mconcat
                [ onlyIf (const True) "inputs" (Ledger.Alonzo.inputs b)
                , onlyIf (not . null) "collateral" (Ledger.Alonzo.collateral b)
                , onlyIf (const True) "outputs" (Ledger.Alonzo.outputs b)
                , onlyIf (not . null) "collateral" (Ledger.Alonzo.collateral b)
                , onlyIf (not . null) "certificates" (Ledger.Alonzo.txcerts b)
                , onlyIf (not . null . Ledger.unWdrl) "withdrawals" (Ledger.Alonzo.txwdrls b)
                , onlyIf (const True) "fees" (Ledger.Alonzo.txfee b)
                , onlyIf (not . isOpenInterval) "validity" (Ledger.Alonzo.txvldt b)
                , onlyIf (not . null) "requiredSignatures" (Ledger.Alonzo.reqSignerHashes b)
                , onlyIf (/= mempty) "mint" (Ledger.Alonzo.mint b)
                , onlyIf isSJust "scriptIntegrityHash" (Ledger.Alonzo.scriptIntegrityHash b)
                , onlyIf isSJust "auxiliaryDataHash" (Ledger.Alonzo.adHash b)
                , onlyIf isSJust "networkId" (Ledger.Alonzo.txnetworkid b)
                ]

--
-- TxDats
--

instance
    ( Typeable era
    , Crypto (Ledger.Crypto era)
    ) =>
    ToJSON (Ledger.Alonzo.TxDats era)
    where
    toJSON (Ledger.Alonzo.TxDats datums) = toJSON datums

instance
    ( Typeable era
    ) =>
    ToJSON (Ledger.Alonzo.Data era)
    where
    toJSON = String . SE.decodeUtf8 . Base16.encode . serialize'

--
-- TxId
--

instance Crypto crypto => ToJSON (Ledger.TxId crypto) where
    toJSON = String . txIdToText @crypto

txIdToText :: Ledger.TxId crypto -> Text
txIdToText (Ledger.TxId h) = safeHashToText h

--
-- TxWitness
--

instance
    ( ToJSON (Core.Script era)
    , Core.Script era ~ AlonzoScript era
    , Era era
    ) =>
    ToJSON (Ledger.Alonzo.TxWitness era)
    where
    toJSON (Ledger.Alonzo.TxWitness vkeys boots scripts datums redeemers) =
        object $
            mconcat
                [ onlyIf (not . null) "keys" vkeys
                , onlyIf (not . null) "bootstrap" boots
                , onlyIf (not . null) "scripts" scripts
                , onlyIf (not . Ledger.Alonzo.nullDats) "datums" datums
                , onlyIf (not . Ledger.Alonzo.nullRedeemers) "redeemers" redeemers
                ]

--
-- TxIn
--

instance ToJSON (Ledger.TxIn StandardCrypto) where
    toJSON (Ledger.TxIn ti ti') =
        case toJSON ti of
            String txt -> String $ txt <> "#" <> pack (show ti')
            other -> error $ "Invalid encoding for txId: " <> show other

-- TxOut
instance ToJSON (Ledger.Babbage.BabbageTxOut LedgerEra) where
    toJSON (BabbageTxOut addr vl datum refScript) =
        object $
            [ "address" .= addr
            , "value" .= vl
            ]
                <> onlyIf isSJust "reference" refScript
                <> onlyIf (/= NoDatum) "datum" datum

-- Datum

instance ToJSON (Ledger.Babbage.Datum LedgerEra) where
    toJSON NoDatum = Null
    toJSON (DatumHash sh) = toJSON sh
    toJSON (Datum sbs) = String $ pack $ show $ binaryDataToData sbs

-- Value

instance ToJSON (MaryValue StandardCrypto) where
    toJSON (MaryValue n tokens) =
        object
            [ "lovelace" .= n
            , "tokens" .= tokens
            ]

instance ToJSONKey (PolicyID StandardCrypto) where
    toJSONKey = toJSONKeyText $ \(PolicyID (ScriptHash sh)) ->
        SE.decodeUtf8
            . Base16.encode
            . Crypto.hashToBytes
            $ sh

instance ToJSON (PolicyID StandardCrypto) where
    toJSON (PolicyID (ScriptHash sh)) =
        String
            . SE.decodeUtf8
            . Base16.encode
            . Crypto.hashToBytes
            $ sh

instance ToJSONKey AssetName where
    toJSONKey = toJSONKeyText $ \(AssetName sbs) ->
        SE.decodeUtf8
            . Base16.encode
            . fromShort
            $ sbs

instance ToJSON AssetName where
    toJSON (AssetName sbs) =
        String
            . SE.decodeUtf8
            . Base16.encode
            . fromShort
            $ sbs

--
-- ValidatedTx
--

instance
    ( ToJSON (Ledger.Alonzo.TxWitness era)
    , ToJSON (Core.TxBody era)
    , ToJSON (Core.AuxiliaryData era)
    , ToJSON (Core.Script era)
    , Core.Script era ~ AlonzoScript era
    , Core.EraTxBody era
    , Era era
    ) =>
    ToJSON (AlonzoTx era)
    where
    toJSON (AlonzoTx txbody witnesses isvalid auxData) =
        object $
            mconcat
                [ ["id" .= Block.txid txbody]
                , ["body" .= txbody]
                , ["witnesses" .= witnesses]
                , ["isValid" .= isvalid]
                , onlyIf isSJust "auxiliaryData" auxData
                ]

--
-- ValidityInterval
--

instance ToJSON Ledger.Mary.ValidityInterval where
    toJSON (Ledger.Mary.ValidityInterval notBefore notAfter) =
        object
            [ "notBefore" .= notBefore
            , "notAfter" .= notAfter
            ]

--
-- Wdrl
--

instance Crypto crypto => ToJSON (Ledger.Wdrl crypto) where
    toJSON = toJSON . Map.mapKeys rewardAcntToText . Ledger.unWdrl

--
-- WitVKey
--

instance Crypto crypto => ToJSON (Ledger.WitVKey 'Ledger.Witness crypto) where
    toJSON = String . SE.decodeUtf8 . Base16.encode . serializeEncoding' . prefixWithTag
      where
        prefixWithTag wit = encodeListLen 2 <> encodeWord 0 <> toCBOR wit

--
-- Helpers
--

onlyIf :: ToJSON a => (a -> Bool) -> Aeson.Key -> a -> [Pair]
onlyIf predicate k v =
    [(k, toJSON v) | predicate v]

isOpenInterval :: Ledger.Mary.ValidityInterval -> Bool
isOpenInterval = \case
    Ledger.Mary.ValidityInterval SNothing SNothing -> True
    _ -> False
