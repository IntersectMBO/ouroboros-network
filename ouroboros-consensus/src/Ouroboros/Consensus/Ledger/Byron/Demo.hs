{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Ledger.Byron.Demo
  ( -- * Running Byron in the demo
    Config (..)
  , ByronExtNodeConfig
    -- * Forging a new block
  , forgeBlock
    -- * Elaboration from our mock transactions into transactions on the real ledger
  , elaborateTx
    -- * Serialisation
  , encodeHeader
  , encodeBlock
  , encodeHeaderHash
  , decodeHeader
  , decodeBlock
  , decodeHeaderHash
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (void)
import           Crypto.Random (MonadRandom)
import           Data.Bifunctor (bimap)
import           Data.Bimap (Bimap)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (coerce)
import           Data.Foldable (find)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Reflection (Given (..))
import qualified Data.Set as Set
import           Data.Typeable
import qualified Data.Vector as V

import           GHC.Stack (HasCallStack)

import           Cardano.Binary (Annotated (..), ByteSpan, fromCBOR, reAnnotate,
                     slice, toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Ssc as CC.Ssc
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.UTxO as CC.UTxO
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Ledger.Byron
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node (CoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT

{-------------------------------------------------------------------------------
  Running Byron in the demo
-------------------------------------------------------------------------------}

instance (Given CC.Block.HeaderHash, Given CC.Slot.EpochSlots)
      => BlockSupportsPBft PBftCardanoCrypto (ByronHeader Config)

-- Extended configuration we need for the demo
data Config = Config {
      -- | Mapping from generic keys to core node IDs
      --
      -- The keys in this map are the verification keys of the core nodes - that
      -- is, the delegates of the genesis keys.
      pbftCoreNodes       :: Bimap Crypto.VerificationKey CoreNodeId
    , pbftProtocolMagic   :: Crypto.ProtocolMagic
    , pbftProtocolVersion :: CC.Update.ProtocolVersion
    , pbftSoftwareVersion :: CC.Update.SoftwareVersion
    , pbftEpochSlots      :: CC.Slot.EpochSlots
    , pbftGenesisHash     :: CC.Genesis.GenesisHash
    , pbftGenesisDlg      :: CC.Genesis.GenesisDelegation
    , pbftSecrets         :: CC.Genesis.GeneratedSecrets
    }

type ByronExtNodeConfig = ExtNodeConfig Config (PBft PBftCardanoCrypto)

{-------------------------------------------------------------------------------
  Forging a new block
-------------------------------------------------------------------------------}

forgeBlock
  :: forall m cfg.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
     , Given Crypto.ProtocolMagicId
     , Given CC.Block.HeaderHash
     , Given CC.Slot.EpochSlots
     , Typeable cfg
     )
  => NodeConfig ByronExtNodeConfig
  -> SlotNo                       -- ^ Current slot
  -> BlockNo                      -- ^ Current block number
  -> ChainHash (ByronHeader cfg)  -- ^ Previous hash
  -> [GenTx (ByronBlock cfg)]     -- ^ Txs to add in the block
  -> ()                           -- ^ Leader proof ('IsLeader')
  -> m (ByronBlock Config)
forgeBlock cfg curSlot curNo prevHash txs () = do
    ouroborosPayload <- mkPayload (Proxy @(ByronBlock cfg)) cfg () preHeader
    return $ forge ouroborosPayload
  where
    Config {..} = encNodeConfigExt cfg

    txPayload :: CC.UTxO.TxPayload
    txPayload = CC.UTxO.mkTxPayload (map (void . unByronTx) txs)

    body :: CC.Block.Body
    body = CC.Block.ABody {
          CC.Block.bodyTxPayload     = txPayload
        , CC.Block.bodySscPayload    = CC.Ssc.SscPayload
        , CC.Block.bodyDlgPayload    = CC.Delegation.UnsafeAPayload [] ()
        , CC.Block.bodyUpdatePayload = CC.Update.APayload Nothing [] ()
        }

    proof :: CC.Block.Proof
    proof = CC.Block.mkProof body

    prevHeaderHash :: CC.Block.HeaderHash
    prevHeaderHash = case prevHash of
      GenesisHash -> CC.Block.genesisHeaderHash pbftGenesisHash
      BlockHash h -> h

    slotId :: CC.Slot.SlotId
    slotId = CC.Slot.unflattenSlotId pbftEpochSlots $ coerce curSlot

    preHeader :: CC.Block.ToSign
    preHeader = CC.Block.ToSign {
          CC.Block.tsHeaderHash      = prevHeaderHash
        , CC.Block.tsSlot            = slotId
        , CC.Block.tsDifficulty      = coerce curNo
        , CC.Block.tsBodyProof       = proof
        , CC.Block.tsProtocolVersion = pbftProtocolVersion
        , CC.Block.tsSoftwareVersion = pbftSoftwareVersion
        }

    forge :: Payload ByronExtNodeConfig CC.Block.ToSign -> ByronBlock Config
    forge ouroborosPayload =
        ByronBlock $ annotateBlock pbftEpochSlots block
      where
        block :: CC.Block.Block
        block = CC.Block.ABlock {
              CC.Block.blockHeader     = header
            , CC.Block.blockBody       = body
            , CC.Block.blockAnnotation = ()
            }

        headerGenesisKey :: Crypto.VerificationKey
        dlgCertificate :: CC.Delegation.Certificate
        (headerGenesisKey, dlgCertificate) = case findDelegate of
            Just x  -> x
            Nothing -> error "Issuer is not a valid genesis key delegate."
          where
            dlgMap = CC.Genesis.unGenesisDelegation pbftGenesisDlg
            VerKeyCardanoDSIGN issuer = pbftIssuer . encPayloadP $ ouroborosPayload
            findDelegate = fmap (\crt -> (Crypto.pskIssuerVK crt, crt))
                         . find (\crt -> Crypto.pskDelegateVK crt == issuer)
                         $ Map.elems dlgMap

        headerSignature :: CC.Block.BlockSignature
        headerSignature = CC.Block.BlockSignature
                        $ Crypto.AProxySignature dlgCertificate (coerce sig)
          where
            sig :: Crypto.Signature Encoding
            SignedDSIGN (SigCardanoDSIGN sig) = pbftSignature $ encPayloadP ouroborosPayload

        header :: CC.Block.Header
        header = CC.Block.AHeader {
              CC.Block.aHeaderProtocolMagicId = ann (Crypto.getProtocolMagicId pbftProtocolMagic)
            , CC.Block.aHeaderPrevHash        = ann prevHeaderHash
            , CC.Block.aHeaderSlot            = ann (coerce curSlot)
            , CC.Block.aHeaderDifficulty      = ann (coerce curNo)
            , CC.Block.headerProtocolVersion  = pbftProtocolVersion
            , CC.Block.headerSoftwareVersion  = pbftSoftwareVersion
            , CC.Block.aHeaderProof           = ann proof
            , CC.Block.headerGenesisKey       = headerGenesisKey
            , CC.Block.headerSignature        = headerSignature
            , CC.Block.headerAnnotation       = ()
            , CC.Block.headerExtraAnnotation  = ()
            }

        ann :: b -> Annotated b ()
        ann b = Annotated b ()

{-------------------------------------------------------------------------------
  Elaboration from our mock transactions into transactions on the real ledger
-------------------------------------------------------------------------------}

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
        $ pbftGenesisConfig (pbftParams (encNodeConfigP cfg))
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

{-------------------------------------------------------------------------------
  Add annotation
-------------------------------------------------------------------------------}

annotateBlock :: CC.Slot.EpochSlots -> CC.Block.Block -> CC.Block.ABlock ByteString
annotateBlock epochSlots =
      (\bs -> splice bs (CBOR.deserialiseFromBytes (CC.Block.fromCBORABlock epochSlots) bs))
    . CBOR.toLazyByteString
    . CC.Block.toCBORBlock epochSlots
  where
    splice :: Lazy.ByteString
           -> Either err (Lazy.ByteString, CC.Block.ABlock ByteSpan)
           -> CC.Block.ABlock ByteString
    splice _ (Left _err) =
      error "annotateBlock: serialization roundtrip failure"
    splice bs (Right (_leftover, txAux)) =
      (Lazy.toStrict . slice bs) <$> txAux

annotateHeader :: CC.Slot.EpochSlots -> CC.Block.Header -> CC.Block.AHeader ByteString
annotateHeader epochSlots =
      (\bs -> splice bs (CBOR.deserialiseFromBytes (CC.Block.fromCBORAHeader epochSlots) bs))
    . CBOR.toLazyByteString
    . CC.Block.toCBORHeader epochSlots
  where
    splice :: Lazy.ByteString
           -> Either err (Lazy.ByteString, CC.Block.AHeader ByteSpan)
           -> CC.Block.AHeader ByteString
    splice _ (Left _err) =
      error "annotateBlock: serialization roundtrip failure"
    splice bs (Right (_leftover, txAux)) =
      (Lazy.toStrict . slice bs) <$> txAux

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeHeader :: NodeConfig ByronExtNodeConfig
             -> ByronHeader Config -> Encoding
encodeHeader cfg =
      CC.Block.toCBORHeader epochSlots
    . void
    . unByronHeader
  where
    epochSlots = pbftEpochSlots (encNodeConfigExt cfg)

encodeBlock :: NodeConfig ByronExtNodeConfig
            -> ByronBlock Config -> Encoding
encodeBlock cfg =
      CC.Block.toCBORBlock epochSlots
    . void
    . unByronBlock
  where
    epochSlots = pbftEpochSlots (encNodeConfigExt cfg)

encodeHeaderHash :: HeaderHash (ByronHeader Config) -> Encoding
encodeHeaderHash = toCBOR

decodeHeader :: NodeConfig ByronExtNodeConfig
             -> Decoder s (ByronHeader Config)
decodeHeader cfg =
    ByronHeader . annotate <$> CC.Block.fromCBORAHeader epochSlots
  where
    -- TODO #560: Re-annotation can be done but requires some rearranging in
    -- the codecs Original ByteSpan's refer to bytestring we don't have, so
    -- we'll ignore them
    annotate :: CC.Block.AHeader a -> CC.Block.AHeader ByteString
    annotate = annotateHeader epochSlots . void

    epochSlots = pbftEpochSlots (encNodeConfigExt cfg)

decodeBlock :: NodeConfig ByronExtNodeConfig
            -> Decoder s (ByronBlock Config)
decodeBlock cfg =
    ByronBlock . annotate <$> CC.Block.fromCBORABlock epochSlots
  where
    -- TODO #560: Re-annotation can be done but requires some rearranging in
    -- the codecs Original ByteSpan's refer to bytestring we don't have, so
    -- we'll ignore them
    annotate :: CC.Block.ABlock a -> CC.Block.ABlock ByteString
    annotate = annotateBlock epochSlots . void

    epochSlots = pbftEpochSlots (encNodeConfigExt cfg)

decodeHeaderHash :: Decoder s (HeaderHash (ByronHeader Config))
decodeHeaderHash = fromCBOR
