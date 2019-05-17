{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_ghc -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.Byron where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Encoding
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Bifunctor (bimap)
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce
import           Data.FingerTree (Measured (..))
import           Data.Foldable (find)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Typeable
import qualified Data.Vector as V
import           Data.Word
import           Formatting

import           Cardano.Binary (Annotated (..), ByteSpan, fromCBOR, reAnnotate,
                     slice, toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as V.Interface
import qualified Cardano.Chain.Delegation.Validation.Scheduling as V.Scheduling
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Ssc as CC.Ssc
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Chain.UTxO as CC.UTxO
import qualified Cardano.Crypto as Crypto
import           Cardano.Prelude (panic)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node (CoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util.Condense

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

-- | Hard-coded number of slots per epoch in the Byron era
byronEpochSlots :: CC.Slot.EpochSlots
byronEpochSlots = Dummy.dummyEpochSlots

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain
-- for real as when we are running the demo.
newtype ByronBlock cfg = ByronBlock { unByronBlock :: CC.Block.ABlock ByteString }
  deriving (Eq, Show)

instance Condense (ByronBlock cfg) where
  -- TODO
  condense = condense . ByronHeader . CC.Block.blockHeader . unByronBlock

newtype ByronHeader cfg = ByronHeader { unByronHeader :: CC.Block.AHeader ByteString }
  deriving (Eq, Show)

instance Condense (ByronHeader cfg) where
  -- TODO
  condense hdr =
      "(issuer: " <> condenseKey issuer <>
      ", delegate: " <> condenseKey delegate <> ")"
    where
      psigPsk = Crypto.psigPsk
              . CC.Block.unBlockSignature
              . CC.Block.headerSignature
              . unByronHeader
              $ hdr
      issuer   = Crypto.pskIssuerVK psigPsk
      delegate = Crypto.pskDelegateVK psigPsk

      condenseKey :: Crypto.VerificationKey -> String
      condenseKey = T.unpack . sformat build


instance Condense (ChainHash (ByronHeader cfg)) where
  condense GenesisHash   = "genesis"
  condense (BlockHash h) = show h

byronHeader :: ByronBlock cfg -> ByronHeader cfg
byronHeader (ByronBlock b) = ByronHeader (CC.Block.blockHeader b)

instance Typeable cfg => Measured BlockMeasure (ByronBlock cfg) where
  measure = blockMeasure

instance Typeable cfg => Measured BlockMeasure (ByronHeader cfg) where
  measure = blockMeasure

convertSlot :: CC.Slot.FlatSlotId -> SlotNo
convertSlot = coerce

convertFlatSlotId :: SlotNo -> CC.Slot.FlatSlotId
convertFlatSlotId = coerce

instance Typeable cfg => HasHeader (ByronBlock cfg) where
  type HeaderHash (ByronBlock cfg) = CC.Block.HeaderHash

  blockHash      =            blockHash     . byronHeader
  blockPrevHash  = castHash . blockPrevHash . byronHeader
  blockSlot      =            blockSlot     . byronHeader
  blockNo        =            blockNo       . byronHeader
  blockInvariant = const True

genesisHash :: CC.Block.HeaderHash
genesisHash = coerce Dummy.dummyGenesisHash

instance Typeable cfg => HasHeader (ByronHeader cfg) where
  type HeaderHash (ByronHeader cfg) = CC.Block.HeaderHash

  -- Implementation of 'blockHash' derived from
  --
  -- > blockHashAnnotated :: ABlock ByteString -> HeaderHash
  -- > blockHashAnnotated = hashDecoded . fmap wrapHeaderBytes . blockHeader
  --
  -- I couldn't find a version for headers
  blockHash = Crypto.hashDecoded . fmap CC.Block.wrapHeaderBytes . unByronHeader

  -- We should distinguish the genesis hash
  -- TODO: I think this already lives somewhere. I don't know where. In fact,
  -- I think Erik or Ru already wrote this very 'HasHeader' instance :/
  blockPrevHash (ByronHeader h) = case CC.Block.headerPrevHash h of
    h' | h' == genesisHash -> GenesisHash
    _                      -> BlockHash $ CC.Block.headerPrevHash $ h

  blockSlot      = convertSlot . CC.Block.headerSlot . unByronHeader
  blockNo        = BlockNo . CC.Common.unChainDifficulty . CC.Block.headerDifficulty . unByronHeader
  blockInvariant = const True

instance StandardHash (ByronBlock cfg)
instance StandardHash (ByronHeader cfg)

instance Typeable cfg => LedgerConfigView (ByronBlock cfg) where
  ledgerConfigView EncNodeConfig{..} =
      ByronLedgerConfig $ pbftGenesisConfig (pbftParams encNodeConfigP)

instance UpdateLedger (ByronBlock cfg) where
  data LedgerState (ByronBlock cfg) = ByronLedgerState
      { blsCurrent :: CC.Block.ChainValidationState
        -- | Slot-bounded snapshots of the chain state
      , blsSnapshots :: Seq.Seq (SlotBounded CC.Block.ChainValidationState)
      }
    deriving (Eq, Show)
  newtype LedgerError (ByronBlock cfg) = ByronLedgerError CC.Block.ChainValidationError
    deriving (Eq, Show)
  newtype LedgerConfig (ByronBlock cfg) = ByronLedgerConfig Genesis.Config

  applyLedgerBlock (ByronLedgerConfig cfg) (ByronBlock block) (ByronLedgerState state snapshots)
    = mapExcept (bimap ByronLedgerError id) $ do
      CC.Block.BodyState { CC.Block.utxo, CC.Block.updateState, CC.Block.delegationState } <- CC.Block.updateBody bodyEnv bodyState block
      let
        state' = state
          { CC.Block.cvsLastSlot     = CC.Block.blockSlot block
          , CC.Block.cvsPreviousHash = Right $ CC.Block.blockHashAnnotated block
          , CC.Block.cvsUtxo         = utxo
          , CC.Block.cvsUpdateState  = updateState
          , CC.Block.cvsDelegationState = delegationState
          }
        snapshots' = trimSnapshots $
          if (CC.Block.cvsDelegationState state' == CC.Block.cvsDelegationState state)
          then snapshots
          else
            let startOfSnapshot = case snapshots of
                  _ Seq.:|> a -> sbUpper a
                  Seq.Empty   -> SlotNo 0
            in snapshots Seq.|> slotBounded startOfSnapshot (convertSlot $ CC.Block.blockSlot block) state'

      pure $ ByronLedgerState state' snapshots'
    where
      bodyState = CC.Block.BodyState
        { CC.Block.utxo        = CC.Block.cvsUtxo state
        , CC.Block.updateState = CC.Block.cvsUpdateState state
        , CC.Block.delegationState = CC.Block.cvsDelegationState state
        }
      bodyEnv = CC.Block.BodyEnvironment
        { CC.Block.protocolMagic = fixPM $ Genesis.configProtocolMagic cfg
        , CC.Block.k = Genesis.configK cfg
        , CC.Block.numGenKeys
        , CC.Block.protocolParameters = CC.UPI.adoptedProtocolParameters . CC.Block.cvsUpdateState $ state
        , CC.Block.currentEpoch = CC.Slot.slotNumberEpoch (Genesis.configEpochSlots cfg) (CC.Block.blockSlot block)
        }
      numGenKeys :: Word8
      numGenKeys =
        case length (Genesis.unGenesisWStakeholders $ Genesis.configBootStakeholders cfg) of
          n
            | n > fromIntegral (maxBound :: Word8) -> panic
              "updateBody: Too many genesis keys"
            | otherwise -> fromIntegral n
      fixPM (Crypto.AProtocolMagic a b) = Crypto.AProtocolMagic (reAnnotate a) b
      trimSnapshots = Seq.dropWhileL (\ss -> sbUpper ss
                                       < convertSlot (CC.Block.blockSlot block) - 2*(coerce $ Genesis.configK cfg))

  applyLedgerHeader (ByronLedgerConfig cfg) (ByronBlock block) (ByronLedgerState state snapshots)
    = mapExcept (bimap ByronLedgerError (\i -> ByronLedgerState i snapshots)) $ do
      updateState <- CC.Block.updateHeader headerEnv (CC.Block.cvsUpdateState state) (CC.Block.blockHeader block)
      pure $ state
        { CC.Block.cvsLastSlot     = CC.Block.blockSlot block
        , CC.Block.cvsPreviousHash = Right $ CC.Block.blockHashAnnotated block
        , CC.Block.cvsUpdateState  = updateState
        }
    where
      headerEnv = CC.Block.HeaderEnvironment
        { CC.Block.protocolMagic = fixPMI $ Genesis.configProtocolMagicId cfg
        , CC.Block.k          = Genesis.configK cfg
        , CC.Block.numGenKeys
        , CC.Block.delegationMap
        , CC.Block.lastSlot   = CC.Block.cvsLastSlot state
        }
      numGenKeys :: Word8
      numGenKeys =
        case length (Genesis.unGenesisWStakeholders $ Genesis.configBootStakeholders cfg) of
          n
            | n > fromIntegral (maxBound :: Word8) -> panic
              "updateBody: Too many genesis keys"
            | otherwise -> fromIntegral n

      delegationMap =
        V.Interface.delegationMap
        $ CC.Block.cvsDelegationState state

      fixPMI pmi = reAnnotate $ Annotated pmi ()

{-------------------------------------------------------------------------------
  Support for PBFT consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol (ByronBlock cfg) = ExtNodeConfig cfg (PBft PBftCardanoCrypto)

type instance BlockProtocol (ByronHeader cfg) = ExtNodeConfig cfg (PBft PBftCardanoCrypto)

instance Typeable cfg => HasPreHeader (ByronBlock cfg) where
  type PreHeader (ByronBlock cfg) = CC.Block.ToSign
  blockPreHeader = unAnnotated . CC.Block.recoverSignedBytes byronEpochSlots
                   . CC.Block.blockHeader . unByronBlock

-- TODO get rid of this once we have a BlockHeader type family
instance Typeable cfg => HasPreHeader (ByronHeader cfg) where
  type PreHeader (ByronHeader cfg) = CC.Block.ToSign
  blockPreHeader = unAnnotated . CC.Block.recoverSignedBytes byronEpochSlots
                   . unByronHeader

-- TODO get rid of this once we have a BlockHeader type family
instance Typeable cfg => HasPayload (PBft PBftCardanoCrypto) (ByronHeader cfg) where
  blockPayload _ (ByronHeader header) = PBftPayload
    { pbftIssuer = VerKeyCardanoDSIGN
                   . Crypto.pskDelegateVK
                   . Crypto.psigPsk
                   . CC.Block.unBlockSignature
                   . CC.Block.headerSignature
                   $ header
    , pbftSignature = SignedDSIGN
                      . SigCardanoDSIGN
                      . Crypto.Signature
                      . Crypto.psigSig
                      . CC.Block.unBlockSignature
                      . CC.Block.headerSignature
                      $ header
    }


instance Typeable cfg => HasPayload (PBft PBftCardanoCrypto) (ByronBlock cfg) where
  blockPayload cfg = blockPayload cfg . byronHeader

-- | Override the delegation map from the ledger view
--
-- This is to work around a bug in cardano-ledger
-- <https://github.com/input-output-hk/cardano-ledger/issues/504>
reconstructDelegationMap :: Bimap CC.Common.StakeholderId CC.Common.StakeholderId
reconstructDelegationMap =
    go $ Genesis.gdHeavyDelegation Dummy.dummyGenesisData
  where
    go :: Genesis.GenesisDelegation
       -> Bimap CC.Common.StakeholderId CC.Common.StakeholderId
    go = Bimap.fromList . map go' . Map.toList . Genesis.unGenesisDelegation

    go' :: (CC.Common.StakeholderId, Delegation.Certificate)
        -> (CC.Common.StakeholderId, CC.Common.StakeholderId)
    go' (from, to) =
        if issuer /= from
          then error "reconstructDelegationMap: unexpected issuer"
          else (from, delegate)
      where
        issuer, delegate :: CC.Common.StakeholderId
        issuer   = CC.Common.mkStakeholderId $ Crypto.pskIssuerVK   to
        delegate = CC.Common.mkStakeholderId $ Crypto.pskDelegateVK to

instance Typeable cfg => ProtocolLedgerView (ByronBlock cfg) where
  protocolLedgerView _ns (ByronLedgerState ls _) = PBftLedgerView
    reconstructDelegationMap
{-
    -- Delegation map
    ( Delegation.unMap
      . V.Interface.delegationMap
      . CC.Block.cvsDelegationState
      $ ls
    )
-}

  -- There are two cases here:
  --
  -- - The view we want is in the past. In this case, we attempt to find a
  --   snapshot which contains the relevant slot, and extract the delegation map
  --   from that.
  --
  -- - The view we want is in the future. In this case, we need to check the
  --   upcoming delegations to see what new delegations will be made in the
  --   future, and update the current delegation map based on that.
  anachronisticProtocolLedgerView _ns (ByronLedgerState ls ss) slot =
      case find (containsSlot slot) ss of
        -- We can find a snapshot which supports this slot
        Just sb -> Just $ PBftLedgerView . Delegation.unMap
                  . V.Interface.delegationMap
                  . CC.Block.cvsDelegationState <$> sb
        -- No snapshot - we could be in the past or in the future
        Nothing ->
          -- TODO Check that the slot is within 2k slots
          if slot >= currentSlot -- && slot <= currentSlot + 2*k
          then Just $ PBftLedgerView <$> applyUpcomingUpdates
          else Nothing
    where
      currentSlot = convertSlot $ CC.Block.cvsLastSlot ls
      containsSlot s sb = sbLower sb <= s && sbUpper sb >= s
      applyUpcomingUpdates = let
          dsNow = Delegation.unMap
                  . V.Interface.delegationMap
                  . CC.Block.cvsDelegationState
                  $ ls
          dsScheduled = V.Scheduling.scheduledDelegations
                  . V.Interface.schedulingState
                  . CC.Block.cvsDelegationState
                  $ ls
        in case Seq.takeWhileL (\sd -> convertSlot (V.Scheduling.sdSlot sd) <= slot) dsScheduled of
          Seq.Empty -> slotBounded currentSlot slot dsNow
          -- TODO We can issue the ledger view for longer than just up to the
          -- requested slot, but we need to know k to do so
          toApply@(_ Seq.:|> la) -> slotBounded (convertSlot . V.Scheduling.sdSlot $ la) slot
            $ foldl (\acc x -> Bimap.insert (V.Scheduling.sdDelegator x) (V.Scheduling.sdDelegate x) acc) dsNow toApply

{-------------------------------------------------------------------------------
  Running Byron in the demo
-------------------------------------------------------------------------------}

-- Extended configuration we need for the demo
data ByronDemoConfig = ByronDemoConfig {
      -- | Mapping from generic keys to core node IDs
      --
      -- The keys in this map are the verification keys of the core nodes - that
      -- is, the delegates of the genesis keys.
      pbftCoreNodes       :: Map Crypto.VerificationKey CoreNodeId -- TODO Bimap

    , pbftProtocolMagic   :: Crypto.ProtocolMagic
    , pbftProtocolVersion :: CC.Update.ProtocolVersion
    , pbftSoftwareVersion :: CC.Update.SoftwareVersion
    , pbftEpochSlots      :: CC.Slot.EpochSlots

      -- | TODO ok?
      --
      -- We can use 'CC.Dummy.dummyGenesisHash' for this
    , pbftGenesisHash     :: Genesis.GenesisHash
    , pbftGenesisDlg      :: Genesis.GenesisDelegation
    , pbftSecrets         :: Genesis.GeneratedSecrets
    }

type ByronPayload =
  Payload
    (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
    CC.Block.ToSign

forgeByronDemoBlock
  :: ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
     )
  => NodeConfig (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
  -> SlotNo                                -- ^ Current slot
  -> BlockNo                               -- ^ Current block number
  -> ChainHash (ByronHeader cfg)           -- ^ Previous hash
  -> Map (Hash ShortHash Mock.Tx) Mock.Tx  -- ^ Txs to add in the block
  -> ()                                    -- ^ Leader proof (IsLeader)
  -> m (ByronBlock ByronDemoConfig)
forgeByronDemoBlock cfg curSlot curNo prevHash txs () = do
    ouroborosPayload <- mkPayload toCBOR cfg () preHeader
    return $ forgeBlock ouroborosPayload
  where
    ByronDemoConfig {..} = encNodeConfigExt cfg

    txPayload :: CC.UTxO.TxPayload
    txPayload = CC.UTxO.mkTxPayload $ map (elaborateTx cfg) (Map.elems txs)

    body :: CC.Block.Body
    body = CC.Block.ABody {
          CC.Block.bodyTxPayload     = txPayload
        , CC.Block.bodySscPayload    = CC.Ssc.SscPayload
        , CC.Block.bodyDlgPayload    = Delegation.UnsafeAPayload [] ()
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

    forgeBlock :: ByronPayload -> ByronBlock ByronDemoConfig
    forgeBlock ouroborosPayload =
        ByronBlock $ annotateBlock pbftEpochSlots block
      where
        block :: CC.Block.Block
        block = CC.Block.ABlock {
              CC.Block.blockHeader     = header
            , CC.Block.blockBody       = body
            , CC.Block.blockAnnotation = ()
            }

        headerGenesisKey :: Crypto.VerificationKey
        dlgCertificate :: Delegation.Certificate
        (headerGenesisKey, dlgCertificate) = case findDelegate of
            Just x  -> x
            Nothing -> error "Issuer is not a valid genesis key delegate."
          where
            dlgMap = Genesis.unGenesisDelegation pbftGenesisDlg
            VerKeyCardanoDSIGN issuer = pbftIssuer . encPayloadP $ ouroborosPayload
            findDelegate = fmap (\crt -> (Crypto.pskIssuerVK crt, crt))
                           . find (\crt -> Crypto.pskDelegateVK crt == issuer)
                           $ Map.elems dlgMap

        headerSignature :: CC.Block.BlockSignature
        headerSignature = CC.Block.BlockSignature $ Crypto.AProxySignature dlgCertificate (coerce sig)
          where
            sig :: Crypto.Signature Encoding
            SignedDSIGN (SigCardanoDSIGN sig) = pbftSignature $ encPayloadP ouroborosPayload

        header :: CC.Block.Header
        header = CC.Block.AHeader {
              CC.Block.aHeaderProtocolMagicId = ann (Crypto.getProtocolMagicId pbftProtocolMagic)
            , CC.Block.aHeaderPrevHash        = ann prevHeaderHash
            , CC.Block.aHeaderSlot            = ann (convertFlatSlotId curSlot)
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
elaborateTx :: NodeConfig (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
            -> Mock.Tx -> CC.UTxO.TxAux
elaborateTx cfg (Mock.Tx ins outs) = CC.UTxO.mkTxAux tx witness
  where
    -- mockInp and mockOut in [0 .. 3] (index of rich actor)
    [(_hash, mockInp)]    = Set.toList ins
    [(mockAddr, mockVal)] = outs
    Just mockOut          = lookup mockAddr (zip ["a", "b", "c", "d"] [0..])

    tx :: CC.UTxO.Tx
    tx = CC.UTxO.UnsafeTx {
          txInputs     = txIn  :| []
        , txOutputs    = txOut :| []
        , txAttributes = CC.Common.mkAttributes ()
        }

    txIn :: CC.UTxO.TxIn
    txIn = fst . fst $ initialUtxo Map.! mockInp

    -- TODO: Can we reuse these special "initial balance" addresses? Not sure
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
          Genesis.gsRichSecrets $ pbftSecrets (encNodeConfigExt cfg)

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
    . toCBORBlockWithoutBoundary epochSlots
  where
    splice :: Lazy.ByteString
           -> Either err (Lazy.ByteString, CC.Block.ABlock ByteSpan)
           -> CC.Block.ABlock ByteString
    splice _ (Left _err) =
      error "annotateBlock: serialization roundtrip failure"
    splice bs (Right (_leftover, txAux)) =
      (Lazy.toStrict . slice bs) <$> txAux

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeByronDemoHeader :: NodeConfig (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
                      -> ByronHeader ByronDemoConfig -> Encoding
encodeByronDemoHeader cfg =
      CC.Block.toCBORHeader' epochSlots
    . fmap (const ())
    . unByronHeader
  where
    epochSlots = pbftEpochSlots (encNodeConfigExt cfg)

encodeByronDemoBlock :: NodeConfig (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
                     -> ByronBlock ByronDemoConfig -> Encoding
encodeByronDemoBlock cfg =
      toCBORBlockWithoutBoundary epochSlots
    . fmap (const ())
    . unByronBlock
  where
    epochSlots = pbftEpochSlots (encNodeConfigExt cfg)

encodeByronDemoHeaderHash :: NodeConfig (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
                          -> HeaderHash (ByronHeader ByronDemoConfig) -> Encoding
encodeByronDemoHeaderHash _cfg = toCBOR

encodeByronDemoPreHeader :: NodeConfig (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
                         -> PreHeader (ByronBlock ByronDemoConfig) -> Encoding
encodeByronDemoPreHeader _cfg = toCBOR

decodeByronDemoHeader :: NodeConfig (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
                      -> Decoder s (ByronHeader ByronDemoConfig)
decodeByronDemoHeader cfg =
    fmap (ByronHeader . annotate) $
      CC.Block.fromCBORAHeader epochSlots
  where
    -- TODO: We should avoid re-encoding
    annotate :: CC.Block.AHeader ByteSpan -> CC.Block.AHeader ByteString
    annotate h = fmap (\_ -> CBOR.toStrictByteString . CC.Block.toCBORHeader epochSlots $ fmap (const ()) h) h

    epochSlots = pbftEpochSlots (encNodeConfigExt cfg)

decodeByronDemoBlock :: NodeConfig (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
                     -> Decoder s (ByronBlock ByronDemoConfig)
decodeByronDemoBlock cfg =
    fmap (ByronBlock . annotate) $
      CC.Block.fromCBORABlock epochSlots
  where
    -- TODO: Can we avoid having to re-encode?
    annotate :: CC.Block.ABlock ByteSpan -> CC.Block.ABlock ByteString
    annotate b = fmap (\_ -> CBOR.toStrictByteString . CC.Block.toCBORBlock epochSlots $ fmap (const ()) b) b

    epochSlots = pbftEpochSlots (encNodeConfigExt cfg)

decodeByronDemoHeaderHash :: NodeConfig (ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto))
                          -> Decoder s (HeaderHash (ByronHeader ByronDemoConfig))
decodeByronDemoHeaderHash _cfg = fromCBOR

{-------------------------------------------------------------------------------
  This should be exported from -ledger
-------------------------------------------------------------------------------}

toCBORBlockWithoutBoundary :: CC.Slot.EpochSlots -> CC.Block.Block -> Encoding
toCBORBlockWithoutBoundary epochSlots block
  =  Encoding.encodeListLen 3
  <> CC.Block.toCBORHeader' epochSlots (CC.Block.blockHeader block)
  <> toCBOR (CC.Block.blockBody block)
  <> (Encoding.encodeListLen 1 <> toCBOR (mempty :: Map Word8 Lazy.ByteString))
