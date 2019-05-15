{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_ghc -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.Byron where

import           Cardano.Binary (Annotated (..), reAnnotate)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as V.Interface
import qualified Cardano.Chain.Delegation.Validation.Scheduling as V.Scheduling
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Crypto as Crypto
import           Cardano.Prelude (panic)
import           Control.Monad.Except
import           Data.Bifunctor (bimap)
import qualified Data.Bimap as Bimap
import           Data.ByteString (ByteString)
import           Data.Coerce
import           Data.FingerTree (Measured (..))
import           Data.Foldable (find)
import qualified Data.Sequence as Seq
import           Data.Word
import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Crypto.DSIGN.Class (SignedDSIGN (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Network.Block

-- | Hard-coded number of slots per epoch in the Byron era
byronEpochSlots :: CC.Slot.EpochSlots
byronEpochSlots = CC.Slot.EpochSlots 21600

-- | Newtype wrapper to avoid orphan instances
newtype ByronBlock = ByronBlock { unByronBlock :: CC.Block.ABlock ByteString }
  deriving (Eq, Show)

newtype ByronHeader = ByronHeader { unByronHeader :: CC.Block.AHeader ByteString }

byronHeader :: ByronBlock -> ByronHeader
byronHeader (ByronBlock b) = ByronHeader (CC.Block.blockHeader b)

instance StandardHash ByronBlock

instance Measured BlockMeasure ByronBlock where
  measure = blockMeasure

convertSlot :: CC.Slot.FlatSlotId -> SlotNo
convertSlot = fromIntegral @Word64 . coerce

instance HasHeader ByronBlock where
  type HeaderHash ByronBlock = CC.Block.HeaderHash

  blockHash = CC.Block.blockHashAnnotated . unByronBlock
  -- TODO distinguish the genesis hash? How do we do this after the fact?
  blockPrevHash = BlockHash . CC.Block.blockPrevHash . unByronBlock
  blockSlot = convertSlot . CC.Block.blockSlot . unByronBlock
  blockNo = BlockNo . CC.Common.unChainDifficulty . CC.Block.blockDifficulty . unByronBlock
  blockInvariant = const True


instance UpdateLedger ByronBlock where
  data LedgerState ByronBlock = ByronLedgerState
      { blsCurrent :: CC.Block.ChainValidationState
        -- | Slot-bounded snapshots of the chain state
      , blsSnapshots :: Seq.Seq (SlotBounded CC.Block.ChainValidationState)
      }
    deriving (Eq, Show)
  newtype LedgerError ByronBlock = ByronLedgerError CC.Block.ChainValidationError
    deriving (Eq, Show)
  newtype LedgerConfig ByronBlock = ByronLedgerConfig Genesis.Config

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

type instance BlockProtocol ByronBlock = PBft PBftCardanoCrypto

instance HasPreHeader ByronBlock where
  type PreHeader ByronBlock = CC.Block.ToSign
  blockPreHeader = unAnnotated . CC.Block.recoverSignedBytes byronEpochSlots
                   . CC.Block.blockHeader . unByronBlock

instance HasPayload (PBft PBftCardanoCrypto) ByronBlock where
  blockPayload _ (ByronBlock aBlock) = PBftPayload
    { pbftIssuer = VerKeyCardanoDSIGN
                   . Crypto.pskIssuerVK
                   . Crypto.psigPsk
                   . CC.Block.unBlockSignature
                   . CC.Block.headerSignature
                   . CC.Block.blockHeader
                   $ aBlock
    , pbftSignature = SignedDSIGN
                      . SigCardanoDSIGN
                      . Crypto.Signature
                      . Crypto.psigSig
                      . CC.Block.unBlockSignature
                      . CC.Block.headerSignature
                      . CC.Block.blockHeader
                      $ aBlock
    }

instance ProtocolLedgerView ByronBlock where
  protocolLedgerView _ns (ByronLedgerState ls _) = PBftLedgerView
    -- Delegation map
    ( Delegation.unMap
      . V.Interface.delegationMap
      . CC.Block.cvsDelegationState
      $ ls
    )

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
