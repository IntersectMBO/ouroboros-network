{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTSyntax            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Ledger.Shelley.Ledger where

import qualified BaseTypes as SL
import           BlockChain (BHBody (..), bhHash, bhbody, bheader)
import           Cardano.Binary (fromCBOR, toCBOR)
import           Cardano.Ledger.Shelley.API
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.Slot (WithOrigin (..), fromWithOrigin)
import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Arrow (left)
import           Control.Monad.Except (ExceptT (..), runExcept, withExcept)
import           Control.Monad.Identity (Identity (..))
import           Data.Either (fromRight)
import           GHC.Generics (Generic)
import qualified LedgerState as SL
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Shelley.Block
import qualified Ouroboros.Consensus.Ledger.Shelley.History as History
import           Ouroboros.Consensus.Ledger.Shelley.TPraos ()
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Network.Block

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

data CombinedLedgerError
  = TickError (TickTransitionError TPraosStandardCrypto)
  | BBodyError (BlockTransitionError TPraosStandardCrypto)
  deriving (Eq, Generic, Show)

instance NoUnexpectedThunks CombinedLedgerError

instance UpdateLedger ShelleyBlock where
  data LedgerState ShelleyBlock
    = ShelleyLedgerState
        { ledgerTip :: Point ShelleyBlock,
          history :: History.LedgerViewHistory,
          shelleyLedgerState :: SL.NewEpochState TPraosStandardCrypto
        }
    deriving (Eq, Show, Generic)
  type LedgerError ShelleyBlock = CombinedLedgerError

  newtype LedgerConfig ShelleyBlock = ShelleyLedgerConfig SL.Globals
    deriving (Generic)

  ledgerConfigView TPraosNodeConfig {tpraosParams} =
    ShelleyLedgerConfig $
      SL.Globals
        { SL.epochInfo = tpraosEpochInfo tpraosParams,
          SL.slotsPerKESPeriod = tpraosKESPeriod tpraosParams,
          SL.securityParameter = sp,
          SL.startRewards = 3 * sp,
          SL.slotsPrior = 3 * sp
        }
    where
      SecurityParam sp = tpraosSecurityParam tpraosParams

  applyChainTick
    (ShelleyLedgerConfig globals)
    slotNo
    (ShelleyLedgerState pt history bhState) =
      TickedLedgerState
        . ShelleyLedgerState pt history
        $ applyTickTransition globals bhState slotNo

  applyLedgerBlock
    (ShelleyLedgerConfig globals)
    sb@(ShelleyBlock blk)
    (ShelleyLedgerState _ history bhState) =
      do
        st' <- withExcept BBodyError $ applyBlockTransition globals bhState blk
        let history' =
              if currentLedgerView bhState == currentLedgerView st'
                then history
                else
                  History.snapOld
                    (SL.securityParameter globals)
                    (blockSlot sb)
                    (currentLedgerView bhState)
                    history
        pure $! ShelleyLedgerState newPt history' st'
      where
        newPt =
          BlockPoint
            (bheaderSlotNo . bhbody $ bheader blk)
            (ShelleyHash . bhHash $ bheader blk)

  -- TODO actual reapplication
  reapplyLedgerBlock cfg blk ls =
    let ExceptT (Identity e) = applyLedgerBlock cfg blk ls
        err = error "Panic! Reapplication of Shelley ledger block failed"
     in fromRight err e

  ledgerTipPoint = ledgerTip

instance NoUnexpectedThunks (LedgerState ShelleyBlock)

instance ProtocolLedgerView ShelleyBlock where
  protocolLedgerView _cfg (ShelleyLedgerState _ _ ls) = currentLedgerView ls

  anachronisticProtocolLedgerView
    cfg
    ( ShelleyLedgerState
        (fromWithOrigin genesisSlotNo . pointSlot -> now)
        history
        st
      )
    woslot = case History.find woslot history of
      Just lv -> Right lv
      Nothing
        | woslot < At maxLo -> Left TooFarBehind -- lower bound is inclusive
        | woslot >= At maxHi -> Left TooFarAhead -- upper bound is exclusive
        | otherwise ->
          left (const TooFarAhead) -- TODO maybe incorrect,
              -- but we don't expect an error here
            . runExcept
            $ futureLedgerView
              globals
              st
              (fromWithOrigin genesisSlotNo woslot)
      where
        SecurityParam k = tpraosSecurityParam . tpraosParams $ cfg
        ShelleyLedgerConfig globals = ledgerConfigView cfg
        maxHi, maxLo :: SlotNo
        maxLo =
          SlotNo $
            if (2 * k) > unSlotNo now
              then 0
              else unSlotNo now - (2 * k)
        maxHi = SlotNo $ unSlotNo now + (2 * k)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeShelleyLedgerState :: LedgerState ShelleyBlock -> Encoding
encodeShelleyLedgerState
  ShelleyLedgerState
    { ledgerTip,
      history,
      shelleyLedgerState
    } = mconcat
    [ CBOR.encodeListLen 3
    , encode ledgerTip
    , History.encode history
    , toCBOR shelleyLedgerState
    ]

decodeShelleyLedgerState :: Decoder r (LedgerState ShelleyBlock)
decodeShelleyLedgerState = do
  CBOR.decodeListLenOf 3
  ShelleyLedgerState
    <$> decode
    <*> History.decode
    <*> fromCBOR
