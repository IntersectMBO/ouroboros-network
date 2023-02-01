{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Extended (
    -- * Extended ledger state
    ExtLedgerCfg (..)
  , ExtLedgerState (..)
  , ExtValidationError (..)
    -- * Serialisation
  , decodeExtLedgerState
  , encodeExtLedgerState
    -- * Casts
  , castExtLedgerState
    -- * Type family instances
  , LedgerTables (..)
  , Ticked (..)
  , Ticked1 (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Data.Coerce
import           Data.Functor ((<&>))
import           Data.Kind (Type)
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked (Ticked1)

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
type ExtLedgerState :: Type -> LedgerStateKind
data ExtLedgerState blk mk = ExtLedgerState {
      ledgerState :: !(LedgerState blk mk)
    , headerState :: !(HeaderState blk)
    }
  deriving (Generic)

deriving instance (Show (LedgerState blk mk), LedgerSupportsProtocol blk) => Show (ExtLedgerState     blk mk)
deriving instance (Eq   (LedgerState blk mk), LedgerSupportsProtocol blk) => Eq   (ExtLedgerState     blk mk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance (NoThunks (LedgerState blk mk), LedgerSupportsProtocol blk) => NoThunks (ExtLedgerState blk mk) where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk))

data ExtValidationError blk =
    ExtValidationErrorLedger !(LedgerError blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  deriving (Generic)

instance LedgerSupportsProtocol blk => NoThunks (ExtValidationError blk)
deriving instance LedgerSupportsProtocol blk => Show (ExtValidationError blk)
deriving instance LedgerSupportsProtocol blk => Eq   (ExtValidationError blk)

{-------------------------------------------------------------------------------
  The extended ledger can behave like a ledger
-------------------------------------------------------------------------------}

data instance Ticked1 (ExtLedgerState blk) mk = TickedExtLedgerState {
      tickedLedgerState :: Ticked1 (LedgerState blk) mk
    , tickedLedgerView  :: Ticked  (LedgerView (BlockProtocol blk))
    , tickedHeaderState :: Ticked  (HeaderState blk)
    }

-- | " Ledger " configuration for the extended ledger
--
-- Since the extended ledger also does the consensus protocol validation, we
-- also need the consensus config.
newtype ExtLedgerCfg blk = ExtLedgerCfg {
      getExtLedgerCfg :: TopLevelConfig blk
    }
  deriving (Generic)

instance ( ConsensusProtocol (BlockProtocol blk)
         , NoThunks (BlockConfig   blk)
         , NoThunks (CodecConfig   blk)
         , NoThunks (LedgerConfig  blk)
         , NoThunks (StorageConfig blk)
         ) => NoThunks (ExtLedgerCfg blk)

type instance LedgerCfg (ExtLedgerState blk) = ExtLedgerCfg blk

type instance HeaderHash (ExtLedgerState blk)    = HeaderHash blk
type instance HeaderHash (ExtLedgerState blk mk) = HeaderHash blk

instance StandardHash (LedgerState blk) => StandardHash (ExtLedgerState blk)
instance StandardHash (LedgerState blk) => StandardHash (ExtLedgerState blk mk)

instance IsLedger (LedgerState blk) => GetTip (ExtLedgerState blk Canonical) where
  getTip = castPoint . getTip . ledgerState

instance IsLedger (LedgerState blk) => GetTip (Ticked1 (ExtLedgerState blk) Canonical) where
  getTip = castPoint . getTip . tickedLedgerState

instance ( LedgerSupportsProtocol blk
         )
      => IsLedger (ExtLedgerState blk) where
  type LedgerErr (ExtLedgerState blk) = ExtValidationError blk

  type AuxLedgerEvent (ExtLedgerState blk) = AuxLedgerEvent (LedgerState blk)

  applyChainTickLedgerResult cfg slot (ExtLedgerState ledger header) =
      castLedgerResult ledgerResult <&> \tickedLedgerState ->
      let tickedLedgerView :: Ticked (LedgerView (BlockProtocol blk))
          tickedLedgerView = protocolLedgerView lcfg tickedLedgerState

          tickedHeaderState :: Ticked (HeaderState blk)
          tickedHeaderState =
              tickHeaderState
                (configConsensus $ getExtLedgerCfg cfg)
                tickedLedgerView
                slot
                header
      in TickedExtLedgerState {..}
    where
      lcfg :: LedgerConfig blk
      lcfg = configLedger $ getExtLedgerCfg cfg

      ledgerResult = applyChainTickLedgerResult lcfg slot ledger

instance LedgerSupportsProtocol blk => ApplyBlock (ExtLedgerState blk) blk where
  applyBlockLedgerResult cfg blk TickedExtLedgerState{..} = do
    ledgerResult <-
        withExcept ExtValidationErrorLedger
      $ applyBlockLedgerResult
          (configLedger $ getExtLedgerCfg cfg)
          blk
          tickedLedgerState
    hdr <-
        withExcept ExtValidationErrorHeader
      $ validateHeader @blk
          (getExtLedgerCfg cfg)
          tickedLedgerView
          (getHeader blk)
          tickedHeaderState
    pure $ (\l -> ExtLedgerState l hdr) <$> castLedgerResult ledgerResult

  reapplyBlockLedgerResult cfg blk TickedExtLedgerState{..} =
      (\l -> ExtLedgerState l hdr) <$> castLedgerResult ledgerResult
    where
      ledgerResult =
        reapplyBlockLedgerResult
          (configLedger $ getExtLedgerCfg cfg)
          blk
          tickedLedgerState
      hdr      =
        revalidateHeader
          (getExtLedgerCfg cfg)
          tickedLedgerView
          (getHeader blk)
          tickedHeaderState


{-------------------------------------------------------------------------------
 Ledger Tables
-------------------------------------------------------------------------------}

instance HasLedgerTables (LedgerState blk)
      => HasLedgerTables (ExtLedgerState blk) where

  newtype LedgerTables (ExtLedgerState blk) mk = ExtLedgerStateTables {
      unExtLedgerStateTables :: LedgerTables (LedgerState blk) mk
      }
    deriving (Generic)

  projectLedgerTables (ExtLedgerState lstate _) =
      ExtLedgerStateTables (projectLedgerTables lstate)
  withLedgerTables (ExtLedgerState lstate hstate) (ExtLedgerStateTables tables) =
      ExtLedgerState (lstate `withLedgerTables` tables) hstate

  traverseLedgerTables f (ExtLedgerStateTables l) =
    ExtLedgerStateTables <$> traverseLedgerTables f l

  pureLedgerTables  f = coerce $ pureLedgerTables  @(LedgerState blk) f
  mapLedgerTables   f = coerce $ mapLedgerTables   @(LedgerState blk) f
  zipLedgerTables   f = coerce $ zipLedgerTables   @(LedgerState blk) f
  zipLedgerTables2  f = coerce $ zipLedgerTables2  @(LedgerState blk) f
  foldLedgerTables  f = coerce $ foldLedgerTables  @(LedgerState blk) f
  foldLedgerTables2 f = coerce $ foldLedgerTables2 @(LedgerState blk) f
  namesLedgerTables   = coerce $ namesLedgerTables @(LedgerState blk)
  zipLedgerTablesA  f (ExtLedgerStateTables l) (ExtLedgerStateTables r) =
    ExtLedgerStateTables <$> zipLedgerTablesA f l r
  zipLedgerTables2A  f (ExtLedgerStateTables l) (ExtLedgerStateTables c) (ExtLedgerStateTables r) =
     ExtLedgerStateTables <$> zipLedgerTables2A f l c r

deriving newtype instance Eq (LedgerTables (LedgerState blk) mk)
                       => Eq (LedgerTables (ExtLedgerState blk) mk)
deriving newtype instance NoThunks (LedgerTables (LedgerState blk) mk)
                       => NoThunks (LedgerTables (ExtLedgerState blk) mk)
deriving newtype instance Show (LedgerTables (LedgerState blk) mk)
                       => Show (LedgerTables (ExtLedgerState blk) mk)

instance ( LedgerSupportsProtocol blk
         , CanSerializeLedgerTables (LedgerState blk)
         )
      => CanSerializeLedgerTables (ExtLedgerState blk) where
  codecLedgerTables = ExtLedgerStateTables codecLedgerTables

instance LedgerTablesAreTrivial (LedgerState blk)
      => LedgerTablesAreTrivial (ExtLedgerState blk) where
  convertMapKind (ExtLedgerState st hst) =
      flip ExtLedgerState hst $ convertMapKind st

  trivialLedgerTables = ExtLedgerStateTables trivialLedgerTables

instance HasTickedLedgerTables (LedgerState blk)
      => HasTickedLedgerTables (ExtLedgerState blk) where
  projectLedgerTablesTicked (TickedExtLedgerState lstate _view _hstate) =
      ExtLedgerStateTables (projectLedgerTablesTicked lstate)
  withLedgerTablesTicked
    (TickedExtLedgerState lstate view hstate)
    (ExtLedgerStateTables tables) =
      TickedExtLedgerState (lstate `withLedgerTablesTicked` tables) view hstate

instance CanStowLedgerTables (LedgerState blk)
      => CanStowLedgerTables (ExtLedgerState blk) where

   stowLedgerTables (ExtLedgerState lstate hstate) =
     ExtLedgerState (stowLedgerTables lstate) hstate

   unstowLedgerTables (ExtLedgerState lstate hstate) =
     ExtLedgerState (unstowLedgerTables lstate) hstate

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState :: (LedgerState   blk mk -> Encoding)
                     -> (ChainDepState (BlockProtocol blk) -> Encoding)
                     -> (AnnTip        blk -> Encoding)
                     -> ExtLedgerState blk mk -> Encoding
encodeExtLedgerState encodeLedgerState
                     encodeChainDepState
                     encodeAnnTip
                     ExtLedgerState{..} = mconcat [
      encodeLedgerState  ledgerState
    , encodeHeaderState' headerState
    ]
  where
    encodeHeaderState' = encodeHeaderState
                           encodeChainDepState
                           encodeAnnTip

decodeExtLedgerState :: (forall s. Decoder s (LedgerState    blk mk))
                     -> (forall s. Decoder s (ChainDepState  (BlockProtocol blk)))
                     -> (forall s. Decoder s (AnnTip         blk))
                     -> (forall s. Decoder s (ExtLedgerState blk mk))
decodeExtLedgerState decodeLedgerState
                     decodeChainDepState
                     decodeAnnTip = do
    ledgerState <- decodeLedgerState
    headerState <- decodeHeaderState'
    return ExtLedgerState{..}
  where
    decodeHeaderState' = decodeHeaderState
                           decodeChainDepState
                           decodeAnnTip

{-------------------------------------------------------------------------------
  Casts
-------------------------------------------------------------------------------}

castExtLedgerState
  :: ( Coercible (LedgerState blk  mk)
                 (LedgerState blk' mk)
     , Coercible (ChainDepState (BlockProtocol blk))
                 (ChainDepState (BlockProtocol blk'))
     , TipInfo blk ~ TipInfo blk'
     )
  => ExtLedgerState blk mk -> ExtLedgerState blk' mk
castExtLedgerState ExtLedgerState{..} = ExtLedgerState {
      ledgerState = coerce ledgerState
    , headerState = castHeaderState headerState
    }
