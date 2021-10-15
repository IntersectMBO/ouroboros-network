{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
  , Ticked1 (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Data.Coerce
import           Data.Functor ((<&>))
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
import           GHC.Show (showCommaSpace, showSpace)
import           NoThunks.Class (NoThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState blk (mk :: MapKind) = ExtLedgerState {
      ledgerState :: !(LedgerState blk mk)
    , headerState :: !(HeaderState blk)
    }
  deriving (Generic)

instance InMemory (LedgerState blk) => InMemory (ExtLedgerState blk) where
  convertMapKind est =
    ExtLedgerState {
        ledgerState = convertMapKind $ ledgerState est
      , headerState = headerState est
      }

data ExtValidationError blk =
    ExtValidationErrorLedger !(LedgerError blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  deriving (Generic)

instance LedgerSupportsProtocol blk => NoThunks (ExtValidationError blk)

-- TODO this might be wrong but I'd like to understand why.
deriving instance (Show (LedgerState blk mk), Show (HeaderState blk))
  => Show (ExtLedgerState blk mk)

deriving instance LedgerSupportsProtocol blk => Show (ExtValidationError    blk)
deriving instance LedgerSupportsProtocol blk => Eq   (ExtValidationError    blk)

instance LedgerSupportsProtocol blk => ShowLedgerState (ExtLedgerState blk) where
  showsLedgerState mk st =
      showParen True $ showString "ExtLedgerState {"
        . showSpace      . showString "headerState = " . shows               headerState
        . showCommaSpace . showString "ledgerState = " . showsLedgerState mk ledgerState
        . showString " }"
    where
      ExtLedgerState _dummy _ = st
      ExtLedgerState {
          headerState
        , ledgerState
        } = st

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance (Typeable mk, LedgerSupportsProtocol blk, NoThunks (LedgerState blk mk)) => NoThunks (ExtLedgerState blk mk) where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk mk))

deriving instance ( LedgerSupportsProtocol blk
                  , Eq (ChainDepState (BlockProtocol blk))
                  , Eq (LedgerState blk mk)
                  ) => Eq (ExtLedgerState blk mk)

{-------------------------------------------------------------------------------
  The extended ledger can behave like a ledger
-------------------------------------------------------------------------------}

data instance Ticked1 (ExtLedgerState blk) mk = TickedExtLedgerState {
      tickedLedgerState :: Ticked1 (LedgerState blk) mk
    , tickedLedgerView  :: Ticked (LedgerView (BlockProtocol blk))
    , tickedHeaderState :: Ticked (HeaderState blk)
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

type instance HeaderHash (ExtLedgerState blk)    = HeaderHash (LedgerState blk)
type instance HeaderHash (ExtLedgerState blk mk) = HeaderHash (LedgerState blk)

instance StandardHash (LedgerState blk) => StandardHash (ExtLedgerState blk)
instance StandardHash (LedgerState blk) => StandardHash (ExtLedgerState blk mk)

instance IsLedger (LedgerState blk) => GetTip (ExtLedgerState blk mk) where
  getTip = castPoint . getTip . ledgerState

instance IsLedger (LedgerState blk) => GetTip (Ticked1 (ExtLedgerState blk) mk) where
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

instance (LedgerSupportsProtocol blk, TableStuff (LedgerState blk)) => TableStuff (ExtLedgerState blk) where

  newtype LedgerTables (ExtLedgerState blk) mk = ExtLedgerStateTables (LedgerTables (LedgerState blk) mk)
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

instance ( LedgerSupportsProtocol blk
         , SufficientSerializationForAnyBackingStore (LedgerState blk)
         )
      => SufficientSerializationForAnyBackingStore (ExtLedgerState blk) where
  codecLedgerTables = ExtLedgerStateTables codecLedgerTables

deriving instance ShowLedgerState (LedgerTables (LedgerState blk)) => ShowLedgerState (LedgerTables (ExtLedgerState blk))

deriving instance Eq        (LedgerTables (LedgerState blk) mk)               => Eq       (LedgerTables (ExtLedgerState blk) mk)
instance (NoThunks (LedgerTables (LedgerState blk) mk), Typeable mk) => NoThunks (LedgerTables (ExtLedgerState blk) mk)

instance InMemory (LedgerTables (LedgerState blk)) => InMemory (LedgerTables (ExtLedgerState blk)) where
  convertMapKind (ExtLedgerStateTables st) =
      ExtLedgerStateTables $ convertMapKind st

instance (LedgerSupportsProtocol blk, TickedTableStuff (LedgerState blk)) => TickedTableStuff (ExtLedgerState blk) where
  projectLedgerTablesTicked (TickedExtLedgerState lstate _view _hstate) =
      ExtLedgerStateTables (projectLedgerTablesTicked lstate)
  withLedgerTablesTicked
    (TickedExtLedgerState lstate view hstate)
    (ExtLedgerStateTables tables) =
      TickedExtLedgerState (lstate `withLedgerTablesTicked` tables) view hstate

instance (LedgerSupportsProtocol blk) => ApplyBlock (ExtLedgerState blk) blk where
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

  getBlockKeySets = ExtLedgerStateTables . getBlockKeySets

instance
     ( LedgerSupportsProtocol blk
     , StowableLedgerTables (LedgerState blk)
     )
  => StowableLedgerTables (ExtLedgerState blk) where

  stowLedgerTables ExtLedgerState{headerState, ledgerState} =
      ExtLedgerState {
          headerState
        , ledgerState = stowLedgerTables ledgerState
        }

  unstowLedgerTables ExtLedgerState{headerState, ledgerState} =
      ExtLedgerState {
          headerState
        , ledgerState = unstowLedgerTables ledgerState
        }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState :: (LedgerState   blk mk -> Encoding)
                     -> (ChainDepState (BlockProtocol blk) -> Encoding)
                     -> (AnnTip        blk    -> Encoding)
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
