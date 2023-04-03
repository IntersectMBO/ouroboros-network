{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
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
    -- * Type family instances
  , LedgerTables (..)
  , Ticked1 (..)
  ) where

import           Cardano.Ledger.Binary.Plain (Decoder, Encoding,
                     decodeRecordNamed, encodeListLen)
import           Control.Monad.Except
import           Data.Coerce
import           Data.Functor ((<&>))
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
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

data ExtValidationError blk =
    ExtValidationErrorLedger !(LedgerError blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  deriving (Generic)

deriving instance LedgerSupportsProtocol blk => Eq (ExtValidationError blk)
deriving instance LedgerSupportsProtocol blk => NoThunks (ExtValidationError blk)
deriving instance LedgerSupportsProtocol blk => Show (ExtValidationError blk)

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState blk mk = ExtLedgerState {
      ledgerState :: !(LedgerState blk mk)
    , headerState :: !(HeaderState blk)
    }
  deriving (Generic)

deriving instance (IsMapKind mk, LedgerSupportsProtocol blk)
               => Eq (ExtLedgerState blk mk)
deriving instance (IsMapKind mk, LedgerSupportsProtocol blk)
               => Show (ExtLedgerState blk mk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance (IsMapKind mk, LedgerSupportsProtocol blk)
      => NoThunks (ExtLedgerState blk mk) where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk))

type instance HeaderHash (ExtLedgerState blk) = HeaderHash (LedgerState blk)
instance StandardHash (LedgerState blk) => StandardHash (ExtLedgerState blk)

instance IsLedger (LedgerState blk) => GetTip (ExtLedgerState blk) where
  getTip = castPoint . getTip . ledgerState

{-------------------------------------------------------------------------------
  The extended ledger configuration
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  The ticked extended ledger state
-------------------------------------------------------------------------------}

data instance Ticked1 (ExtLedgerState blk) mk = TickedExtLedgerState {
      tickedLedgerState :: Ticked1 (LedgerState blk) mk
    , tickedLedgerView  :: Ticked (LedgerView (BlockProtocol blk))
    , tickedHeaderState :: Ticked (HeaderState blk)
    }

instance IsLedger (LedgerState blk) => GetTip (Ticked1 (ExtLedgerState blk)) where
  getTip = castPoint . getTip . tickedLedgerState

{-------------------------------------------------------------------------------
  Ledger interface
-------------------------------------------------------------------------------}

instance LedgerSupportsProtocol blk => IsLedger (ExtLedgerState blk) where
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

  getBlockKeySets = ExtLedgerStateTables . getBlockKeySets

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
      encodeListLen 2
    , encodeLedgerState  ledgerState
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
                     decodeAnnTip =
    decodeRecordNamed "ExtLedgerState" (const 2) $ do
      ledgerState <- decodeLedgerState
      headerState <- decodeHeaderState'
      return ExtLedgerState{..}
  where
    decodeHeaderState' = decodeHeaderState
                           decodeChainDepState
                           decodeAnnTip

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
  zipLedgerTables2A  f tl tc tr =
     ExtLedgerStateTables <$> zipLedgerTables2A f l c r
   where
     ExtLedgerStateTables l = tl
     ExtLedgerStateTables c = tc
     ExtLedgerStateTables r = tr

deriving instance Eq (LedgerTables (LedgerState blk) mk)
               => Eq (LedgerTables (ExtLedgerState blk) mk)
deriving newtype instance NoThunks (LedgerTables (LedgerState blk) mk)
                       => NoThunks (LedgerTables (ExtLedgerState blk) mk)
deriving instance Show (LedgerTables (LedgerState blk) mk)
               => Show (LedgerTables (ExtLedgerState blk) mk)

instance CanSerializeLedgerTables (LedgerState blk)
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
