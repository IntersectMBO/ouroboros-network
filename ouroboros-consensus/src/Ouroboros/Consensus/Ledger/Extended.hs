{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Ledger.Extended (
    -- * Extended ledger state
    ExtLedgerCfg (..)
  , ExtLedgerState (..)
  , ExtValidationError (..)
  , LedgerMustSupportUTxOHD'
    -- * Serialisation
  , decodeExtLedgerState
  , encodeExtLedgerState
    -- * Casts
  , castExtLedgerState
    -- * Type family instances
  , LedgerTables' (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Data.Coerce
import           Data.Functor ((<&>))
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
--import           GHC.Show (showCommaSpace, showSpace)
import           NoThunks.Class (NoThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.SupportsUTxOHD
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState blk = ExtLedgerState {
      ledgerState :: !(LedgerState blk)
    , headerState :: !(HeaderState blk)
    }
  deriving (Generic)

data ExtValidationError blk =
    ExtValidationErrorLedger !(LedgerError blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  deriving (Generic)

instance LedgerSupportsProtocol blk => NoThunks (ExtValidationError blk)

-- TODO this might be wrong but I'd like to understand why.
deriving instance (Show (LedgerState blk), Show (HeaderState blk))
  => Show (ExtLedgerState blk)

deriving instance LedgerSupportsProtocol blk => Show (ExtValidationError    blk)
deriving instance LedgerSupportsProtocol blk => Eq   (ExtValidationError    blk)

-- instance LedgerSupportsProtocol blk => ShowLedgerState (ExtLedgerState blk) where
--   showsLedgerState mk st =
--       showParen True $ showString "ExtLedgerState {"
--         . showSpace      . showString "headerState = " . shows               headerState
--         . showCommaSpace . showString "ledgerState = " . showsLedgerState mk ledgerState
--         . showString " }"
--     where
--       ExtLedgerState _dummy _ = st
--       ExtLedgerState {
--           headerState
--         , ledgerState
--         } = st

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance (LedgerSupportsProtocol blk, NoThunks (LedgerState blk)) => NoThunks (ExtLedgerState blk) where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk))

deriving instance ( LedgerSupportsProtocol blk
                  , Eq (ChainDepState (BlockProtocol blk))
                  , Eq (LedgerState blk)
                  ) => Eq (ExtLedgerState blk)

{-------------------------------------------------------------------------------
  The extended ledger can behave like a ledger
-------------------------------------------------------------------------------}

data instance Ticked (ExtLedgerState blk) = TickedExtLedgerState {
      tickedLedgerState :: Ticked (LedgerState blk)
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

type instance HeaderHash (ExtLedgerState blk)       = HeaderHash (LedgerState blk)

instance StandardHash (LedgerState blk) => StandardHash (ExtLedgerState blk)

instance GetTip (LedgerState blk) => GetTip (ExtLedgerState blk) where
  getTip = castPoint . getTip . ledgerState

instance GetTip (Ticked (LedgerState blk)) => GetTip (Ticked (ExtLedgerState blk)) where
  getTip = castPoint . getTip . tickedLedgerState

instance ( IsLedger (LedgerState  blk)
         , LedgerSupportsProtocol blk
         )
      => IsLedger (ExtLedgerState blk) where
  type LedgerErr (ExtLedgerState blk) = ExtValidationError blk

  type AuxLedgerEvent (ExtLedgerState blk) = AuxLedgerEvent (LedgerState blk)

  applyChainTickLedgerResult cfg slot (ConsensusLedgerState (ExtLedgerState ledger header) tbs) =
      castLedgerResult ledgerResult <&> \(TickedConsensusLedgerState tickedLedgerState tbs') ->
      let tickedLedgerView :: Ticked (LedgerView (BlockProtocol blk))
          tickedLedgerView = protocolLedgerView lcfg tickedLedgerState

          tickedHeaderState :: Ticked (HeaderState blk)
          tickedHeaderState =
              tickHeaderState
                (configConsensus $ getExtLedgerCfg cfg)
                tickedLedgerView
                slot
                header
      in TickedConsensusLedgerState (TickedExtLedgerState {..}) (fmapTables ExtLedgerStateTables tbs')
    where
      lcfg :: LedgerConfig blk
      lcfg = configLedger $ getExtLedgerCfg cfg

      ledgerResult = applyChainTickLedgerResult lcfg slot (ConsensusLedgerState ledger $ fmapTables unExtLedgerStateTables tbs)

instance LedgerSupportsProtocol blk => ApplyBlock (ExtLedgerState blk) blk where
  applyBlockLedgerResult cfg blk (TickedConsensusLedgerState TickedExtLedgerState{..} tbs) = do
    ledgerResult <-
        withExcept ExtValidationErrorLedger
      $ applyBlockLedgerResult
          (configLedger $ getExtLedgerCfg cfg)
          blk
          (TickedConsensusLedgerState tickedLedgerState $ fmapTables unExtLedgerStateTables tbs)
    hdr <-
        withExcept ExtValidationErrorHeader
      $ validateHeader @blk
          (getExtLedgerCfg cfg)
          tickedLedgerView
          (getHeader blk)
          tickedHeaderState
    pure $ (\cls -> ConsensusLedgerState (ExtLedgerState (consensusLedger cls) hdr) (fmapTables ExtLedgerStateTables $ consensusTables cls)) <$> castLedgerResult ledgerResult

  reapplyBlockLedgerResult cfg blk (TickedConsensusLedgerState TickedExtLedgerState{..} tbs) =
      (\cls -> ConsensusLedgerState (ExtLedgerState (consensusLedger cls) hdr) (fmapTables ExtLedgerStateTables $ consensusTables cls)) <$> castLedgerResult ledgerResult
    where
      ledgerResult =
        reapplyBlockLedgerResult
          (configLedger $ getExtLedgerCfg cfg)
          blk
          (TickedConsensusLedgerState tickedLedgerState $ fmapTables unExtLedgerStateTables tbs)
      hdr      =
        revalidateHeader
          (getExtLedgerCfg cfg)
          tickedLedgerView
          (getHeader blk)
          tickedHeaderState

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState :: (LedgerState   blk -> Encoding)
                     -> (ChainDepState (BlockProtocol blk) -> Encoding)
                     -> (AnnTip        blk -> Encoding)
                     -> ExtLedgerState blk -> Encoding
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

decodeExtLedgerState :: (forall s. Decoder s (LedgerState    blk))
                     -> (forall s. Decoder s (ChainDepState  (BlockProtocol blk)))
                     -> (forall s. Decoder s (AnnTip         blk))
                     -> (forall s. Decoder s (ExtLedgerState blk))
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
  :: ( Coercible (LedgerState blk )
                 (LedgerState blk')
     , Coercible (ChainDepState (BlockProtocol blk))
                 (ChainDepState (BlockProtocol blk'))
     , TipInfo blk ~ TipInfo blk'
     )
  => ExtLedgerState blk -> ExtLedgerState blk'
castExtLedgerState ExtLedgerState{..} = ExtLedgerState {
      ledgerState = coerce ledgerState
    , headerState = castHeaderState headerState
    }

{-------------------------------------------------------------------------------
  UTxO-HD
-------------------------------------------------------------------------------}

newtype instance LedgerTables' (ExtLedgerState blk) mk = ExtLedgerStateTables { unExtLedgerStateTables :: LedgerTables' (LedgerState blk) mk } deriving Generic

instance (LedgerSupportsProtocol blk, TableStuff (LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables)) => TableStuff (LedgerTablesGADT (LedgerTables' (ExtLedgerState blk)) WithLedgerTables) where

  traverseLedgerTables f (JustTables (ExtLedgerStateTables l)) =
    fmapTables ExtLedgerStateTables <$> traverseLedgerTables f (JustTables l)

  pureLedgerTables  f = coerce $ pureLedgerTables  @(LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables) f
  mapLedgerTables   f = coerce $ mapLedgerTables   @(LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables) f
  zipLedgerTables   f = coerce $ zipLedgerTables   @(LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables) f
  zipLedgerTables2  f = coerce $ zipLedgerTables2  @(LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables) f
  foldLedgerTables  f = coerce $ foldLedgerTables  @(LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables) f
  foldLedgerTables2 f = coerce $ foldLedgerTables2 @(LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables) f
  namesLedgerTables   = coerce $ namesLedgerTables @(LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables)
  zipLedgerTablesA  f (JustTables (ExtLedgerStateTables l)) (JustTables (ExtLedgerStateTables r)) =
    fmapTables ExtLedgerStateTables <$> zipLedgerTablesA f (JustTables l) (JustTables r)
  zipLedgerTables2A  f (JustTables (ExtLedgerStateTables l)) (JustTables (ExtLedgerStateTables c)) (JustTables (ExtLedgerStateTables r)) =
     fmapTables ExtLedgerStateTables <$> zipLedgerTables2A f (JustTables l) (JustTables c) (JustTables r)

deriving instance Eq        (LedgerTables' (LedgerState blk) mk)               => Eq       (LedgerTables' (ExtLedgerState blk) mk)
instance          (NoThunks (LedgerTables' (LedgerState blk) mk), Typeable mk) => NoThunks (LedgerTables' (ExtLedgerState blk) mk)


instance ( LedgerSupportsProtocol blk
         , StowableLedgerTables (ConsensusLedgerState (LedgerState blk) WithLedgerTables)
         )
      => StowableLedgerTables (ConsensusLedgerState (ExtLedgerState blk) WithLedgerTables) where

  stowLedgerTables (ConsensusLedgerState ExtLedgerState{headerState, ledgerState} tbs) =
    let ConsensusLedgerState ledgerState' tbs' = stowLedgerTables $ ConsensusLedgerState ledgerState $ fmapTables unExtLedgerStateTables tbs in
      ConsensusLedgerState ExtLedgerState {
          headerState
        , ledgerState = ledgerState'
        } $ fmapTables ExtLedgerStateTables tbs'

  unstowLedgerTables (ConsensusLedgerState ExtLedgerState{headerState, ledgerState} tbs) =
    let ConsensusLedgerState ledgerState' tbs' = unstowLedgerTables $ ConsensusLedgerState ledgerState $ fmapTables unExtLedgerStateTables tbs in
      ConsensusLedgerState ExtLedgerState {
          headerState
        , ledgerState = ledgerState'
        } $ fmapTables ExtLedgerStateTables tbs'

instance ( LedgerSupportsProtocol blk
         , SufficientSerializationForAnyBackingStore (LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables)
         )
      => SufficientSerializationForAnyBackingStore (LedgerTablesGADT (LedgerTables' (ExtLedgerState blk)) WithLedgerTables) where
  codecLedgerTables = fmapTables ExtLedgerStateTables codecLedgerTables

-- instance ExtractLedgerTables (LedgerState blk) => ExtractLedgerTables (ExtLedgerState blk) where
--   extractLedgerTables ExtLedgerState{..} = ExtLedgerState{ledgerState = extractLedgerTables ledgerState,..}
--   destroyLedgerTables ExtLedgerState{..} = ExtLedgerState{ledgerState = destroyLedgerTables ledgerState,..}


-- instance IgnoresTables (LedgerState blk) => IgnoresTables (ExtLedgerState blk) where
--   convertTables ExtLedgerState{..} = ExtLedgerState{ledgerState = convertTables ledgerState, ..}


instance GetsBlockKeySets blk (LedgerTablesGADT (LedgerTables' (LedgerState blk)) WithLedgerTables)
      => GetsBlockKeySets blk (LedgerTablesGADT (LedgerTables' (ExtLedgerState blk)) WithLedgerTables) where
  getBlockKeySets = fmapTables ExtLedgerStateTables . getBlockKeySets
  getTransactionKeySets = fmapTables ExtLedgerStateTables . getTransactionKeySets

-- TODO @js: remove this?
type LedgerMustSupportUTxOHD' blk wt = ( LedgerMustSupportUTxOHD (ExtLedgerState blk) blk wt
                                       , LedgerMustSupportUTxOHD (LedgerState blk) blk wt
                                       )

-- instance -- (LedgerSupportsProtocol blk, LedgerMustSupportUTxOHD (LedgerState blk) blk WithLedgerTables)    =>
--   LedgerMustSupportUTxOHD (ExtLedgerState blk) blk WithLedgerTables
-- instance -- (LedgerSupportsProtocol blk, LedgerMustSupportUTxOHD (LedgerState blk) blk WithoutLedgerTables) =>
--   LedgerMustSupportUTxOHD (ExtLedgerState blk) blk WithoutLedgerTables
-- instance (LedgerSupportsProtocol blk, LedgerSupportsUTxOHD    (LedgerState blk) blk                    ) => LedgerSupportsUTxOHD    (ExtLedgerState blk) blk
