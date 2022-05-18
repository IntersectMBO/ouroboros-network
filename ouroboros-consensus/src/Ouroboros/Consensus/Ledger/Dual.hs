{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Ouroboros.Consensus.Ledger.Dual (
    Bridge (..)
    -- * Pair types
  , DualBlock (..)
  , DualGenTxErr (..)
  , DualHeader
  , DualLedgerConfig (..)
  , DualLedgerError (..)
    -- * Lifted functions
  , ctxtDualMain
  , dualExtValidationErrorMain
  , dualTopLevelConfigMain
    -- * Type class family instances
  , BlockConfig (..)
  , CodecConfig (..)
  , GenTx (..)
  , Header (..)
  , LedgerState (..)
  , LedgerTables (..)
  , NestedCtxt_ (..)
  , StorageConfig (..)
  , Ticked1 (..)
  , TxId (..)
  , Validated (..)
    -- * Serialisation
  , decodeDualBlock
  , decodeDualGenTx
  , decodeDualGenTxErr
  , decodeDualGenTxId
  , decodeDualHeader
  , decodeDualLedgerState
  , encodeDualBlock
  , encodeDualGenTx
  , encodeDualGenTxErr
  , encodeDualGenTxId
  , encodeDualHeader
  , encodeDualLedgerState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short
import           Data.Functor ((<&>))
import           Data.Kind (Type)
import           Data.Typeable
import           GHC.Generics (Generic)
import           GHC.Show (showCommaSpace, showSpace)
import           GHC.Stack
import           NoThunks.Class (AllowThunk (..), NoThunks (..))

import           Cardano.Binary (enforceSize)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.Storage.Serialisation

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | Dual block
--
-- The dual block is used to instantiate the consensus with a dual ledger,
-- consisting of two ledger states associated with two types of blocks. The
-- (consensus) chain state will still be updated based on one block type only,
-- which is therefore designed as the /main/ block, while the other block is
-- designated as the /auxiliary/ block.
--
-- The auxiliary block is optional; this can be used if some " main " blocks
-- should have no effect on the auxiliary ledger state at all. The motivating
-- example is EBBs: if the main blocks are real Byron blocks, and the auxiliary
-- blocks are Byron spec blocks, then regular Byron blocks correspond to Byron
-- spec blocks, but EBBs don't correspond to a spec block at all and should
-- leave the Byron spec ledger state unchanged.
--
-- NOTE: The dual ledger is used for testing purposes only; we do not do any
-- meaningful 'NoThunks' checks here.
data DualBlock m a = DualBlock {
      dualBlockMain   :: m
    , dualBlockAux    :: Maybe a
    , dualBlockBridge :: BridgeBlock m a
    }

deriving instance (Show m, Show a, Show (BridgeBlock m a)) => Show (DualBlock m a)
deriving instance (Eq   m, Eq   a, Eq   (BridgeBlock m a)) => Eq   (DualBlock m a)

instance (Typeable m, Typeable a)
    => ShowProxy (DualBlock m a) where

instance Condense m => Condense (DualBlock m a) where
  condense = condense . dualBlockMain

type instance HeaderHash (DualBlock m a) = HeaderHash m
instance StandardHash m => StandardHash (DualBlock m a)

instance ConvertRawHash m => ConvertRawHash (DualBlock m a) where
  toShortRawHash   _ = toShortRawHash   (Proxy @m)
  fromShortRawHash _ = fromShortRawHash (Proxy @m)
  hashSize         _ = hashSize         (Proxy @m)

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

newtype instance Header (DualBlock m a) = DualHeader { dualHeaderMain :: Header m }
  deriving NoThunks via AllowThunk (Header (DualBlock m a))

instance Bridge m a => GetHeader (DualBlock m a) where
  getHeader = DualHeader . getHeader . dualBlockMain

  blockMatchesHeader hdr =
      blockMatchesHeader (dualHeaderMain hdr) . dualBlockMain

  -- We can look at the concrete header to see if this is an EBB
  headerIsEBB = headerIsEBB . dualHeaderMain

type DualHeader m a = Header (DualBlock m a)

deriving instance Show (Header m) => Show (DualHeader m a)

instance (Typeable m, Typeable a)
    => ShowProxy (DualHeader m a) where

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data instance BlockConfig (DualBlock m a) = DualBlockConfig {
      dualBlockConfigMain :: BlockConfig m
    , dualBlockConfigAux  :: BlockConfig a
    }
  deriving NoThunks via AllowThunk (BlockConfig (DualBlock m a))

instance ConfigSupportsNode m => ConfigSupportsNode (DualBlock m a) where
  getSystemStart  = getSystemStart  . dualBlockConfigMain
  getNetworkMagic = getNetworkMagic . dualBlockConfigMain

{-------------------------------------------------------------------------------
  Splitting the config
-------------------------------------------------------------------------------}

-- | This is only used for block production
dualTopLevelConfigMain :: TopLevelConfig (DualBlock m a) -> TopLevelConfig m
dualTopLevelConfigMain TopLevelConfig{..} = TopLevelConfig{
      topLevelConfigProtocol = topLevelConfigProtocol
    , topLevelConfigLedger   = dualLedgerConfigMain  topLevelConfigLedger
    , topLevelConfigBlock    = dualBlockConfigMain   topLevelConfigBlock
    , topLevelConfigCodec    = dualCodecConfigMain   topLevelConfigCodec
    , topLevelConfigStorage  = dualStorageConfigMain topLevelConfigStorage
    }

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

data instance CodecConfig (DualBlock m a) = DualCodecConfig {
      dualCodecConfigMain :: !(CodecConfig m)
    , dualCodecConfigAux  :: !(CodecConfig a)
    }
  deriving (Generic)

instance ( NoThunks (CodecConfig m)
         , NoThunks (CodecConfig a)
         ) => NoThunks (CodecConfig (DualBlock m a))
  -- Use generic instance

{-------------------------------------------------------------------------------
  StorageConfig
-------------------------------------------------------------------------------}

data instance StorageConfig (DualBlock m a) = DualStorageConfig {
      dualStorageConfigMain :: !(StorageConfig m)
    , dualStorageConfigAux  :: !(StorageConfig a)
    }
  deriving (Generic)

instance ( NoThunks (StorageConfig m)
         , NoThunks (StorageConfig a)
         ) => NoThunks (StorageConfig (DualBlock m a))
  -- Use generic instance

{-------------------------------------------------------------------------------
  Bridge two ledgers
-------------------------------------------------------------------------------}

-- | Bridge the two ledgers
class (
        -- Requirements on the main block
        HasHeader              m
      , GetHeader              m
      , HasHeader     (Header  m)
      , LedgerSupportsProtocol m
      , HasHardForkHistory     m
      , LedgerSupportsMempool  m
      , CommonProtocolParams   m
      , HasTxId (GenTx         m)
      , Show (ApplyTxErr       m)

        -- Requirements on the auxiliary block
        -- No 'LedgerSupportsProtocol' for @a@!
      , Typeable                a
      , UpdateLedger            a
      , LedgerSupportsMempool   a
      , Show (ApplyTxErr        a)
      , NoThunks (LedgerConfig  a)
      , NoThunks (CodecConfig   a)
      , NoThunks (StorageConfig a)

        -- Requirements on the various bridges
      , Show      (BridgeLedger m a)
      , Eq        (BridgeLedger m a)
      , Serialise (BridgeLedger m a)
      , Serialise (BridgeBlock  m a)
      , Serialise (BridgeTx     m a)
      , Show      (BridgeTx     m a)
      ) => Bridge m a where

  -- | Additional information relating both ledgers
  type BridgeLedger m a :: Type

  -- | Information required to update the bridge when applying a block
  type BridgeBlock m a :: Type

  -- | Information required to update the bridge when applying a transaction
  type BridgeTx m a :: Type

  updateBridgeWithBlock :: DualBlock m a
                        -> BridgeLedger m a -> BridgeLedger m a

  updateBridgeWithTx :: Validated (GenTx (DualBlock m a))
                     -> BridgeLedger m a -> BridgeLedger m a

{-------------------------------------------------------------------------------
  HasHeader instance
-------------------------------------------------------------------------------}

instance Bridge m a => HasHeader (DualBlock m a) where
  getHeaderFields = getBlockHeaderFields

instance Bridge m a => HasHeader (DualHeader m a) where
  getHeaderFields = castHeaderFields . getHeaderFields . dualHeaderMain

instance Bridge m a => GetPrevHash (DualBlock m a) where
  headerPrevHash = castHash . headerPrevHash . dualHeaderMain

{-------------------------------------------------------------------------------
  Protocol
-------------------------------------------------------------------------------}

type instance BlockProtocol (DualBlock m a) = BlockProtocol m

instance Bridge m a => BlockSupportsProtocol (DualBlock m a) where
  validateView cfg = validateView (dualBlockConfigMain cfg) . dualHeaderMain
  selectView   cfg = selectView   (dualBlockConfigMain cfg) . dualHeaderMain

{-------------------------------------------------------------------------------
  Ledger errors
-------------------------------------------------------------------------------}

-- | Both ledger rules threw an error
--
-- We do not verify that the errors agree, merely that they both report /some/
-- error.
--
-- If only /one/ of the two semantics reports an error, we fail with an 'error'
-- (see 'agreeOnError'), rather than a regular chain failure; if this happens,
-- it indicates a bug, and the node should fail (rather than just, for example,
-- reject a block).
data DualLedgerError m a = DualLedgerError {
        dualLedgerErrorMain :: LedgerError m
      , dualLedgerErrorAux  :: LedgerError a
      }
  deriving NoThunks via AllowThunk (DualLedgerError m a)

deriving instance ( Show (LedgerError m)
                  , Show (LedgerError a)
                  ) => Show (DualLedgerError m a)
deriving instance ( Eq (LedgerError m)
                  , Eq (LedgerError a)
                  ) => Eq (DualLedgerError m a)

{-------------------------------------------------------------------------------
  Update the ledger
-------------------------------------------------------------------------------}

data DualLedgerConfig m a = DualLedgerConfig {
      dualLedgerConfigMain :: LedgerConfig m
    , dualLedgerConfigAux  :: LedgerConfig a
    }
  deriving NoThunks via AllowThunk (DualLedgerConfig m a)

type instance LedgerCfg (LedgerState (DualBlock m a)) = DualLedgerConfig m a

instance Bridge m a => GetTip (LedgerState (DualBlock m a) mk) where
  getTip = castPoint . getTip . dualLedgerStateMain

instance Bridge m a => GetTip (Ticked1 (LedgerState (DualBlock m a)) mk) where
  getTip = castPoint . getTip . tickedDualLedgerStateMain

data instance Ticked1 (LedgerState (DualBlock m a)) mk = TickedDualLedgerState {
      tickedDualLedgerStateMain    :: Ticked1 (LedgerState m) mk
    , tickedDualLedgerStateAux     :: Ticked1 (LedgerState a) mk
    , tickedDualLedgerStateBridge  :: BridgeLedger m a

      -- | The original, unticked ledger for the auxiliary block
      --
      -- The reason we keep this in addition to the ticked ledger state is that
      -- not every main block is paired with an auxiliary block. When there is
      -- no auxiliary block, the auxiliary ledger state remains unchanged.
    , tickedDualLedgerStateAuxOrig :: LedgerState a EmptyMK
    }
  deriving NoThunks via AllowThunk (Ticked1 (LedgerState (DualBlock m a)) mk)

instance Bridge m a => IsLedger (LedgerState (DualBlock m a)) where
  type LedgerErr (LedgerState (DualBlock m a)) = DualLedgerError   m a

  -- | The dual ledger events are exactly those of the main ledger; it ignores
  -- any possible auxiliary ledger events.
  --
  -- NOTE: This may change. It's unclear whether we want the two ledgers to emit
  -- the same events. And right now we only use the Dual ledger for our tests,
  -- and do so only with the Byron and ByronSpecs ledgers, neither of which have
  -- any events. So we make this easy choice for for now.
  type AuxLedgerEvent (LedgerState (DualBlock m a)) = AuxLedgerEvent (LedgerState m)

  applyChainTickLedgerResult DualLedgerConfig{..}
                             slot
                             DualLedgerState{..} =
      castLedgerResult ledgerResult <&> \main -> TickedDualLedgerState {
          tickedDualLedgerStateMain    = main
        , tickedDualLedgerStateAux     = applyChainTick
                                           dualLedgerConfigAux
                                           slot
                                          dualLedgerStateAux
        , tickedDualLedgerStateAuxOrig =
            forgetLedgerTables dualLedgerStateAux
        , tickedDualLedgerStateBridge  = dualLedgerStateBridge
        }
    where
      ledgerResult = applyChainTickLedgerResult
                       dualLedgerConfigMain
                       slot
                       dualLedgerStateMain

instance Bridge m a => TableStuff (LedgerState (DualBlock m a)) where

  data LedgerTables (LedgerState (DualBlock m a)) mk =
      DualBlockLedgerTables
        (LedgerTables (LedgerState m) mk)
        (LedgerTables (LedgerState a) mk)
    deriving (Generic)
    deriving NoThunks via AllowThunk (LedgerTables (LedgerState (DualBlock m a)) mk)

  projectLedgerTables DualLedgerState{..} =
      DualBlockLedgerTables
        (projectLedgerTables dualLedgerStateMain)
        (projectLedgerTables dualLedgerStateAux)

  withLedgerTables DualLedgerState{..} (DualBlockLedgerTables main aux) =
      DualLedgerState {
          dualLedgerStateMain   = withLedgerTables dualLedgerStateMain main
        , dualLedgerStateAux    = withLedgerTables dualLedgerStateAux  aux
        , dualLedgerStateBridge = dualLedgerStateBridge
        }

  pureLedgerTables f =
      DualBlockLedgerTables
        (pureLedgerTables f)
        (pureLedgerTables f)

  mapLedgerTables f (DualBlockLedgerTables main aux) =
      DualBlockLedgerTables
        (mapLedgerTables f main)
        (mapLedgerTables f aux)

  traverseLedgerTables f (DualBlockLedgerTables main aux) =
          DualBlockLedgerTables
      <$> traverseLedgerTables f main
      <*> traverseLedgerTables f aux

  zipLedgerTables
    f
    (DualBlockLedgerTables mainL auxL)
    (DualBlockLedgerTables mainR auxR) =
      DualBlockLedgerTables
        (zipLedgerTables f mainL mainR)
        (zipLedgerTables f auxL  auxR)

  zipLedgerTablesA
    f
    (DualBlockLedgerTables mainL auxL)
    (DualBlockLedgerTables mainR auxR) =
          DualBlockLedgerTables
      <$> (zipLedgerTablesA f mainL mainR)
      <*> (zipLedgerTablesA f auxL  auxR)

  zipLedgerTables2A
    f
    (DualBlockLedgerTables main0 aux0)
    (DualBlockLedgerTables main1 aux1)
    (DualBlockLedgerTables main2 aux2) =
          DualBlockLedgerTables
      <$> (zipLedgerTables2A f main0 main1 main2)
      <*> (zipLedgerTables2A f aux0  aux1  aux2)



  zipLedgerTables2
    f
    (DualBlockLedgerTables mainL auxL)
    (DualBlockLedgerTables mainM auxM)
    (DualBlockLedgerTables mainR auxR) =
      DualBlockLedgerTables
        (zipLedgerTables2 f mainL mainM mainR)
        (zipLedgerTables2 f auxL  auxM  auxR)

  foldLedgerTables f (DualBlockLedgerTables main aux) =
       foldLedgerTables f main
    <> foldLedgerTables f aux

  foldLedgerTables2
    f
    (DualBlockLedgerTables main1 aux1)
    (DualBlockLedgerTables main2 aux2) =
           foldLedgerTables2 f main1 main2
        <> foldLedgerTables2 f aux1 aux2

  namesLedgerTables = DualBlockLedgerTables namesLedgerTables namesLedgerTables

deriving instance
     ( Eq (LedgerTables (LedgerState m) mk)
     , Eq (LedgerTables (LedgerState a) mk)
     )
  => Eq (LedgerTables (LedgerState (DualBlock m a)) mk)

instance Bridge m a => TickedTableStuff (LedgerState (DualBlock m a)) where

  projectLedgerTablesTicked TickedDualLedgerState{..} =
      DualBlockLedgerTables
        (projectLedgerTablesTicked tickedDualLedgerStateMain)
        (projectLedgerTablesTicked tickedDualLedgerStateAux)

  withLedgerTablesTicked
    TickedDualLedgerState{..}
    (DualBlockLedgerTables main aux) =
      TickedDualLedgerState {
          tickedDualLedgerStateMain   =
            withLedgerTablesTicked tickedDualLedgerStateMain main
        , tickedDualLedgerStateAux    =
            withLedgerTablesTicked tickedDualLedgerStateAux  aux
        , tickedDualLedgerStateBridge
        , tickedDualLedgerStateAuxOrig
        }

instance ShowLedgerState (LedgerTables (LedgerState (DualBlock m a))) where
  showsLedgerState = error "showsLedgerState @LedgerTables DualBlock"

instance ShowLedgerState (LedgerTables (Ticked1 (LedgerState (DualBlock m a)))) where
  showsLedgerState = error "showsLedgerState @Ticked1 LedgerTables DualBlock"

instance Bridge m a => ApplyBlock (LedgerState (DualBlock m a)) (DualBlock m a) where

  applyBlockLedgerResult cfg
                         block@DualBlock{..}
                         TickedDualLedgerState{..} = do
      (ledgerResult, aux') <-
        agreeOnError DualLedgerError (
            applyBlockLedgerResult
              (dualLedgerConfigMain cfg)
              dualBlockMain
              tickedDualLedgerStateMain
          , applyMaybeBlock
              (dualLedgerConfigAux cfg)
              dualBlockAux
              tickedDualLedgerStateAux
              tickedDualLedgerStateAuxOrig
          )
      return $ castLedgerResult ledgerResult <&> \main' -> DualLedgerState {
          dualLedgerStateMain   = main'
        , dualLedgerStateAux    = aux'
        , dualLedgerStateBridge = updateBridgeWithBlock
                                    block
                                    tickedDualLedgerStateBridge
        }

  reapplyBlockLedgerResult cfg
                           block@DualBlock{..}
                           TickedDualLedgerState{..} =
    castLedgerResult ledgerResult <&> \main' -> DualLedgerState {
        dualLedgerStateMain   = main'
      , dualLedgerStateAux    = reapplyMaybeBlock
                                  (dualLedgerConfigAux cfg)
                                  dualBlockAux
                                  tickedDualLedgerStateAux
                                  tickedDualLedgerStateAuxOrig
      , dualLedgerStateBridge = updateBridgeWithBlock
                                  block
                                  tickedDualLedgerStateBridge
      }
    where
      ledgerResult = reapplyBlockLedgerResult
                       (dualLedgerConfigMain cfg)
                       dualBlockMain
                       tickedDualLedgerStateMain

  getBlockKeySets DualBlock{dualBlockMain, dualBlockAux} =
      DualBlockLedgerTables m a
    where
      m = getBlockKeySets                             dualBlockMain
      a = maybe polyEmptyLedgerTables getBlockKeySets dualBlockAux

data instance LedgerState (DualBlock m a) mk = DualLedgerState {
      dualLedgerStateMain   :: LedgerState m mk
    , dualLedgerStateAux    :: LedgerState a mk
    , dualLedgerStateBridge :: BridgeLedger m a
    }
  deriving NoThunks via AllowThunk (LedgerState (DualBlock m a) mk)

instance Bridge m a => UpdateLedger (DualBlock m a)

deriving instance ( Eq (LedgerState m mk)
                  , Eq (LedgerState a mk)
                  , Bridge m a
                  ) => Eq (LedgerState (DualBlock m a) mk)

instance ( ShowLedgerState (LedgerState m)
         , ShowLedgerState (LedgerState a)
         , Bridge m a
         ) => ShowLedgerState (LedgerState (DualBlock m a)) where
  showsLedgerState mk st =
      showString "DualLedgerState" . showSpace . showString "{" .
                             showsLedgerState mk dualLedgerStateMain
          . showCommaSpace . showsLedgerState mk dualLedgerStateAux
          . showCommaSpace . shows dualLedgerStateBridge
          . showString "}"
    where
      -- these patterns together ensure we're printing all the fields; the
      -- record pattern should have the same arity as the plain pattern
      DualLedgerState _dummy _ _ = st
      DualLedgerState {
          dualLedgerStateMain
        , dualLedgerStateAux
        , dualLedgerStateBridge
        } = st

instance
     ( Bridge m a
     , StowableLedgerTables (LedgerState m)
     , StowableLedgerTables (LedgerState a)
     )
  => StowableLedgerTables (LedgerState (DualBlock m a)) where

  stowLedgerTables st =
      DualLedgerState {
          dualLedgerStateMain   = stowLedgerTables dualLedgerStateMain
        , dualLedgerStateAux    = stowLedgerTables dualLedgerStateAux
        , dualLedgerStateBridge
        }
    where
      DualLedgerState {
          dualLedgerStateMain
        , dualLedgerStateAux
        , dualLedgerStateBridge
        } = st

  unstowLedgerTables st =
      DualLedgerState {
          dualLedgerStateMain   = unstowLedgerTables dualLedgerStateMain
        , dualLedgerStateAux    = unstowLedgerTables dualLedgerStateAux
        , dualLedgerStateBridge
        }
    where
      DualLedgerState {
          dualLedgerStateMain
        , dualLedgerStateAux
        , dualLedgerStateBridge
        } = st

  isCandidateForUnstow = isCandidateForUnstowDefault

{-------------------------------------------------------------------------------
  Utilities for working with the extended ledger state
-------------------------------------------------------------------------------}

dualExtValidationErrorMain :: ExtValidationError (DualBlock m a)
                           -> ExtValidationError m
dualExtValidationErrorMain = \case
    ExtValidationErrorLedger e -> ExtValidationErrorLedger (dualLedgerErrorMain e)
    ExtValidationErrorHeader e -> ExtValidationErrorHeader (castHeaderError     e)

{-------------------------------------------------------------------------------
  LedgerSupportsProtocol

  These definitions are asymmetric because the auxiliary block is not involved
  in the consensus protocol, and has no 'LedgerSupportsProtocol' instance.
-------------------------------------------------------------------------------}

instance Bridge m a => HasAnnTip (DualBlock m a) where
  type TipInfo (DualBlock m a) = TipInfo m
  tipInfoHash _ = tipInfoHash (Proxy @m)
  getTipInfo    = getTipInfo . dualHeaderMain

instance Bridge m a => BasicEnvelopeValidation (DualBlock m a) where
  expectedFirstBlockNo  _ = expectedFirstBlockNo  (Proxy @m)
  expectedNextBlockNo   _ = expectedNextBlockNo   (Proxy @m)
  minimumPossibleSlotNo _ = minimumPossibleSlotNo (Proxy @m)
  minimumNextSlotNo     _ = minimumNextSlotNo     (Proxy @m)

instance Bridge m a => ValidateEnvelope (DualBlock m a) where
  type OtherHeaderEnvelopeError (DualBlock m a) = OtherHeaderEnvelopeError m

  additionalEnvelopeChecks cfg ledgerView hdr =
      additionalEnvelopeChecks
        (dualTopLevelConfigMain cfg)
        ledgerView
        (dualHeaderMain hdr)

instance Bridge m a => LedgerSupportsProtocol (DualBlock m a) where
  protocolLedgerView cfg state =
      protocolLedgerView
        (dualLedgerConfigMain      cfg)
        (tickedDualLedgerStateMain state)

  ledgerViewForecastAt cfg state =
      ledgerViewForecastAt
        (dualLedgerConfigMain cfg)
        (dualLedgerStateMain  state)

instance Bridge m a => HasHardForkHistory (DualBlock m a) where
  type HardForkIndices (DualBlock m a) = HardForkIndices m

  hardForkSummary cfg state =
      hardForkSummary
        (dualLedgerConfigMain cfg)
        (dualLedgerStateMain  state)

{-------------------------------------------------------------------------------
  Querying the ledger
-------------------------------------------------------------------------------}

data instance BlockQuery (DualBlock m a) fp result
  deriving (Show)

instance SmallQuery (BlockQuery (DualBlock m a)) where
  proveSmallQuery _k = \case {}

instance (Typeable m, Typeable a)
    => ShowProxy (BlockQuery (DualBlock m a)) where

-- | Not used in the tests: no constructors
instance Bridge m a => QueryLedger (DualBlock m a) where
  answerBlockQuery _ = \case {}

instance EqQuery (BlockQuery (DualBlock m a)) where
  eqQuery = \case {}

instance ShowQuery (BlockQuery (DualBlock m a)) where
  showResult = \case {}

instance IsQuery (BlockQuery (DualBlock m a))

-- | Forward to the main ledger
instance Bridge m a => CommonProtocolParams (DualBlock m a) where
  maxHeaderSize = maxHeaderSize . dualLedgerStateMain
  maxTxSize     = maxTxSize     . dualLedgerStateMain

{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

data DualGenTxErr m a = DualGenTxErr {
      dualGenTxErrMain :: ApplyTxErr m
    , dualGenTxErrAux  :: ApplyTxErr a
    }

instance (Typeable m, Typeable a)
    => ShowProxy (DualGenTxErr m a) where

data instance GenTx (DualBlock m a) = DualGenTx {
      dualGenTxMain   :: GenTx m
    , dualGenTxAux    :: GenTx a
    , dualGenTxBridge :: BridgeTx m a
    }
  deriving NoThunks via AllowThunk (GenTx (DualBlock m a))

data instance Validated (GenTx (DualBlock m a)) = ValidatedDualGenTx {
      vDualGenTxMain   :: Validated (GenTx m)
    , vDualGenTxAux    :: Validated (GenTx a)
    , vDualGenTxBridge :: BridgeTx m a
    }
  deriving NoThunks via AllowThunk (Validated (GenTx (DualBlock m a)))

instance (Typeable m, Typeable a)
    => ShowProxy (GenTx (DualBlock m a)) where

type instance ApplyTxErr (DualBlock m a) = DualGenTxErr m a

instance Bridge m a => LedgerSupportsMempool (DualBlock m a) where
  applyTx DualLedgerConfig{..}
          wti
          slot
          DualGenTx{..}
          TickedDualLedgerState{..} = do
      ((main', mainVtx), (aux', auxVtx)) <-
        agreeOnError DualGenTxErr (
            applyTx
              dualLedgerConfigMain
              wti
              slot
              dualGenTxMain
              tickedDualLedgerStateMain
          , applyTx
              dualLedgerConfigAux
              wti
              slot
              dualGenTxAux
              tickedDualLedgerStateAux
          )
      let vtx = ValidatedDualGenTx {
                vDualGenTxMain   = mainVtx
              , vDualGenTxAux    = auxVtx
              , vDualGenTxBridge = dualGenTxBridge
              }
      return $ flip (,) vtx $ TickedDualLedgerState {
          tickedDualLedgerStateMain    = main'
        , tickedDualLedgerStateAux     = aux'
        , tickedDualLedgerStateAuxOrig = tickedDualLedgerStateAuxOrig
        , tickedDualLedgerStateBridge  = updateBridgeWithTx
                                           vtx
                                           tickedDualLedgerStateBridge
        }

  reapplyTx DualLedgerConfig{..}
            slot
            tx@ValidatedDualGenTx{..}
            TickedDualLedgerState{..} = do
      (main', aux') <-
        agreeOnError DualGenTxErr (
            reapplyTx
              dualLedgerConfigMain
              slot
              vDualGenTxMain
              tickedDualLedgerStateMain
          , reapplyTx
              dualLedgerConfigAux
              slot
              vDualGenTxAux
              tickedDualLedgerStateAux
          )
      return $ TickedDualLedgerState {
          tickedDualLedgerStateMain    = main'
        , tickedDualLedgerStateAux     = aux'
        , tickedDualLedgerStateAuxOrig = tickedDualLedgerStateAuxOrig
        , tickedDualLedgerStateBridge  = updateBridgeWithTx
                                           tx
                                           tickedDualLedgerStateBridge
        }

  txsMaxBytes   = txsMaxBytes . tickedDualLedgerStateMain
  txInBlockSize = txInBlockSize . dualGenTxMain

  txForgetValidated vtx =
      DualGenTx {
          dualGenTxMain   = txForgetValidated vDualGenTxMain
        , dualGenTxAux    = txForgetValidated vDualGenTxAux
        , dualGenTxBridge = vDualGenTxBridge
        }
    where
      ValidatedDualGenTx {
            vDualGenTxMain
          , vDualGenTxAux
          , vDualGenTxBridge
          } = vtx

  getTransactionKeySets tx =
      DualBlockLedgerTables
        (getTransactionKeySets dualGenTxMain)
        (getTransactionKeySets dualGenTxAux)
    where
      DualGenTx {
          dualGenTxMain
        , dualGenTxAux
        } = tx

-- We don't need a pair of IDs, as long as we can unique ID the transaction
newtype instance TxId (GenTx (DualBlock m a)) = DualGenTxId {
      dualGenTxIdMain :: GenTxId m
    }
  deriving NoThunks via AllowThunk (TxId (GenTx (DualBlock m a)))

instance (Typeable m, Typeable a)
    => ShowProxy (TxId (GenTx (DualBlock m a))) where

instance Bridge m a => HasTxId (GenTx (DualBlock m a)) where
  txId = DualGenTxId . txId . dualGenTxMain

deriving instance Bridge m a => Show (GenTx (DualBlock m a))
deriving instance Bridge m a => Show (Validated (GenTx (DualBlock m a)))
deriving instance Bridge m a => Show (DualGenTxErr m a)

deriving instance Show (GenTxId m) => Show (TxId (GenTx (DualBlock m a)))
deriving instance Eq   (GenTxId m) => Eq   (TxId (GenTx (DualBlock m a)))
deriving instance Ord  (GenTxId m) => Ord  (TxId (GenTx (DualBlock m a)))

{-------------------------------------------------------------------------------
  Nested contents

  Since we only have a single header, we just delegate to the main block.
-------------------------------------------------------------------------------}

newtype instance NestedCtxt_ (DualBlock m a) f x where
    CtxtDual :: NestedCtxt_ m f x -> NestedCtxt_ (DualBlock m a) f x

deriving instance Show (NestedCtxt_ m f x)
               => Show (NestedCtxt_ (DualBlock m a) f x)

instance SameDepIndex (NestedCtxt_ m f)
      => SameDepIndex (NestedCtxt_ (DualBlock m a) f) where
  sameDepIndex (CtxtDual ctxt) (CtxtDual ctxt') =
     sameDepIndex ctxt ctxt'

ctxtDualMain :: NestedCtxt_ (DualBlock m a) f x -> NestedCtxt_ m f x
ctxtDualMain (CtxtDual ctxtMain) = ctxtMain

instance HasNestedContent Header m
      => HasNestedContent Header (DualBlock m a) where
  unnest = depPairFirst (mapNestedCtxt CtxtDual) . unnest . dualHeaderMain
  nest   = DualHeader . nest . depPairFirst (mapNestedCtxt ctxtDualMain)

instance ReconstructNestedCtxt Header m
      => ReconstructNestedCtxt Header (DualBlock m a) where
  reconstructPrefixLen _ =
      -- Account for the outer @encodeListLen 3@
      1 `addPrefixLen` reconstructPrefixLen (Proxy @(Header m))
  reconstructNestedCtxt _ prefix size =
      case reconstructNestedCtxt (Proxy @(Header m)) prefixMain size of
        SomeSecond ctxt -> SomeSecond (mapNestedCtxt CtxtDual ctxt)
    where
      prefixMain = Short.pack . drop 1 . Short.unpack $ prefix

instance EncodeDiskDepIx (NestedCtxt Header) m
      => EncodeDiskDepIx (NestedCtxt Header) (DualBlock m a) where
  encodeDiskDepIx ccfg (SomeSecond ctxt) =
      encodeDiskDepIx
        (dualCodecConfigMain ccfg)
        (SomeSecond (mapNestedCtxt ctxtDualMain ctxt))

instance EncodeDiskDep (NestedCtxt Header) m
      => EncodeDiskDep (NestedCtxt Header) (DualBlock m a) where
  encodeDiskDep ccfg ctxt =
      encodeDiskDep
        (dualCodecConfigMain ccfg)
        (mapNestedCtxt ctxtDualMain ctxt)

{-------------------------------------------------------------------------------
  HasBinaryBlockInfo
-------------------------------------------------------------------------------}

-- | The binary info just refers to the main block
--
-- This is sufficient, because we never need just the header of the auxiliary.
instance HasBinaryBlockInfo m => HasBinaryBlockInfo (DualBlock m a) where
  getBinaryBlockInfo DualBlock{..} =
      BinaryBlockInfo {
          headerSize   = headerSize   mainBinaryBlockInfo
        , headerOffset = headerOffset mainBinaryBlockInfo + 1
        }
    where
      mainBinaryBlockInfo :: BinaryBlockInfo
      mainBinaryBlockInfo = getBinaryBlockInfo dualBlockMain

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

-- | 'InspectLedger' just refers to the main block
--
-- 'InspectLedger' is intended to check the ledger state against the node's
-- configuration, and hence takes a full 'TopLevelConfig'. However, we cannot
-- construct that for the auxiliary block, since we have no protocol config
-- for it. We therefore just use the main block.
instance InspectLedger m => InspectLedger (DualBlock m a) where
  type LedgerWarning (DualBlock m a) = LedgerWarning m
  type LedgerUpdate  (DualBlock m a) = LedgerUpdate  m

  inspectLedger cfg before after = map castLedgerEvent $
      inspectLedger
        (dualTopLevelConfigMain cfg)
        (dualLedgerStateMain    before)
        (dualLedgerStateMain    after)


{-------------------------------------------------------------------------------
  PeerSelection
-------------------------------------------------------------------------------}

instance LedgerSupportsPeerSelection m
      => LedgerSupportsPeerSelection (DualBlock m a) where
  getPeers = getPeers . dualLedgerStateMain

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

type instance CannotForge           (DualBlock m a) = CannotForge           m
type instance ForgeStateInfo        (DualBlock m a) = ForgeStateInfo        m
type instance ForgeStateUpdateError (DualBlock m a) = ForgeStateUpdateError m

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Lift 'applyLedgerBlock' to @Maybe blk@
--
-- Returns state unchanged on 'Nothing'
applyMaybeBlock :: UpdateLedger blk
                => LedgerConfig blk
                -> Maybe blk
                -> TickedLedgerState blk ValuesMK
                -> LedgerState blk EmptyMK
                -> Except (LedgerError blk) (LedgerState blk DiffMK)
applyMaybeBlock _   Nothing      _   st =
    -- if there is no block, then are no changes to track
    return $ st `withLedgerTables` polyEmptyLedgerTables
applyMaybeBlock cfg (Just block) tst _  =
    applyLedgerBlock cfg block tst

-- | Lift 'reapplyLedgerBlock' to @Maybe blk@
--
-- See also 'applyMaybeBlock'
reapplyMaybeBlock :: UpdateLedger blk
                  => LedgerConfig blk
                  -> Maybe blk
                  -> TickedLedgerState blk ValuesMK
                  -> LedgerState blk EmptyMK
                  -> LedgerState blk DiffMK
reapplyMaybeBlock _   Nothing      _   st =
    -- if there is no block, then are no changes to track
    st `withLedgerTables` polyEmptyLedgerTables
reapplyMaybeBlock cfg (Just block) tst _  =
    reapplyLedgerBlock cfg block tst

-- | Used when the concrete and abstract implementation should agree on errors
--
-- The abstract-versus-concrete tests from the ledger folks tests precisely
-- this, so if this fails, it indicates a bug somewhere and we error out.
agreeOnError :: (Show e, Show e', HasCallStack)
             => (e -> e' -> err)
             -> (Except e a, Except e' b)
             -> Except err (a, b)
agreeOnError f (ma, mb) =
    case (runExcept ma, runExcept mb) of
      (Left e, Left e') ->
        throwError $ f e e'
      (Left e, Right _) ->
        error $ "agreeOnError: unexpected error " ++ show e
      (Right _, Left e') ->
        error $ "agreeOnError: unexpected error " ++ show e'
      (Right a, Right b) ->
        return (a, b)

{-------------------------------------------------------------------------------
  Serialisation

  For now we just require 'Serialise' for the auxiliary block.
-------------------------------------------------------------------------------}

encodeDualBlock :: (Bridge m a, Serialise a)
                => (m -> Encoding)
                -> DualBlock m a -> Encoding
encodeDualBlock encodeMain DualBlock{..} = mconcat [
        encodeListLen 3
      , encodeMain dualBlockMain
      , encode dualBlockAux
      , encode dualBlockBridge
      ]

decodeDualBlock :: (Bridge m a, Serialise a)
                => Decoder s (Lazy.ByteString -> m)
                -> Decoder s (Lazy.ByteString -> DualBlock m a)
decodeDualBlock decodeMain = do
    enforceSize "DualBlock" 3
    dualBlock
      <$> decodeMain
      <*> decode
      <*> decode
  where
    dualBlock :: (Lazy.ByteString -> m)
              -> Maybe a
              -> BridgeBlock m a
              -> (Lazy.ByteString -> DualBlock m a)
    dualBlock conc abst bridge bs = DualBlock (conc bs) abst bridge

encodeDualHeader :: (Header m -> Encoding)
                 -> Header (DualBlock m a) -> Encoding
encodeDualHeader encodeMain DualHeader{..} = encodeMain dualHeaderMain

decodeDualHeader :: Decoder s (Lazy.ByteString -> Header m)
                 -> Decoder s (Lazy.ByteString -> Header (DualBlock m a))
decodeDualHeader decodeMain =
    dualHeader <$> decodeMain
  where
    dualHeader :: (Lazy.ByteString -> Header m)
               -> (Lazy.ByteString -> Header (DualBlock m a))
    dualHeader conc bs = DualHeader (conc bs)

encodeDualGenTx :: (Bridge m a, Serialise (GenTx a))
                => (GenTx m -> Encoding)
                -> GenTx (DualBlock m a) -> Encoding
encodeDualGenTx encodeMain DualGenTx{..} = mconcat [
      encodeListLen 3
    , encodeMain dualGenTxMain
    , encode     dualGenTxAux
    , encode     dualGenTxBridge
    ]

decodeDualGenTx :: (Bridge m a, Serialise (GenTx a))
                => Decoder s (GenTx m)
                -> Decoder s (GenTx (DualBlock m a))
decodeDualGenTx decodeMain = do
    enforceSize "DualGenTx" 3
    DualGenTx
      <$> decodeMain
      <*> decode
      <*> decode

encodeDualGenTxId :: (GenTxId m -> Encoding)
                  -> GenTxId (DualBlock m a) -> Encoding
encodeDualGenTxId encodeMain = encodeMain . dualGenTxIdMain

decodeDualGenTxId :: Decoder s (GenTxId m)
                  -> Decoder s (GenTxId (DualBlock m a))
decodeDualGenTxId decodeMain = DualGenTxId <$> decodeMain

encodeDualGenTxErr :: Serialise (ApplyTxErr a)
                   => (ApplyTxErr m -> Encoding)
                   -> ApplyTxErr (DualBlock m a) -> Encoding
encodeDualGenTxErr encodeMain DualGenTxErr{..} = mconcat [
      encodeListLen 2
    , encodeMain dualGenTxErrMain
    , encode     dualGenTxErrAux
    ]

decodeDualGenTxErr :: Serialise (ApplyTxErr a)
                   => Decoder s (ApplyTxErr m)
                   -> Decoder s (ApplyTxErr (DualBlock m a))
decodeDualGenTxErr decodeMain = do
    enforceSize "DualGenTxErr" 2
    DualGenTxErr
      <$> decodeMain
      <*> decode

encodeDualLedgerState :: (Bridge m a, Serialise (LedgerState a mk))
                      => (LedgerState m mk -> Encoding)
                      -> LedgerState (DualBlock m a) mk -> Encoding
encodeDualLedgerState encodeMain DualLedgerState{..} = mconcat [
      encodeListLen 3
    , encodeMain dualLedgerStateMain
    , encode     dualLedgerStateAux
    , encode     dualLedgerStateBridge
    ]

decodeDualLedgerState :: (Bridge m a, Serialise (LedgerState a mk))
                      => Decoder s (LedgerState m mk)
                      -> Decoder s (LedgerState (DualBlock m a) mk)
decodeDualLedgerState decodeMain = do
    enforceSize "DualLedgerState" 3
    DualLedgerState
      <$> decodeMain
      <*> decode
      <*> decode
