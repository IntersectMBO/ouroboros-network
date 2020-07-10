{-# LANGUAGE ConstraintKinds            #-}
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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Ouroboros.Consensus.Ledger.Dual (
    Bridge(..)
    -- * Pair types
  , DualBlock(..)
  , DualHeader
  , DualLedgerConfig(..)
  , DualLedgerError(..)
  , DualGenTxErr(..)
    -- * Lifted functions
  , dualExtValidationErrorMain
  , dualFullBlockConfigMain
  , dualFullBlockConfigAux
  , dualTopLevelConfigMain
  , ctxtDualMain
    -- * Type class family instances
  , Header(..)
  , BlockConfig(..)
  , CodecConfig(..)
  , LedgerState(..)
  , GenTx(..)
  , TxId(..)
  , NestedCtxt_(..)
  , Ticked(..)
    -- * Serialisation
  , encodeDualBlock
  , decodeDualBlock
  , encodeDualHeader
  , decodeDualHeader
  , encodeDualGenTx
  , decodeDualGenTx
  , encodeDualGenTxId
  , decodeDualGenTxId
  , encodeDualGenTxErr
  , decodeDualGenTxErr
  , encodeDualLedgerState
  , decodeDualLedgerState
  , dualBinaryBlockInfo
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short
import           Data.FingerTree.Strict (Measured (..))
import           Data.Typeable
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Binary (enforceSize)
import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))

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
-- meaningful 'NoUnexpectedThunks' checks here.
data DualBlock m a = DualBlock {
      dualBlockMain   :: m
    , dualBlockAux    :: Maybe a
    , dualBlockBridge :: BridgeBlock m a
    }

deriving instance (Show m, Show a, Show (BridgeBlock m a)) => Show (DualBlock m a)
deriving instance (Eq   m, Eq   a, Eq   (BridgeBlock m a)) => Eq   (DualBlock m a)

instance Condense m => Condense (DualBlock m a) where
  condense = condense . dualBlockMain

type instance HeaderHash (DualBlock m a) = HeaderHash m
instance StandardHash m => StandardHash (DualBlock m a)

instance ConvertRawHash m => ConvertRawHash (DualBlock m a) where
  toRawHash   _ = toRawHash   (Proxy @m)
  fromRawHash _ = fromRawHash (Proxy @m)
  hashSize    _ = hashSize    (Proxy @m)

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

newtype instance Header (DualBlock m a) = DualHeader { dualHeaderMain :: Header m }
  deriving NoUnexpectedThunks via AllowThunk (Header (DualBlock m a))

instance Bridge m a => GetHeader (DualBlock m a) where
  getHeader = DualHeader . getHeader . dualBlockMain

  blockMatchesHeader hdr =
      blockMatchesHeader (dualHeaderMain hdr) . dualBlockMain

  -- We can look at the concrete header to see if this is an EBB
  headerIsEBB = headerIsEBB . dualHeaderMain

type DualHeader m a = Header (DualBlock m a)

deriving instance Show (Header m) => Show (DualHeader m a)

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data instance BlockConfig (DualBlock m a) = DualBlockConfig {
      dualBlockConfigMain :: BlockConfig m
    , dualBlockConfigAux  :: BlockConfig a
    }
  deriving NoUnexpectedThunks via AllowThunk (BlockConfig (DualBlock m a))

instance ConfigSupportsNode m => ConfigSupportsNode (DualBlock m a) where
  getSystemStart     = getSystemStart     . dualBlockConfigMain
  getNetworkMagic    = getNetworkMagic    . dualBlockConfigMain
  getProtocolMagicId = getProtocolMagicId . dualBlockConfigMain

{-------------------------------------------------------------------------------
  Splitting the config
-------------------------------------------------------------------------------}

dualFullBlockConfigMain ::
     FullBlockConfig (LedgerState (DualBlock m a)) (DualBlock m a)
  -> FullBlockConfig (LedgerState m) m
dualFullBlockConfigMain FullBlockConfig{..} = FullBlockConfig{
      blockConfigLedger = dualLedgerConfigMain blockConfigLedger
    , blockConfigBlock  = dualBlockConfigMain  blockConfigBlock
    , blockConfigCodec  = dualCodecConfigMain  blockConfigCodec
    }

dualFullBlockConfigAux ::
     FullBlockConfig (LedgerState (DualBlock m a)) (DualBlock m a)
  -> FullBlockConfig (LedgerState a) a
dualFullBlockConfigAux FullBlockConfig{..} = FullBlockConfig{
      blockConfigLedger = dualLedgerConfigAux blockConfigLedger
    , blockConfigBlock  = dualBlockConfigAux  blockConfigBlock
    , blockConfigCodec  = dualCodecConfigAux  blockConfigCodec
    }

-- | This is only used for block production
dualTopLevelConfigMain :: TopLevelConfig (DualBlock m a) -> TopLevelConfig m
dualTopLevelConfigMain TopLevelConfig{..} = TopLevelConfig{
      topLevelConfigProtocol = topLevelConfigProtocol
    , topLevelConfigBlock    = dualFullBlockConfigMain topLevelConfigBlock
    }

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

data instance CodecConfig (DualBlock m a) = DualCodecConfig {
      dualCodecConfigMain :: CodecConfig m
    , dualCodecConfigAux  :: CodecConfig a
    }
  deriving (Generic)

instance ( NoUnexpectedThunks (CodecConfig m)
         , NoUnexpectedThunks (CodecConfig a)
         ) => NoUnexpectedThunks (CodecConfig (DualBlock m a))
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
      , Typeable              a
      , UpdateLedger          a
      , LedgerSupportsMempool a
      , Show (ApplyTxErr      a)
      , NoUnexpectedThunks (LedgerConfig a)
      , NoUnexpectedThunks (CodecConfig a)

        -- Requirements on the various bridges
      , Show      (BridgeLedger m a)
      , Eq        (BridgeLedger m a)
      , Serialise (BridgeLedger m a)
      , Serialise (BridgeBlock  m a)
      , Serialise (BridgeTx     m a)
      , Show      (BridgeTx     m a)
      ) => Bridge m a where

  -- | Additional information relating both ledgers
  type BridgeLedger m a :: *

  -- | Information required to update the bridge when applying a block
  type BridgeBlock m a :: *

  -- | Information required to update the bridge when applying a transaction
  type BridgeTx m a :: *

  updateBridgeWithBlock :: DualBlock m a
                        -> BridgeLedger m a -> BridgeLedger m a

  updateBridgeWithTx :: GenTx (DualBlock m a)
                     -> BridgeLedger m a -> BridgeLedger m a

{-------------------------------------------------------------------------------
  HasHeader instance
-------------------------------------------------------------------------------}

instance Bridge m a => Measured BlockMeasure (DualBlock m a) where
  measure = blockMeasure

instance Bridge m a => HasHeader (DualBlock m a) where
  getHeaderFields = getBlockHeaderFields

instance Bridge m a => HasHeader (DualHeader m a) where
  getHeaderFields = castHeaderFields . getHeaderFields . dualHeaderMain

instance Bridge m a => GetPrevHash (DualBlock m a) where
  headerPrevHash cfg =
        castHash
      . headerPrevHash (dualCodecConfigMain cfg)
      . dualHeaderMain

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
  deriving NoUnexpectedThunks via AllowThunk (DualLedgerError m a)

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
  deriving NoUnexpectedThunks via AllowThunk (DualLedgerConfig m a)

type instance LedgerCfg (LedgerState (DualBlock m a)) = DualLedgerConfig m a

instance Bridge m a => GetTip (LedgerState (DualBlock m a)) where
  getTip = castPoint . getTip . dualLedgerStateMain

instance Bridge m a => GetTip (Ticked (LedgerState (DualBlock m a))) where
  getTip = castPoint . getTip . tickedDualLedgerStateMain

data instance Ticked (LedgerState (DualBlock m a)) = TickedDualLedgerState {
      tickedDualLedgerStateMain    :: Ticked (LedgerState m)
    , tickedDualLedgerStateAux     :: Ticked (LedgerState a)
    , tickedDualLedgerStateBridge  :: BridgeLedger m a

      -- | The original, unticked ledger for the auxiliary block
      --
      -- The reason we keep this in addition to the ticked ledger state is that
      -- not every main block is paired with an auxiliary block. When there is
      -- no auxiliary block, the auxiliary ledger state remains unchanged.
    , tickedDualLedgerStateAuxOrig :: LedgerState a
    }
  deriving NoUnexpectedThunks via AllowThunk (Ticked (LedgerState (DualBlock m a)))

instance Bridge m a => IsLedger (LedgerState (DualBlock m a)) where
  type LedgerErr (LedgerState (DualBlock m a)) = DualLedgerError   m a

  applyChainTick DualLedgerConfig{..}
                 slot
                 DualLedgerState{..} = TickedDualLedgerState {
        tickedDualLedgerStateMain    = applyChainTick
                                         dualLedgerConfigMain
                                         slot
                                         dualLedgerStateMain
      , tickedDualLedgerStateAux     = applyChainTick
                                         dualLedgerConfigAux
                                         slot
                                         dualLedgerStateAux
      , tickedDualLedgerStateAuxOrig = dualLedgerStateAux
      , tickedDualLedgerStateBridge  = dualLedgerStateBridge
      }

instance Bridge m a => ApplyBlock (LedgerState (DualBlock m a)) (DualBlock m a) where

  applyLedgerBlock cfg
                   block@DualBlock{..}
                   TickedDualLedgerState{..} = do
      (main', aux') <-
        agreeOnError DualLedgerError (
            applyLedgerBlock
              (dualFullBlockConfigMain cfg)
              dualBlockMain
              tickedDualLedgerStateMain
          , applyMaybeBlock
              (dualFullBlockConfigAux cfg)
              dualBlockAux
              tickedDualLedgerStateAux
              tickedDualLedgerStateAuxOrig
          )
      return DualLedgerState {
          dualLedgerStateMain   = main'
        , dualLedgerStateAux    = aux'
        , dualLedgerStateBridge = updateBridgeWithBlock
                                    block
                                    tickedDualLedgerStateBridge
        }

  reapplyLedgerBlock cfg
                     block@DualBlock{..}
                     TickedDualLedgerState{..} =
    DualLedgerState {
          dualLedgerStateMain   = reapplyLedgerBlock
                                    (dualFullBlockConfigMain cfg)
                                    dualBlockMain
                                    tickedDualLedgerStateMain
        , dualLedgerStateAux    = reapplyMaybeBlock
                                    (dualFullBlockConfigAux cfg)
                                    dualBlockAux
                                    tickedDualLedgerStateAux
                                    tickedDualLedgerStateAuxOrig
        , dualLedgerStateBridge = updateBridgeWithBlock
                                    block
                                    tickedDualLedgerStateBridge
      }

data instance LedgerState (DualBlock m a) = DualLedgerState {
      dualLedgerStateMain   :: LedgerState m
    , dualLedgerStateAux    :: LedgerState a
    , dualLedgerStateBridge :: BridgeLedger m a
    }
  deriving NoUnexpectedThunks via AllowThunk (LedgerState (DualBlock m a))

instance Bridge m a => UpdateLedger (DualBlock m a)

deriving instance ( Show (LedgerState m)
                  , Show (LedgerState a)
                  , Bridge m a
                  ) => Show (LedgerState (DualBlock m a))
deriving instance ( Eq (LedgerState m)
                  , Eq (LedgerState a)
                  , Bridge m a
                  ) => Eq (LedgerState (DualBlock m a))

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

data instance Query (DualBlock m a) result
  deriving (Show)

-- | Not used in the tests: no constructors
instance Bridge m a => QueryLedger (DualBlock m a) where
  answerQuery _ = \case {}
  eqQuery       = \case {}

instance ShowQuery (Query (DualBlock m a)) where
  showResult = \case {}

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

data instance GenTx (DualBlock m a) = DualGenTx {
      dualGenTxMain   :: GenTx m
    , dualGenTxAux    :: GenTx a
    , dualGenTxBridge :: BridgeTx m a
    }
  deriving NoUnexpectedThunks via AllowThunk (GenTx (DualBlock m a))

type instance ApplyTxErr (DualBlock m a) = DualGenTxErr m a

instance Bridge m a => LedgerSupportsMempool (DualBlock m a) where
  applyTx DualLedgerConfig{..}
          slot
          tx@DualGenTx{..}
          TickedDualLedgerState{..} = do
      (main', aux') <-
        agreeOnError DualGenTxErr (
            applyTx
              dualLedgerConfigMain
              slot
              dualGenTxMain
              tickedDualLedgerStateMain
          , applyTx
              dualLedgerConfigAux
              slot
              dualGenTxAux
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

  reapplyTx DualLedgerConfig{..}
            slot
            tx@DualGenTx{..}
            TickedDualLedgerState{..} = do
      (main', aux') <-
        agreeOnError DualGenTxErr (
            reapplyTx
              dualLedgerConfigMain
              slot
              dualGenTxMain
              tickedDualLedgerStateMain
          , reapplyTx
              dualLedgerConfigAux
              slot
              dualGenTxAux
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

  maxTxCapacity = maxTxCapacity . tickedDualLedgerStateMain
  txInBlockSize = txInBlockSize . dualGenTxMain

-- We don't need a pair of IDs, as long as we can unique ID the transaction
newtype instance TxId (GenTx (DualBlock m a)) = DualGenTxId {
      dualGenTxIdMain :: GenTxId m
    }
  deriving NoUnexpectedThunks via AllowThunk (TxId (GenTx (DualBlock m a)))

instance Bridge m a => HasTxId (GenTx (DualBlock m a)) where
  txId = DualGenTxId . txId . dualGenTxMain

deriving instance Bridge m a => Show (GenTx (DualBlock m a))
deriving instance Bridge m a => Show (DualGenTxErr m a)

deriving instance Show (GenTxId m) => Show (TxId (GenTx (DualBlock m a)))
deriving instance Eq   (GenTxId m) => Eq   (TxId (GenTx (DualBlock m a)))
deriving instance Ord  (GenTxId m) => Ord  (TxId (GenTx (DualBlock m a)))

{-------------------------------------------------------------------------------
  Nested contents

  Since we only have a single header, we just delegate to the main block.
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (DualBlock m a) f x where
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
        SomeBlock ctxt -> SomeBlock (mapNestedCtxt CtxtDual ctxt)
    where
      prefixMain = Short.pack . drop 1 . Short.unpack $ prefix

instance EncodeDiskDepIx (NestedCtxt Header) m
      => EncodeDiskDepIx (NestedCtxt Header) (DualBlock m a) where
  encodeDiskDepIx ccfg (SomeBlock ctxt) =
      encodeDiskDepIx
        (dualCodecConfigMain ccfg)
        (SomeBlock (mapNestedCtxt ctxtDualMain ctxt))

instance EncodeDiskDep (NestedCtxt Header) m
      => EncodeDiskDep (NestedCtxt Header) (DualBlock m a) where
  encodeDiskDep ccfg ctxt =
      encodeDiskDep
        (dualCodecConfigMain ccfg)
        (mapNestedCtxt ctxtDualMain ctxt)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Lift 'applyLedgerBlock' to @Maybe blk@
--
-- Returns state unchanged on 'Nothing'
applyMaybeBlock :: UpdateLedger blk
                => FullBlockConfig (LedgerState blk) blk
                -> Maybe blk
                -> TickedLedgerState blk
                -> LedgerState blk
                -> Except (LedgerError blk) (LedgerState blk)
applyMaybeBlock _   Nothing      _   st = return st
applyMaybeBlock cfg (Just block) tst _  = applyLedgerBlock cfg block tst

-- | Lift 'reapplyLedgerBlock' to @Maybe blk@
--
-- See also 'applyMaybeBlock'
reapplyMaybeBlock :: UpdateLedger blk
                  => FullBlockConfig (LedgerState blk) blk
                  -> Maybe blk
                  -> TickedLedgerState blk
                -> LedgerState blk
                  -> LedgerState blk
reapplyMaybeBlock _   Nothing      _   st = st
reapplyMaybeBlock cfg (Just block) tst _  = reapplyLedgerBlock cfg block tst

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

encodeDualLedgerState :: (Bridge m a, Serialise (LedgerState a))
                      => (LedgerState m -> Encoding)
                      -> LedgerState (DualBlock m a) -> Encoding
encodeDualLedgerState encodeMain DualLedgerState{..} = mconcat [
      encodeListLen 3
    , encodeMain dualLedgerStateMain
    , encode     dualLedgerStateAux
    , encode     dualLedgerStateBridge
    ]

decodeDualLedgerState :: (Bridge m a, Serialise (LedgerState a))
                      => Decoder s (LedgerState m)
                      -> Decoder s (LedgerState (DualBlock m a))
decodeDualLedgerState decodeMain = do
    enforceSize "DualLedgerState" 3
    DualLedgerState
      <$> decodeMain
      <*> decode
      <*> decode

-- | The binary info just refers to the main block
--
-- This is sufficient, because we never need just the header of the auxiliary.
dualBinaryBlockInfo :: (m -> BinaryBlockInfo)
                    -> DualBlock m a -> BinaryBlockInfo
dualBinaryBlockInfo mainGetBinaryBlockInfo DualBlock{..} =
    BinaryBlockInfo {
        headerSize   = headerSize   mainBinaryBlockInfo
      , headerOffset = headerOffset mainBinaryBlockInfo + 1
      }
  where
    mainBinaryBlockInfo :: BinaryBlockInfo
    mainBinaryBlockInfo = mainGetBinaryBlockInfo dualBlockMain
