{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE EmptyDataDeriving       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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
  , dualTopLevelConfigMain
    -- * Type class family instances
  , Header(..)
  , BlockConfig(..)
  , LedgerState(..)
  , GenTx(..)
  , TxId(..)
    -- * Serialisation
  , encodeDualBlockWithInfo
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
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise
import           Control.Exception (assert)
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Typeable
import           GHC.Stack

import           Cardano.Binary (enforceSize)
import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Storage.Common

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Node.LedgerDerivedInfo
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Util.Condense

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

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

instance GetHeader m => GetHeader (DualBlock m a) where
  newtype Header (DualBlock m a) = DualHeader { dualHeaderMain :: Header m }
    deriving NoUnexpectedThunks via AllowThunk (Header (DualBlock m a))

  getHeader = DualHeader . getHeader . dualBlockMain

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

-- | This is only used for block production
dualTopLevelConfigMain :: TopLevelConfig (DualBlock m a) -> TopLevelConfig m
dualTopLevelConfigMain TopLevelConfig{..} = TopLevelConfig{
      configConsensus =                      configConsensus
    , configLedger    = dualLedgerConfigMain configLedger
    , configBlock     = dualBlockConfigMain  configBlock
    }

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
      , LedgerDerivedInfo      m
      , ApplyTx                m
      , HasTxId (GenTx         m)
      , Show (ApplyTxErr       m)

        -- Requirements on the auxiliary block
        -- No 'LedgerSupportsProtocol' for @a@!
      , Typeable         a
      , UpdateLedger     a
      , ApplyTx          a
      , Show (ApplyTxErr a)
      , NoUnexpectedThunks (LedgerConfig a)

        -- Requirements on the various bridges
      , Show      (BridgeLedger m a)
      , Eq        (BridgeLedger m a)
      , Serialise (BridgeLedger m a)
      , Serialise (BridgeBlock  m a)
      , Serialise (BridgeTx     m a)
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
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader

  blockInvariant = const True

instance Bridge m a => HasHeader (DualHeader m a) where
  blockHash      =            blockHash     . dualHeaderMain
  blockPrevHash  = castHash . blockPrevHash . dualHeaderMain
  blockSlot      =            blockSlot     . dualHeaderMain
  blockNo        =            blockNo       . dualHeaderMain

  blockInvariant = const True

{-------------------------------------------------------------------------------
  Protocol
-------------------------------------------------------------------------------}

type instance NodeState     (DualBlock m a) = NodeState     m
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

instance Bridge m a => IsLedger (LedgerState (DualBlock m a)) where
  type LedgerCfg (LedgerState (DualBlock m a)) = DualLedgerConfig m a
  type LedgerErr (LedgerState (DualBlock m a)) = DualLedgerError   m a

  applyChainTick DualLedgerConfig{..}
                 slot
                 DualLedgerState{..} =
      assert (tickedSlotNo tickedM == tickedSlotNo tickedA) $
      TickedLedgerState (tickedSlotNo tickedM) DualLedgerState {
          dualLedgerStateMain   = tickedLedgerState tickedM
        , dualLedgerStateAux    = tickedLedgerState tickedA
        , dualLedgerStateBridge = dualLedgerStateBridge
        }
    where
      tickedM :: TickedLedgerState m
      tickedM = applyChainTick
                  dualLedgerConfigMain
                  slot
                  dualLedgerStateMain

      tickedA :: TickedLedgerState a
      tickedA = applyChainTick
                  dualLedgerConfigAux
                  slot
                  dualLedgerStateAux

instance Bridge m a => ApplyBlock (LedgerState (DualBlock m a)) (DualBlock m a) where
  applyLedgerBlock DualLedgerConfig{..}
                   block@DualBlock{..}
                   (TickedLedgerState slot DualLedgerState{..}) = do
      (main', aux') <-
        agreeOnError DualLedgerError (
            applyLedgerBlock
              dualLedgerConfigMain
              dualBlockMain
              (TickedLedgerState slot dualLedgerStateMain)
          , applyMaybeBlock
              dualLedgerConfigAux
              dualBlockAux
              (TickedLedgerState slot dualLedgerStateAux)
          )
      return DualLedgerState {
          dualLedgerStateMain   = main'
        , dualLedgerStateAux    = aux'
        , dualLedgerStateBridge = updateBridgeWithBlock
                                    block
                                    dualLedgerStateBridge
        }

  reapplyLedgerBlock DualLedgerConfig{..}
                     block@DualBlock{..}
                     (TickedLedgerState slot DualLedgerState{..}) =
    DualLedgerState {
          dualLedgerStateMain   = reapplyLedgerBlock
                                    dualLedgerConfigMain
                                    dualBlockMain
                                    (TickedLedgerState slot dualLedgerStateMain)
        , dualLedgerStateAux    = reapplyMaybeBlock
                                    dualLedgerConfigAux
                                    dualBlockAux
                                    (TickedLedgerState slot dualLedgerStateAux)
        , dualLedgerStateBridge = updateBridgeWithBlock
                                    block
                                    dualLedgerStateBridge
      }

  ledgerTipPoint = castPoint
                 . (ledgerTipPoint :: LedgerState m -> Point m)
                 . dualLedgerStateMain

instance Bridge m a => UpdateLedger (DualBlock m a) where
  data LedgerState (DualBlock m a) = DualLedgerState {
        dualLedgerStateMain   :: LedgerState m
      , dualLedgerStateAux    :: LedgerState a
      , dualLedgerStateBridge :: BridgeLedger m a
      }
    deriving NoUnexpectedThunks via AllowThunk (LedgerState (DualBlock m a))

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
  getTipInfo = getTipInfo . dualHeaderMain

instance Bridge m a => ValidateEnvelope (DualBlock m a) where
  type OtherHeaderEnvelopeError (DualBlock m a) = OtherHeaderEnvelopeError m
  validateEnvelope cfg ledgerView t =
        withExcept castHeaderEnvelopeError
      . validateEnvelope (dualTopLevelConfigMain cfg) ledgerView (castAnnTip <$> t)
      . dualHeaderMain

  firstBlockNo          _ = firstBlockNo          (Proxy @m)
  minimumPossibleSlotNo _ = minimumPossibleSlotNo (Proxy @m)

instance Bridge m a => LedgerSupportsProtocol (DualBlock m a) where
  protocolLedgerView cfg state =
      protocolLedgerView
        (dualLedgerConfigMain cfg)
        (dualLedgerStateMain  state)

  ledgerViewForecastAt_ cfg state =
      ledgerViewForecastAt_
        (dualLedgerConfigMain cfg)
        (dualLedgerStateMain  state)

instance Bridge m a => HasHardForkHistory (DualBlock m a) where
  type HardForkIndices (DualBlock m a) = HardForkIndices m

  hardForkShape cfg =
      hardForkShape
        (dualBlockConfigMain cfg)

  hardForkTransitions cfg state =
      hardForkTransitions
        (dualLedgerConfigMain cfg)
        (dualLedgerStateMain  state)

instance Bridge m a => LedgerDerivedInfo (DualBlock m a) where
  knownSlotLengths cfg =
      knownSlotLengths
        (dualBlockConfigMain cfg)

{-------------------------------------------------------------------------------
  Querying the ledger
-------------------------------------------------------------------------------}

-- | Not used in the tests: no constructors
instance Bridge m a => QueryLedger (DualBlock m a) where
  data Query (DualBlock m a) result
    deriving (Show)

  answerQuery _ = \case {}
  eqQuery       = \case {}

instance ShowQuery (Query (DualBlock m a)) where
  showResult = \case {}

{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

data DualGenTxErr m a = DualGenTxErr {
      dualGenTxErrMain :: ApplyTxErr m
    , dualGenTxErrAux  :: ApplyTxErr a
    }

instance Bridge m a => ApplyTx (DualBlock m a) where
  data GenTx (DualBlock m a) = DualGenTx {
        dualGenTxMain   :: GenTx m
      , dualGenTxAux    :: GenTx a
      , dualGenTxBridge :: BridgeTx m a
      }
    deriving NoUnexpectedThunks via AllowThunk (GenTx (DualBlock m a))

  type ApplyTxErr (DualBlock m a) = DualGenTxErr m a

  txSize DualGenTx{..} = txSize dualGenTxMain + txSize dualGenTxAux

  applyTx DualLedgerConfig{..}
          tx@DualGenTx{..}
          (TickedLedgerState slot DualLedgerState{..}) = do
      (TickedLedgerState _ main', TickedLedgerState _ aux') <-
        agreeOnError DualGenTxErr (
            applyTx
              dualLedgerConfigMain
              dualGenTxMain
              (TickedLedgerState slot dualLedgerStateMain)
          , applyTx
              dualLedgerConfigAux
              dualGenTxAux
              (TickedLedgerState slot dualLedgerStateAux)
          )
      return $ TickedLedgerState slot DualLedgerState {
          dualLedgerStateMain   = main'
        , dualLedgerStateAux    = aux'
        , dualLedgerStateBridge = updateBridgeWithTx
                                    tx
                                    dualLedgerStateBridge
        }

  reapplyTx DualLedgerConfig{..}
            tx@DualGenTx{..}
            (TickedLedgerState slot DualLedgerState{..}) = do
      (TickedLedgerState _ main', TickedLedgerState _ aux') <-
        agreeOnError DualGenTxErr (
            reapplyTx
              dualLedgerConfigMain
              dualGenTxMain
              (TickedLedgerState slot dualLedgerStateMain)
          , reapplyTx
              dualLedgerConfigAux
              dualGenTxAux
              (TickedLedgerState slot dualLedgerStateAux)
          )
      return $ TickedLedgerState slot DualLedgerState {
          dualLedgerStateMain   = main'
        , dualLedgerStateAux    = aux'
        , dualLedgerStateBridge = updateBridgeWithTx
                                    tx
                                    dualLedgerStateBridge
        }

instance Bridge m a => HasTxId (GenTx (DualBlock m a)) where
  -- We don't need a pair of IDs, as long as we can unique ID the transaction
  newtype TxId (GenTx (DualBlock m a)) = DualGenTxId {
        dualGenTxIdMain :: GenTxId m
      }
    deriving NoUnexpectedThunks via AllowThunk (TxId (GenTx (DualBlock m a)))

  txId = DualGenTxId . txId . dualGenTxMain

deriving instance ( Show (GenTx m)
                  , Show (GenTx a)
                  , Show (BridgeTx m a)
                  ) => Show (GenTx (DualBlock m a))
deriving instance ( Show (ApplyTxErr m)
                  , Show (ApplyTxErr a)
                  ) => Show (DualGenTxErr m a)

deriving instance Show (GenTxId m) => Show (TxId (GenTx (DualBlock m a)))
deriving instance Eq   (GenTxId m) => Eq   (TxId (GenTx (DualBlock m a)))
deriving instance Ord  (GenTxId m) => Ord  (TxId (GenTx (DualBlock m a)))

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Lift 'applyLedgerBlock' to @Maybe blk@
--
-- Returns state unchanged on 'Nothing'
applyMaybeBlock :: UpdateLedger blk
                => LedgerConfig blk
                -> Maybe blk
                -> TickedLedgerState blk
                -> Except (LedgerError blk) (LedgerState blk)
applyMaybeBlock _   Nothing      = return . tickedLedgerState
applyMaybeBlock cfg (Just block) = applyLedgerBlock cfg block

-- | Lift 'reapplyLedgerBlock' to @Maybe blk@
--
-- See also 'applyMaybeBlock'
reapplyMaybeBlock :: UpdateLedger blk
                  => LedgerConfig blk
                  -> Maybe blk
                  -> TickedLedgerState blk
                  -> LedgerState blk
reapplyMaybeBlock _   Nothing      = tickedLedgerState
reapplyMaybeBlock cfg (Just block) = reapplyLedgerBlock cfg block

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

-- | The binary info just refers to the main block
--
-- This is sufficient, because we never need just the header of the auxiliary.
encodeDualBlockWithInfo :: (Bridge m a, Serialise a)
                        => (m -> BinaryInfo Encoding)
                        -> DualBlock m a -> BinaryInfo Encoding
encodeDualBlockWithInfo encodeMainWithInfo DualBlock{..} =
    BinaryInfo {
        headerSize   = headerSize   mainWithInfo
      , headerOffset = headerOffset mainWithInfo + 1
      , binaryBlob   = mconcat [
                           encodeListLen 3
                         , binaryBlob mainWithInfo
                         , encode dualBlockAux
                         , encode dualBlockBridge
                         ]
      }
  where
    mainWithInfo :: BinaryInfo Encoding
    mainWithInfo = encodeMainWithInfo dualBlockMain

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
