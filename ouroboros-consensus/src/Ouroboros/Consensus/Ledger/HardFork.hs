{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.HardFork (
    CanHardForkBlock (..)
  , Header (..)
  , ForkedHeaderHash (..)
  , LedgerState (..)
  , ForkedApplyTxErr (..)
  , encodeForkedApplyTxErr
  , GenTx (..)
  , GenTxId (..)
  ) where

import           Control.Monad.Except (Except, throwError, withExcept)
import           Data.Bifunctor (bimap)
import           Data.FingerTree (Measured (..))
import           Data.Maybe (isJust)
import           Data.Word (Word8)
import           Unsafe.Coerce (unsafeCoerce)

import           Cardano.Binary (Encoding, ToCBOR (..), encodeListLen)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.HardFork

-- These should move to another module with the specific CanHardForkBlock definition for PBFT and Praos
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Praos

{-------------------------------------------------------------------------------
  Class for hard forking between ledgers
-------------------------------------------------------------------------------}

-- | Translate from one ledger state to another at the boundary of a hard fork
class
  (UpdateLedger blk1, UpdateLedger blk2)
  => CanHardForkBlock blk1 blk2 where
  -- | The slot number at which the hard fork will come into affect
  --
  --   If this is @Just n@, the new (type of) ledger state should be used from
  --   slot @n@ forward. This should only be 'Just' a value when we are sure
  --   that the hard fork will happen (in other words, when update proposals etc
  --   have been accepted and are part of the stable chain).
  hardForksAt :: LedgerState blk1 -> Maybe SlotNo
  translateLedgerStateForHardFork :: LedgerState blk1 -> LedgerState blk2
  translateHeaderHash :: HeaderHash blk1 -> HeaderHash blk2

{-------------------------------------------------------------------------------
  BlockProtocol
-------------------------------------------------------------------------------}

-- | 'Forked' blocks compose the corresponding 'BlockProtocol's of the
--   sub-blocks using the 'HardForksTo' protocol combinator
type instance
  BlockProtocol (Forked blk1 blk2) =
    BlockProtocol blk1 `HardForksTo` BlockProtocol blk2

{-------------------------------------------------------------------------------
  Header and Hashes
-------------------------------------------------------------------------------}

type instance HeaderHash (Forked blk1 blk2) = ForkedHeaderHash blk1 blk2

newtype ForkedHeaderHash blk1 blk2
  = ForkedHeaderHash
      { unForkedHeaderHash :: Forked (HeaderHash blk1) (HeaderHash blk2)
      }

deriving instance
  (Show (HeaderHash blk1), Show (HeaderHash blk2))
  => Show (ForkedHeaderHash blk1 blk2)

deriving instance
  (Ord (HeaderHash blk1), Ord (HeaderHash blk2), CanHardForkBlock blk1 blk2)
  => Ord (ForkedHeaderHash blk1 blk2)

instance
  (Eq (HeaderHash blk1), Eq (HeaderHash blk2), CanHardForkBlock blk1 blk2)
  => Eq (ForkedHeaderHash blk1 blk2) where
  forkedHeaderHash1 == forkedHeaderHash2 =
    case (unForkedHeaderHash forkedHeaderHash1, unForkedHeaderHash forkedHeaderHash2) of
      (BeforeFork a, BeforeFork b) -> a == b
      (BeforeFork a, AfterFork b)  -> translateHeaderHash @blk1 @blk2 a == b
      (AfterFork a, BeforeFork b)  -> a == translateHeaderHash @blk1 @blk2 b
      (AfterFork a, AfterFork b)   -> a == b

instance
  (Measured BlockMeasure blk1, Measured BlockMeasure blk2)
  => Measured BlockMeasure (Forked blk1 blk2) where
  measure = forked measure measure

instance (GetHeader blk1, GetHeader blk2) => GetHeader (Forked blk1 blk2) where

  newtype Header (Forked blk1 blk2)
    = ForkedHeader
        { unForkedHeader :: Forked (Header blk1) (Header blk2)
        }

  getHeader = ForkedHeader . bimap getHeader getHeader

deriving instance
  (Show (Header blk1), Show (Header blk2))
  => Show (Header (Forked blk1 blk2))

instance
  ( SupportedHeader p1 (Header blk1)
  , SupportedHeader p2 (Header blk2)
  , HasHeader (Header blk2)
  )
  => HardForkSupportedHeader p1 p2 (Header (Forked blk1 blk2)) where

  type ForkedBefore (Header (Forked blk1 blk2)) = Header blk1

  type ForkedAfter (Header (Forked blk1 blk2)) = Header blk2

  getForkedHeader = unForkedHeader

instance
  (StandardHash blk1, StandardHash blk2, CanHardForkBlock blk1 blk2)
  => StandardHash (Forked blk1 blk2)

instance
  (HasHeader blk1, HasHeader blk2, CanHardForkBlock blk1 blk2)
  => HasHeader (Forked blk1 blk2) where

  blockHash = ForkedHeaderHash . bimap blockHash blockHash

  blockPrevHash =
    forked
      (fmapChainHash (ForkedHeaderHash . BeforeFork) . blockPrevHash)
      (fmapChainHash (ForkedHeaderHash . AfterFork) . blockPrevHash)

  blockSlot = forked blockSlot blockSlot

  blockNo = forked blockNo blockNo

  blockInvariant = forked blockInvariant blockInvariant


instance
  ( HasHeader blk1
  , HasHeader (Header blk1)
  , HasHeader blk2
  , HasHeader (Header blk2)
  , CanHardForkBlock blk1 blk2
  )
  => HasHeader (Header (Forked blk1 blk2)) where

  blockHash = ForkedHeaderHash . bimap blockHash blockHash . unForkedHeader

  blockPrevHash =
    forked
      (fmapChainHash (ForkedHeaderHash . BeforeFork) . blockPrevHash)
      (fmapChainHash (ForkedHeaderHash . AfterFork) . blockPrevHash) .
      unForkedHeader

  blockSlot = forked blockSlot blockSlot . unForkedHeader

  blockNo = forked blockNo blockNo . unForkedHeader

  blockInvariant = forked blockInvariant blockInvariant . unForkedHeader

{-------------------------------------------------------------------------------
  UpdateLedger
-------------------------------------------------------------------------------}

data ForkedLedgerError blk1 blk2
  = NewBlockBeforeFork
  | OldBlockAfterFork
  | BeforeForkError (LedgerError blk1)
  | AfterForkError (LedgerError blk2)

deriving instance
  (Show (LedgerError blk1), Show (LedgerError blk2))
  => Show (ForkedLedgerError blk1 blk2)


instance
  ( SupportedBlock blk1
  , SupportedBlock blk2
  , HardForkSupportedHeader
      (BlockProtocol blk1)
      (BlockProtocol blk2)
      (Header (Forked blk1 blk2))
  , CanHardFork (BlockProtocol blk1) (BlockProtocol blk2)
  , CanHardForkBlock blk1 blk2
  )
  => SupportedBlock (Forked blk1 blk2)

instance
  (SupportedBlock (Forked blk1 blk2), CanHardForkBlock blk1 blk2)
  => UpdateLedger (Forked blk1 blk2) where

  newtype LedgerState (Forked blk1 blk2)
    = ForkedLedgerState
        { unForkedLedgerState :: Forked (LedgerState blk1) (LedgerState blk2)
        }

  type LedgerError (Forked blk1 blk2) = ForkedLedgerError blk1 blk2

  data LedgerConfig (Forked blk1 blk2)
    = ForkedLedgerConfig
        { ledgerConfigBeforeFork :: LedgerConfig blk1
        , ledgerConfigAfterFork :: LedgerConfig blk2
        }

  ledgerConfigView ForkedNodeConfig {nodeConfigBeforeFork, nodeConfigAfterFork} =
    ForkedLedgerConfig
      { ledgerConfigBeforeFork = ledgerConfigView nodeConfigBeforeFork
      , ledgerConfigAfterFork = ledgerConfigView nodeConfigAfterFork
      }

  -- Apply chain tick, which includes translating the 'LedgerState' at the fork
  applyChainTick ForkedLedgerConfig {ledgerConfigBeforeFork, ledgerConfigAfterFork} slotNo =
    forked
      applyChainTickBeforeFork
      applyChainTickAfterFork .
      unForkedLedgerState
    where
      applyChainTickBeforeFork ledgerState = do
        ledgerState' <-
          withExcept BeforeForkError $
            applyChainTick ledgerConfigBeforeFork slotNo ledgerState
        let hardForkSlot = hardForksAt @blk1 @blk2 ledgerState'
        if isJust hardForkSlot && Just slotNo >= hardForkSlot
        then
          return . ForkedLedgerState . AfterFork $
            translateLedgerStateForHardFork @blk1 @blk2 ledgerState'
        else return . ForkedLedgerState . BeforeFork $ ledgerState'

      applyChainTickAfterFork =
        withExcept AfterForkError .
          fmap (ForkedLedgerState . AfterFork) .
          applyChainTick ledgerConfigAfterFork slotNo

  -- Apply a Block to the 'LedgerState'
  applyLedgerBlock =
    applyToForkedLedgerState
      applyLedgerBlock
      BeforeForkError
      applyLedgerBlock
      AfterForkError
      NewBlockBeforeFork
      OldBlockAfterFork

  reapplyLedgerBlock =
    reapplyToSameForkedLedgerState
      reapplyLedgerBlock
      reapplyLedgerBlock
      "reapplyLedgerBlock: unexpected error NewBlockBeforeFork"
      "reapplyLedgerBlock: unexpected error OldBlockAfterFork"

  ledgerTipPoint =
    forked
      (fmapPoint (ForkedHeaderHash . BeforeFork) . ledgerTipPoint)
      (fmapPoint (ForkedHeaderHash . AfterFork) . ledgerTipPoint) .
      unForkedLedgerState

deriving instance
  (Eq (LedgerState blk1), Eq (LedgerState blk2))
  => Eq (LedgerState (Forked blk1 blk2))

deriving instance
  (Show (LedgerState blk1), Show (LedgerState blk2))
  => Show (LedgerState (Forked blk1 blk2))

{-------------------------------------------------------------------------------
  ProtocolLedgerView
-------------------------------------------------------------------------------}

instance
  ( ProtocolLedgerView blk1
  , ProtocolLedgerView blk2
  , CanHardFork (BlockProtocol blk1) (BlockProtocol blk2)
  , CanHardForkBlock blk1 blk2
  )
  => ProtocolLedgerView (Forked blk1 blk2) where

  protocolLedgerView ForkedNodeConfig {nodeConfigBeforeFork, nodeConfigAfterFork} =
    bimap
      (protocolLedgerView nodeConfigBeforeFork)
      (protocolLedgerView nodeConfigAfterFork) .
      unForkedLedgerState

  -- TODO: Think about this, as were going to have to do something fancy
  anachronisticProtocolLedgerView nodeConfig (ForkedLedgerState forkedLedgerState) =
    fmap (fmap BeforeFork) .
    anachronisticProtocolLedgerView
      @blk1
      (nodeConfigBeforeFork nodeConfig)
      (unsafeBeforeFork forkedLedgerState)

{-------------------------------------------------------------------------------
  ApplyTx
-------------------------------------------------------------------------------}

data ForkedApplyTxErr blk1 blk2
  = OldTransactionAfterFork
  | NewTransactionBeforeFork
  | BeforeForkApplyTxErr (ApplyTxErr blk1)
  | AfterForkApplyTxErr (ApplyTxErr blk2)

deriving instance
  (Show (ApplyTxErr blk1), Show (ApplyTxErr blk2))
  => Show (ForkedApplyTxErr blk1 blk2)

encodeForkedApplyTxErr
  :: (ApplyTxErr blk1 -> Encoding)
  -> (ApplyTxErr blk2 -> Encoding)
  -> ForkedApplyTxErr blk1 blk2
  -> Encoding
encodeForkedApplyTxErr encodeBeforeFork encodeAfterFork = \case
  OldTransactionAfterFork ->
    encodeListLen 1 <> toCBOR (0 :: Word8)
  NewTransactionBeforeFork ->
    encodeListLen 1 <> toCBOR (1 :: Word8)
  BeforeForkApplyTxErr err ->
    encodeListLen 2 <> toCBOR (2 :: Word8) <> encodeBeforeFork err
  AfterForkApplyTxErr err ->
    encodeListLen 2 <> toCBOR (3 :: Word8) <> encodeAfterFork err


instance
  ( ApplyTx blk1
  , ApplyTx blk2
  , CanHardFork (BlockProtocol blk1) (BlockProtocol blk2)
  , CanHardForkBlock blk1 blk2
  )
  => ApplyTx (Forked blk1 blk2) where

  newtype GenTx (Forked blk1 blk2)
    = ForkedGenTx
        { unForkedGenTx :: Forked (GenTx blk1) (GenTx blk2)
        }

  newtype GenTxId (Forked blk1 blk2)
    = ForkedGenTxId
        { unForkedGenTxId :: Forked (GenTxId blk1) (GenTxId blk2)
        }

  txId = ForkedGenTxId . bimap txId txId . unForkedGenTx

  txSize = forked txSize txSize . unForkedGenTx

  type ApplyTxErr (Forked blk1 blk2) = ForkedApplyTxErr blk1 blk2

  applyTx ledgerConfig =
    applyToForkedLedgerState
      applyTx
      BeforeForkApplyTxErr
      applyTx
      AfterForkApplyTxErr
      NewTransactionBeforeFork
      OldTransactionAfterFork
      ledgerConfig .
      unForkedGenTx

  reapplyTx ledgerConfig =
    applyToForkedLedgerState
      reapplyTx
      BeforeForkApplyTxErr
      reapplyTx
      AfterForkApplyTxErr
      NewTransactionBeforeFork
      OldTransactionAfterFork
      ledgerConfig .
      unForkedGenTx

  reapplyTxSameState ledgerConfig =
      reapplyToSameForkedLedgerState
        reapplyTxSameState
        reapplyTxSameState
        "reapplyTxSameState: impossible! NewTransactionBeforeFork"
        "reapplyTxSameState: impossible! OldTransactionAfterFork"
        ledgerConfig
    . unForkedGenTx

deriving instance
  (Show (GenTx blk1), Show (GenTx blk2)) => Show (GenTx (Forked blk1 blk2))

deriving instance
  (Eq (GenTxId blk1), Eq (GenTxId blk2)) => Eq (GenTxId (Forked blk1 blk2))

deriving instance
  (Ord (GenTxId blk1), Ord (GenTxId blk2)) => Ord (GenTxId (Forked blk1 blk2))

{-------------------------------------------------------------------------------
  ForkedLedgerState Combinators
-------------------------------------------------------------------------------}

applyToForkedLedgerState
  :: (LedgerConfig blk1 -> a -> LedgerState blk1 -> Except beforeError (LedgerState blk1))
  -- ^ Function to apply before the fork
  -> (beforeError -> error)
  -- ^ Wrapper for errors before the fork
  -> (LedgerConfig blk2 -> b -> LedgerState blk2 -> Except afterError (LedgerState blk2))
  -- ^ Function to apply after the fork
  -> (afterError -> error)
  -- ^ Wrapper for errors after the fork
  -> error
  -- ^ Error for new thing before fork
  -> error
  -- ^ Error for old thing after fork
  -> LedgerConfig (Forked blk1 blk2)
  -> Forked a b
  -> LedgerState (Forked blk1 blk2)
  -> Except error (LedgerState (Forked blk1 blk2))
applyToForkedLedgerState
  applyBeforeFork
  beforeWrapper
  applyAfterFork
  afterWrapper
  newBeforeForkError
  oldAfterForkError
  ledgerConfig
  forkedToApply
  forkedLedgerState =
    case (forkedToApply, unForkedLedgerState forkedLedgerState) of
      -- Before the fork, act as ledger for @blk1@
      (BeforeFork toApply, BeforeFork ledgerState) ->
        withExcept beforeWrapper . fmap (ForkedLedgerState . BeforeFork) $
          applyBeforeFork
            (ledgerConfigBeforeFork ledgerConfig)
            toApply
            ledgerState
      -- After the fork, act as ledger for @blk2@
      (AfterFork toApply, AfterFork ledgerState) ->
        withExcept afterWrapper . fmap (ForkedLedgerState . AfterFork) $
          applyAfterFork
            (ledgerConfigAfterFork ledgerConfig)
            toApply
            ledgerState
      -- Throw error when receiving a new thing before the fork
      (AfterFork _, BeforeFork _) -> throwError newBeforeForkError
      -- Throw error when receiving an old thing after the fork
      (BeforeFork _, AfterFork _) -> throwError oldAfterForkError

reapplyToSameForkedLedgerState
  :: (LedgerConfig blk1 -> a -> LedgerState blk1 -> LedgerState blk1)
  -- ^ Wrapper for errors before the fork
  -> (LedgerConfig blk2 -> b -> LedgerState blk2 -> LedgerState blk2)
  -- ^ Wrapper for errors after the fork
  -> String
  -- ^ Error for new thing before fork
  -> String
  -- ^ Error for old thing after fork
  -> LedgerConfig (Forked blk1 blk2)
  -> Forked a b
  -> LedgerState (Forked blk1 blk2)
  -> LedgerState (Forked blk1 blk2)
reapplyToSameForkedLedgerState
  applyBeforeFork
  applyAfterFork
  errorNewBefore
  errorOldAfter
  ledgerConfig
  forkedToApply
  forkedLedgerState =
    case (forkedToApply, unForkedLedgerState forkedLedgerState) of
      (BeforeFork toApply, BeforeFork ledgerState) ->
        ForkedLedgerState . BeforeFork $
          applyBeforeFork
            (ledgerConfigBeforeFork ledgerConfig)
            toApply
            ledgerState
      (AfterFork toApply, AfterFork ledgerState) ->
        ForkedLedgerState . AfterFork $
          applyAfterFork
            (ledgerConfigAfterFork ledgerConfig)
            toApply
            ledgerState
      (BeforeFork _, AfterFork _) -> error errorNewBefore
      (AfterFork _, BeforeFork _) -> error errorOldAfter

{-------------------------------------------------------------------------------
  Simple Hard Fork Instance
-------------------------------------------------------------------------------}

instance
  CanHardForkBlock
    (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
    (SimplePraosBlock SimpleMockCrypto PraosMockCrypto) where

  -- Currently hard forking at slot 0 as we cannot fork yet
  hardForksAt _ = Just $ SlotNo 0

  -- This is safe as we know they're both 'MockState'
  translateLedgerStateForHardFork = unsafeCoerce

  translateHeaderHash = unsafeCoerce
