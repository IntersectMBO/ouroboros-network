{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances requires for consensus/ledger integration
module Ouroboros.Consensus.Byron.Ledger.Ledger (
    -- * Ledger integration
    LedgerConfig(..)
  , LedgerState(..)
  , Query(..)
  , initByronLedgerState
    -- * Serialisation
  , encodeByronLedgerState
  , decodeByronLedgerState
  , encodeByronQuery
  , decodeByronQuery
  , encodeByronResult
  , decodeByronResult
    -- * Auxiliary
  , validationErrorImpossible
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import           Data.Type.Equality ((:~:) (Refl))
import           GHC.Generics (Generic)

import           Cardano.Prelude (Natural, NoUnexpectedThunks)

import           Cardano.Binary (fromCBOR, toCBOR)
import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Genesis as Gen
import qualified Cardano.Chain.Update as CC
import qualified Cardano.Chain.Update.Validation.Interface as UPI
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Chain.ValidationMode as CC

import           Ouroboros.Network.Block (Point (..), SlotNo (..), blockSlot)
import           Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.LedgerDerivedInfo
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Config
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Ledger.DelegationHistory
                     (DelegationHistory)
import qualified Ouroboros.Consensus.Byron.Ledger.DelegationHistory as History
import           Ouroboros.Consensus.Byron.Ledger.HeaderValidation ()
import           Ouroboros.Consensus.Byron.Ledger.PBFT

instance UpdateLedger ByronBlock where

  data LedgerState ByronBlock = ByronLedgerState {
        byronLedgerState       :: !CC.ChainValidationState
      , byronDelegationHistory :: !DelegationHistory
      }
    deriving (Eq, Show, Generic, NoUnexpectedThunks)

  type LedgerError ByronBlock = CC.ChainValidationError

  newtype LedgerConfig ByronBlock = ByronLedgerConfig {
        unByronLedgerConfig :: Gen.Config
      }
    deriving (Generic, NoUnexpectedThunks)

  applyChainTick cfg slotNo ByronLedgerState{..} =
      TickedLedgerState slotNo ByronLedgerState {
          byronDelegationHistory = byronDelegationHistory
        , byronLedgerState       = CC.applyChainTick
                                      (unByronLedgerConfig cfg)
                                      (toByronSlotNo slotNo)
                                      byronLedgerState
        }

  applyLedgerBlock = applyByronBlock validationMode
    where
      validationMode = CC.fromBlockValidationMode CC.BlockValidation

  reapplyLedgerBlock cfg blk st =
      validationErrorImpossible $
        applyByronBlock validationMode cfg blk st
    where
      validationMode = CC.fromBlockValidationMode CC.NoBlockValidation

  ledgerTipPoint (ByronLedgerState state _) =
      case CC.cvsPreviousHash state of
        -- In this case there are no blocks in the ledger state. The genesis
        -- block does not occupy a slot, so its point is Origin.
        Left _genHash -> Point Point.origin
        Right hdrHash -> Point (Point.block slot (ByronHash hdrHash))
          where
            slot = fromByronSlotNo (CC.cvsLastSlot state)

initByronLedgerState :: Gen.Config
                     -> Maybe CC.UTxO -- ^ Optionally override UTxO
                     -> LedgerState ByronBlock
initByronLedgerState genesis mUtxo = ByronLedgerState {
      byronLedgerState       = override mUtxo initState
    , byronDelegationHistory = History.empty
    }
  where
    initState :: CC.ChainValidationState
    Right initState = runExcept $ CC.initialChainValidationState genesis

    override :: Maybe CC.UTxO
             -> CC.ChainValidationState -> CC.ChainValidationState
    override Nothing     st = st
    override (Just utxo) st = st { CC.cvsUtxo = utxo }

instance QueryLedger ByronBlock where
  data Query ByronBlock :: * -> * where
    GetUpdateInterfaceState :: Query ByronBlock UPI.State

  answerQuery GetUpdateInterfaceState ledgerState =
    CC.cvsUpdateState (byronLedgerState ledgerState)

  eqQuery GetUpdateInterfaceState GetUpdateInterfaceState = Just Refl

deriving instance Eq (Query ByronBlock result)
deriving instance Show (Query ByronBlock result)

instance ShowQuery (Query ByronBlock) where
  showResult GetUpdateInterfaceState = show

instance LedgerSupportsProtocol ByronBlock where
  protocolLedgerView _cfg =
        toPBftLedgerView
      . CC.getDelegationMap
      . byronLedgerState

  -- Delegation state for a particular point in time
  --
  -- The situation looks something like this:
  --
  -- > (Label for reference)        0             1      2             3      4
  -- > -------------------------------------------------------------------------
  -- > Delegation changes           v             v      v             v      v
  -- > Snapshots            [......)[............)[.....)      NOW
  -- > Requested slot                      A                B       C      D
  --
  -- where NOW refers to the slot number of the last block we applied, and
  -- the requested slot must be within a @[-2k .. +2k)@ window around NOW.
  --
  -- Note that there can be no delegation changes between (2) and (NOW): if
  -- there were, we'd have another snapshot. Four possibilities:
  --
  -- A. We have a historical snapshot of the delegation state that contains the
  --    requested slot. If so, we just return that.
  -- B. The slot is in the past, but we have no snapshot. In this case, it must
  --    be that the current ledger state is valid, between points (2) and (3).
  -- C. The slot is in the future, but before the first scheduled change to the
  --    delegation state. Again, current ledger state is valid, also between
  --    points (2) and (3).
  -- D. The slot is in the future, but after the first scheduled update. We must
  --    apply that scheduled update; the resulting delegation state will be
  --    valid from the point of that first schedulded update (3) until the
  --    next (4).
  --
  -- We can collapse cases (B, C, D) into a single one as follows: split the
  -- scheduled delegations into the ones that should have been applied at the
  -- requested slot, and those that are still in the future. For the example
  -- above this amounts to
  --
  -- B. ([], (3, 4)]
  -- C. ([], (3, 4)]
  -- D. ([3], [4])
  --
  -- Then take the delegation state from in current ledger state, and apply the
  -- updates that should be applied. The resulting delegation state must be
  -- given the following validity bounds:
  --
  -- * The lower bound will be the slot number of the last update that was
  --   applied, or the upper bound of the last historical snapshot if no
  --   updates needed to be applied. If there are no historical snapshots,
  --   then the lower bound is genesis (the history is only empty if the
  --   delegation state never changed).
  -- * The upper bound will be the slot number of the first update that was
  --   not yet applied; if no such update is known, it will be set to the
  --   the maximum upper bound @(NOW + 2k)@.
  --
  -- TODO: verify that the sdSlot of ScheduledDelegation is the slot at which
  -- it becomes active (i.e., that delegation should be applied /in/ that slot)
  -- i.e., that delegate is allowed to issue a block in that very same slot.
  anachronisticProtocolLedgerView cfg (ByronLedgerState ls ss) slot =
      case History.find slot ss of
        Just sb -> Right (toPBftLedgerView sb) -- Case (A)
        Nothing -- Case (B), (C) or (D)
          | slot <  At maxLo -> Left TooFarBehind -- lower bound is inclusive
          | slot >= At maxHi -> Left TooFarAhead  -- upper bound is exclusive
          | otherwise        -> case slot of
              Origin -> Left TooFarBehind -- this should be unreachable
              At s -> Right $ toPBftLedgerView $
                CC.previewDelegationMap (toByronSlotNo s) ls
    where
      SecurityParam k = genesisSecurityParam (unByronLedgerConfig cfg)

      now, maxHi, maxLo :: SlotNo
      now   = fromByronSlotNo $ CC.cvsLastSlot ls
      maxLo = SlotNo $ if (2 * k) > unSlotNo now
                        then 0
                        else unSlotNo now - (2 * k)
      maxHi = SlotNo $ unSlotNo now + (2 * k)

instance LedgerDerivedInfo ByronBlock where
  knownSlotLengths =
        singletonSlotLengths
      . slotLengthFromMillisec
      . (fromIntegral :: Natural -> Integer)
      . CC.ppSlotDuration
      . Gen.configProtocolParameters
      . byronGenesisConfig

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Mark computation as validation error free
--
-- Given a 'BlockValidationMode' of 'NoBlockValidation', a call to
-- 'applyByronBlock' shouldn't fail since the ledger layer won't be performing
-- any block validation checks. However, because 'applyByronBlock' can fail in
-- the event it is given a 'BlockValidationMode' of 'BlockValidation', it still
-- /looks/ like it can fail (since its type doesn't change based on the
-- 'ValidationMode') and we must still treat it as such.
validationErrorImpossible :: forall err a. Except err a -> a
validationErrorImpossible = cantBeError . runExcept
  where
    cantBeError :: Either err a -> a
    cantBeError (Left  _) = error "validationErrorImpossible: unexpected error"
    cantBeError (Right a) = a

{-------------------------------------------------------------------------------
  Applying a block

  Most of the work here is done by the ledger layer. We just need to pass
  the right arguments, and maintain the snapshots.
-------------------------------------------------------------------------------}

applyByronBlock :: CC.ValidationMode
                -> LedgerConfig ByronBlock
                -> ByronBlock
                -> LedgerState ByronBlock
                -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
applyByronBlock validationMode
                fcfg@(ByronLedgerConfig cfg)
                fblk@(ByronBlock blk _ (ByronHash blkHash))
                ls = do
    let TickedLedgerState _slot ls' = applyChainTick fcfg (blockSlot fblk) ls
    case blk of
      CC.ABOBBlock    blk' -> applyABlock validationMode cfg blk' blkHash ls'
      CC.ABOBBoundary blk' -> applyABoundaryBlock        cfg blk'         ls'

applyABlock :: CC.ValidationMode
            -> Gen.Config
            -> CC.ABlock ByteString
            -> CC.HeaderHash
            -> LedgerState (ByronBlock)
            -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
applyABlock validationMode cfg blk blkHash ByronLedgerState{..} = do
    state' <- CC.validateBlock cfg validationMode blk blkHash byronLedgerState
    -- If the delegation state changed, take a snapshot of the old state
    let history'
          |    CC.cvsDelegationState state'
            == CC.cvsDelegationState byronLedgerState
                      = byronDelegationHistory
          | otherwise = History.snapOld
                          (Gen.configK cfg)
                          (fromByronSlotNo $ CC.blockSlot blk)
                          (CC.getDelegationMap byronLedgerState) -- the old state!
                          byronDelegationHistory
    return $ ByronLedgerState state' history'

-- | Apply boundary block
--
-- Since boundary blocks don't modify the delegation state, they also don't
-- modify the delegation history.
applyABoundaryBlock :: Gen.Config
                    -> CC.ABoundaryBlock ByteString
                    -> LedgerState ByronBlock
                    -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
applyABoundaryBlock cfg blk ByronLedgerState{..} = do
    current' <- CC.validateBoundary cfg blk byronLedgerState
    return $ ByronLedgerState current' byronDelegationHistory

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeByronLedgerState :: LedgerState ByronBlock -> Encoding
encodeByronLedgerState ByronLedgerState{..} = mconcat
    [ CBOR.encodeListLen 2
    , encode byronLedgerState
    , History.encodeDelegationHistory byronDelegationHistory
    ]

decodeByronLedgerState :: Decoder s (LedgerState ByronBlock)
decodeByronLedgerState = do
    CBOR.decodeListLenOf 2
    ByronLedgerState
      <$> decode
      <*> History.decodeDelegationHistory

encodeByronQuery :: Query ByronBlock result -> Encoding
encodeByronQuery query = case query of
    GetUpdateInterfaceState -> CBOR.encodeWord8 0

decodeByronQuery :: Decoder s (Some (Query ByronBlock))
decodeByronQuery = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> return $ Some GetUpdateInterfaceState
      _ -> fail $ "decodeByronQuery: invalid tag " <> show tag

encodeByronResult :: Query ByronBlock result -> result -> Encoding
encodeByronResult query = case query of
    GetUpdateInterfaceState -> toCBOR

decodeByronResult :: Query ByronBlock result
                  -> forall s. Decoder s result
decodeByronResult query = case query of
    GetUpdateInterfaceState -> fromCBOR
