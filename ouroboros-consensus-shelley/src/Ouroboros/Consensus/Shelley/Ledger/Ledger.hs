{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Ledger (
    ShelleyLedgerError (..)
  , LedgerConfig (..)
  , LedgerState (..)
  , UpdateLedger (..)
  , QueryLedger (..)
  , Query (..)
  , NonMyopicMemberRewards (..)
    -- * Auxiliary
  , getPParams
    -- * Serialisation
  , decodeShelleyLedgerState
  , encodeShelleyLedgerState
  , encodeShelleyQuery
  , decodeShelleyQuery
  , encodeShelleyResult
  , decodeShelleyResult
  , encodeShelleyExtLedgerState
  , encodeShelleyHeaderState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise, decode, encode)
import           Control.Monad.Except
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import           Data.Type.Equality ((:~:) (Refl))
import           GHC.Generics (Generic)

import           Cardano.Binary (enforceSize, fromCBOR, toCBOR)
import           Cardano.Prelude (Natural, NoUnexpectedThunks (..),
                     OnlyCheckIsWHNF (..))
import           Cardano.Slotting.Slot (EpochNo, WithOrigin (..),
                     fromWithOrigin)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.BlockchainTime (singletonSlotLengths)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.LedgerDerivedInfo
import           Ouroboros.Consensus.Util.Versioned

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.TxData as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Ledger.TPraos ()
import           Ouroboros.Consensus.Shelley.Protocol

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

data ShelleyLedgerError c
  = TickError  !(SL.TickTransitionError  c)
  | BBodyError !(SL.BlockTransitionError c)
    -- TODO the three exceptions below are duplicated from CHAIN
  | ObsoleteNode
      Natural  -- ^ Current major protocol version
      Natural  -- ^ Maximum major protocol version
  | HeaderSizeTooLarge
      Natural  -- ^ Size of the header
      Natural  -- ^ Maximum size of a header
  | BodySizeTooLarge
      Natural  -- ^ Size of the block body
      Natural  -- ^ Maximum size of a block body
  deriving (Eq, Generic, Show)

instance NoUnexpectedThunks (ShelleyLedgerError c)

instance TPraosCrypto c => UpdateLedger (ShelleyBlock c) where
  data LedgerState (ShelleyBlock c) = ShelleyLedgerState {
        ledgerTip    :: !(Point (ShelleyBlock c))
      , history      :: !(History.LedgerViewHistory c)
      , shelleyState :: !(SL.ShelleyState c)
      }
    deriving (Eq, Show, Generic, NoUnexpectedThunks)

  type LedgerError (ShelleyBlock c) = ShelleyLedgerError c

  newtype LedgerConfig (ShelleyBlock c) = ShelleyLedgerConfig SL.Globals
    deriving (Generic)
    deriving NoUnexpectedThunks via OnlyCheckIsWHNF "LedgerConfig ShelleyBlock" SL.Globals
    -- TODO normal form

  applyChainTick
    (ShelleyLedgerConfig globals)
    slotNo
    (ShelleyLedgerState pt history bhState) =
      TickedLedgerState slotNo
        . ShelleyLedgerState pt history
        $ SL.applyTickTransition globals bhState slotNo

  applyLedgerBlock (ShelleyLedgerConfig globals) blk tickedLedger = do
      let TickedLedgerState _ oldSt = tickedLedger
          ShelleyLedgerState _ history oldShelleyState = oldSt
          SL.Globals { maxMajorPV } = globals
          pparams = getPParams oldShelleyState
          SL.ProtVer majorPV _ = SL._protocolVersion pparams
      -- Do the checks part of the CHAIN transition that are not part of TICK,
      -- PRTCL, or BBODY.
      --
      -- TODO move to a separate function in cardano-ledger-specs that can be
      -- used in @chainTransition@ and here.
      majorPV <= maxMajorPV ?! ObsoleteNode majorPV maxMajorPV

      let maxHeaderSize = SL._maxBHSize pparams
          maxBodySize   = SL._maxBBSize pparams
          header        = shelleyHeaderRaw $ getHeader blk
          headerBody    = SL.bhbody header
          headerSize    = fromIntegral $ SL.bHeaderSize header
          -- Note: the header (body) contains the size of the block body
          bodySize      = SL.hBbsize headerBody

      headerSize <= maxHeaderSize ?! HeaderSizeTooLarge headerSize maxHeaderSize
      bodySize   <= maxBodySize   ?! BodySizeTooLarge   bodySize   maxBodySize

      -- Note: we don't apply the PRTCL transition, we do that in
      -- 'updateConsensusState' (called in 'validateHeader') together with
      -- this function in 'applyExtLedgerState'.

      -- Apply the BBODY transition using the ticked state
      newShelleyState <- withExcept BBodyError $
        SL.applyBlockTransition globals oldShelleyState (shelleyBlockRaw blk)

      let history'
            -- TODO how expensive is this check?
            | SL.currentLedgerView oldShelleyState ==
              SL.currentLedgerView newShelleyState
            = history
            | otherwise
            = History.snapOld
                (SL.securityParameter globals)
                (blockSlot blk)
                (SL.currentLedgerView oldShelleyState)
                history

      return ShelleyLedgerState {
          ledgerTip    = blockPoint blk
        , history      = history'
        , shelleyState = newShelleyState
        }

  -- TODO actual reapplication:
  -- https://github.com/input-output-hk/cardano-ledger-specs/issues/1303
  reapplyLedgerBlock cfg blk ledgerState =
    case runExcept (applyLedgerBlock cfg blk ledgerState) of
      Right ledgerState' -> ledgerState'
      Left  err          -> error $
        "Reapplication of Shelley ledger block failed: " <> show err

  ledgerTipPoint = ledgerTip

instance TPraosCrypto c => LedgerSupportsProtocol (ShelleyBlock c) where
  protocolLedgerView _cfg = SL.currentLedgerView . shelleyState

  anachronisticProtocolLedgerView_ cfg ledgerState slot =
      case History.find slot history of
        Just lv -> return lv
        Nothing
          | slot <  At maxLo
          -> throwError TooFarBehind -- lower bound is inclusive
          | slot >= At maxHi
          -> throwError TooFarAhead  -- upper bound is exclusive
          | otherwise
          -> -- If 'futureLedgerView' fails, it is a bug
             return $ either (error "futureLedgerView failed") id $
             SL.futureLedgerView globals shelleyState forSlot
    where
      ShelleyLedgerState { ledgerTip , history , shelleyState } =
        ledgerState

      ShelleyLedgerConfig globals = cfg

      k = SL.securityParameter globals

      now, maxHi, maxLo :: SlotNo
      maxLo = SlotNo $ if (2 * k) > unSlotNo now
                        then 0
                        else unSlotNo now - (2 * k)
      maxHi = SlotNo $ unSlotNo now + (2 * k)
      -- The slot of the last block last applied to the ledger, /now/
      -- according to the ledger.
      now   = fromWithOrigin
        (minimumPossibleSlotNo (Proxy @(ShelleyBlock c)))
        (pointSlot ledgerTip)

      -- The slot for which we want to get a 'ProtocolLedgerView'
      forSlot :: SlotNo
      forSlot = fromWithOrigin
        (minimumPossibleSlotNo (Proxy @(ShelleyBlock c)))
        slot

instance HasHardForkHistory (ShelleyBlock c) where
  type HardForkIndices (ShelleyBlock c) = '[()]

  hardForkShape cfg = HardFork.singletonShape $ shelleyEraParams cfg

  -- TODO: This is wrong. We should look at the state of the ledger.
  -- Once we actually start using the hard fork combinator, we should fix this.
  -- <https://github.com/input-output-hk/ouroboros-network/issues/1786>
  hardForkTransitions _ _ = HardFork.transitionsUnknown

instance LedgerDerivedInfo (ShelleyBlock c) where
  knownSlotLengths =
    singletonSlotLengths . HardFork.eraSlotLength . shelleyEraParams

{-------------------------------------------------------------------------------
  QueryLedger
-------------------------------------------------------------------------------}

newtype NonMyopicMemberRewards c = NonMyopicMemberRewards {
      unNonMyopicMemberRewards :: Map (SL.Credential c)
                                      (Map (SL.KeyHash c) SL.Coin)
    }
  deriving stock   (Show)
  deriving newtype (Eq)

instance Crypto c => Serialise (NonMyopicMemberRewards c) where
  encode = toCBOR . unNonMyopicMemberRewards
  decode = NonMyopicMemberRewards <$> fromCBOR

instance TPraosCrypto c => QueryLedger (ShelleyBlock c) where
  data Query (ShelleyBlock c) :: Type -> Type where
    GetLedgerTip :: Query (ShelleyBlock c) (Point (ShelleyBlock c))
    GetEpochNo :: Query (ShelleyBlock c) EpochNo
    -- | Calculate the Non-Myopic Pool Member Rewards for a set of
    -- credentials. See 'SL.getNonMyopicMemberRewards'
    GetNonMyopicMemberRewards
      :: Set (SL.Credential c)
      -> Query (ShelleyBlock c) (NonMyopicMemberRewards c)
    GetCurrentPParams
      :: Query (ShelleyBlock c) SL.PParams
    GetProposedPParamsUpdates
      :: Query (ShelleyBlock c) (SL.ProposedPPUpdates c)
    GetStakeDistribution
      :: Query (ShelleyBlock c) (SL.PoolDistr c)

  answerQuery (ShelleyLedgerConfig globals) query st = case query of
    GetLedgerTip -> ledgerTip st
    GetEpochNo -> SL.nesEL $ shelleyState st
    GetNonMyopicMemberRewards creds -> NonMyopicMemberRewards $
        SL.getNonMyopicMemberRewards globals (shelleyState st) creds
    GetCurrentPParams -> getPParams $ shelleyState st
    GetProposedPParamsUpdates -> getProposedPPUpdates $ shelleyState st
    GetStakeDistribution -> SL.nesPd $ shelleyState st

  eqQuery GetLedgerTip GetLedgerTip
    = Just Refl
  eqQuery GetLedgerTip _
    = Nothing
  eqQuery GetEpochNo GetEpochNo
    = Just Refl
  eqQuery GetEpochNo _
    = Nothing
  eqQuery (GetNonMyopicMemberRewards creds) (GetNonMyopicMemberRewards creds')
    | creds == creds'
    = Just Refl
    | otherwise
    = Nothing
  eqQuery (GetNonMyopicMemberRewards _) _
    = Nothing
  eqQuery GetCurrentPParams GetCurrentPParams
    = Just Refl
  eqQuery GetCurrentPParams _
    = Nothing
  eqQuery GetProposedPParamsUpdates GetProposedPParamsUpdates
    = Just Refl
  eqQuery GetProposedPParamsUpdates _
    = Nothing
  eqQuery GetStakeDistribution GetStakeDistribution
    = Just Refl
  eqQuery GetStakeDistribution _
    = Nothing

deriving instance Eq   (Query (ShelleyBlock c) result)
deriving instance Show (Query (ShelleyBlock c) result)

instance Crypto c => ShowQuery (Query (ShelleyBlock c)) where
  showResult GetLedgerTip                   = show
  showResult GetEpochNo                     = show
  showResult (GetNonMyopicMemberRewards {}) = show
  showResult GetCurrentPParams              = show
  showResult GetProposedPParamsUpdates      = show
  showResult GetStakeDistribution           = show

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getPParams :: SL.ShelleyState c -> SL.PParams
getPParams = SL.esPp . SL.nesEs

getProposedPPUpdates :: SL.ShelleyState c -> SL.ProposedPPUpdates c
getProposedPPUpdates = SL._ppups . SL._utxoState . SL.esLState . SL.nesEs

-- | The \"oh noes?!\" operator.
(?!) :: MonadError e m => Bool -> e -> m ()
True  ?! _ = return ()
False ?! e = throwError e
infix 1 ?!

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

serialisationFormatVersion0 :: VersionNumber
serialisationFormatVersion0 = 0

encodeShelleyExtLedgerState :: Crypto c
                            => ExtLedgerState (ShelleyBlock c)
                            -> Encoding
encodeShelleyExtLedgerState = encodeExtLedgerState
    encodeShelleyLedgerState
    toCBOR
    toCBOR
    toCBOR

encodeShelleyHeaderState :: Crypto c
                         => HeaderState (ShelleyBlock c)
                         -> Encoding
encodeShelleyHeaderState = encodeHeaderState
    toCBOR
    toCBOR
    toCBOR

encodeShelleyLedgerState :: Crypto c => LedgerState (ShelleyBlock c) -> Encoding
encodeShelleyLedgerState
    ShelleyLedgerState { ledgerTip, history, shelleyState } =
    encodeVersion serialisationFormatVersion0 $ mconcat
      [ CBOR.encodeListLen 3
      , encode ledgerTip
      , History.encodeLedgerViewHistory history
      , toCBOR shelleyState
      ]

decodeShelleyLedgerState :: Crypto c => Decoder r (LedgerState (ShelleyBlock c))
decodeShelleyLedgerState = decodeVersion
    [(serialisationFormatVersion0, Decode decodeShelleyLedgerState0)]
  where
    decodeShelleyLedgerState0 = do
      enforceSize "LedgerState ShelleyBlock" 3
      ShelleyLedgerState
        <$> decode
        <*> History.decodeLedgerViewHistory
        <*> fromCBOR

encodeShelleyQuery :: Crypto c => Query (ShelleyBlock c) result -> Encoding
encodeShelleyQuery query = case query of
    GetLedgerTip ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 0
    GetEpochNo ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 1
    GetNonMyopicMemberRewards creds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 2 <> toCBOR creds
    GetCurrentPParams ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 3
    GetProposedPParamsUpdates ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 4
    GetStakeDistribution ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 5

decodeShelleyQuery :: Crypto c => Decoder s (Some (Query (ShelleyBlock c)))
decodeShelleyQuery = do
    len <- CBOR.decodeListLen
    tag <- CBOR.decodeWord8
    case (len, tag) of
      (1, 0) -> return $ Some GetLedgerTip
      (1, 1) -> return $ Some GetEpochNo
      (2, 2) -> Some . GetNonMyopicMemberRewards <$> fromCBOR
      (1, 3) -> return $ Some GetCurrentPParams
      (1, 4) -> return $ Some GetProposedPParamsUpdates
      (1, 5) -> return $ Some GetStakeDistribution
      _      -> fail $
        "decodeShelleyQuery: invalid (len, tag): (" <>
        show len <> ", " <> show tag <> ")"

encodeShelleyResult
  :: Crypto c
  => Query (ShelleyBlock c) result -> result -> Encoding
encodeShelleyResult query = case query of
    GetLedgerTip                 -> encodePoint encode
    GetEpochNo                   -> encode
    GetNonMyopicMemberRewards {} -> encode
    GetCurrentPParams            -> toCBOR
    GetProposedPParamsUpdates    -> toCBOR
    GetStakeDistribution         -> toCBOR

decodeShelleyResult
  :: Crypto c
  => Query (ShelleyBlock c) result
  -> forall s. Decoder s result
decodeShelleyResult query = case query of
    GetLedgerTip                 -> decodePoint decode
    GetEpochNo                   -> decode
    GetNonMyopicMemberRewards {} -> decode
    GetCurrentPParams            -> fromCBOR
    GetProposedPParamsUpdates    -> fromCBOR
    GetStakeDistribution         -> fromCBOR
