{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Ouroboros.Consensus.HardFork.History.EraParams (
    -- * API
    EraParams (..)
  , SafeZone (..)
    -- * Defaults
  , defaultEraParams
  ) where

import           Codec.CBOR.Decoding (Decoder, decodeListLen, decodeWord8)
import           Codec.CBOR.Encoding (Encoding, encodeListLen, encodeWord8)
import           Codec.Serialise (Serialise (..))
import           Control.Monad (void)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Config.SecurityParam

{-------------------------------------------------------------------------------
  OVERVIEW

  The overall chain consists of various /era/s. Each era has its own set of era
  parameters such as the slot length and epoch size, as well as its own block
  format, ledger format, ledger rules, etc. It is assumed that the overall
  /shape/ of the chain is known. In other words, we statically know which eras
  we expect and what their parameters are; adding an additional era would
  require a code update. What we /don't/ know precisely is /when/ we transition
  from one era to the next, i.e., the hard fork transition points.

  When we are at genesis, the chain therefore looks something like this:

  > *-------------------?--------------------?--------------------
  > ^
  > \-- tip

  where we have (in this example) three eras (say, Byron, Shelley and Goguen)
  and therefore two hard fork transitions. Hard forks happen at epoch
  boundaries; the exact 'EpochNo' of each hard fork is determined by the era
  preceding it. Naturally, the exact 'EpochNo' of /past/ hard forks is known:

  > ---------------A--------------*----------?--------------------
  >                               ^
  >                               \-- tip

  where A is a known hard fork transition, and the next hard fork transition
  is still unknown.

  SAFE ZONES

  Future hard fork points may be known or unknown, where "known" means
  "certain"; i.e., for Byron, it would mean an update proposal has been voted
  on, confirmed, endorsed, and that endorsement is at least @k@ blocks deep into
  the chain; for Shelley it means an update proposal is voted on and accepted,
  and that acceptance is at least @k@ blocks deep into the chain.

  When a hard fork point is still unknown, we assume that each era determines a
  "safe zone": a number of slots from the tip of the ledger in which it is
  guaranteed that the hard fork will not happen.

  > CASE (i)
  >
  > ---------------A--------------*----------?--------------------
  >                               \..../
  >                                safe
  >                                zone

  Since the hard fork will not happen in the safe zone, we can extend the use of
  the current set of era parameters past the tip into the safe zone, giving us a
  limited ability to make predictions for the future (such as converting between
  slot numbers and wallclock time).

  We assume that once a transition point is known (and no longer subject to
  roll-back), this is guaranteed not to change anymore and we can use the era's
  parameters up to the transition point:

  > CASE (ii)
  >
  > ---------------A--------------*----------------B--------------
  >                               \.............../
  >                                implicitly safe

  Moreover, we assume that we can extend B's safe zone from the point of the
  hard fork transition:

  > CASE (iii)
  >
  > ---------------A--------------*----------------B--------------
  >                               \.............../\..../
  >                                implicitly safe  safe
  >                                                 zone

  This is justified because the safe zones arise from stability requirements
  for the transactions that establish the transition point. The earliest point
  such a transaction could be included in the chain is after the hard fork
  transition, since it must be a transaction from the /new/ era.

  NOTE ON STABILITY

  If we used as yet /unconfirmed/ update proposals to determine hard fork
  transition points, then any of the resulting time conversions would be
  subject to rollback; if we switched to a different fork, time conversions
  might suddenly look different. Whilst this /may/ be doable, in practice this
  is a headache we would very much like to avoid. For example, it might mean
  that when a block comes in and we determine that it's from the future,
  we might have prematurely marked it as invalid. So, we insist that time
  conversions must be based on update propsals that are /certain/ (no longer
  subject to rollback). This means that the "safe zone" we have been discussing
  above must extend from the point of stability forward. Moreover, the safe zone
  must be long enough to include a sufficient number of blocks such that we can
  evaluate enough headers of an alternative fork (without having its blocks)
  to decide that we want to switch to that fork; since in the worst case that
  means we have to evaluate @k@ headers (or @k+1@), the safe zone must be long
  enough to cover @k@ blocks (and therefore a safe zone of @2k@ slots for Byron,
  and (probably) a safe zone of @3k/f@ slots for Shelley). Effectively, this
  means that consensus wants "stability itself to be stable"; we need a double
  safe zone after an update proposal has been confirmed.
-------------------------------------------------------------------------------}

-- | Parameters that can vary across hard forks
data EraParams = EraParams {
      eraEpochSize  :: !EpochSize
    , eraSlotLength :: !SlotLength
    , eraSafeZone   :: !SafeZone
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoThunks)

-- | Default 'EraParams'
--
-- We set
--
-- * epoch size to @10k@ slots
-- * the safe zone to @2k@ slots
-- * the upper bound to 'NoLowerBound'
--
-- This is primarily useful for tests.
defaultEraParams :: SecurityParam -> SlotLength -> EraParams
defaultEraParams (SecurityParam k) slotLength = EraParams {
      eraEpochSize  = EpochSize (k * 10)
    , eraSlotLength = slotLength
    , eraSafeZone   = StandardSafeZone (k * 2)
    }

-- | Zone in which it is guaranteed that no hard fork can take place
data SafeZone =
    -- | Standard safe zone
    --
    -- We record
    --
    -- * Number of slots from the tip of the ledger.
    --   This should be (at least) the number of slots in which we are
    --   guaranteed to have @k@ blocks.
    -- * Optionally, an 'EpochNo' before which no hard fork can take place.
    StandardSafeZone !Word64

    -- | Pretend the transition to the next era will not take place.
    --
    -- This constructor is marked as unsafe because it effectively extends
    -- the safe zone of this era indefinitely into the future. This means that
    -- we might reach invalid conclusions when doing
    --
    -- * slot to time conversions for blocks that are past the actual safe zone
    -- * time to slot conversions for the current time, when behind in syncing
    --
    -- This is safe when the code is simply not yet ready to transition to the
    -- next era, because in that case, we can be sure that blocks that come in
    -- are still from this era. It also means that we can always /produce/ a
    -- block, no matter how far ahead of the current ledger we are.
    --
    -- If the code is ready for the transition, just awaiting an update
    -- proposal, then 'LowerBound' can be used instead.
    --
    -- This constructor can be regarded as an " extreme " version of
    -- 'LowerBound', and can be used for similar reasons.
  | UnsafeIndefiniteSafeZone
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoThunks)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise SafeZone where
  encode = \case
      StandardSafeZone safeFromTip -> mconcat [
          encodeListLen 3
        , encodeWord8 0
        , encode safeFromTip
          -- For backward compatibility we still encode safeBeforeEpoch
        , encodeSafeBeforeEpoch
        ]
      UnsafeIndefiniteSafeZone -> mconcat [
          encodeListLen 1
        , encodeWord8 1
        ]
  decode = do
    size <- decodeListLen
    tag  <- decodeWord8
    case (size, tag) of
      (3, 0) -> StandardSafeZone <$> decode <* decodeSafeBeforeEpoch
      (1, 1) -> return UnsafeIndefiniteSafeZone
      _      -> fail $ "SafeZone: invalid size and tag " <> show (size, tag)

-- | Artificial encoder for backward compatibility, see #2646.
encodeSafeBeforeEpoch :: Encoding
encodeSafeBeforeEpoch = encodeListLen 1 <> encodeWord8 0

-- | Artificial decoder for backward compatibility, see #2646.
decodeSafeBeforeEpoch :: Decoder s ()
decodeSafeBeforeEpoch = do
    size <- decodeListLen
    tag  <- decodeWord8
    case (size, tag) of
      (1, 0) -> return ()
      (2, 1) -> void $ decode @EpochNo
      _      -> fail $ "SafeBeforeEpoch: invalid size and tag " <> show (size, tag)

instance Serialise EraParams where
  encode = toCBOR
  decode = fromCBOR

instance ToCBOR EraParams where
  toCBOR EraParams{..} = mconcat [
        encodeListLen 3
      , encode (unEpochSize eraEpochSize)
      , encode eraSlotLength
      , encode eraSafeZone
      ]

instance FromCBOR EraParams where
  fromCBOR = do
      enforceSize "EraParams" 3
      eraEpochSize  <- EpochSize <$> decode
      eraSlotLength <- decode
      eraSafeZone   <- decode
      return EraParams{..}
