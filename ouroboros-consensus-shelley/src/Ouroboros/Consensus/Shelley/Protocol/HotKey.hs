{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Hot key
--
-- Intended for qualified import
module Ouroboros.Consensus.Shelley.Protocol.HotKey (
    -- * KES Info
    KESEvolution
  , KESInfo (..)
  , kesAbsolutePeriod
    -- * KES Status
  , KESStatus (..)
  , kesStatus
    -- * Hot Key
  , HotKey (..)
  , KESEvolutionError (..)
  , KESEvolutionInfo
  , mkHotKey
  , sign
  ) where

import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import qualified Cardano.Crypto.KES as Relative (Period)

import           Ouroboros.Consensus.Block (UpdateInfo (..))
import           Ouroboros.Consensus.Util.IOLike

import           Cardano.Ledger.Crypto (Crypto)
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as Absolute (KESPeriod (..))

{-------------------------------------------------------------------------------
  KES Info
-------------------------------------------------------------------------------}

-- | We call the relative periods that a KES key is valid its evolution, to
-- avoid confusion with absolute periods.
type KESEvolution = Relative.Period

data KESInfo = KESInfo {
      kesStartPeriod :: !Absolute.KESPeriod
   ,  kesEndPeriod   :: !Absolute.KESPeriod
      -- ^ Currently derived from 'TPraosParams':
      -- > kesEndPeriod = kesStartPeriod + tpraosMaxKESEvo
    , kesEvolution   :: !KESEvolution
      -- ^ Current evolution or /relative period/.
      --
      -- Invariant:
      -- > kesStartPeriod + kesEvolution in [kesStartPeriod, kesEndPeriod)
    }
  deriving (Show, Generic, NoThunks)

-- | Return the absolute KES period
kesAbsolutePeriod :: KESInfo -> Absolute.KESPeriod
kesAbsolutePeriod KESInfo { kesStartPeriod, kesEvolution } =
    Absolute.KESPeriod $ start + kesEvolution
  where
    Absolute.KESPeriod start = kesStartPeriod

{-------------------------------------------------------------------------------
  KES Status
-------------------------------------------------------------------------------}

data KESStatus =
    -- | The given period is before the start period of the KES key.
    BeforeKESStart
      Absolute.KESPeriod  -- ^ Given period
      Absolute.KESPeriod  -- ^ Start period of the KES key

    -- | The given period is in the range of the KES key.
  | InKESRange
      KESEvolution  -- ^ Relative period or evolution corresponding to the
                    -- given absolute period

    -- | The given period is after the end period of the KES key.
  | AfterKESEnd
      Absolute.KESPeriod  -- ^ Given period
      Absolute.KESPeriod  -- ^ End period of the KES key

-- | Return the evolution of the given KES period, /when/ it falls within the
-- range of the 'HotKey' (@[hkStart, hkEnd)@).
--
-- Note that the upper bound is exclusive, the spec says:
-- > c0 <= kesPeriod s < c0 + MaxKESEvo
kesStatus :: KESInfo -> Absolute.KESPeriod -> KESStatus
kesStatus KESInfo { kesStartPeriod = lo'@(Absolute.KESPeriod lo)
                  , kesEndPeriod   = hi'@(Absolute.KESPeriod hi)
                  }
          cur'@(Absolute.KESPeriod cur)
    | cur <  lo = BeforeKESStart cur' lo'
    | cur >= hi = AfterKESEnd cur' hi'
    | otherwise = InKESRange (cur - lo)

{-------------------------------------------------------------------------------
  Hot Key
-------------------------------------------------------------------------------}

-- | Failed to evolve the KES key.
data KESEvolutionError =
    -- | The KES key could not be evolved to the target period.
    KESCouldNotEvolve
      KESInfo
      Absolute.KESPeriod
        -- ^ Target period outside the range of the current KES key. Typically
        -- the current KES period according to the wallclock slot.

    -- | The KES key was already poisoned.
  | KESKeyAlreadyPoisoned
      KESInfo
      Absolute.KESPeriod
        -- ^ Target period outside the range of the current KES key. Typically
        -- the current KES period according to the wallclock slot.
  deriving (Show)

-- | Result of evolving the KES key.
type KESEvolutionInfo = UpdateInfo KESInfo KESEvolutionError

-- | API to interact with the key.
data HotKey c m = HotKey {
      -- | Evolve the KES signing key to the given absolute KES period.
      --
      -- When the key cannot evolve anymore, we poison it.
      evolve     :: Absolute.KESPeriod -> m KESEvolutionInfo
      -- | Return 'KESInfo' of the signing key.
    , getInfo    :: m KESInfo
      -- | Return 'True' when the signing key is poisoned because it expired.
    , isPoisoned :: m Bool
      -- | Sign the given @toSign@ with the current signing key.
      --
      -- PRECONDITION: the key is not poisoned.
      --
      -- POSTCONDITION: the signature is in normal form.
    , sign_      :: forall toSign. (SL.KESignable c toSign, HasCallStack)
                 => toSign -> m (SL.SignedKES c toSign)
    }

sign ::
     (SL.KESignable c toSign, HasCallStack)
  => HotKey c m
  -> toSign -> m (SL.SignedKES c toSign)
sign = sign_

-- | The actual KES key, unless it expired, in which case it is replaced by
-- \"poison\".
data KESKey c =
    KESKey !(SL.SignKeyKES c)
  | KESKeyPoisoned
  deriving (Generic)

instance Crypto c => NoThunks (KESKey c)

kesKeyIsPoisoned :: KESKey c -> Bool
kesKeyIsPoisoned KESKeyPoisoned = True
kesKeyIsPoisoned (KESKey _)     = False

data KESState c = KESState {
      kesStateInfo :: !KESInfo
    , kesStateKey  :: !(KESKey c)
    }
  deriving (Generic)

instance Crypto c => NoThunks (KESState c)

mkHotKey ::
     forall m c. (Crypto c, IOLike m)
  => SL.SignKeyKES c
  -> Absolute.KESPeriod  -- ^ Start period
  -> Word64              -- ^ Max KES evolutions
  -> m (HotKey c m)
mkHotKey initKey startPeriod@(Absolute.KESPeriod start) maxKESEvolutions = do
    varKESState <- newMVar initKESState
    return HotKey {
        evolve     = evolveKey varKESState
      , getInfo    = kesStateInfo <$> readMVar varKESState
      , isPoisoned = kesKeyIsPoisoned . kesStateKey <$> readMVar varKESState
      , sign_      = \toSign -> do
          KESState { kesStateInfo, kesStateKey } <- readMVar varKESState
          case kesStateKey of
            KESKeyPoisoned -> error "trying to sign with a poisoned key"
            KESKey key     -> do
              let evolution = kesEvolution kesStateInfo
                  signed    = SL.signedKES () evolution toSign key
              -- Force the signature to WHNF (for 'SignedKES', WHNF implies
              -- NF) so that we don't have any thunks holding on to a key that
              -- might be destructively updated when evolved.
              evaluate signed
      }
  where
    initKESState :: KESState c
    initKESState = KESState {
        kesStateInfo = KESInfo {
            kesStartPeriod = startPeriod
          , kesEndPeriod   = Absolute.KESPeriod (start + fromIntegral maxKESEvolutions)
            -- We always start from 0 as the key hasn't evolved yet.
          , kesEvolution   = 0
          }
      , kesStateKey = KESKey initKey
      }

-- | Evolve the 'HotKey' so that its evolution matches the given KES period.
--
-- When the given KES period is after the end period of the 'HotKey', we
-- poison the key and return 'UpdateFailed'.
--
-- When the given KES period is before the start period of the 'HotKey' or
-- when the given period is before the key's period, we don't evolve the key
-- and return 'Updated'.
--
-- When the given KES period is within the range of the 'HotKey' and the given
-- period is after the key's period, we evolve the key and return 'Updated'.
--
-- When the key is poisoned, we always return 'UpdateFailed'.
evolveKey ::
     forall m c. (Crypto c, IOLike m)
  => StrictMVar m (KESState c) -> Absolute.KESPeriod -> m KESEvolutionInfo
evolveKey varKESState targetPeriod = modifyMVar varKESState $ \kesState -> do
    let info = kesStateInfo kesState
    -- We mask the evolution process because if we got interrupted after
    -- calling 'forgetSignKeyKES', which destructively updates the current
    -- signing key, we would leave an erased key in the state, which might
    -- cause a segfault when used afterwards.
    uninterruptibleMask_ $ case kesStateKey kesState of

      KESKeyPoisoned ->
        let err = KESKeyAlreadyPoisoned info targetPeriod
        in return (kesState, UpdateFailed err)

      KESKey key -> case kesStatus info targetPeriod of
        -- When the absolute period is before the start period, we can't
        -- update the key. 'checkCanForge' will say we can't forge because the
        -- key is not valid yet.
        BeforeKESStart {} ->
            return (kesState, Updated info)

        -- When the absolute period is after the end period, we can't evolve
        -- anymore and poison the expired key.
        AfterKESEnd {} ->
            let err = KESCouldNotEvolve info targetPeriod
            in return (poisonState kesState, UpdateFailed err)

        InKESRange targetEvolution
          -- No evolving needed
          | targetEvolution <= kesEvolution info
          -> return (kesState, Updated info)

          -- Evolving needed
          | otherwise
          -> (\s' -> (s', Updated (kesStateInfo s'))) <$>
               go targetEvolution info key

  where
    poisonState :: KESState c -> KESState c
    poisonState kesState = kesState { kesStateKey = KESKeyPoisoned }

    -- | PRECONDITION:
    --
    -- > targetEvolution >= curEvolution
    go :: KESEvolution -> KESInfo -> SL.SignKeyKES c -> m (KESState c)
    go targetEvolution info key
      | targetEvolution <= curEvolution
      = return $ KESState { kesStateInfo = info, kesStateKey = KESKey key }
      | otherwise
      = case SL.updateKES () key curEvolution of
          -- This cannot happen
          Nothing    -> error "Could not update KES key"
          Just !key' -> do
            -- Clear the memory associated with the old key
            forgetSignKeyKES key
            let info' = info { kesEvolution = curEvolution + 1 }
            go targetEvolution info' key'
      where
        curEvolution = kesEvolution info
