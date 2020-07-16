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
    KESInfo (..)
  , kesAbsolutePeriod
  , KESEvolution
  , HotKey (..)
  , mkHotKey
  , toEvolution
  ) where

import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Crypto.KES.Class hiding (forgetSignKeyKES)
import qualified Cardano.Crypto.KES.Class as Relative (Period)
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Shelley.Spec.Ledger.Crypto (Crypto (..))
import qualified Shelley.Spec.Ledger.OCert as Absolute (KESPeriod (..))

import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Shelley.Protocol.Crypto

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
  deriving (Show, Generic, NoUnexpectedThunks)

-- | Return the absolute KES period
kesAbsolutePeriod :: KESInfo -> Absolute.KESPeriod
kesAbsolutePeriod KESInfo { kesStartPeriod, kesEvolution } =
    Absolute.KESPeriod $ start + kesEvolution
  where
    Absolute.KESPeriod start = kesStartPeriod

-- | Return the evolution of the given KES period, /when/ it falls within the
-- range of the 'HotKey' (@[hkStart, hkEnd)@).
--
-- Note that the upper bound is exclusive, the spec says:
-- > c0 <= kesPeriod s < c0 + MaxKESEvo
toEvolution :: KESInfo -> Absolute.KESPeriod -> Maybe KESEvolution
toEvolution KESInfo { kesStartPeriod = Absolute.KESPeriod lo
                    , kesEndPeriod   = Absolute.KESPeriod hi
                   }
            (Absolute.KESPeriod cur)
    | lo <= cur, cur < hi = Just (cur - lo)
    | otherwise           = Nothing

{-------------------------------------------------------------------------------
  Hot Key
-------------------------------------------------------------------------------}

-- | API to interact with the key.
data HotKey c m = HotKey {
      -- | Evolve the KES signing key to the given absolute KES period.
      --
      -- When the key can no longer be evolved, this is a no-op.
      evolve  :: Absolute.KESPeriod -> m KESInfo
      -- | Return 'KESInfo' of the signing key.
    , getInfo :: m KESInfo
      -- | Sign the given @toSign@ with the current signing key.
      --
      -- POSTCONDITION: the signature is in normal form.
      --
      -- NOTE: will still sign when the key failed to evolve.
    , sign    :: forall toSign. Signable (KES c) toSign
              => toSign -> m (SignedKES (KES c) toSign)
    }

data KESState c = KESState {
      kesStateInfo :: !KESInfo
    , kesStateKey  :: !(SignKeyKES (KES c))
    }
  deriving (Generic)

instance Crypto c => NoUnexpectedThunks (KESState c)

mkHotKey ::
     forall m c. (TPraosCrypto c, IOLike m)
  => SignKeyKES (KES c)
  -> Absolute.KESPeriod  -- ^ Start period
  -> Word64              -- ^ Max KES evolutions
  -> m (HotKey c m)
mkHotKey initKey startPeriod@(Absolute.KESPeriod start) maxKESEvolutions = do
    varKESState <- newMVar initKESState
    return HotKey {
        evolve  = evolveKey varKESState
      , getInfo = kesStateInfo <$> readMVar varKESState
      , sign    = \toSign -> do
          KESState { kesStateInfo, kesStateKey = key } <- readMVar varKESState
          let evolution = kesEvolution kesStateInfo
              signed    = signedKES () evolution toSign key
          -- Force the signature to WHNF (for 'SignedKES', WHNF implies NF) so
          -- that we don't have any thunks holding on to a key that might be
          -- destructively updated when evolved.
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
      , kesStateKey = initKey
      }

-- | Evolve the 'HotKey' so that its evolution matches the given KES period.
--
-- When the 'HotKey' has already evolved further than the given KES
-- period, we do nothing.
--
-- When the given KES period is outside the bounds of the 'HotKey', we
-- also do nothing.
--
-- In both cases, the 'checkKesPeriod' check performed on the 'KESInfo' in
-- 'checkIsLeader' will tell that the key is unusable, which means that we
-- will not forge a block and thus not try to sign anything using the key.
evolveKey ::
     forall m c. (TPraosCrypto c, IOLike m)
  => StrictMVar m (KESState c) -> Absolute.KESPeriod -> m KESInfo
evolveKey varKESState targetPeriod = modifyMVar varKESState $ \kesState ->
    -- We mask the evolution process because if we got interrupted after
    -- calling 'forgetSignKeyKES', which destructively updates the current
    -- signing key, we would leave an erased key in the state, which might
    -- cause a segfault when used afterwards.
    uninterruptibleMask_ $ do
      kesState' <- case toEvolution (kesStateInfo kesState) targetPeriod of
        -- The absolute target period is outside the bounds
        Nothing              -> return kesState
        Just targetEvolution -> go targetEvolution kesState
      return (kesState', kesStateInfo kesState')
  where
    go :: KESEvolution -> KESState c -> m (KESState c)
    go targetEvolution kesState@KESState { kesStateInfo, kesStateKey = key }
      | targetEvolution == curEvolution
      = return kesState
      | targetEvolution < curEvolution
      = return kesState
      | otherwise
      = case updateKES () key curEvolution of
          -- This cannot happen
          Nothing    -> error "Could not update KES key"
          Just !key' -> do
            -- Clear the memory associated with the old key
            forgetSignKeyKES key
            let kesState' = KESState {
                    kesStateInfo = kesStateInfo {
                        kesEvolution = curEvolution + 1
                      }
                  , kesStateKey = key'
                  }
            go targetEvolution kesState'
      where
        curEvolution = kesEvolution kesStateInfo
