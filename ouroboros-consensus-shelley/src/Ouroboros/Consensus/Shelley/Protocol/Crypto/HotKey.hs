{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Hot key
--
-- Intended for qualified import
module Ouroboros.Consensus.Shelley.Protocol.Crypto.HotKey (
    KESEvolution
  , HotKey (..)
  , toPeriod
  , evolve
  ) where

import           GHC.Generics (Generic)

import           Cardano.Crypto.KES.Class
import qualified Cardano.Crypto.KES.Class as Relative (Period)
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Shelley.Spec.Ledger.Crypto (Crypto (..))
import qualified Shelley.Spec.Ledger.OCert as Absolute (KESPeriod (..))

import           Ouroboros.Consensus.Util.IOLike


-- | We call the relative periods that a KES key is valid its evolution, to
-- avoid confusion with absolute periods.
type KESEvolution = Relative.Period

-- | A hot KES key
data HotKey c = HotKey {
      hkStart     :: !Absolute.KESPeriod
   ,  hkEnd       :: !Absolute.KESPeriod
      -- ^ Currently derived from `TPraosParams`:
      -- > hkEnd = hkStart + tpraosMaxKESEvo
    , hkEvolution :: !KESEvolution
      -- ^ Invariant:
      -- > hkStart + hkEvolution in [hkStart, hkEnd)
    , hkKey       :: !(SignKeyKES (KES c))
    }
  deriving (Generic)

instance Show (HotKey c) where
  show HotKey { hkStart, hkEnd, hkEvolution } = mconcat [
        "<HotKey: start = "
      , show hkStart
      , ", end = "
      , show hkEnd
      , ", evolution = "
      , show hkEvolution
      , ">"
      ]

instance Crypto c => NoUnexpectedThunks (HotKey c)

-- | Return the evolution of the given KES period, /when/ it falls within the
-- range of the 'HotKey' (@[hkStart, hkEnd)@).
--
-- Note that the upper bound is exclusive, the spec says:
-- > c0 <= kesPeriod s < c0 + MaxKESEvo
toEvolution :: HotKey c -> Absolute.KESPeriod -> Maybe KESEvolution
toEvolution HotKey { hkStart = Absolute.KESPeriod lo
                   , hkEnd   = Absolute.KESPeriod hi
                   }
            (Absolute.KESPeriod cur)
    | lo <= cur, cur < hi = Just (cur - lo)
    | otherwise           = Nothing

-- | Return the current period of the 'HotKey'.
toPeriod :: HotKey c -> Absolute.KESPeriod
toPeriod HotKey { hkStart = Absolute.KESPeriod lo, hkEvolution } =
    Absolute.KESPeriod (lo + hkEvolution)

-- | Evolve the 'HotKey' so that its evolution matches the given KES period.
--
-- When the 'HotKey' has already evolved further than the given KES period, we
-- return 'Nothing'.
--
-- When the given KES period is outside the bounds of the 'HotKey', we return
-- 'Nothing'.
evolve ::
     forall c m. (IOLike m, Crypto c)
  => Absolute.KESPeriod
  -> HotKey c
  -> m (Maybe (HotKey c))
evolve targetPeriod hk = do
    let mNewHotKey = do
           targetEvolution <- toEvolution hk targetPeriod
           key' <- go targetEvolution (hkEvolution hk) (hkKey hk)
           return HotKey {
               hkStart     = hkStart hk
             , hkEnd       = hkEnd   hk
             , hkEvolution = targetEvolution
             , hkKey       = key'
             }
    -- Any stateful code (for instance, running finalizers to clear the
    -- memory associated with the old key) would happen here or in (a
    -- monadic) 'go'.
    return mNewHotKey
  where
    go :: KESEvolution
       -> KESEvolution
       -> SignKeyKES (KES c)
       -> Maybe (SignKeyKES (KES c))
    go targetEvolution curEvolution key
      | targetEvolution == curEvolution
      = Just key
      | targetEvolution < curEvolution
      = Nothing
      | otherwise
      = case updateKES () key curEvolution of
          -- This cannot happen
          Nothing   -> error "Could not update KES key"
          Just key' -> go targetEvolution (curEvolution + 1) key'
