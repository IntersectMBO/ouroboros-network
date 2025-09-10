{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The 'Bundle' and 'TaggedBundle' types.
-- These represent KES credential \"bundles\", consisting of the key itself,
-- its current KES period, and an operational certificate (\"OpCert\").
-- \"Tagged bundles\" are tagged with a timestamp indicating their age, and may
-- contain either a 'Bundle' (which indicates a key update), or 'Nothing'
-- (which indicates a key deletion).
-- We need to track key deletions as explicit data points in order to propagate
-- them between KES agents.
--
-- Consider a scenario where 3 KES agents are connected as follows:
--
-- @@@
-- A <==> B <==> C
-- @@@
--
-- Now the following happens:
--
-- 1. A key is installed into agent A.
-- 2. The key propagates from agent A to agent B, and from there to agent C.
-- 3. Agent B shuts down.
-- 4. The key in agent A is deleted; agent A no longer holds a key.
-- 5. Agent B restarts.
-- 6. Agent B connects to agent C and receives the key.
-- 7. Agent A receives the key from agent B, thus undoing the delete.
--
-- This is undesirable, hence we represent key deletions as 'TaggedBundle's
-- without keys. This way, an agent can hold and propagate a \"key deletion\"
-- the same way it can propagate actual key bundles. Now the above scenario
-- plays out as follows:
--
-- 1. A key is installed into agent A.
-- 2. The key propagates from agent A to agent B, and from there to agent C.
-- 3. Agent B shuts down.
-- 4. The key in agent A is deleted; agent A now holds a key deletion.
-- 5. Agent B restarts.
-- 6. Agent B connects to agent C and receives the key.
-- 7. Agent B connects to agent A and receives the deletion. Since the deletion
--    is newer than the key it got from agent C, it deletes the key and stores
--    the deletion.
-- 8. Agent B pushes the deletion to agent C. Since the deletion is newer than
--    the key agent C holds, agent C deletes its key and stores the deletion.
module Cardano.KESAgent.KES.Bundle where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Data.Time -- (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX -- (utcTimeToPOSIXSeconds, )
import Data.Word (Word64)
import Text.Printf

-- | A bundle of a KES key with a period, plus the matching op cert.
-- The key itself is stored as a 'CRef', rather than directly, which allows us
-- to pass keys around and forget them exactly when the last reference is
-- dropped. The downside to this is that we need to be explicit about those
-- references, which is what the 'CRef' type achieves.
data Bundle m c
  = Bundle
  { bundleSKP :: CRef m (SignKeyWithPeriodKES (KES c))
  , bundleOC :: OCert c
  }

-- | Integral POSIX timestamps with microsecond accuracy. These are used to tag
-- key bundles and key bundle deletions for the purpose of resolving
-- synchronization conflicts.
newtype Timestamp = Timestamp {timestampValue :: Word64}
  deriving (Eq, Ord, Enum, Bounded, Num, Real, Integral)

-- | Convert a 'UTCTime' to a 'Timestamp', rounding down to the nearest
-- microsecond.
timestampFromUTC :: UTCTime -> Timestamp
timestampFromUTC =
  floor . (* 1000000) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

-- | Convert a 'Timestamp' to a 'UTCTime'. Note that due to the difference in
-- leap second handling (slewing to 86400 seconds vs. adding an extra second),
-- rounding error may occur.
timestampToUTC :: Timestamp -> UTCTime
timestampToUTC = posixSecondsToUTCTime . (/ 1000000) . fromIntegral . timestampValue

instance Pretty Timestamp where
  pretty (Timestamp micro) =
    printf "%u.%03u" (micro `div` 1000000) (micro `mod` 1000000)

instance Show Timestamp where
  show (Timestamp micro) =
    printf "%u.%03u" (micro `div` 1000000) (micro `mod` 1000000)

-- | A key bundle, or key bundle deletion, tagged with a 'Timestamp'.
data TaggedBundle m c
  = TaggedBundle
  { taggedBundle :: Maybe (Bundle m c)
  -- ^ Nothing means delete(d)
  , taggedBundleTimestamp :: Timestamp
  }
