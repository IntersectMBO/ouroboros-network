module Ouroboros.Storage.LedgerDB.MemPolicy (
    MemPolicy -- opaque
  , memPolicyVerify
  , memPolicyValid
  , memPolicyFromSkips
  , memPolicyToSkips
  , memPolicyFromOffsets
  , memPolicyToOffsets
  , memPolicyMaxRollback
  , defaultMemPolicy
  ) where

import           Data.Either (isRight)
import           Data.Word

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Ouroboros.Storage.LedgerDB.Offsets

{-------------------------------------------------------------------------------
  Memory policy
-------------------------------------------------------------------------------}

-- | Memory policy
newtype MemPolicy = MemPolicy {
    -- | Internal representation
    --
    -- The memory policy is defined in terms of "how many blocks do I need to
    -- see until my next snapshot?", starting with "how many blocks do I need to
    -- see until I store the /first/ snapshot?".
    --
    -- For example, if we want to store snapshots at offsets 0, 2 and 5 from the
    -- tip of the chain, the memory policy will be @[0, 1, 2]@:
    --
    -- >                        (tip)
    -- >      5  (4) (3)  2  (1)  0
    -- >     -----------------
    -- >     l4  l5  l6  l7  l8  l9
    -- >     ^^          ^^      ^^
    -- >      \__________/\______/
    -- >          2           1
    --
    -- Given sufficient number of blocks, we will store as many snapshots as
    -- there are entries in the memory policy. A memory policy @[0, 0..]@ would
    -- mean we keep absolutely all snapshots.
    memPolicyToSkips :: [Word64]
  }
  deriving (Show, Eq)

-- | Default in-memory policy
--
-- The in-memory policy specifies how many snapshots of the ledger state we
-- must keep in memory. More in-memory snapshots means faster recovery from
-- forks (which may be important for the short lived forks in Praos), but also
-- means a larger memory footprint (although conceivably this is reduced through
-- sharing). For the current ledger state a single copy of the ledger state is
-- roughly 1.4 GB (though this can likely be shrunk by optimizing various data
-- structures).
--
-- The policy for the snapshots in memory is independent from the policy for
-- which snapshots we write to disk. Indeed, the most recent snapshot on disk
-- may well be older than the oldest in-memory snapshot (in fact, this is
-- likely). It cannot be the case of course that the most recent on disk
-- snapshot is /newer/ than the oldest in-memory snapshot (this would mean if we
-- reopen the ledger DB we would not be able to rollback sufficiently far).
-- The distance between snapshots on disk and in memory is also unrelated.
--
-- TODO: Right now this just keeps the minimum: the most recent snapshot and
-- one @k@ blocks back. Once we move to Praos we might want to keep an
-- additional snapshot, say, 10 back, to be able to deal more efficiently with
-- short-lived forks  (trading memory footprint for execution time).
defaultMemPolicy :: SecurityParam -> MemPolicy
defaultMemPolicy (SecurityParam k) =
    memPolicyFromOffsets $ offsetsWithoutValues [0, k + 1]

memPolicyVerify :: MemPolicy -> Either String ()
memPolicyVerify (MemPolicy [])    = Left "MemPolicy should not be emty"
memPolicyVerify (MemPolicy (0:_)) = Right ()
memPolicyVerify (MemPolicy (_:_)) = Left "MemPolicy should start with 0"

memPolicyValid :: MemPolicy -> Bool
memPolicyValid = isRight . memPolicyVerify

memPolicyFromSkips :: [Word64] -> MemPolicy
memPolicyFromSkips = MemPolicy

-- | Memory policy in terms of the offsets from the tip of the blockchain
memPolicyToOffsets :: MemPolicy -> Offsets ()
memPolicyToOffsets = offsetsWithoutValues . go 0 . memPolicyToSkips
  where
    go :: Word64 -> [Word64] -> [Word64]
    go _         []     = []
    go curOffset (0:ss) = curOffset : go (curOffset + 1) ss
    go curOffset (s:ss) = go (curOffset + s) (0 : ss)

memPolicyFromOffsets :: Offsets () -> MemPolicy
memPolicyFromOffsets = MemPolicy . go 0 . offsetsDropValues
  where
    go :: Word64 -> [Word64] -> [Word64]
    go _         []     = []
    go curOffset (o:os) = o - curOffset : go (o + 1) os

-- | Maxinum number of blocks we can roll back given the policy
--
-- Of course there is no guarantee that we can /actually/ roll back this far
-- (if we are too close to genesis it might be less).
memPolicyMaxRollback :: MemPolicy -> Word64
memPolicyMaxRollback = last . offsetsDropValues . memPolicyToOffsets
