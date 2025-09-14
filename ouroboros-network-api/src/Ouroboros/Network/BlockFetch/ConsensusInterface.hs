{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}

module Ouroboros.Network.BlockFetch.ConsensusInterface
  ( PraosFetchMode (..)
  , FetchMode (..)
  , BlockFetchConsensusInterface (..)
  , ChainSelStarvation (..)
  , ChainComparison (..)
  , mkReadFetchMode
    -- * Utilities
  , WithFingerprint (..)
  , Fingerprint (..)
  , initialWithFingerprint
  ) where

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime (UTCTime)
import Control.Monad.Class.MonadTime.SI (Time)
import Data.Functor ((<&>))

import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks)

import Cardano.Network.ConsensusMode (ConsensusMode (..))
import Cardano.Network.Types (LedgerStateJudgement (..))
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.Block
import Ouroboros.Network.SizeInBytes (SizeInBytes)

data PraosFetchMode =
       -- | Use this mode when we are catching up on the chain but are stil
       -- well behind. In this mode the fetch logic will optimise for
       -- throughput rather than latency.
       --
       FetchModeBulkSync

       -- | Use this mode for block-producing nodes that have a known deadline
       -- to produce a block and need to get the best chain before that. In
       -- this mode the fetch logic will optimise for picking the best chain
       -- within the given deadline.
     | FetchModeDeadline

       -- TODO: add an additional mode for in-between: when we are a core node
       -- following the chain but do not have an imminent deadline, or are a
       -- relay forwarding chains within the network.
       --
       -- This is a mixed mode because we have to combine the distribution of
       -- time to next block under praos, with the distribution of latency of
       -- our peers, and also the consensus preference.

  deriving (Eq, Show)

-- | The fetch mode that the block fetch logic should use.
data FetchMode = FetchModeGenesis | PraosFetchMode PraosFetchMode
  deriving (Eq, Show)

-- | Construct 'readFetchMode' for 'BlockFetchConsensusInterface' by branching
-- on the 'ConsensusMode'.
mkReadFetchMode
  :: Functor m
  => ConsensusMode
  -> m LedgerStateJudgement
     -- ^ Used for 'GenesisMode'.
  -> m PraosFetchMode
     -- ^ Used for 'PraosMode' for backwards compatibility.
  -> m FetchMode
mkReadFetchMode consensusMode getLedgerStateJudgement getFetchMode =
    case consensusMode of
      GenesisMode -> getLedgerStateJudgement <&> \case
        YoungEnough -> PraosFetchMode FetchModeDeadline
        TooOld      -> FetchModeGenesis
      PraosMode   -> PraosFetchMode <$> getFetchMode

-- | The consensus layer functionality that the block fetch logic requires.
--
-- These are provided as input to the block fetch by the consensus layer.
--
data BlockFetchConsensusInterface peer header block m =
     BlockFetchConsensusInterface {

       -- | Read the K-suffixes of the candidate chains.
       --
       -- Assumptions:
       -- * Their headers must be already validated.
       -- * They may contain /fewer/ than @K@ blocks.
       -- * Their anchor does not have to intersect with the current chain.
       readCandidateChains    :: STM m (Map peer (AnchoredFragment header)),

       -- | Read the K-suffix of the current chain.
       --
       -- This must contain info on the last @K@ blocks (unless we're near
       -- the chain genesis of course).
       --
       readCurrentChain       :: STM m (AnchoredFragment header),

       -- | Read the current fetch mode that the block fetch logic should use.
       --
       -- The fetch mode is a dynamic part of the block fetch policy. In
       -- 'FetchModeBulkSync' it follows a policy that optimises for expected
       -- bandwidth over latency to fetch any particular block, whereas in
       -- 'FetchModeDeadline' it follows a policy optimises for the latency
       -- to fetch blocks, at the expense of wasting bandwidth.
       --
       -- 'FetchModeGenesis' should be used when the genesis node is syncing to
       -- ensure it isn't leashed.
       --
       -- This mode should be set so that when the node's current chain is near
       -- to \"now\" it uses the deadline mode, and when it is far away it uses
       -- the bulk sync mode.
       --
       readFetchMode          :: STM m FetchMode,

       -- | Recent, only within last K
       readFetchedBlocks      :: STM m (Point block -> Bool),

       -- | This method allocates an @addFetchedBlock@ function per client.
       -- That function and 'readFetchedBlocks' are required to be linked. Upon
       -- successful completion of @addFetchedBlock@ it must be the case that
       -- 'readFetchedBlocks' reports the block.
       mkAddFetchedBlock      :: STM m (Point block -> block -> m ()),

       -- | The highest stored/downloaded slot number.
       --
       -- This is used to optimise the filtering of fragments in the block
       -- fetch logic: when removing already downloaded blocks from a
       -- fragment, the filtering (with a linear cost) is stopped as soon as a
       -- block has a slot number higher than this slot number, as it cannot
       -- have been downloaded anyway.
       readFetchedMaxSlotNo    :: STM m MaxSlotNo,

       -- | Compare chain fragments. This might involve further state, such as
       -- Peras certificates (which give certain blocks additional weight).
       readChainComparison     :: STM m (WithFingerprint (ChainComparison header)),

       -- | Much of the logic for deciding which blocks to download from which
       -- peer depends on making estimates based on recent performance metrics.
       -- These estimates of course depend on the amount of data we will be
       -- downloading.
       --
       blockFetchSize          :: header -> SizeInBytes,

       -- | Given a block header, validate the supposed corresponding block
       -- body.
       --
       blockMatchesHeader      :: header -> block -> Bool,

       -- | Calculate when a header's block was forged.
       headerForgeUTCTime      :: header -> UTCTime,

       -- | Information on the ChainSel starvation status; whether it is ongoing
       -- or has ended recently. Needed by the bulk sync decision logic.
       readChainSelStarvation :: STM m ChainSelStarvation,

       -- | Action to inform CSJ (ChainSync Jumping) that the given peer has not
       -- been performing adequately with respect to BlockFetch, and that it
       -- should be demoted from the dynamo role. Can be set to @const (pure
       -- ())@ in all other scenarios.
       demoteChainSyncJumpingDynamo :: peer -> m ()
     }


-- | Whether ChainSel is starved or has been recently.
--
-- The bulk sync fetch decision logic needs to decide whether the current
-- focused peer has starved ChainSel recently. This datatype is used to
-- represent this piece of information.
data ChainSelStarvation
  = ChainSelStarvationOngoing
  | ChainSelStarvationEndedAt Time
  deriving (Eq, Show, NoThunks, Generic)


data ChainComparison header =
     ChainComparison {
       -- | Given the current chain, is the given chain plausible as a candidate
       -- chain. Classically for Ouroboros this would simply check if the
       -- candidate is strictly longer, but it can also involve further
       -- criteria:
       --
       --  * Tiebreakers (e.g. based on the opcert numbers and VRFs) for chains
       --    of equal length.
       --
       --  * Weight in the context of Ouroboros Peras, due to a boost from a
       --    Peras certificate.
       --
       plausibleCandidateChain :: HasCallStack
                               => AnchoredFragment header
                               -> AnchoredFragment header
                               -> Bool,

       -- | Compare two candidate chains and return a preference ordering.
       -- This is used as part of selecting which chains to prioritise for
       -- downloading block bodies.
       --
       compareCandidateChains  :: HasCallStack
                               => AnchoredFragment header
                               -> AnchoredFragment header
                               -> Ordering
     }

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | Simple type that can be used to indicate some value (without/only with an
-- expensive 'Eq' instance) changed.
newtype Fingerprint = Fingerprint Word64
  deriving stock (Show, Eq, Generic)
  deriving newtype (Enum)
  deriving anyclass (NoThunks)

-- | Store a value together with its 'Fingerprint'.
data WithFingerprint a = WithFingerprint
  { forgetFingerprint :: !a
  , getFingerprint    :: !Fingerprint
  }
  deriving stock (Show, Functor, Generic)
  deriving anyclass (NoThunks)

-- | Attach @'Fingerprint' 0@ to the given value. When the underlying @a@ is
-- changed, the 'Fingerprint' must be updated to a new unique value (e.g. via
-- 'succ').
initialWithFingerprint :: a -> WithFingerprint a
initialWithFingerprint a = WithFingerprint a (Fingerprint 0)
