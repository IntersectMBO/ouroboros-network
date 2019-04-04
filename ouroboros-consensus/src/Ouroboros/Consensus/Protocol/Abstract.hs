{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.Abstract (
    -- * Abstract definition of the Ouroboros protocl
    OuroborosTag(..)
  , HasPreHeader(..)
  , HasPayload(..)
  , SecurityParam(..)
  , selectChain
  , selectUnvalidatedChain
    -- * State monad for Ouroboros state
  , HasNodeState
  , HasNodeState_(..)
  , NodeStateT
  , NodeStateT_ -- opaque
  , nodeStateT
  , runNodeStateT
  , evalNodeStateT
  , runNodeState
  , evalNodeState
  ) where

import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except
import           Control.Monad.State
import           Crypto.Random (MonadRandom (..))
import           Data.Bifunctor (first)
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Kind (Constraint)
import           Data.List (sortBy)
import           Data.Maybe (listToMaybe)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)

import           Control.Monad.Class.MonadSay

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import           Ouroboros.Network.Chain (Chain)

import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
import           Ouroboros.Consensus.Util.Random

-- | The (open) universe of Ouroboros protocols
--
-- This class encodes the part that is independent from any particular
-- block representation.
class ( Show (ChainState    p)
      , Show (ValidationErr p)
      , Typeable p -- so that p can appear in exceptions
      ) => OuroborosTag p where

  -- | Static node configuration
  --
  -- Every method in this class takes the node configuration as a parameter,
  -- so having this as a data family rather than a type family resolves most
  -- ambiguity.
  data family NodeConfig p :: *

  -- | State of the node required to run the protocol
  type family NodeState p :: *

  -- | The protocol specific part that should included in the block
  --
  -- The type argument is the type of the block header /without/ the
  -- Ouroboros specific part
  --
  -- This is a data family because of limitations in GHC's support for
  -- quantified constraints; see
  -- <https://ghc.haskell.org/trac/ghc/ticket/14860>
  -- <https://ghc.haskell.org/trac/ghc/ticket/15347>
  data family Payload p :: * -> *

  -- | Blockchain dependent protocol-specific state
  type family ChainState p :: *

  -- | Evidence that a node is the leader
  type family IsLeader p :: *

  -- | Projection of the ledger state the Ouroboros protocol needs access to
  type family LedgerView p :: *

  -- | Validation errors
  type family ValidationErr p :: *

  -- | Blocks that the protocol can run on
  type family SupportedBlock p :: * -> Constraint

  -- | Construct the ouroboros-specific payload of a block
  --
  -- Gets the proof that we are the leader and the preheader as arguments.
  mkPayload :: (HasNodeState p m, MonadRandom m)
            => (ph -> Encoding)
            -> NodeConfig p
            -> IsLeader p
            -> ph
            -> m (Payload p ph)

  -- | Do we prefer the candidate chain over ours?
  --
  -- Returns 'True' when we prefer the candidate over our chain.
  --
  -- PRECONDITION: the candidate chain does not extend into the future.
  --
  -- Note: we make no assumptions about the anchor points of the fragments.
  --
  -- Note: we do not assume that the candidate fragments fork less than @k@
  -- blocks back.
  preferCandidate :: HasHeader b
                  => NodeConfig p
                  -> AnchoredFragment b      -- ^ Our chain
                  -> AnchoredFragment b      -- ^ Candidate
                  -> Bool
  preferCandidate _ ours cand =
    AF.compareHeadBlockNo cand ours == GT
    -- TODO handle genesis

  -- | Compare two candidates, both of which we prefer to our own chain
  --
  -- Note: we make no assumptions about the anchor points of the fragments.
  --
  -- Note: we do not assume that the candidate fragments fork less than @k@
  -- blocks back.
  compareCandidates :: HasHeader b
                    => NodeConfig p
                    -> AnchoredFragment b -> AnchoredFragment b -> Ordering
  compareCandidates _ = AF.compareHeadBlockNo

  -- | Check if a node is the leader
  checkIsLeader :: (HasNodeState p m, MonadRandom m)
                => NodeConfig p
                -> SlotNo
                -> LedgerView p
                -> ChainState p
                -> m (Maybe (IsLeader p))

  -- | Apply a block
  applyChainState :: SupportedBlock p b
                  => (PreHeader b -> Encoding) -- Serialiser for the preheader
                  -> NodeConfig p
                  -> LedgerView p -- /Updated/ ledger state
                  -> b
                  -> ChainState p -- /Previous/ Ouroboros state
                  -> Except (ValidationErr p) (ChainState p)

  -- | We require that protocols support a @k@ security parameter
  protocolSecurityParam :: NodeConfig p -> SecurityParam

-- | Protocol security parameter
--
-- We interpret this as the number of rollbacks we support.
--
-- i.e., k == 0: we can't roll back at all
--       k == 1: we can roll back at most one block, etc
--
-- NOTE: This talks about the number of /blocks/ we can roll back, not
-- the number of /slots/.
newtype SecurityParam = SecurityParam { maxRollbacks :: Word64 }

-- | Extract the pre-header from a block
class (HasHeader b) => HasPreHeader b where
  type family PreHeader b :: *
  blockPreHeader :: b -> PreHeader b

-- | Blocks that contain the ouroboros payload
--
-- We do /not/ impose a functional dependency here, so that in principle
-- blocks can be defined to have payloads for multiple protocols. This is
-- important for protocol combinators.
class HasPreHeader b => HasPayload p b where
  blockPayload :: proxy p -> b -> Payload p (PreHeader b)

{-------------------------------------------------------------------------------
  Chain selection
-------------------------------------------------------------------------------}

-- | Chain selection between our chain and list of candidates
--
-- This is only a /model/ of chain selection: in reality of course we will not
-- work with entire chains in memory. This function is intended as an
-- explanation of how chain selection should work conceptually.
--
-- The @l@ parameter here models the ledger state for each chain, and serves as
-- evidence that the chains we are selecting between have been validated. (It
-- would /not/ be  correct to run chain selection on unvalidated chains and then
-- somehow fail if the selected chain turns out to be invalid.)
--
-- Returns 'Nothing' if we stick with our current chain.
selectChain :: forall p b l. (OuroborosTag p, HasHeader b)
            => NodeConfig p
            -> Chain b           -- ^ Our chain
            -> [(Chain b, l)]    -- ^ Upstream chains
            -> Maybe (Chain b, l)
selectChain cfg ours' candidates' =
    fmap (first toChain) $ listToMaybe $
    sortBy (flip (compareCandidates cfg `on` fst)) preferred
  where
    ours       = AF.fromChain ours'
    candidates = map (first AF.fromChain) candidates'
    preferred :: [(AnchoredFragment b, l)]
    preferred = filter (preferCandidate cfg ours . fst) candidates
    toChain :: AnchoredFragment b -> Chain b
    toChain af
      | Just c <- AF.toChain af
      = c
      | otherwise
      = error "impossible: fragment was anchored at genesis"

-- | Chain selection on unvalidated chains
selectUnvalidatedChain :: forall p b. (OuroborosTag p, HasHeader b)
                       => NodeConfig p
                       -> Chain b
                       -> [Chain b]
                       -> Maybe (Chain b)
selectUnvalidatedChain cfg ours = fmap fst . selectChain cfg ours . map (, ())

{-------------------------------------------------------------------------------
  State monad
-------------------------------------------------------------------------------}

type HasNodeState p = HasNodeState_ (NodeState p)

-- | State monad for the Ouroboros specific state
--
-- We introduce this so that we can have both MonadState and OuroborosState
-- in a monad stack.
class Monad m => HasNodeState_ s m | m -> s where
  getNodeState :: m s
  putNodeState :: s -> m ()

instance HasNodeState_ s m => HasNodeState_ s (ChaChaT m) where
  getNodeState = lift $ getNodeState
  putNodeState = lift . putNodeState

{-------------------------------------------------------------------------------
  Monad transformer introducing 'HasNodeState_'
-------------------------------------------------------------------------------}

newtype NodeStateT_ s m a = NodeStateT { unNodeStateT :: StateT s m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadSay)

type NodeStateT p = NodeStateT_ (NodeState p)

nodeStateT :: (s -> m (a, s)) -> NodeStateT_ s m a
nodeStateT = NodeStateT . StateT

runNodeStateT :: NodeStateT_ s m a -> s -> m (a, s)
runNodeStateT = runStateT . unNodeStateT

evalNodeStateT :: Monad m => NodeStateT_ s m a -> s -> m a
evalNodeStateT act = fmap fst . runNodeStateT act

runNodeState :: (forall m. Monad m => NodeStateT_ s m a) -> s -> (a, s)
runNodeState act = runIdentity . runNodeStateT act

evalNodeState :: (forall m. Monad m => NodeStateT_ s m a) -> s -> a
evalNodeState act = fst . runNodeState act

instance Monad m => HasNodeState_ s (NodeStateT_ s m) where
  getNodeState = NodeStateT $ get
  putNodeState = NodeStateT . put

instance MonadRandom m => MonadRandom (NodeStateT_ s m) where
  getRandomBytes = lift . getRandomBytes
