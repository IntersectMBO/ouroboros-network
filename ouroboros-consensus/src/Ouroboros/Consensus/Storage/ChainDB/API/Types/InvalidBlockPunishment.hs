{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

-- | How to punish the sender of a invalid block
module Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment (
    -- * opaque
    InvalidBlockPunishment
  , enact
    -- * combinators
  , mkPunishThisThread
  , noPunishment
  ) where

import qualified Control.Exception as Exn
import           NoThunks.Class

import           Ouroboros.Consensus.Util.IOLike

-- | How to handle a discovered invalid block
--
-- This type is opaque because the soundness of the punishment is subtle because
-- of where it is invoked during the chain selection. As a result, arbitrary
-- monadic actions would be foot guns. Instead, this module defines a small DSL
-- for punishment that we judge to be sound.
newtype InvalidBlockPunishment m = InvalidBlockPunishment {
    enact :: m ()
  }
  -- this type is never actually an accumulator, so the -> NoThunks instance is fine
  deriving NoThunks via
    OnlyCheckWhnfNamed "InvalidBlockPunishment" (InvalidBlockPunishment m)

-- | A noop punishment
noPunishment :: Applicative m => InvalidBlockPunishment m
noPunishment = InvalidBlockPunishment $ pure ()

-- | Create a punishment that kills this thread
mkPunishThisThread :: IOLike m => m (InvalidBlockPunishment m)
mkPunishThisThread = do
    tid <- myThreadId
    pure $ InvalidBlockPunishment $ do
      throwTo tid PeerSentAnInvalidBlockException

-- | Thrown asynchronously to the client thread that added the block whose
-- processing involved an invalid block.
--
-- See 'mkPunishThisThread'.
data PeerSentAnInvalidBlockException = PeerSentAnInvalidBlockException
  deriving (Show)

instance Exn.Exception PeerSentAnInvalidBlockException
