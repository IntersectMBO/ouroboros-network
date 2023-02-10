{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Collection functions and exported symbols to be able to run a
-- 'quickcheck-state-machine' counterexample using the GHC repl.
--
-- To test a counterexample first fire up the repl, and enable the
-- 'FlexibleContexts' and 'TypeFamilies' extensions, and set multi-line input.
--
-- > cabal new-repl test-storage
-- > import Test.Ouroboros.Storage.ChainDB.StateMachine.RunOnRepl
-- > :set -XFlexibleContexts -XTypeFamilies +m
--
-- The commands that are part of the counterexample are usually several lines
-- long. Thus it is better to create a local definition for them:
--
-- > cmds = (<PASTE THE COMMANDS OF THE COUNTEREXAMPLE HERE>)
--
-- Note the use of parentheses to prevent GHCi from ending the multiline input
-- prematurely (the copied counterexample in this case).
--
-- Then, the model and system under tests can be tested for lockstep agreement
-- by running:
--
-- > quickCheckCmdsLockStep someClockSkew someChunkInfo counterexample
--
-- Where 'someClockSkew' and 'someChunkInfo' are the ones given by the
-- counterexample found by quickcheck-statemachine.
module Test.Ouroboros.Storage.ChainDB.StateMachine.RunOnRepl (
    -- * Running the counterexamples
    quickCheckCmdsLockStep
    -- * Patterns needed to disambiguate the 'At' and 'Command' symbols printed
    --   by the 'ChainDB.StateMachine' tests.
  , pattern At
  , pattern Command
  -- * Re-exports needed for compiling a 'ChainDB.StateMachine' inside the repl.
  -- ** quickcheck-state-machine re-exports
  , Commands (..)
  , Reference (Reference)
  , Symbolic (Symbolic)
  , Var (Var)
  -- ** ChainDB.StateMachine re-exports
  , Cmd (..)
  , Resp (..)
  , Success (..)
  , MaxClockSkew (MaxClockSkew)
  , runCmdsLockstep
  -- ** Test packages re-exports
  , TestBlock (..)
  , TestHeader (..)
  , TestBody (..)
  , TestHeaderHash (..)
  , TestBodyHash (..)
  , ChainLength (..)
  , EBB (EBB, RegularBlock)
  , SmallChunkInfo (SmallChunkInfo)
  -- ** Ouroboros consensus re-exports
  , SlotNo (..)
  , BlockNo (..)
  , ChainHash (..)
  , EpochNo (..)
  , ChainType(TentativeChain)
  , ChainUpdate (RollBack)
  , Block (..)
  , ChunkInfo (..)
  , ChunkSize (..)
  )
where

import           Test.StateMachine.Types (Reference (Reference),
                     Symbolic (Symbolic), Var (Var), Commands (Commands))
import qualified Test.StateMachine.Types as StateMachine.Types
import           Test.QuickCheck (quickCheck)

import           Test.Util.Orphans.ToExpr ()
import           Test.Util.ChunkInfo (SmallChunkInfo (SmallChunkInfo))

import           Ouroboros.Consensus.Block (SlotNo (SlotNo), BlockNo (BlockNo),
                     ChainHash (GenesisHash, BlockHash), EpochNo (EpochNo))
import           Ouroboros.Consensus.Storage.ImmutableDB
                     (ChunkInfo (UniformChunkSize),
                     ChunkSize (ChunkSize, chunkCanContainEBB, numRegularBlocks))

import           Ouroboros.Consensus.Storage.ChainDB (ChainType(TentativeChain))

import           Ouroboros.Network.Block (ChainUpdate (RollBack))
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Point (Block (..))
import qualified Ouroboros.Network.Point as Point

import           Test.Ouroboros.Storage.Orphans ()
import           Test.Ouroboros.Storage.TestBlock (TestBlock (..), TestHeader (..),
                     TestBody (..), TestHeaderHash (..), TestBodyHash (..),
                     ChainLength (ChainLength), EBB (EBB, RegularBlock))
import           Test.Ouroboros.Storage.ChainDB.StateMachine (Cmd (..), Resp (..),
                     Success (..), MaxClockSkew (MaxClockSkew), runCmdsLockstep,
                     IterRef, FollowerRef)
import qualified Test.Ouroboros.Storage.ChainDB.StateMachine as StateMachine

pattern At :: Block SlotNo (Block.HeaderHash blk) -> Block.Point blk
pattern At x = Block.Point (Point.At x)

pattern Command ::
     t1 blk1 (IterRef blk1 m1 Symbolic) (FollowerRef blk1 m1 Symbolic)
  -> t2 blk2 (IterRef blk2 m2 Symbolic) (FollowerRef blk2 m2 Symbolic)
  -> [Var]
  -> StateMachine.Types.Command (StateMachine.At t1 blk1 m1) (StateMachine.At t2 blk2 m2)
pattern Command cmd rsp xs =
  StateMachine.Types.Command (StateMachine.At cmd) (StateMachine.At rsp) xs

quickCheckCmdsLockStep ::
     MaxClockSkew
  -> SmallChunkInfo
  -> Commands (StateMachine.At Cmd TestBlock IO) (StateMachine.At Resp TestBlock IO)
  -> IO ()
quickCheckCmdsLockStep maxClockSkew chunkInfo cmds =
  quickCheck $ runCmdsLockstep maxClockSkew chunkInfo cmds
