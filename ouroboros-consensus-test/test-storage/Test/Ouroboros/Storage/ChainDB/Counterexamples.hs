{-# LANGUAGE RecordWildCards #-}
module Test.Ouroboros.Storage.ChainDB.Counterexamples (tests) where

import           Control.Monad (void)
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainType (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Network.Block (Point (..), blockPoint)
import           Test.Ouroboros.Storage.ChainDB.StateMachine hiding (tests)
import           Test.Ouroboros.Storage.TestBlock
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic (monadicIO)
import           Test.StateMachine.Step (Step, feed, stepCommands)
import qualified Test.StateMachine.Types as QSM
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.ChunkInfo (SmallChunkInfo (..))

tests :: TestTree
tests = testGroup "ChainDB QSM-derived unit tests" [
  -- The example are labeled by Github issue, in the manner <repo-name>_<issue-number>.
  testProperty "example_ouroboros_network_4183" (prop_example example_ouroboros_network_4183)
  ]

data Example blk m = Example {
    exCommands     :: QSM.Commands (At Cmd blk m) (At Resp blk m)
  , exChunkInfo    :: ChunkInfo
  , exMaxClockSkew :: MaxClockSkew
  }

prop_example :: Example TestBlock IO -> Property
prop_example Example {..} = monadicIO $ do
  (_, prop) <- QC.run $ executeCommands exMaxClockSkew
                                        (SmallChunkInfo exChunkInfo)
                                        exCommands
  pure prop

example_ouroboros_network_4183 :: Example TestBlock IO
example_ouroboros_network_4183 =
  let exChunkInfo = simpleChunkInfo 11
      exMaxClockSkew = MaxClockSkew 100000

      fork i = TestBody { tbForkNo = i, tbIsValid = True }
      exCommands = stepCommands (smUnused exMaxClockSkew exChunkInfo) $ do
        b1 <- addBlock $ firstEBB (const True) $ fork 0
        b2 <- addBlock $ mkNextBlock b1 0 $ fork 0
        b3 <- addBlock $ mkNextBlock b2 1 $ fork 0
        b4 <- addBlock $ mkNextBlock b2 1 $ fork 1
        f <- newFollower SelectedChain
        followerForward f [blockPoint b1]
        void $ addBlock $ mkNextBlock b4 2 $ fork 1
        persistBlks
        void $ addBlock $ mkNextBlock b3 2 $ fork 0
        followerInstruction f
  in Example {..}

newFollower :: ChainType -> Step (Model blk m) (At Cmd blk m) m (At Resp blk m)
  (FollowerRef blk m QSM.Symbolic)
newFollower ct = do
  QSM.Command _ resp _ <- feed (At $ NewFollower ct)
  case getResp $ unAt resp of
    Right (Flr flr) -> pure $ flr
    _               -> error "Not supposed to happen"

persistBlks :: Step (Model blk m) (At Cmd blk m) m (At Resp blk m) ()
persistBlks = void $ feed (At $ PersistBlks)

followerForward :: FollowerRef blk m QSM.Symbolic -> [Point blk]
                -> Step (Model blk m) (At Cmd blk m) m (At Resp blk m) ()
followerForward flr pts = void $ feed (At $ FollowerForward flr pts)

followerInstruction :: FollowerRef blk m QSM.Symbolic
                    -> Step (Model blk m) (At Cmd blk m) m (At Resp blk m) ()
followerInstruction flr = void $ feed (At $ FollowerInstruction flr)

addBlock :: blk -> Step (Model blk m) (At Cmd blk m) m (At Resp blk m) blk
addBlock blk = feed (At $ AddBlock blk) >> pure blk
