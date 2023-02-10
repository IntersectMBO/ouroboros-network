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

chunkInfo :: ChunkInfo
chunkInfo = UniformChunkSize (ChunkSize {chunkCanContainEBB = True, numRegularBlocks = 11})

maxClockSkew :: MaxClockSkew
maxClockSkew = MaxClockSkew 100000

tests :: TestTree
tests = testGroup "ChainDB QSM-derived unit tests" [
  testProperty "small chain" (prop_bogus example)
  ]

prop_bogus :: QSM.Commands (At Cmd TestBlock IO) (At Resp TestBlock IO) -> Property
prop_bogus commands = monadicIO $ do
  (_, prop) <- QC.run $ executeCommands maxClockSkew (SmallChunkInfo chunkInfo) commands
  pure prop

example :: QSM.Commands (At Cmd TestBlock IO) (At Resp TestBlock IO)
example =
  let fork i = TestBody { tbForkNo = i, tbIsValid = True }
  in stepCommands (smUnused maxClockSkew chunkInfo) $ do
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
addBlock blk = do
  void $ feed (At $ AddBlock blk)
  pure blk
