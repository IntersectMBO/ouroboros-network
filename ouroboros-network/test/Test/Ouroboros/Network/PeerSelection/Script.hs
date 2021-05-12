{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}

module Test.Ouroboros.Network.PeerSelection.Script (

    -- * Test scripts
    Script(..),
    scriptHead,
    singletonScript,
    initScript,
    stepScript,
    stepScriptSTM,
    arbitraryScriptOf,

    -- * Timed scripts
    ScriptDelay(..),
    TimedScript,
    playTimedScript,

    -- * Pick scripts
    PickScript,
    PickMembers(..),
    arbitraryPickScript,
    interpretPickScript,

  ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer

import           Test.Ouroboros.Network.PeerSelection.Instances ()

import           Test.QuickCheck
import           Test.QuickCheck.Utils


--
-- Test script abstraction
--

newtype Script a = Script (NonEmpty a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

singletonScript :: a -> Script a
singletonScript x = (Script (x :| []))

scriptHead :: Script a -> a
scriptHead (Script (x :| _)) = x

arbitraryScriptOf :: Int -> Gen a -> Gen (Script a)
arbitraryScriptOf maxSz a =
    sized $ \sz -> do
      n <- choose (1, max 1 (min maxSz sz))
      (Script . NonEmpty.fromList) <$> vectorOf n a

initScript :: MonadSTM m => Script a -> m (TVar m (Script a))
initScript = newTVarIO

stepScript :: MonadSTM m => TVar m (Script a) -> m a
stepScript scriptVar = atomically (stepScriptSTM scriptVar)

stepScriptSTM :: MonadSTMTx stm tvar tmvar tqueue tbqueue => tvar (Script a) -> stm a
stepScriptSTM scriptVar = do
    Script (x :| xs) <- readTVar scriptVar
    case xs of
      []     -> return ()
      x':xs' -> writeTVar scriptVar (Script (x' :| xs'))
    return x

instance Arbitrary a => Arbitrary (Script a) where
    arbitrary = sized $ \sz -> arbitraryScriptOf sz arbitrary

    shrink (Script (x :| [])) = [ Script (x' :| []) | x' <- shrink x ]
    shrink (Script (x :| xs)) =
        Script (x :| [])                          -- drop whole tail
      : Script (x :| take (length xs `div` 2) xs) -- drop half the tail
      : Script (x :| init xs)                     -- drop only last

        -- drop none, shrink only elements
      : [ Script (x' :| xs) | x'  <- shrink x ]
     ++ [ Script (x :| xs') | xs' <- shrinkListElems shrink xs ]


--
-- Timed scripts
--

type TimedScript a = Script (a, ScriptDelay)

data ScriptDelay = NoDelay | ShortDelay | LongDelay
  deriving (Eq, Show)

instance Arbitrary ScriptDelay where
  arbitrary = frequency [ (1, pure NoDelay)
                        , (1, pure ShortDelay)
                        , (4, pure LongDelay) ]

  shrink LongDelay  = [NoDelay, ShortDelay]
  shrink ShortDelay = [NoDelay]
  shrink NoDelay    = []

playTimedScript :: (MonadAsync m, MonadTimer m)
                => TimedScript a -> m (TVar m a)
playTimedScript (Script ((x0,d0) :| script)) = do
    v <- newTVarIO x0
    _ <- async $ do
           threadDelay (interpretScriptDelay d0)
           sequence_ [ do atomically (writeTVar v x)
                          threadDelay (interpretScriptDelay d)
                     | (x,d) <- script ]
    return v
  where
    interpretScriptDelay NoDelay    = 0
    interpretScriptDelay ShortDelay = 1
    interpretScriptDelay LongDelay  = 3600


--
-- Pick scripts
--

-- | A pick script is used to interpret the 'policyPickKnownPeersForGossip' and
-- the 'policyPickColdPeersToForget'. It selects elements from the given
-- choices by their index (modulo the number of choices). This representation
-- was chosen because it allows easy shrinking.
--
type PickScript peeraddr = Script (PickMembers peeraddr)

data PickMembers peeraddr = PickFirst
                          | PickAll
                          | PickSome (Set peeraddr)
  deriving (Eq, Show)

instance (Arbitrary peeraddr, Ord peeraddr) =>
         Arbitrary (PickMembers peeraddr) where
    arbitrary = arbitraryPickMembers (Set.fromList <$> listOf1 arbitrary)

    shrink (PickSome ixs) = PickFirst
                          : PickAll
                          : [ PickSome ixs'
                            | ixs' <- shrink ixs
                            , not (Set.null ixs') ]
    shrink PickAll        = [PickFirst]
    shrink PickFirst      = []

arbitraryPickMembers :: Gen (Set peeraddr) -> Gen (PickMembers peeraddr)
arbitraryPickMembers pickSome =
    frequency [ (1, pure PickFirst)
              , (1, pure PickAll)
              , (2, PickSome <$> pickSome)
              ]

arbitraryPickScript :: Gen (Set peeraddr) -> Gen (PickScript peeraddr)
arbitraryPickScript pickSome =
    sized $ \sz ->
      arbitraryScriptOf sz (arbitraryPickMembers pickSome)

interpretPickScript :: (MonadSTMTx stm tvar tmvar tqueue tbqueue, Ord peeraddr)
                    => tvar PickScript
                    -> Set peeraddr
                    -> Int
                    -> stm (Set peeraddr)
interpretPickScript scriptVar available pickNum
  | Set.null available
  = error "interpretPickScript: given empty map to pick from"
  | pickNum <= 0
  = error "interpretPickScript: given invalid pickNum"

  | Set.size available <= pickNum
  = return available

  | otherwise
  = do pickmembers <- stepScriptSTM scriptVar
       return (interpretPickMembers pickmembers available pickNum)

interpretPickMembers :: Ord peeraddr
                     => PickMembers peeraddr
                     -> Set peeraddr -> Int -> Set peeraddr
interpretPickMembers PickFirst     ps _ = Set.singleton (Set.elemAt 0 ps)
interpretPickMembers PickAll       ps n = Set.take n ps
interpretPickMembers (PickSome as) ps n
  | Set.null ps' = Set.singleton (Set.elemAt 0 ps)
  | otherwise    = Set.take n ps'
  where
    ps' = Set.intersection ps as

