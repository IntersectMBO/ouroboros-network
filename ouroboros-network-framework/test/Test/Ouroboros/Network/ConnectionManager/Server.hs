{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Ouroboros.Network.ConnectionManager.Server where

import           Control.Applicative
import           Control.Monad (MonadPlus, join)
import           Data.Foldable (toList)
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified Ouroboros.Network.ConnectionManager.Server.ControlChannel as Server


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.ConnectionManager.Server"
  [ testGroup "peekAlt"
    [ testProperty "foldr    (List)"  (prop_peekAlt_foldr    @[]    @Int)
    , testProperty "foldr    (Maybe)" (prop_peekAlt_foldr    @Maybe @Int)
    , testProperty "sequence (Maybe)" (prop_peekAlt_sequence @Maybe @Int)
    , testProperty "cons     (Maybe)" (prop_peekAlt_cons     @Maybe @Int)
    ]
  ]


--
-- peekAlt properties
--

-- We are ulitmately interested in this properties for `STM` functor, but we
-- only test them for 'Maybe' monad.  This is enough since there is an
-- isomrphism (it preserves 'Alternative' operations) in `Kleisli IO`:
--
-- > toSTM :: Maybe a -> IO (STM m a)
-- > toSTM Nothing  = pure retry
-- > toSTM (Just a) = pure (pure a)
--
-- with an inverse:
--
-- > fromSTM :: STM m a -> IO (Maybe a)
-- > fromSTM ma = atomically (ma `orElse` (pure Nothing))


prop_peekAlt_foldr
    :: forall m a.
       ( Eq (m a)
       , Show (m a)
       , Alternative m )
    => [m a] -> Property
prop_peekAlt_foldr as =
    (fst <$> Server.peekAlt (Seq.fromList as))
    === 
    (foldr (<|>) empty as)


-- | Recursively calling 'peekAlt' is like filtering non 'empty' elements and
-- 'sequence'.
--
prop_peekAlt_sequence
    :: forall m a.
       ( Eq (m a)
       , Eq (m [a])
       , Eq (m (a, StrictSeq (m a)))
       , Show (m [a])
       , MonadPlus m )
    => [m a] -> Property
prop_peekAlt_sequence as =
      peekAll [] (Seq.fromList as)
      ===
      sequence (filter (/= empty) as)
    where
      -- recursievly 'peekAlt' and collect results
      peekAll :: [a] -> StrictSeq (m a) -> m [a]
      peekAll acc s =
       case Server.peekAlt s of
         res | res == empty -> pure (reverse acc)
             | otherwise    -> join $ (\(a, s') -> peekAll (a : acc) s') <$> res

    
-- | Calling `peekAlt` and then cominging the result with a cons ('<|'), should
-- put the first non 'empty' element in front.
--
prop_peekAlt_cons
    :: forall m a.
      ( Eq   (m a)
      , Eq   (m [m a])
      , Show (m [m a])
      , Alternative m )
    => [m a] -> Property
prop_peekAlt_cons as =
    let x = Server.peekAlt (Seq.fromList as)

        mhead :: m a
        mhead = fst <$> x

        mtail :: m (StrictSeq (m a))
        mtail = snd <$> x

    in ((toList . (mhead Seq.<|)) <$> mtail)
       ===
       case span (empty ==) as of
         -- if all 'as' entries where 'empty'
         (_, []) -> empty
         -- otherwise take the first element of `as'`, then list all the empty
         -- elements from start of `as`, then the rest of `as'`.
         (empties, (a : as')) -> pure (a : empties ++ as')
