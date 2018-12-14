{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Test.Protocol.Codec.PingPong where

import Data.Functor.Identity (runIdentity)
import Data.Type.Equality
import Data.Text (Text, pack)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Protocol.Codec
import Protocol.Path

import Protocol.PingPong.Codec
import Protocol.PingPong.Type

import Test.Protocol.Codec.Coherent

prop_ping_pong_coherent :: Property
prop_ping_pong_coherent = forAllShow genPath showPath doTest
  where

  -- TODO proper show. Can use 'showTr' to get this.
  showPath = \_ -> "some path"

  doTest :: SomeTransitionPath PingPongMessage 'StIdle -> Property
  doTest (SomeTransitionPath path) =
    prop_coherent (==) show show show (runIdentity (codecTree decodeFull eqTr pingPongCodec path))

  -- PingPongMessage is so simple that every path from 'StIdle is determined
  -- by a non-negative number.
  pathOfLength :: Word -> SomeTransitionPath PingPongMessage 'StIdle
  pathOfLength 0 = SomeTransitionPath $ PathCons $ Transition MsgDone PathNil
  pathOfLength n = case pathOfLength (n-1) of
    SomeTransitionPath it -> SomeTransitionPath $ PathCons $ Transition MsgPing $
      PathCons $ Transition MsgPong $ it

  -- To generate a path, just pick a non-negative number.
  -- Don't pick one that's too big; the coherence test is space hungry; it
  -- deals with all possible paths through the codec (2^{length of path}).
  -- FIXME we could ease this restriction by randomly testing decoder paths.
  genPath = pathOfLength <$> choose (0, 5)

  eqTr :: forall from to to' .
          PingPongMessage from to
       -> PingPongMessage from to'
       -> Maybe (to :~: to')
  eqTr MsgPing MsgPing = Just Refl
  eqTr MsgPong MsgPong = Just Refl
  eqTr MsgDone MsgDone = Just Refl
  eqTr _       _       = Nothing

  decodeFull :: Monad m => String -> Decoder Text String m x -> m (Either Text x)
  decodeFull str decoder = do
    (it, remaining) <- foldOverInput decoder (singletonInput [str])
    case remaining of
      Nothing -> pure it
      -- If there's more input, we'll drain it. giving a list of all of the
      -- remaining inputs. Those inputs happen to be of type [String], so
      -- we mconcat it twice to get the entire remaining input as a String.
      Just input -> drainInput input >>= \lst -> case mconcat (mconcat lst) of
        []    -> pure it
        bad@(_ : _) -> pure $ Left $ pack $ "decoder did not use all of its input: " ++ bad

tests :: TestTree
tests = testGroup "PingPong"
  [ testProperty "coherent codec" prop_ping_pong_coherent
  ]
