-- FIXME: AllowAmbiguousTypes should not be needed.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC "-Wno-unticked-promoted-constructors" #-}
module Test.Ouroboros.Network.Protocol.BlockFetch.Codec where

import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Type.Equality
import Data.Text (Text, pack)

import Codec.Serialise.Class (Serialise)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Protocol.Codec
import Protocol.Codec.Coherent
import Protocol.Path

import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.BlockFetch.Codec.Cbor


prop_block_fetch_client_cbor_coherent
  :: forall range.
     ( Serialise range
     , Show range
     , Arbitrary range
     )
  => Property
prop_block_fetch_client_cbor_coherent = forAllShow genPath showPath doTest
 where
  showPath
    :: (SomeTransitionPath (BlockRequestClientMessage range) StClientIdle)
    -> String
  showPath (SomeTransitionPath path) = foldPath fn "" path
   where
    fn :: (k -> String)
       -> Transition (BlockRequestClientMessage range) k states
       -> String
    fn g (Transition tr next) = show tr ++ g next

  doTest
    :: SomeTransitionPath (BlockRequestClientMessage range) StClientIdle
    -> Property
  doTest (SomeTransitionPath path) =
    prop_coherent (==) show show show (runST (codecTree decodeFull eqTr blockFetchClientCodec' path))

  -- Every path from StClientIdle is uniquelly determined by a list of ranges
  pathFromList
    :: [range]
    -> SomeTransitionPath (BlockRequestClientMessage range) StClientIdle
  pathFromList [] = SomeTransitionPath $ PathCons $ Transition MessageDone PathNil
  pathFromList (range : rest) = case pathFromList rest of
    SomeTransitionPath it -> SomeTransitionPath $ PathCons $ Transition (MessageRequestRange range) it

  genPath :: Gen (SomeTransitionPath (BlockRequestClientMessage range) StClientIdle)
  genPath = pathFromList <$> resize 5 (listOf arbitrary)

  eqTr :: forall from to to' .
          BlockRequestClientMessage range from to
       -> BlockRequestClientMessage range from to'
       -> Maybe (to :~: to')
  eqTr MessageRequestRange{} MessageRequestRange{} = Just Refl
  eqTr MessageDone           MessageDone           = Just Refl
  eqTr _                     _                     = Nothing

  decodeFull :: Monad m => ByteString -> Decoder Text ByteString m x -> m (Either Text x)
  decodeFull str decoder = do
    (it, remaining) <- foldOverInput decoder (singletonInput [str])
    case remaining of
      Nothing -> pure it
      -- If there's more input, we'll drain it. giving a list of all of the
      -- remaining inputs. Those inputs happen to be of type [String], so
      -- we mconcat it twice to get the entire remaining input as a String.
      Just input -> drainInput input >>= \lst ->
        let rest = mconcat (mconcat lst)
        in if BS.null rest
             then pure it
             else pure $ Left $ pack $ "decoder did not use all of its input: " ++ (show rest)

tests :: TestTree
tests = testGroup "BlockFetchClientProtocol.Codec.Cbor"
  [ testProperty "coherent codec" (prop_block_fetch_client_cbor_coherent @(Int, Int))
  ]
