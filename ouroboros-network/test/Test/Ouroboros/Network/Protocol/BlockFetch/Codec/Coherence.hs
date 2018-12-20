-- FIXME: AllowAmbiguousTypes should not be needed.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC "-Wno-unticked-promoted-constructors" #-}
module Test.Ouroboros.Network.Protocol.BlockFetch.Codec.Coherence where

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

import Ouroboros.Network.Protocol.Codec.Cbor (convertCborCodec')
import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.BlockFetch.Codec.Cbor


prop_block_fetch_client_cbor_coherent
  :: forall range.
     ( Serialise range
     , Show range
     , Arbitrary range
     )
  => Property
prop_block_fetch_client_cbor_coherent = forAllShow genPath (showSomeTransitionPath show) doTest
 where
  doTest
    :: SomeTransitionPath (BlockRequestClientMessage range) StClientIdle
    -> Property
  doTest (SomeTransitionPath path) =
    prop_coherent (==) show show show (runST (codecTree decodeFull eqTr (convertCborCodec' blockFetchClientCodec) path))

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

prop_block_fetch_server_cbor_coherent
  :: forall block.
     ( Serialise block
     , Show block
     , Arbitrary block
     )
  => Property
prop_block_fetch_server_cbor_coherent = forAllShow genPath (showSomeTransitionPath show) doTest
 where
  doTest
    :: SomeTransitionPath (BlockRequestServerMessage block) StServerAwaiting
    -> Property
  doTest (SomeTransitionPath path) =
    prop_coherent (==) show show show (runST (codecTree decodeFull eqTr (convertCborCodec' blockFetchServerCodec) path))

  -- TODO: generate paths which send multiple batches

  -- either send @'MessgeNoBlocks'@ followed by @'MessageServerDone'@
  -- or if the list is non empty @'MessageStartBatch'@, @'MessageBlock'@
  -- (repeated over all the blocks), followed by @'MessageBatchDone'@ and @'MessageServerDone'@
  blocksPath :: [block] -> SomeTransitionPath (BlockRequestServerMessage block) StServerAwaiting
  blocksPath []     = SomeTransitionPath $ PathCons $ Transition MessageNoBlocks $ PathCons $ Transition MessageServerDone $ PathNil
  blocksPath blocks = case go blocks of
    SomeTransitionPath it -> SomeTransitionPath $ PathCons $ Transition MessageStartBatch it
   where
    go :: [block]
      -> SomeTransitionPath (BlockRequestServerMessage block) StServerSending
    go []             = SomeTransitionPath $ PathCons $ Transition MessageBatchDone $ PathCons $ Transition MessageServerDone $ PathNil
    go (block : rest) = case go rest of
      SomeTransitionPath it -> SomeTransitionPath $ PathCons $ Transition (MessageBlock block) it

  -- like 'blockPath', but ends block transition with @'MessageServerError'@
  -- rather than @'MessageBatchDone'@
  errorPath :: [block] -> SomeTransitionPath (BlockRequestServerMessage block) StServerAwaiting
  errorPath []     = SomeTransitionPath $ PathCons $ Transition MessageNoBlocks $ PathCons $ Transition MessageServerDone $ PathNil
  errorPath blocks = case go blocks of
    SomeTransitionPath it -> SomeTransitionPath $ PathCons $ Transition MessageStartBatch it
   where
    go :: [block]
      -> SomeTransitionPath (BlockRequestServerMessage block) StServerSending
    go []             = SomeTransitionPath $ PathCons $ Transition MessageServerError $ PathCons $ Transition MessageServerDone $ PathNil
    go (block : rest) = case go rest of
      SomeTransitionPath it -> SomeTransitionPath $ PathCons $ Transition (MessageBlock block) it

  genPath :: Gen (SomeTransitionPath (BlockRequestServerMessage block) StServerAwaiting)
  genPath = oneof
    [ blocksPath <$> resize 5 (listOf arbitrary)
    , errorPath  <$> resize 5 (listOf arbitrary)
    ]

  eqTr :: forall from to to' .
          BlockRequestServerMessage block from to
       -> BlockRequestServerMessage block from to'
       -> Maybe (to :~: to')
  eqTr MessageStartBatch  MessageStartBatch  = Just Refl
  eqTr (MessageBlock _)   (MessageBlock _)   = Just Refl
  eqTr MessageBatchDone   MessageBatchDone   = Just Refl
  eqTr MessageServerError MessageServerError = Just Refl
  eqTr MessageNoBlocks    MessageNoBlocks    = Just Refl
  eqTr MessageServerDone  MessageServerDone  = Just Refl
  eqTr _                  _                  = Nothing

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
tests = testGroup "BlockFetchProtocol.Codec.Cbor"
  [ testProperty "BlockFetchClientProtocol: coherent cbor codec" (prop_block_fetch_client_cbor_coherent @(Int, Int))
  , testProperty "BlockFetchServerProtcol: coher cbor codec"
    (prop_block_fetch_server_cbor_coherent @Int)
  ]
