{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Protocol.ChainSync.Codec.Id where

import Data.Text (pack)

import Protocol.Codec
import Protocol.Transition

import Ouroboros.Network.Protocol.ChainSync.Type

-- | A codec for the chain exchange protocol in which the concrete encode and
-- decode types are 'SomeTransition (TrChainExchange point header)'.
-- Encoding is simple: wrap the relevant transition in 'SomeTransition'.
-- Decoding amounts to pattern matching to find those 'SomeTransition's which
-- have the appropriate initial state type.
codecChainSync
  :: Applicative m =>
     Codec m
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           'StIdle
codecChainSync = codecIdle


codecIdle
  :: Applicative m =>
     Codec m
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           'StIdle
codecIdle = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgRequestNext     -> Encoded (SomeTransition tr) codecNext_CanAwait
      MsgFindIntersect _ -> Encoded (SomeTransition tr) codecIntersect
      MsgDone            -> Encoded (SomeTransition tr) codecDone
  , decode = Decoder $ pure $ Partial $ \mtr -> case mtr of
      Just (SomeTransition MsgRequestNext) ->
        Decoder $ pure $ Done Nothing (Decoded MsgRequestNext codecNext_CanAwait)
      Just (SomeTransition tr@(MsgFindIntersect _)) ->
        Decoder $ pure $ Done Nothing (Decoded tr codecIntersect)
      Just (SomeTransition MsgDone) ->
        Decoder $ pure $ Done Nothing (Decoded MsgDone codecDone)
      _ -> Decoder $ pure $ Fail Nothing (pack "expected request")
  }

codecNext_CanAwait
  :: Applicative m =>
     Codec m
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           ('StNext 'StCanAwait)
codecNext_CanAwait = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgAwaitReply -> Encoded (SomeTransition tr) codecNext_MustReply
      MsgRollForward _ _ -> Encoded (SomeTransition tr) codecIdle
      MsgRollBackward _ _ -> Encoded (SomeTransition tr) codecIdle
  , decode = Decoder $ pure $ Partial $ \mtr -> case mtr of
      Just (SomeTransition MsgAwaitReply) ->
        Decoder $ pure $ Done Nothing (Decoded MsgAwaitReply codecNext_MustReply)
      Just (SomeTransition (MsgRollForward a b)) ->
        Decoder $ pure $ Done Nothing (Decoded (MsgRollForward a b) codecIdle)
      Just (SomeTransition (MsgRollBackward a b)) ->
        Decoder $ pure $ Done Nothing (Decoded (MsgRollBackward a b) codecIdle)
      _ -> Decoder $ pure $ Fail Nothing (pack "expected can await response")
  }

codecNext_MustReply
  :: Applicative m =>
     Codec m
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           ('StNext 'StMustReply)
codecNext_MustReply = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgRollForward _ _ -> Encoded (SomeTransition tr) codecIdle
      MsgRollBackward _ _ -> Encoded (SomeTransition tr) codecIdle
  , decode = Decoder $ pure $ Partial $ \mtr -> case mtr of
      Just (SomeTransition (MsgRollForward a b)) ->
        Decoder $ pure $ Done Nothing (Decoded (MsgRollForward a b) codecIdle)
      Just (SomeTransition (MsgRollBackward a b)) ->
        Decoder $ pure $ Done Nothing (Decoded (MsgRollBackward a b) codecIdle)
      _ -> Decoder $ pure $ Fail Nothing (pack "expected must reply response")
  }

codecIntersect
  :: Applicative m =>
     Codec m
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           'StIntersect
codecIntersect = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgIntersectImproved _ _ -> Encoded (SomeTransition tr) codecIdle
      MsgIntersectUnchanged _ -> Encoded (SomeTransition tr) codecIdle
  , decode = Decoder $ pure $ Partial $ \mtr -> case mtr of
      Just (SomeTransition tr@(MsgIntersectImproved _ _)) ->
        Decoder $ pure $ Done Nothing (Decoded tr codecIdle)
      Just (SomeTransition tr@(MsgIntersectUnchanged _)) ->
        Decoder $ pure $ Done Nothing (Decoded tr codecIdle)
      _ -> Decoder $ pure $ Fail Nothing (pack "expected improvement response")
  }

codecDone
  :: Applicative m =>
     Codec m
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           'StDone
codecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Decoder $ pure $ Fail Nothing (pack "expected no more transitions")
  }
