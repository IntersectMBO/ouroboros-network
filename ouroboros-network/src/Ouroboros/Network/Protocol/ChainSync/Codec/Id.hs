{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Protocol.ChainSync.Codec.Id where

import Data.Text (Text, pack)

import Protocol.Codec
import Protocol.Transition

import Ouroboros.Network.Protocol.ChainSync.Type

type DecoderAt point header m state =
  Decoder Text (SomeTransition (ChainSyncMessage point header)) m
    (Decoded (ChainSyncMessage point header) state
      (Codec m Text (SomeTransition (ChainSyncMessage point header)) (SomeTransition (ChainSyncMessage point header)) (ChainSyncMessage point header)))

-- | A codec for the chain exchange protocol in which the concrete encode and
-- decode types are 'SomeTransition (TrChainExchange point header)'.
-- Encoding is simple: wrap the relevant transition in 'SomeTransition'.
-- Decoding amounts to pattern matching to find those 'SomeTransition's which
-- have the appropriate initial state type.
codecChainSync
  :: Applicative m =>
     Codec m Text
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           StIdle
codecChainSync = codecIdle

codecIdle
  :: forall m point header .
     ( Applicative m )
  => Codec m Text
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           StIdle
codecIdle = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgRequestNext     -> Encoded (SomeTransition tr) codecNext_CanAwait
      MsgFindIntersect _ -> Encoded (SomeTransition tr) codecIntersect
      MsgDone            -> Encoded (SomeTransition tr) codecDone
  , decode =
      let loop :: DecoderAt point header m StIdle
          loop = Fold $ pure $ Partial $ Response
            { end  = pure $ Left $ pack "expected request"
            , more = \trs -> case trs of
                SomeTransition MsgRequestNext : rest ->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded MsgRequestNext codecNext_CanAwait
                SomeTransition tr@(MsgFindIntersect _) : rest->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded tr codecIntersect
                SomeTransition MsgDone : rest ->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded MsgDone codecDone
                _ : rest -> Fold $ pure $ Complete rest $ pure $ Left $ pack "expected request"
                [] -> loop
            }
      in loop
  }

codecNext_CanAwait
  :: forall m point header .
     ( Applicative m )
  => Codec m Text
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           (StNext StCanAwait)
codecNext_CanAwait = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgAwaitReply -> Encoded (SomeTransition tr) codecNext_MustReply
      MsgRollForward _ _ -> Encoded (SomeTransition tr) codecIdle
      MsgRollBackward _ _ -> Encoded (SomeTransition tr) codecIdle
  , decode =
      let loop :: DecoderAt point header m (StNext StCanAwait)
          loop = Fold $ pure $ Partial $ Response
            { end  = pure $ Left $ pack "expected can await response"
            , more = \trs -> case trs of
                SomeTransition MsgAwaitReply : rest ->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded MsgAwaitReply codecNext_MustReply
                SomeTransition (MsgRollForward a b) : rest ->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded (MsgRollForward a b) codecIdle
                SomeTransition (MsgRollBackward a b) : rest ->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded (MsgRollBackward a b) codecIdle
                _ : rest ->
                  Fold $ pure $ Complete rest $ pure $ Left $ pack "expected can await response"
                [] -> loop
            }
      in loop
  }

codecNext_MustReply
  :: forall m point header .
     ( Applicative m )
  => Codec m Text
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           (StNext StMustReply)
codecNext_MustReply = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgRollForward _ _ -> Encoded (SomeTransition tr) codecIdle
      MsgRollBackward _ _ -> Encoded (SomeTransition tr) codecIdle
  , decode =
      let loop :: DecoderAt point header m (StNext StMustReply)
          loop = Fold $ pure $ Partial $ Response
            { end  = pure $ Left $ pack "expected must reply response"
            , more = \trs -> case trs of
                SomeTransition (MsgRollForward a b) : rest ->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded (MsgRollForward a b) codecIdle
                SomeTransition (MsgRollBackward a b) : rest ->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded (MsgRollBackward a b) codecIdle
                _ : rest ->
                  Fold $ pure $ Complete rest $ pure $ Left $ pack "expected must reply response"
                [] -> loop
            }
      in loop
  }

codecIntersect
  :: forall m point header .
     ( Applicative m )
  => Codec m Text
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           StIntersect
codecIntersect = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgIntersectImproved _ _ -> Encoded (SomeTransition tr) codecIdle
      MsgIntersectUnchanged _ -> Encoded (SomeTransition tr) codecIdle
  , decode =
      let loop :: DecoderAt point header m StIntersect
          loop = Fold $ pure $ Partial $ Response
            { end  = pure $ Left $ pack "expected improvement response"
            , more = \trs -> case trs of
                SomeTransition tr@(MsgIntersectImproved _ _) : rest ->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded tr codecIdle
                SomeTransition tr@(MsgIntersectUnchanged _) : rest ->
                  Fold $ pure $ Complete rest $ pure $ Right $ Decoded tr codecIdle
                _ : rest ->
                  Fold $ pure $ Complete rest $ pure $ Left $ pack "expected improvement response"
                [] -> loop
            }
      in loop
  }

codecDone
  :: Applicative m =>
     Codec m Text
           (SomeTransition (ChainSyncMessage point header))
           (SomeTransition (ChainSyncMessage point header))
           (ChainSyncMessage point header)
           StDone
codecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Fold $ pure $ Complete [] $ pure $ Left $ pack "expected no more transitions"
  }
