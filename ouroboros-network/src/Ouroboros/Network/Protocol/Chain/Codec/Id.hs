{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Protocol.Chain.Codec.Id where

import Data.Text (Text, pack)

import Protocol.Codec
import Protocol.Transition

import Ouroboros.Network.Protocol.Chain.Type

-- | A codec for the chain exchange protocol in which the concrete encode and
-- decode types are 'SomeTransition (TrChainExchange point header)'.
-- Encoding is simple: wrap the relevant transition in 'SomeTransition'.
-- Decoding amounts to pattern matching to find those 'SomeTransition's which
-- have the appropriate initial state type.
codecChainExchange
  :: Applicative m =>
     Codec m Text
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           'StInit
codecChainExchange = codecInit

type DecoderAt point header m at =
  Decoder Text (SomeTransition (TrChainExchange point header)) m
          (Decoded (TrChainExchange point header) at (Codec m Text (SomeTransition (TrChainExchange point header)) (SomeTransition (TrChainExchange point header)) (TrChainExchange point header)))

codecInit
  :: forall m point header .
     ( Applicative m )
  => Codec m Text
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           'StInit
codecInit = Codec
  { encode = Encoder $ \tr -> case tr of
      TrInit _ -> Encoded (SomeTransition tr) codecIdle
  , decode =
      let loop :: DecoderAt point header m 'StInit
          loop = Fold $ pure $ Partial $ Response
            { end  = pure $ Left $ pack "expected TrInit"
            , more = \trs -> Fold $ case trs of
                SomeTransition tr@(TrInit _) : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded tr codecIdle
                _ : rest ->
                  pure $ Complete rest $ pure $ Left $ pack "expected TrInit"
                [] -> runFold loop
            }
      in loop
  }

codecIdle
  :: forall m point header .
     ( Applicative m )
  => Codec m Text
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           'StIdle
codecIdle = Codec
  { encode = Encoder $ \tr -> case tr of
      TrRequest (ReqImprove _)  -> Encoded (SomeTransition tr) codecBusy_Improve
      TrRequest (ReqDownload _) -> Encoded (SomeTransition tr) codecBusy_Download
      TrRequest (ReqNext)       -> Encoded (SomeTransition tr) codecBusy_Next
      TrConsumerDone            -> Encoded (SomeTransition tr) codecDone
  , decode =
      let loop :: DecoderAt point header m 'StIdle
          loop = Fold $ pure $ Partial $ Response
            { end  = pure $ Left $ pack "expected TrRequest or TrConsumerDone"
            , more = \trs -> Fold $ case trs of
                SomeTransition tr@(TrRequest req) : rest -> pure $ case req of
                  ReqImprove _  -> Complete rest $ pure $ Right $ Decoded tr codecBusy_Improve
                  ReqDownload _ -> Complete rest $ pure $ Right $ Decoded tr codecBusy_Download
                  ReqNext       -> Complete rest $ pure $ Right $ Decoded tr codecBusy_Next
                SomeTransition TrConsumerDone : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded TrConsumerDone codecDone
                _ : rest ->
                  pure $ Complete rest $ pure $ Left $ pack "expected TrRequest or TrConsumerDone"
                [] -> runFold loop
            }
      in loop
  }

codecBusy_Improve
  :: forall m point header .
     ( Applicative m )
  => Codec m Text
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           ('StBusy 'Improve)
codecBusy_Improve = Codec
  { encode = Encoder $ \tr -> case tr of
      TrRespond (ResImprove _) -> Encoded (SomeTransition tr) codecIdle
      TrRespond (ResForked _)  -> Encoded (SomeTransition tr) codecIdle
  , decode =
      let loop :: DecoderAt point header m ('StBusy 'Improve)
          loop = Fold $ pure $ Partial $ Response
            { end  = pure $ Left $ pack "expected improve or fork"
            , more = \trs -> Fold $ case trs of
                SomeTransition tr@(TrRespond (ResImprove _)) : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded tr codecIdle
                SomeTransition (TrRespond (ResForked it)) : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded (TrRespond (ResForked it)) codecIdle
                _ : rest ->
                  pure $ Complete rest $ pure $ Left $ pack "expected improve or fork"
                [] -> runFold loop
            }
      in loop
  }

codecBusy_Download
  :: forall m point header .
     ( Applicative m )
  => Codec m Text
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           ('StBusy 'Download)
codecBusy_Download = Codec
  { encode = Encoder $ \tr -> case tr of
      TrRespond (ResDownloadOne _)  -> Encoded (SomeTransition tr) codecBusy_Download
      TrRespond (ResDownloadDone _) -> Encoded (SomeTransition tr) codecIdle
      TrRespond (ResForked _)       -> Encoded (SomeTransition tr) codecIdle
  , decode =
      let loop :: DecoderAt point header m ('StBusy 'Download)
          loop = Fold $ pure $ Partial $ Response
            { end  = pure $ Left $ pack "expected download or fork"
            , more = \trs -> Fold $ case trs of
                SomeTransition tr@(TrRespond (ResDownloadOne _)) : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded tr codecBusy_Download
                SomeTransition tr@(TrRespond (ResDownloadDone _)) : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded tr codecIdle
                SomeTransition (TrRespond (ResForked it)) : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded (TrRespond (ResForked it)) codecIdle
                _ : rest ->
                  pure $ Complete rest $ pure $ Left $ pack "expected download or fork"
                [] -> runFold loop
            }
      in loop
  }


codecBusy_Next
  :: forall m point header .
     ( Applicative m )
  => Codec m Text
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           ('StBusy 'Next)
codecBusy_Next = Codec
  { encode = Encoder $ \tr -> case tr of
      TrRespond (ResExtend _) -> Encoded (SomeTransition tr) codecIdle
      TrRespond (ResForked _) -> Encoded (SomeTransition tr) codecIdle
      TrProducerDone          -> Encoded (SomeTransition tr) codecDone
  , decode =
      let loop :: DecoderAt point header m ('StBusy 'Next)
          loop = Fold $ pure $ Partial $ Response
            { end  = pure $ Left $ pack "expected extend or fork or done"
            , more = \trs -> Fold $ case trs of
                SomeTransition tr@(TrRespond (ResExtend _)) : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded tr codecIdle
                SomeTransition (TrRespond (ResForked it)) : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded (TrRespond (ResForked it)) codecIdle
                SomeTransition TrProducerDone : rest ->
                  pure $ Complete rest $ pure $ Right $ Decoded TrProducerDone codecDone
                _ : rest ->
                  pure $ Complete rest $ pure $ Left $ pack "expected extend or fork or done"
                [] -> runFold loop
            }
      in loop
  }


codecDone
  :: Applicative m =>
     Codec m Text
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           'StDone
codecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Fold $ pure $ Complete [] $ pure $ Left $ pack "expected some transition"
  }
