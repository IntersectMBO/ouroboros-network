{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Protocol.Chain.Codec.Id where

import Data.Text (pack)

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
     Codec m
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           'StInit
codecChainExchange = codecInit

codecInit
  :: Applicative m =>
     Codec m
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           'StInit
codecInit = Codec
  { encode = Encoder $ \tr -> case tr of
      TrInit _ -> Encoded (SomeTransition tr) codecIdle
  , decode = Decoder $ pure $ Partial $ \mtr -> case mtr of
      Just (SomeTransition tr@(TrInit _)) -> Decoder $ pure $ Done Nothing (Decoded tr codecIdle)
      _ -> Decoder $ pure $ Fail Nothing (pack "expected TrInit")
  }


codecIdle
  :: Applicative m =>
     Codec m
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
  , decode = Decoder $ pure $ Partial $ \mtr -> case mtr of
      Just (SomeTransition tr@(TrRequest req)) -> case req of
        ReqImprove _  -> Decoder $ pure $ Done Nothing (Decoded tr codecBusy_Improve)
        ReqDownload _ -> Decoder $ pure $ Done Nothing (Decoded tr codecBusy_Download)
        ReqNext       -> Decoder $ pure $ Done Nothing (Decoded tr codecBusy_Next)
      Just (SomeTransition TrConsumerDone) ->
        Decoder $ pure $ Done Nothing (Decoded TrConsumerDone codecDone)
      _ -> Decoder $ pure $ Fail Nothing (pack "expected TrRequest or TrConsumerDone")
  }

codecBusy_Improve
  :: Applicative m =>
     Codec m
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           ('StBusy 'Improve)
codecBusy_Improve = Codec
  { encode = Encoder $ \tr -> case tr of
      TrRespond (ResImprove _) -> Encoded (SomeTransition tr) codecIdle
      TrRespond (ResForked _)  -> Encoded (SomeTransition tr) codecIdle
  , decode = Decoder $ pure $ Partial $ \mtr -> case mtr of
      Just (SomeTransition tr@(TrRespond (ResImprove _))) ->
        Decoder $ pure $ Done Nothing (Decoded tr codecIdle)
      Just (SomeTransition (TrRespond (ResForked it))) ->
        Decoder $ pure $ Done Nothing (Decoded (TrRespond (ResForked it)) codecIdle)
      _ -> Decoder $ pure $ Fail Nothing (pack "expected improve or fork")
  }

codecBusy_Download
  :: Applicative m =>
     Codec m
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           ('StBusy 'Download)
codecBusy_Download = Codec
  { encode = Encoder $ \tr -> case tr of
      TrRespond (ResDownloadOne _)  -> Encoded (SomeTransition tr) codecBusy_Download
      TrRespond (ResDownloadDone _) -> Encoded (SomeTransition tr) codecIdle
      TrRespond (ResForked _)       -> Encoded (SomeTransition tr) codecIdle
  , decode = Decoder $ pure $ Partial $ \mtr -> case mtr of
      Just (SomeTransition tr@(TrRespond (ResDownloadOne _))) ->
        Decoder $ pure $ Done Nothing (Decoded tr codecBusy_Download)
      Just (SomeTransition tr@(TrRespond (ResDownloadDone _))) ->
        Decoder $ pure $ Done Nothing (Decoded tr codecIdle)
      Just (SomeTransition (TrRespond (ResForked it))) ->
        Decoder $ pure $ Done Nothing (Decoded (TrRespond (ResForked it)) codecIdle)
      _ -> Decoder $ pure $ Fail Nothing (pack "expected download or fork")
  }


codecBusy_Next
  :: Applicative m =>
     Codec m
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           ('StBusy 'Next)
codecBusy_Next = Codec
  { encode = Encoder $ \tr -> case tr of
      TrRespond (ResExtend _) -> Encoded (SomeTransition tr) codecIdle
      TrRespond (ResForked _) -> Encoded (SomeTransition tr) codecIdle
      TrProducerDone          -> Encoded (SomeTransition tr) codecDone
  , decode = Decoder $ pure $ Partial $ \mtr -> case mtr of
      Just (SomeTransition tr@(TrRespond (ResExtend _))) ->
        Decoder $ pure $ Done Nothing (Decoded tr codecIdle)
      Just (SomeTransition (TrRespond (ResForked it))) ->
        Decoder $ pure $ Done Nothing (Decoded (TrRespond (ResForked it)) codecIdle)
      Just (SomeTransition TrProducerDone) ->
        Decoder $ pure $ Done Nothing (Decoded TrProducerDone codecDone)
      _ -> Decoder $ pure $ Fail Nothing (pack "expected extend or fork or done")
  }


codecDone
  :: Applicative m =>
     Codec m
           (SomeTransition (TrChainExchange point header))
           (SomeTransition (TrChainExchange point header))
           (TrChainExchange point header)
           'StDone
codecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Decoder $ pure $ Fail Nothing (pack "expected some transition")
  }
