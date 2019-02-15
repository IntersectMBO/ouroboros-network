{-# LANGUAGE ScopedTypeVariables #-}
module Test.Ouroboros.Network.Testing.Utils where

import           Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Write as CBOR
import           Data.ByteString (ByteString)

import           Protocol.Channel

import           Control.Monad.Class.MonadSTM


tmvarChannels
  :: forall m. MonadSTM m
  => m (Duplex m m Encoding ByteString, Duplex m m Encoding ByteString)
tmvarChannels = do
  left  <- atomically newEmptyTMVar
  right <- atomically newEmptyTMVar
  let
    leftChan  = uniformDuplex (atomically . putTMVar left . CBOR.toStrictByteString)  (fmap Just (atomically $ takeTMVar right))
    rightChan = uniformDuplex (atomically . putTMVar right . CBOR.toStrictByteString) (fmap Just (atomically $ takeTMVar left))
  pure (leftChan, rightChan)
