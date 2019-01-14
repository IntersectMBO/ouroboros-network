{-# LANGUAGE ScopedTypeVariables #-}
module Test.Ouroboros.Network.Testing.Utils where

import           Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Write as CBOR
import           Data.List (foldl')
import           Data.ByteString (ByteString)

import           Protocol.Channel

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadProbe

import           Test.QuickCheck


-- | FIXME: find a better place for it, this might be useful elsewhere too.
runExperiment
  :: forall m n.
     ( MonadSTM m
     , MonadTimer m
     , MonadProbe m
     , MonadRunProbe m n
     )
  => (Probe m Property -> m ())
  -> n Property
runExperiment exp_ = isValid <$> withProbe exp_
 where
  isValid :: [(Time m, Property)] -> Property
  isValid = foldl' (\acu (_,p) -> acu .&&. p) (property True)

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
