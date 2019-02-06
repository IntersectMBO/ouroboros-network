{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Ouroboros.Network.Testing.Utils where

import           Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Write as CBOR
import           Data.List (foldl')
import           Data.ByteString (ByteString)

import           Protocol.Channel

import           Control.Monad.ST.Lazy (runST)
import           Control.Monad.IOSim (SimM)
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

-- | Run an experiment in @'SimM'@ monad and extract @'Property'@.
--
runPropSimM
  :: (forall s. Probe (SimM s) Property -> SimM s ())
  -> Property
runPropSimM exp_ = runST $ runExperiment exp_

-- | Run an experiment in @'IO'@ monad and extract @'Property'@.
--
runPropIO
  :: (Probe IO Property -> IO ())
  -> Property
runPropIO = ioProperty . runExperiment

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
