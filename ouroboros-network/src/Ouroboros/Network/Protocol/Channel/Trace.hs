module Ouroboros.Network.Protocol.Channel.Trace where

import Cardano.BM.BaseTrace

import Protocol.Channel

-- TBD log before or after? Before seems better...
traceSend
  :: ( Applicative m, Functor n )
  => BaseTrace m send
  -> Duplex m n send recv
  -> Duplex m n send recv
traceSend bt duplex = duplex
  { send = \item -> traceWith bt item *> fmap (traceSend bt) (send duplex item)
  , recv = (fmap . fmap) (traceSend bt) (recv duplex)
  }

-- | Whereas 'traceSend' needs only `Applicative m`, 'traceRecv' needs
-- `Monad m` because it traces a thing produced within `m`.
traceRecv
  :: ( Monad m, Functor n )
  => BaseTrace m (Maybe recv) -- ^ Nothing means end of stream.
  -> Duplex n m send recv
  -> Duplex n m send recv
traceRecv bt duplex = duplex
  { send = fmap (traceRecv bt) . send duplex
  , recv = recv duplex >>= \(item, duplex') ->
      traceWith bt item >> pure (item, traceRecv bt duplex')
  }
