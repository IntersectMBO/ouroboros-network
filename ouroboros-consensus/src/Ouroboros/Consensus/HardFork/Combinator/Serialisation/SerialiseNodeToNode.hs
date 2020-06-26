{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToNode (
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.Serialise as Serialise
import           Control.Exception (throw)
import           Data.Proxy
import           Data.SOP.Strict

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseDisk
                     ()
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))

instance SerialiseHFC xs => SerialiseNodeToNodeConstraints (HardForkBlock xs)

{-------------------------------------------------------------------------------
  Dispatch to first era or HFC
-------------------------------------------------------------------------------}

dispatchEncoder :: forall f xs. (
                     SerialiseHFC xs
                   , forall blk. SerialiseNodeToNodeConstraints blk
                              => SerialiseNodeToNode blk (f blk)
                   )
                => CodecConfig (HardForkBlock xs)
                -> BlockNodeToNodeVersion (HardForkBlock xs)
                -> NS f xs -> Encoding
dispatchEncoder ccfg version ns =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty _ ->
        case (ccfgs, version, ns) of
          (c0 :* _, HardForkNodeToNodeDisabled v0, Z x0) ->
            encodeNodeToNode c0 (unwrapNodeToNodeVersion v0) x0
          (_, HardForkNodeToNodeDisabled _, S later) ->
            throw $ futureEraException (notFirstEra later)
          (_, HardForkNodeToNodeEnabled versions, _) ->
            encodeNS (hczipWith pSHFC aux ccfgs versions) ns
  where
    ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

    aux :: SerialiseNodeToNodeConstraints blk
        => CodecConfig blk
        -> WrapNodeToNodeVersion blk
        -> (f -.-> K Encoding) blk
    aux ccfg' (WrapNodeToNodeVersion v) = Fn $ K . encodeNodeToNode ccfg' v

dispatchDecoder :: forall f xs. (
                     SerialiseHFC xs
                   , forall blk. SerialiseNodeToNodeConstraints blk
                              => SerialiseNodeToNode blk (f blk)
                   )
                => CodecConfig (HardForkBlock xs)
                -> BlockNodeToNodeVersion (HardForkBlock xs)
                -> forall s. Decoder s (NS f xs)
dispatchDecoder ccfg version =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty _ ->
        case (ccfgs, version) of
          (c0 :* _, HardForkNodeToNodeDisabled v0) ->
            Z <$> decodeNodeToNode c0 (unwrapNodeToNodeVersion v0)
          (_, HardForkNodeToNodeEnabled versions) ->
            decodeNS (hczipWith pSHFC aux ccfgs versions)
  where
    ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

    aux :: SerialiseNodeToNodeConstraints blk
        => CodecConfig blk
        -> WrapNodeToNodeVersion blk
        -> (Decoder s :.: f) blk
    aux ccfg' (WrapNodeToNodeVersion v) = Comp $ decodeNodeToNode ccfg' v

after :: (a -> b -> d -> e) -> (c -> d) -> a -> b -> c -> e
after f g x y z = f x y (g z)

{-------------------------------------------------------------------------------
  Blocks/headers
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => SerialiseNodeToNode (HardForkBlock xs) (HardForkBlock xs) where
  encodeNodeToNode ccfg _ = wrapCBORinCBOR   (encodeDiskHfcBlock ccfg)
  decodeNodeToNode ccfg _ = unwrapCBORinCBOR (decodeDiskHfcBlock ccfg)

instance SerialiseHFC xs
      => SerialiseNodeToNode (HardForkBlock xs) (Header (HardForkBlock xs)) where
  encodeNodeToNode = dispatchEncoder `after` (getOneEraHeader . getHardForkHeader)
  decodeNodeToNode = fmap (HardForkHeader . OneEraHeader) .: dispatchDecoder

{-------------------------------------------------------------------------------
  Serialised blocks/headers
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => SerialiseNodeToNode (HardForkBlock xs) (Serialised (HardForkBlock xs)) where
  encodeNodeToNode _ _ = Serialise.encode
  decodeNodeToNode _ _ = Serialise.decode

instance SerialiseHFC xs
      => SerialiseNodeToNode (HardForkBlock xs) (SerialisedHeader (HardForkBlock xs)) where
  encodeNodeToNode = dispatchEncoder `after` distribSerialisedHeader
  decodeNodeToNode = fmap undistribSerialisedHeader .: dispatchDecoder

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => SerialiseNodeToNode (HardForkBlock xs) (GenTx (HardForkBlock xs)) where
  encodeNodeToNode = dispatchEncoder `after` (getOneEraGenTx . getHardForkGenTx)
  decodeNodeToNode = fmap (HardForkGenTx . OneEraGenTx) .: dispatchDecoder

instance SerialiseHFC xs
      => SerialiseNodeToNode (HardForkBlock xs) (GenTxId (HardForkBlock xs)) where
  encodeNodeToNode = dispatchEncoder `after` (getOneEraGenTxId . getHardForkGenTxId)
  decodeNodeToNode = fmap (HardForkGenTxId . OneEraGenTxId) .: dispatchDecoder
