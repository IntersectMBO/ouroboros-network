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
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.SOP (ProofNonEmpty (..), isNonEmpty)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseDisk ()

instance SerialiseHFC xs => SerialiseNodeToNodeConstraints (HardForkBlock xs) where
  estimateBlockSize = estimateHfcBlockSize

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
      ProofNonEmpty {} ->
        case (ccfgs, version, ns) of
          (c0 :* _, HardForkNodeToNodeDisabled v0, Z x0) ->
            encodeNodeToNode c0 v0 x0
          (_, HardForkNodeToNodeDisabled _, S later) ->
            throw $ futureEraException (notFirstEra later)
          (_, HardForkNodeToNodeEnabled _ versions, _) ->
            encodeNS (hczipWith pSHFC aux ccfgs versions) ns
  where
    ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

    aux :: forall blk. (SingleEraBlock blk, SerialiseNodeToNodeConstraints blk)
        => CodecConfig blk
        -> EraNodeToNodeVersion blk
        -> (f -.-> K Encoding) blk
    aux ccfg' (EraNodeToNodeEnabled v) = Fn $ K . encodeNodeToNode ccfg' v
    aux _      EraNodeToNodeDisabled   = Fn $ \_ ->
        throw $ disabledEraException (Proxy @blk)

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
      ProofNonEmpty {} ->
        case (ccfgs, version) of
          (c0 :* _, HardForkNodeToNodeDisabled v0) ->
            Z <$> decodeNodeToNode c0 v0
          (_, HardForkNodeToNodeEnabled _ versions) ->
            decodeNS (hczipWith pSHFC aux ccfgs versions)
  where
    ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

    aux :: forall blk. (SingleEraBlock blk, SerialiseNodeToNodeConstraints blk)
        => CodecConfig blk
        -> EraNodeToNodeVersion blk
        -> forall s. (Decoder s :.: f) blk
    aux ccfg' (EraNodeToNodeEnabled v) = Comp $ decodeNodeToNode ccfg' v
    aux _      EraNodeToNodeDisabled   = Comp $
        fail . show $ disabledEraException (Proxy @blk)

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
