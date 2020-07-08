{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToClient (
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import qualified Codec.Serialise as Serialise
import           Control.Exception (throw)
import           Data.Proxy
import           Data.SOP.Strict

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseDisk
                     ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.SOP (checkIsNonEmpty)

instance SerialiseHFC xs => SerialiseNodeToClientConstraints (HardForkBlock xs)

{-------------------------------------------------------------------------------
  Dispatch to first era or HFC
-------------------------------------------------------------------------------}

dispatchEncoder :: forall f xs. (
                     SerialiseHFC xs
                   , forall blk. SerialiseNodeToClientConstraints blk
                              => SerialiseNodeToClient blk (f blk)
                   )
                => CodecConfig (HardForkBlock xs)
                -> BlockNodeToClientVersion (HardForkBlock xs)
                -> NS f xs -> Encoding
dispatchEncoder ccfg version ns =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty {} ->
        case (ccfgs, version, ns) of
          (c0 :* _, HardForkNodeToClientDisabled v0, Z x0) ->
            encodeNodeToClient c0 (unwrapNodeToClientVersion v0) x0
          (_, HardForkNodeToClientDisabled _, S later) ->
            throw $ futureEraException (notFirstEra later)
          (_, HardForkNodeToClientEnabled versions, _) ->
            encodeNS (hczipWith pSHFC aux ccfgs versions) ns
  where
    ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

    aux :: SerialiseNodeToClientConstraints blk
        => CodecConfig blk
        -> WrapNodeToClientVersion blk
        -> (f -.-> K Encoding) blk
    aux ccfg' (WrapNodeToClientVersion v) = Fn $ K . encodeNodeToClient ccfg' v

dispatchDecoder :: forall f xs. (
                     SerialiseHFC xs
                   , forall blk. SerialiseNodeToClientConstraints blk
                              => SerialiseNodeToClient blk (f blk)
                   )
                => CodecConfig (HardForkBlock xs)
                -> BlockNodeToClientVersion (HardForkBlock xs)
                -> forall s. Decoder s (NS f xs)
dispatchDecoder ccfg version =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty {} ->
        case (ccfgs, version) of
          (c0 :* _, HardForkNodeToClientDisabled v0) ->
            Z <$> decodeNodeToClient c0 (unwrapNodeToClientVersion v0)
          (_, HardForkNodeToClientEnabled versions) ->
            decodeNS (hczipWith pSHFC aux ccfgs versions)
  where
    ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

    aux :: SerialiseNodeToClientConstraints blk
        => CodecConfig blk
        -> WrapNodeToClientVersion blk
        -> (Decoder s :.: f) blk
    aux ccfg' (WrapNodeToClientVersion v) = Comp $ decodeNodeToClient ccfg' v

dispatchEncoderErr :: forall f xs. (
                        SerialiseHFC xs
                      , forall blk. SerialiseNodeToClientConstraints blk
                                 => SerialiseNodeToClient blk (f blk)
                      )
                   => CodecConfig (HardForkBlock xs)
                   -> BlockNodeToClientVersion (HardForkBlock xs)
                   -> Either (MismatchEraInfo xs) (NS f xs) -> Encoding
dispatchEncoderErr ccfg version =
    encodeEitherMismatch version $
      dispatchEncoder ccfg version

dispatchDecoderErr :: forall f xs. (
                        SerialiseHFC xs
                      , forall blk. SerialiseNodeToClientConstraints blk
                                 => SerialiseNodeToClient blk (f blk)
                      )
                   => CodecConfig (HardForkBlock xs)
                   -> BlockNodeToClientVersion (HardForkBlock xs)
                   -> forall s. Decoder s (Either (MismatchEraInfo xs) (NS f xs))
dispatchDecoderErr ccfg version =
    decodeEitherMismatch version $
      dispatchDecoder ccfg version

after :: (a -> b -> d -> e) -> (c -> d) -> a -> b -> c -> e
after f g x y z = f x y (g z)

{-------------------------------------------------------------------------------
  Blocks
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => SerialiseNodeToClient (HardForkBlock xs) (HardForkBlock xs) where
  encodeNodeToClient ccfg _ = wrapCBORinCBOR   (encodeDiskHfcBlock ccfg)
  decodeNodeToClient ccfg _ = unwrapCBORinCBOR (decodeDiskHfcBlock ccfg)

{-------------------------------------------------------------------------------
  Serialised blocks
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => SerialiseNodeToClient (HardForkBlock xs) (Serialised (HardForkBlock xs)) where
  encodeNodeToClient _ _ = Serialise.encode
  decodeNodeToClient _ _ = Serialise.decode

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => SerialiseNodeToClient (HardForkBlock xs) (GenTx (HardForkBlock xs)) where
  encodeNodeToClient = dispatchEncoder `after` (getOneEraGenTx . getHardForkGenTx)
  decodeNodeToClient = fmap (HardForkGenTx . OneEraGenTx) .: dispatchDecoder

instance SerialiseHFC xs
      => SerialiseNodeToClient (HardForkBlock xs) (HardForkApplyTxErr xs) where
  encodeNodeToClient = dispatchEncoderErr `after` (fmap getOneEraApplyTxErr . hardForkApplyTxErrToEither)
  decodeNodeToClient = fmap (hardForkApplyTxErrFromEither . fmap OneEraApplyTxErr) .: dispatchDecoderErr

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => SerialiseNodeToClient (HardForkBlock xs) (SomeBlock Query (HardForkBlock xs)) where
  encodeNodeToClient ccfg version (SomeBlock q) = case q of
      QueryIfCurrent qry -> mconcat [
            Enc.encodeListLen 2
          , Enc.encodeWord8 0
          , dispatchEncoder ccfg version (distribQueryIfCurrent (Some qry))
          ]
      QueryAnytime qry eraIndex -> mconcat [
            Enc.encodeListLen 3
          , Enc.encodeWord8 1
          , Serialise.encode (Some qry)
          , Serialise.encode eraIndex
          ]
      QueryHardFork qry -> mconcat [
            Enc.encodeListLen 2
          , Enc.encodeWord8 2
          , Serialise.encode (Some qry)
          ]

  decodeNodeToClient ccfg version = case isNonEmpty (Proxy @xs) of
      ProofNonEmpty (_ :: Proxy x') (p :: Proxy xs') -> do
        size <- Dec.decodeListLen
        tag  <- Dec.decodeWord8
        case (size, tag) of
          (2, 0) -> injQueryIfCurrent <$> dispatchDecoder ccfg version

          (3, 1) -> do
            Some (qry :: QueryAnytime result) <- Serialise.decode
            eraIndex :: EraIndex (x' ': xs')  <- Serialise.decode
            case checkIsNonEmpty p of
              Nothing -> fail $ "QueryAnytime requires multiple era"
              Just (ProofNonEmpty {}) ->
                return $ SomeBlock (QueryAnytime qry eraIndex)

          (2, 2) -> do
            Some (qry :: QueryHardFork xs result) <- Serialise.decode
            case checkIsNonEmpty p of
              Nothing -> fail $ "QueryHardFork requires multiple era"
              Just (ProofNonEmpty {}) ->
                return $ SomeBlock (QueryHardFork qry)

          _ -> fail $ "HardForkQuery: invalid size and tag" <> show (size, tag)
    where
      injQueryIfCurrent :: NS (SomeBlock Query) xs
                        -> SomeBlock Query (HardForkBlock xs)
      injQueryIfCurrent ns =
          case undistribQueryIfCurrent ns of
            Some q -> SomeBlock (QueryIfCurrent q)

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => SerialiseResult (HardForkBlock xs) (Query (HardForkBlock xs)) where
  encodeResult ccfg version (QueryIfCurrent qry) =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          encodeEitherMismatch version $
            case (ccfgs, version, qry) of
              (c0 :* _, HardForkNodeToClientDisabled v0, QZ qry') ->
                encodeResult c0 (unwrapNodeToClientVersion v0) qry'
              (_, HardForkNodeToClientDisabled _, QS qry') ->
                throw $ futureEraException (hardForkQueryInfo qry')
              (_, HardForkNodeToClientEnabled versions, _) ->
                encodeQueryIfCurrentResult ccfgs versions qry
    where
      ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

  encodeResult _ _ (QueryAnytime qry _) = encodeQueryAnytimeResult qry
  encodeResult _ _ (QueryHardFork qry)  = encodeQueryHardForkResult qry

  decodeResult ccfg version (QueryIfCurrent qry) =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          decodeEitherMismatch version $
            case (ccfgs, version, qry) of
              (c0 :* _, HardForkNodeToClientDisabled v0, QZ qry') ->
                decodeResult c0 (unwrapNodeToClientVersion v0) qry'
              (_, HardForkNodeToClientDisabled _, QS qry') ->
                throw $ futureEraException (hardForkQueryInfo qry')
              (_, HardForkNodeToClientEnabled versions, _) ->
                decodeQueryIfCurrentResult ccfgs versions qry
    where
      ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

  decodeResult _ _ (QueryAnytime qry _) = decodeQueryAnytimeResult qry
  decodeResult _ _ (QueryHardFork qry)  = decodeQueryHardForkResult qry

encodeQueryIfCurrentResult ::
     All SerialiseConstraintsHFC xs
  => NP CodecConfig xs
  -> NP WrapNodeToClientVersion xs
  -> QueryIfCurrent xs result
  -> result -> Encoding
encodeQueryIfCurrentResult (c :* _) (v :* _) (QZ qry) =
    encodeResult c (unwrapNodeToClientVersion v) qry
encodeQueryIfCurrentResult (_ :* cs) (_ :* vs) (QS qry) =
    encodeQueryIfCurrentResult cs vs qry
encodeQueryIfCurrentResult Nil _ qry =
    case qry of {}

decodeQueryIfCurrentResult ::
     All SerialiseConstraintsHFC xs
  => NP CodecConfig xs
  -> NP WrapNodeToClientVersion xs
  -> QueryIfCurrent xs result
  -> Decoder s result
decodeQueryIfCurrentResult (c :* _) (v :* _) (QZ qry) =
    decodeResult c (unwrapNodeToClientVersion v) qry
decodeQueryIfCurrentResult (_ :* cs) (_ :* vs) (QS qry) =
    decodeQueryIfCurrentResult cs vs qry
decodeQueryIfCurrentResult Nil _ qry =
    case qry of {}
