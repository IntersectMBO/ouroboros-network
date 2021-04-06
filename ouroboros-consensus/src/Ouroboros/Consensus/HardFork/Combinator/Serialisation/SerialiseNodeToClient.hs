{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseNodeToClient () where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import qualified Codec.Serialise as Serialise
import           Control.Exception (throw)
import           Data.Proxy
import           Data.SOP.Strict

import           Cardano.Binary (enforceSize)

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.SOP (ProofNonEmpty (..),
                     checkIsNonEmpty, isNonEmpty)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.SerialiseDisk ()

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
            encodeNodeToClient c0 v0 x0
          (_, HardForkNodeToClientDisabled _, S later) ->
            throw $ futureEraException (notFirstEra later)
          (_, HardForkNodeToClientEnabled _ versions, _) ->
            encodeNS (hczipWith pSHFC aux ccfgs versions) ns
  where
    ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

    aux :: forall blk. (SingleEraBlock blk, SerialiseNodeToClientConstraints blk)
        => CodecConfig blk
        -> EraNodeToClientVersion blk
        -> (f -.-> K Encoding) blk
    aux ccfg' (EraNodeToClientEnabled v) = Fn $ K . encodeNodeToClient ccfg' v
    aux _      EraNodeToClientDisabled   = Fn $ \_ ->
        throw $ disabledEraException (Proxy @blk)

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
            Z <$> decodeNodeToClient c0 v0
          (_, HardForkNodeToClientEnabled _ versions) ->
            decodeNS (hczipWith pSHFC aux ccfgs versions)
  where
    ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

    aux :: forall blk. (SingleEraBlock blk, SerialiseNodeToClientConstraints blk)
        => CodecConfig blk
        -> EraNodeToClientVersion blk
        -> forall s. (Decoder s :.: f) blk
    aux ccfg' (EraNodeToClientEnabled v) = Comp $ decodeNodeToClient ccfg' v
    aux _      EraNodeToClientDisabled   = Comp $
        fail . show $ disabledEraException (Proxy @blk)

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

encodeQueryHardFork ::
     HardForkSpecificNodeToClientVersion
  -> Some (QueryHardFork xs)
  -> Encoding
encodeQueryHardFork vHfc = \case
    Some GetInterpreter -> mconcat [
        Enc.encodeListLen 1
      , Enc.encodeWord8 0
      ]
    Some GetCurrentEra
      | vHfc < HardForkSpecificNodeToClientVersion2 ->
        throw HardForkEncoderQueryWrongVersion
      | otherwise -> mconcat [
        Enc.encodeListLen 1
      , Enc.encodeWord8 1
      ]

decodeQueryHardFork :: Decoder s (Some (QueryHardFork xs))
decodeQueryHardFork = do
    enforceSize "QueryHardFork" 1
    tag <- Dec.decodeWord8
    case tag of
      0 -> return $ Some GetInterpreter
      1 -> return $ Some GetCurrentEra
      _ -> fail $ "QueryHardFork: invalid tag " ++ show tag

instance SerialiseHFC xs
      => SerialiseNodeToClient (HardForkBlock xs) (SomeSecond BlockQuery (HardForkBlock xs)) where
  encodeNodeToClient ccfg version (SomeSecond q) = case version of
      HardForkNodeToClientDisabled v0 -> case q of
        QueryIfCurrent qry ->
          case distribQueryIfCurrent (Some qry) of
            Z qry0  -> encodeNodeToClient (hd ccfgs) v0 qry0
            S later -> throw $ futureEraException (notFirstEra later)
        QueryAnytime {} ->
          throw HardForkEncoderQueryHfcDisabled
        QueryHardFork {} ->
          throw HardForkEncoderQueryHfcDisabled

      HardForkNodeToClientEnabled vHfc _ -> case q of
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
          , encodeQueryHardFork vHfc (Some qry)
          ]
    where
      ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

  decodeNodeToClient ccfg version = case version of
      HardForkNodeToClientDisabled v0 ->
        injQueryIfCurrent . Z <$>
          decodeNodeToClient (hd ccfgs) v0
      HardForkNodeToClientEnabled {} -> case isNonEmpty (Proxy @xs) of
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
                  return $ SomeSecond (QueryAnytime qry eraIndex)

            (2, 2) -> do
              Some (qry :: QueryHardFork xs result) <- decodeQueryHardFork
              case checkIsNonEmpty p of
                Nothing -> fail $ "QueryHardFork requires multiple era"
                Just (ProofNonEmpty {}) ->
                  return $ SomeSecond (QueryHardFork qry)

            _ -> fail $ "HardForkQuery: invalid size and tag" <> show (size, tag)
    where
      ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

      injQueryIfCurrent :: NS (SomeSecond BlockQuery) xs
                        -> SomeSecond BlockQuery (HardForkBlock xs)
      injQueryIfCurrent ns =
          case undistribQueryIfCurrent ns of
            Some q -> SomeSecond (QueryIfCurrent q)

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}

instance SerialiseHFC xs
      => SerialiseResult (HardForkBlock xs) (BlockQuery (HardForkBlock xs)) where
  encodeResult ccfg version (QueryIfCurrent qry) =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          encodeEitherMismatch version $
            case (ccfgs, version, qry) of
              (c0 :* _, HardForkNodeToClientDisabled v0, QZ qry') ->
                encodeResult c0 v0 qry'
              (_, HardForkNodeToClientDisabled _, QS qry') ->
                throw $ futureEraException (hardForkQueryInfo qry')
              (_, HardForkNodeToClientEnabled _ versions, _) ->
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
                decodeResult c0 v0 qry'
              (_, HardForkNodeToClientDisabled _, QS qry') ->
                throw $ futureEraException (hardForkQueryInfo qry')
              (_, HardForkNodeToClientEnabled _ versions, _) ->
                decodeQueryIfCurrentResult ccfgs versions qry
    where
      ccfgs = getPerEraCodecConfig $ hardForkCodecConfigPerEra ccfg

  decodeResult _ _ (QueryAnytime qry _) = decodeQueryAnytimeResult qry
  decodeResult _ _ (QueryHardFork qry)  = decodeQueryHardForkResult qry

encodeQueryIfCurrentResult ::
     All SerialiseConstraintsHFC xs
  => NP CodecConfig xs
  -> NP EraNodeToClientVersion xs
  -> QueryIfCurrent xs result
  -> result -> Encoding
encodeQueryIfCurrentResult (c :* _) (EraNodeToClientEnabled v :* _) (QZ qry) =
    encodeResult c v qry
encodeQueryIfCurrentResult (_ :* _) (EraNodeToClientDisabled :* _) (QZ qry) =
    qryDisabledEra qry
  where
    qryDisabledEra :: forall blk result. SingleEraBlock blk
                   => BlockQuery blk result -> result -> Encoding
    qryDisabledEra _ _ = throw $ disabledEraException (Proxy @blk)
encodeQueryIfCurrentResult (_ :* cs) (_ :* vs) (QS qry) =
    encodeQueryIfCurrentResult cs vs qry
encodeQueryIfCurrentResult Nil _ qry =
    case qry of {}

decodeQueryIfCurrentResult ::
     All SerialiseConstraintsHFC xs
  => NP CodecConfig xs
  -> NP EraNodeToClientVersion xs
  -> QueryIfCurrent xs result
  -> Decoder s result
decodeQueryIfCurrentResult (c :* _) (EraNodeToClientEnabled v :* _) (QZ qry) =
    decodeResult c v qry
decodeQueryIfCurrentResult (_ :* _) (EraNodeToClientDisabled :* _) (QZ qry) =
    qryDisabledEra qry
  where
    qryDisabledEra :: forall blk result. SingleEraBlock blk
                   => BlockQuery blk result -> forall s. Decoder s result
    qryDisabledEra _ = fail . show $ disabledEraException (Proxy @blk)
decodeQueryIfCurrentResult (_ :* cs) (_ :* vs) (QS qry) =
    decodeQueryIfCurrentResult cs vs qry
decodeQueryIfCurrentResult Nil _ qry =
    case qry of {}
