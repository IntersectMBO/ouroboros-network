{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoStarIsType #-}

module Cardano.KESAgent.Serialization.Spec.OrphansBase
where

import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Serialization.Spec.Types
import Cardano.KESAgent.Serialization.Spec.Class

import Cardano.Crypto.Hash.Class

import Control.Monad ( unless, replicateM )
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow ( MonadThrow )
import qualified Data.ByteString as BS
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (UTCTime)
import Data.Typeable
import Data.Word
import Data.Int
import GHC.TypeLits ( KnownNat, Nat, natVal )

-- * Instances for basic types

-- ** Unit

instance HasSerInfo () where
  info _ = basicField "void" $ FixedSize 0

instance Monad m => IsSerItem m () where
  sendItem _ () = return ()
  receiveItem _ = return ()
-- ** Int types

instance HasSerInfo Int8 where
  info _ = basicField "int8" $ FixedSize 1

instance (MonadThrow m, MonadST m) => IsSerItem m Int8 where
  sendItem = sendInt8
  receiveItem s = ReadResultT $ receiveInt8 s

instance HasSerInfo Int16 where
  info _ = basicField "int16BE" $ FixedSize 2

instance (MonadThrow m, MonadST m) => IsSerItem m Int16 where
  sendItem = sendInt16
  receiveItem s = ReadResultT $ receiveInt16 s

instance HasSerInfo Int32 where
  info _ = basicField "int32BE" $ FixedSize 4

instance (MonadThrow m, MonadST m) => IsSerItem m Int32 where
  sendItem = sendInt32
  receiveItem s = ReadResultT $ receiveInt32 s

instance HasSerInfo Int64 where
  info _ = basicField "int64BE" $ FixedSize 8

instance (MonadThrow m, MonadST m) => IsSerItem m Int64 where
  sendItem = sendInt64
  receiveItem s = ReadResultT $ receiveInt64 s

instance HasSerInfo Word8 where
  info _ = basicField "word8" $ FixedSize 1

instance (MonadThrow m, MonadST m) => IsSerItem m Word8 where
  sendItem = sendWord8
  receiveItem s = ReadResultT $ receiveWord8 s

instance HasSerInfo Word16 where
  info _ = basicField "word16BE" $ FixedSize 2

instance (MonadThrow m, MonadST m) => IsSerItem m Word16 where
  sendItem = sendWord16
  receiveItem s = ReadResultT $ receiveWord16 s

instance HasSerInfo Word32 where
  info _ = basicField "word32BE" $ FixedSize 4

instance (MonadThrow m, MonadST m) => IsSerItem m Word32 where
  sendItem = sendWord32
  receiveItem s = ReadResultT $ receiveWord32 s

instance HasSerInfo Word64 where
  info _ = basicField "word64BE" $ FixedSize 8

instance (MonadThrow m, MonadST m) => IsSerItem m Word64 where
  sendItem = sendWord64
  receiveItem s = ReadResultT $ receiveWord64 s

-- ** 'UTCTime'

instance HasSerInfo UTCTime where
  info _ = aliasField "POSIXSeconds"
            $ info (Proxy @Int64)

instance (MonadThrow m, MonadST m) => IsSerItem m UTCTime where
  sendItem = sendUTCTime
  receiveItem = ReadResultT . receiveUTCTime

-- ** 'Bool'

instance HasSerInfo Bool where
  info _ = basicField "Bool" (FixedSize 1)

instance (MonadThrow m, MonadST m) => IsSerItem m Bool where
  sendItem s False = sendWord8 s 0
  sendItem s True = sendWord8 s 1

  receiveItem s = do
    n <- ReadResultT (receiveWord8 s)
    case n of
      0 -> return False
      1 -> return True
      _ -> ReadResultT . return $ ReadMalformed "Bool"

instance HasSerInfo a => HasSerInfo (Maybe a) where
  info _ =
    compoundField
      ("Maybe " ++ shortFieldType (info (Proxy @a)))
      [ ("isJust", info (Proxy @Bool))
      , ("value",
          choiceField
            (IndexField "isJust")
            [ info (Proxy @())
            , info (Proxy @a)
            ]
        )
      ]

instance (MonadThrow m, MonadST m, IsSerItem m a) => IsSerItem m (Maybe a) where
  sendItem s Nothing =
    sendItem s False
  sendItem s (Just x) = do
    sendItem s True
    sendItem s x

  receiveItem s = do
    just <- receiveItem s
    if just then do
      Just <$> receiveItem s
    else
      return Nothing

-- ** Newtype wrappers for known- and unknown-sized collections

newtype Sized (len :: Nat) a = Sized { unSized :: a }
  deriving newtype (Show, Eq)

newtype VariableSized a = VariableSized { unVariableSized :: a }
  deriving newtype (Show, Read, Eq, Ord)

-- ** Newtype wrapper for enums

newtype ViaEnum a = ViaEnum { viaEnum :: a }

instance (Typeable a, Show a, Enum a, Bounded a) => HasSerInfo (ViaEnum a) where
  info _ =
    enumField
      (typeName $ Proxy @a)
      (map show [minBound .. maxBound :: a])

typeName :: Typeable a => Proxy a -> String
typeName = tyConName . typeRepTyCon . typeRep

instance (MonadThrow m, MonadST m, Typeable a, Show a, Enum a, Bounded a) => IsSerItem m (ViaEnum a) where
  sendItem s (ViaEnum x) =
    sendItem s (fromIntegral (fromEnum x) :: Word16)

  receiveItem s = do
    nw :: Word16 <- receiveItem s
    let n = fromIntegral nw
    if n < fromEnum (minBound :: a) ||
       n > fromEnum (maxBound :: a) then
      ReadResultT $ return (ReadMalformed (typeName (Proxy @a)))
    else
      return (ViaEnum $ toEnum n)

-- ** 'ByteString'

instance (KnownNat len) => HasSerInfo (Sized len ByteString) where
  info _ =
    let n = fromIntegral $ natVal (Proxy @len)
    in basicField "bytes"
        $ FixedSize n
instance (MonadThrow m, MonadST m, KnownNat len) => IsSerItem m (Sized len ByteString) where
  sendItem s val = do
    actualLen <- sendBS s (unSized val)
    let expectedLen = fromIntegral $ natVal (Proxy @len)
    unless (actualLen == expectedLen) (error "Length mismatch")

  receiveItem s = do
    let n = natVal (Proxy @len)
    Sized <$> ReadResultT (receiveBS s (fromIntegral n))

instance HasSerInfo (VariableSized ByteString) where
  info _ =
    compoundField "ByteString"
      [ ("length", info (Proxy @Word32))
      , ("data", basicField "bytes" $ VarSize "length")
      ]
instance (MonadThrow m, MonadST m) => IsSerItem m (VariableSized ByteString) where
  sendItem s val = do
    let len = BS.length (unVariableSized val)
    sendWord32 s (fromIntegral len)
    actualLen <- sendBS s (unVariableSized val)
    unless (actualLen == len) (error "Length mismatch")

  receiveItem s = do
    len <- ReadResultT (receiveWord32 s)
    VariableSized <$> ReadResultT (receiveBS s (fromIntegral len))

-- ** 'Text'

instance HasSerInfo Text where
  info _ =
    compoundField "UTF8"
      [ ("length", info (Proxy @Word32))
      , ("data", basicField "bytes" $ VarSize "length")
      ]
instance (MonadThrow m, MonadST m) => IsSerItem m Text where
  sendItem s val = do
    let len = BS.length (encodeUtf8 val)
    sendWord32 s (fromIntegral len)
    actualLen <- sendBS s (encodeUtf8 val)
    unless (actualLen == len) (error "Length mismatch")

  receiveItem s = do
    len <- ReadResultT (receiveWord32 s)
    decodeUtf8 <$> ReadResultT (receiveBS s (fromIntegral len))

-- ** Lists

instance HasSerInfo a => HasSerInfo [a] where
  info _ =
    compoundField
      ("[" ++ fieldType (info (Proxy @a)) ++ "]")
      [ ( "listLength", info (Proxy @Word32) )
      , ( "elems"
        , listField
            (VarSize "listLength")
            (info (Proxy @a))
        )
      ]
instance (MonadThrow m, MonadST m, IsSerItem m a) => IsSerItem m [a] where
  sendItem s val = do
    let len = length val
    sendWord32 s (fromIntegral len)
    mapM_ (sendItem s) val

  receiveItem s = do
    len <- ReadResultT (receiveWord32 s)
    replicateM (fromIntegral len) (receiveItem s)

-- * Helpers
-- These mostly live here because it makes the most sense wrt module
-- dependencies. I'd much rather keep them in the
-- 'Cardano.KESAgent.Serialization.Spec.Types' module, but that would create a
-- circular dependency.


fieldType :: FieldInfo -> String
fieldType (BasicField fi) = basicFieldType fi
fieldType (EnumField fi) = enumFieldType fi ++ " = " ++ fieldType (info @Word32 Proxy)
fieldType (CompoundField fi) = compoundFieldType fi
fieldType (ChoiceField fi) = intercalate " | " $ map fieldType (choiceFieldAlternatives fi)
fieldType (ListField fi) = "[" ++ fieldType (listElemInfo fi) ++ "]"
fieldType (AliasField fi) = aliasFieldName fi ++ " = " ++ fieldType (aliasFieldTarget fi)
fieldType (SumField fi) = sumFieldType fi

shortFieldType :: FieldInfo -> String
shortFieldType (BasicField fi) = basicFieldType fi
shortFieldType (EnumField fi) = enumFieldType fi
shortFieldType (CompoundField fi) = compoundFieldType fi
shortFieldType (ChoiceField fi) = intercalate " | " $ map shortFieldType (choiceFieldAlternatives fi)
shortFieldType (ListField fi) = "[" ++ shortFieldType (listElemInfo fi) ++ "]"
shortFieldType (AliasField fi) = aliasFieldName fi
shortFieldType (SumField fi) = sumFieldType fi

formatPath :: [String] -> String
formatPath = intercalate "." . reverse

scopeFieldSize :: String -> FieldSize -> FieldSize
scopeFieldSize scope (VarSize var) = VarSize (scope ++ "." ++ var)
scopeFieldSize scope (BinopSize op a b) = BinopSize op (scopeFieldSize scope a) (scopeFieldSize scope b)
scopeFieldSize scope (RangeSize a b) = RangeSize (scopeFieldSize scope a) (scopeFieldSize scope b)
scopeFieldSize _ x = x

simplifyFieldSize :: FieldSize -> FieldSize
simplifyFieldSize (RangeSize a b) =
  let a' = simplifyFieldSize a
      b' = simplifyFieldSize b
  in
    if a' == b' then
      a'
    else
      case (a', b') of
        (RangeSize aa' ab', RangeSize ba' bb') ->
          simplifyFieldSize (RangeSize (BinopSize FSMin aa' ba') (BinopSize FSMax ab' bb'))
        (a'', RangeSize ba' bb') ->
          simplifyFieldSize (RangeSize (BinopSize FSMin a'' ba') (BinopSize FSMax a'' bb'))
        _ -> RangeSize a' b'

simplifyFieldSize (BinopSize op a b) =
  let a' = simplifyFieldSize a
      b' = simplifyFieldSize b
  in
    case (a', op, b') of
      (UnknownSize, _, _) -> UnknownSize
      (_, _, UnknownSize) -> UnknownSize

      (FixedSize x, FSPlus, BinopSize FSPlus (FixedSize y) z) ->
        simplifyFieldSize (BinopSize FSPlus (FixedSize (x + y)) z)
      (BinopSize FSPlus z (FixedSize y), FSPlus, FixedSize x) ->
        simplifyFieldSize (BinopSize FSPlus (FixedSize (x + y)) z)
      (RangeSize l r, _, c) ->
        simplifyFieldSize (RangeSize (BinopSize op l c) (BinopSize op r c))
      (x, FSPlus, BinopSize FSPlus y z) ->
        simplifyFieldSize (BinopSize FSPlus (BinopSize FSPlus x y) z)

      (FixedSize x, FSMul, BinopSize FSMul (FixedSize y) z) ->
        simplifyFieldSize (BinopSize FSMul (FixedSize (x + y)) z)
      (BinopSize FSMul z (FixedSize y), FSMul, FixedSize x) ->
        simplifyFieldSize (BinopSize FSMul (FixedSize (x + y)) z)

      (FixedSize x, FSPlus, FixedSize y) -> FixedSize (x + y)
      (FixedSize x, FSMul, FixedSize y) -> FixedSize (x * y)

      (FixedSize x, FSMax, FixedSize y) -> FixedSize (max x y)
      (FixedSize x, FSMin, FixedSize y) -> FixedSize (min x y)

      (FixedSize x, FSPlus, RangeSize lo hi) ->
        simplifyFieldSize (RangeSize (BinopSize FSPlus (FixedSize x) lo) (BinopSize FSPlus (FixedSize x) hi))

      (FixedSize 0, FSPlus, y) -> y
      (x, FSPlus, FixedSize 0) -> x
      (FixedSize 1, FSMul, y) -> y
      (x, FSMul, FixedSize 1) -> x
      (FixedSize 0, FSMin, _) -> FixedSize 0
      (_, FSMin, FixedSize 0) -> FixedSize 0
      (FixedSize 0, FSMax, y) -> y
      (x, FSMax, FixedSize 0) -> x

      _ -> BinopSize op a' b'
simplifyFieldSize x = x

resolveSizeScopes :: Map String [String] -> FieldSize -> FieldSize
resolveSizeScopes env (VarSize name) =
  let name' = maybe name formatPath $ Map.lookup name env
  in VarSize name'
resolveSizeScopes env (BinopSize op a b) =
  BinopSize op (resolveSizeScopes env a) (resolveSizeScopes env b)
resolveSizeScopes env (RangeSize a b) =
  RangeSize (resolveSizeScopes env a) (resolveSizeScopes env b)
resolveSizeScopes _ x = x

fieldSize :: FieldInfo -> FieldSize
fieldSize = fieldSizeScoped [] mempty

fieldSizeScoped :: [String] -> Map String [String] -> FieldInfo -> FieldSize
fieldSizeScoped path env (AliasField fi) =
  fieldSizeScoped path env (aliasFieldTarget fi)
fieldSizeScoped _ env (BasicField fi) =
  resolveSizeScopes env (basicFieldSize fi)
fieldSizeScoped _ _ (EnumField _) =
  FixedSize 2
fieldSizeScoped path env (CompoundField fi) =
  let env' = foldl' (\e sfi -> Map.insert (subfieldName sfi) (subfieldName sfi : path) e) env (compoundFieldSubfields fi)
      qualifiedSubfieldSizes sfi =
        let path' = subfieldName sfi : path
            env'' = Map.insert (subfieldName sfi) path' env'
        in
          fieldSizeScoped path' env'' (subfieldInfo sfi)
  in
    case map qualifiedSubfieldSizes (compoundFieldSubfields fi) of
      [] -> FixedSize 0
      (x:xs) -> simplifyFieldSize $ foldl' (BinopSize FSPlus) x xs
fieldSizeScoped path env (ListField fi) =
  let elemSize = maybe UnknownSize FixedSize $
                  knownSize
                    (fieldSizeScoped path env (listElemInfo fi))
  in
    simplifyFieldSize $
      BinopSize FSMul (listSize fi) elemSize
fieldSizeScoped path env (ChoiceField fi) =
  case map (fieldSizeScoped path env) (choiceFieldAlternatives fi) of
    [] -> FixedSize 0
    (x:xs) -> let maxVal = foldl' (BinopSize FSMax) x xs
                  minVal = foldl' (BinopSize FSMin) x xs
              in simplifyFieldSize (RangeSize minVal maxVal)
fieldSizeScoped path env (SumField fi) =
  case map (fieldSizeScoped path env . snd) (sumFieldAlternatives fi) of
    [] -> FixedSize 0
    (x:xs) -> let maxVal = foldl' (BinopSize FSMax) x xs
                  minVal = foldl' (BinopSize FSMin) x xs
              in simplifyFieldSize (RangeSize minVal maxVal)


