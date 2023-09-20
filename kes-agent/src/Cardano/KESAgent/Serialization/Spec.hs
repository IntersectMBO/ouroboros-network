{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.KESAgent.Serialization.Spec
( module Cardano.KESAgent.Serialization.Spec
, Proxy (..)
, StandardCrypto
)
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Util.RefCounting

import Cardano.Binary
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class
import Ouroboros.Network.RawBearer ( RawBearer (..) )

import Control.Monad ( void, unless, when, replicateM, zipWithM_ )
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow ( MonadThrow, throwIO, catch )
import Control.Monad.Trans ( lift )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Typeable
import Data.Word
import Data.Int
import Foreign.Ptr ( castPtr )
import GHC.Generics
import GHC.TypeLits
import Language.Haskell.TH
import Text.Printf

data FieldInfo
  = BasicField BasicFieldInfo
  | CompoundField CompoundFieldInfo
  | ChoiceField ChoiceFieldInfo
  | ListField ListFieldInfo
  deriving (Show)

fieldType :: FieldInfo -> String
fieldType (BasicField info) = basicFieldType info
fieldType (CompoundField info) = compoundFieldType info
fieldType (ChoiceField info) = "choice"
fieldType (ListField info) = "[" ++ fieldType (listElemInfo info) ++ "]"

formatPath :: [String] -> String
formatPath = intercalate "." . reverse

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
fieldSizeScoped path env (BasicField info) =
  resolveSizeScopes env (basicFieldSize info)
fieldSizeScoped path env (CompoundField info) =
  let env' = foldl' (\e sfi -> Map.insert (subfieldName sfi) (subfieldName sfi : path) e) env (compoundFieldSubfields info)
      qualifiedSubfieldSizes sfi =
        let path' = subfieldName sfi : path
            env'' = Map.insert (subfieldName sfi) path' env'
        in
          fieldSizeScoped path' env'' (subfieldInfo sfi)
  in
    case map qualifiedSubfieldSizes (compoundFieldSubfields info) of
      [] -> FixedSize 0
      (x:xs) -> simplifyFieldSize $ foldl' (BinopSize FSPlus) x xs
fieldSizeScoped path env (ListField info) =
  let elemSize = maybe UnknownSize FixedSize $
                  knownSize
                    (fieldSizeScoped path env (listElemInfo info))
  in
    simplifyFieldSize $
      BinopSize FSMul (listSize info) elemSize
fieldSizeScoped path env (ChoiceField info) =
  case map (fieldSizeScoped path env) (choiceFieldAlternatives info) of
    [] -> FixedSize 0
    (x:xs) -> let max = foldl' (BinopSize FSMax) x xs
                  min = foldl' (BinopSize FSMin) x xs
              in simplifyFieldSize (RangeSize min max)
  

data FieldSize
  = FixedSize !Int
  | VarSize !String
  | BinopSize !FieldSizeBinop !FieldSize !FieldSize
  | RangeSize !FieldSize !FieldSize
  | UnknownSize
  deriving (Show, Eq)

knownSize :: FieldSize -> Maybe Int
knownSize (FixedSize i) = Just i
knownSize VarSize {} = Nothing
knownSize (BinopSize FSPlus a b) = (+) <$> knownSize a <*> knownSize b
knownSize (BinopSize FSMul a b) = (*) <$> knownSize a <*> knownSize b
knownSize (BinopSize FSMax a b) = max <$> knownSize a <*> knownSize b
knownSize (BinopSize FSMin a b) = min <$> knownSize a <*> knownSize b
knownSize _ = Nothing

data FieldSizeBinop
  = FSPlus
  | FSMul
  | FSMax
  | FSMin
  deriving (Show, Eq)

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
        (a, RangeSize ba' bb') ->
          simplifyFieldSize (RangeSize (BinopSize FSMin a ba') (BinopSize FSMax a bb'))
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
      (RangeSize a b, op, c) ->
        simplifyFieldSize (RangeSize (BinopSize op a c) (BinopSize op b c))
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
      (FixedSize 0, FSMin, y) -> FixedSize 0
      (x, FSMin, FixedSize 0) -> FixedSize 0
      (FixedSize 0, FSMax, y) -> y
      (x, FSMax, FixedSize 0) -> x

      _ -> BinopSize op a' b'
simplifyFieldSize x = x

data BasicFieldInfo =
  BasicFieldInfo
    { basicFieldType :: !String
    , basicFieldSize :: !FieldSize
    }
  deriving (Show)

data CompoundFieldInfo =
  CompoundFieldInfo
    { compoundFieldType :: !String
    , compoundFieldSubfields :: ![SubfieldInfo]
    }
  deriving (Show)

data ListFieldInfo =
  ListFieldInfo
    { listSize :: !FieldSize
    , listElemInfo :: !FieldInfo
    }
  deriving (Show)

data SubfieldInfo =
  SubfieldInfo
    { subfieldName :: !String
    , subfieldInfo :: !FieldInfo
    }
  deriving (Show)

data ChoiceCondition
  = IndexField !String
  | IndexFlag !String Word32
  deriving (Show)

data ChoiceFieldInfo =
  ChoiceFieldInfo
    { choiceCondition :: !ChoiceCondition
    , choiceFieldAlternatives :: ![FieldInfo]
    }
  deriving (Show)

basicField :: String -> FieldSize -> FieldInfo
basicField ty size = BasicField $ BasicFieldInfo ty size

compoundField :: String -> [(String, FieldInfo)] -> FieldInfo
compoundField ty subfields =
  CompoundField $
    CompoundFieldInfo
      ty
      [ SubfieldInfo name i
      | (name, i) <- subfields
      ]

choiceField :: ChoiceCondition -> [FieldInfo] -> FieldInfo
choiceField cond subfields =
  ChoiceField $
    ChoiceFieldInfo
      cond
      subfields

listField :: FieldSize -> FieldInfo -> FieldInfo
listField lengthExpr elemInfo =
  ListField $
    ListFieldInfo
      lengthExpr
      elemInfo

class HasSerInfo a where
  info :: Proxy a -> FieldInfo

class HasSerInfo a => IsSerItem m a where
  sendItem :: RawBearer m -> a -> m ()
  receiveItem :: RawBearer m -> ReadResultT m a

instance HasSerInfo () where
  info _ = basicField "void" $ FixedSize 0

instance Monad m => IsSerItem m () where
  sendItem s () = return ()
  receiveItem s = return ()

instance HasSerInfo Int8 where
  info _ = basicField "Int8" $ FixedSize 1
  
instance (MonadThrow m, MonadST m) => IsSerItem m Int8 where
  sendItem = sendInt8
  receiveItem s = ReadResultT $ receiveInt8 s

instance HasSerInfo Int16 where
  info _ = basicField "Int16BE" $ FixedSize 2
  
instance (MonadThrow m, MonadST m) => IsSerItem m Int16 where
  sendItem = sendInt16
  receiveItem s = ReadResultT $ receiveInt16 s

instance HasSerInfo Int32 where
  info _ = basicField "Int32BE" $ FixedSize 4
  
instance (MonadThrow m, MonadST m) => IsSerItem m Int32 where
  sendItem = sendInt32
  receiveItem s = ReadResultT $ receiveInt32 s

instance HasSerInfo Int64 where
  info _ = basicField "Int64BE" $ FixedSize 8
  
instance (MonadThrow m, MonadST m) => IsSerItem m Int64 where
  sendItem = sendInt64
  receiveItem s = ReadResultT $ receiveInt64 s

instance HasSerInfo Word8 where
  info _ = basicField "Word8" $ FixedSize 1
  
instance (MonadThrow m, MonadST m) => IsSerItem m Word8 where
  sendItem = sendWord8
  receiveItem s = ReadResultT $ receiveWord8 s

instance HasSerInfo Word16 where
  info _ = basicField "Word16BE" $ FixedSize 2
  
instance (MonadThrow m, MonadST m) => IsSerItem m Word16 where
  sendItem = sendWord16
  receiveItem s = ReadResultT $ receiveWord16 s

instance HasSerInfo Word32 where
  info _ = basicField "Word32BE" $ FixedSize 4
  
instance (MonadThrow m, MonadST m) => IsSerItem m Word32 where
  sendItem = sendWord32
  receiveItem s = ReadResultT $ receiveWord32 s

instance HasSerInfo Word64 where
  info _ = basicField "Word64BE" $ FixedSize 8
  
instance (MonadThrow m, MonadST m) => IsSerItem m Word64 where
  sendItem = sendWord64
  receiveItem s = ReadResultT $ receiveWord64 s

instance HasSerInfo UTCTime where
  info _ = basicField "POSIX Seconds (Word64BE)" $ FixedSize 8

instance (MonadThrow m, MonadST m) => IsSerItem m UTCTime where
  sendItem = sendUTCTime
  receiveItem = ReadResultT . receiveUTCTime

newtype Sized (len :: Nat) a = Sized { unSized :: a }
  deriving newtype (Show, Eq)

instance (KnownNat len) => HasSerInfo (Sized len ByteString) where
  info _ =
    let n = fromIntegral $ natVal (Proxy @len)
    in basicField
        ("ByteString[" ++ show n ++ "]")
        $ FixedSize n
instance (MonadThrow m, MonadST m, KnownNat len) => IsSerItem m (Sized len ByteString) where
  sendItem s val = do
    actualLen <- sendBS s (unSized val)
    let expectedLen = fromIntegral $ natVal (Proxy @len)
    unless (actualLen == expectedLen) (error "Length mismatch")

  receiveItem s = do
    let n = fromIntegral $ natVal (Proxy @len)
    Sized <$> ReadResultT (receiveBS s (fromIntegral n))

newtype VariableSized a = VariableSized { unVariableSized :: a }
  deriving newtype (Show, Read, Eq, Ord)

instance HasSerInfo (VariableSized ByteString) where
  info _ =
    compoundField "ByteString"
      [ ("length", info (Proxy @Word32))
      , ("data", basicField "[Word8]" $ VarSize "length")
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

instance ( KESAlgorithm kes
         ) => HasSerInfo (SignKeyKES kes) where
   info _ =
     basicField
       ("SignKeyKES " ++ algorithmNameKES (Proxy @kes))
       (FixedSize . fromIntegral $ sizeSignKeyKES (Proxy @kes))
instance ( MonadSTM m
         , MonadThrow m
         , DirectSerialise m (SignKeyKES kes)
         , DirectDeserialise m (SignKeyKES kes)
         , KESAlgorithm kes
         ) => IsSerItem m (SignKeyKES kes) where
    sendItem s val = do
      directSerialise
        (\buf bufSize -> do
          n <- send s (castPtr buf) (fromIntegral bufSize)
          when (fromIntegral n /= bufSize) (error "AAAAA")
        ) val

    receiveItem s = ReadResultT $ do
      sk <- directDeserialise
        (\buf bufSize -> do
            unsafeReceiveN s buf bufSize >>= \case
              ReadOK n -> do
                when (fromIntegral n /= bufSize) (throwIO (ReadMalformed "Incorrect number of key bytes" :: ReadResult ()))
              x ->  do
                throwIO x
        )
      return $ ReadOK sk


instance ( KESAlgorithm kes ) => HasSerInfo (VerKeyKES kes) where
    info _ =
      basicField
        ("VerKeyKES " ++ algorithmNameKES (Proxy @kes))
        (FixedSize . fromIntegral $ sizeVerKeyKES (Proxy @kes))
instance ( MonadST m
         , MonadSTM m
         , MonadThrow m
         , KESAlgorithm kes
         ) => IsSerItem m (VerKeyKES kes) where
    sendItem s val =
      sendItem s (Sized (rawSerialiseVerKeyKES val) :: Sized (SizeVerKeyKES kes) ByteString)

    receiveItem s = do
      sized <- receiveItem s
      let deser = rawDeserialiseVerKeyKES $ unSized (sized :: Sized (SizeVerKeyKES kes) ByteString)
      case deser of
        Nothing -> ReadResultT . return $ ReadMalformed "Invalid serialised VerKeyKES"
        Just vk -> return vk

instance HasSerInfo KESPeriod where
  info _ = info (Proxy @Word64)

instance (MonadThrow m, MonadST m) => IsSerItem m KESPeriod where
  sendItem s (KESPeriod p) =
    sendItem s (fromIntegral p :: Word64)
  receiveItem s =
    KESPeriod . (fromIntegral @Word64) <$> receiveItem s

instance ( Crypto c , Typeable c) => HasSerInfo (OCert c) where
    info _ =
      compoundField
        ("OCert " ++ algorithmNameKES (Proxy @(KES c)) ++ " " ++ algorithmNameDSIGN (Proxy @(DSIGN c)))
        [ ("size", info (Proxy @Word32))
        , ("data", basicField "raw-serialized OCert" (VarSize "size"))
        ]

instance ( MonadThrow m, MonadST m, Crypto c , Typeable c) => IsSerItem m (OCert c) where
    sendItem s oc =
      sendItem s (VariableSized . serialize' $ oc)

    receiveItem s =
      unsafeDeserialize' . unVariableSized <$> receiveItem s

instance HasSerInfo a => HasSerInfo (CRef m a) where
  info (p :: proxy (CRef m a)) = info (Proxy @a)

class ToCRef m a where
  toCRef :: a -> m (CRef m a)

instance (MonadThrow m, MonadSTM m, ToCRef m a, IsSerItem m a) => IsSerItem m (CRef m a) where
  sendItem s var = withCRefValue var $ sendItem s
  receiveItem s = receiveItem s >>= lift . toCRef

instance (MonadThrow m, MonadSTM m, MonadST m, KESAlgorithm kes)
          => ToCRef m (SignKeyWithPeriodKES kes) where
  toCRef = newCRef (forgetSignKeyKES . skWithoutPeriodKES)

instance HasSerInfo Period where
  info _ = basicField "Period" (FixedSize 32)

instance (MonadThrow m, MonadST m) => IsSerItem m Period where
  sendItem s = sendWord32 s . fromIntegral
  receiveItem s = fromIntegral <$> (receiveItem s :: ReadResultT m Word32)

tyVarName :: TyVarBndr a -> Name
tyVarName (PlainTV n _) = n
tyVarName (KindedTV n _ _) = n

-- | Derive 'HasSerInfo' for a record type that must be qualified with a type
-- argument that has a 'Crypto' instance, and whose associated 'KES' and
-- 'DSIGN' types have 'KESAlgorithm' and 'DSIGNAlgorithm' instances,
-- respectively.
deriveHasSerInfoWithCrypto :: Name -> DecsQ
deriveHasSerInfoWithCrypto typeName = do
  reify typeName >>= \case
    TyConI (DataD [] tyName tyVars Nothing [RecC conName fields] []) -> do
      [d| instance
              ( KESAlgorithm (KES $(varT $ tyVarName $ head tyVars))
              , DSIGNAlgorithm (DSIGN $(varT $ tyVarName $ head tyVars))
              ) => HasSerInfo $(foldl appT (conT tyName) [ varT (tyVarName bndr) | bndr <- tyVars ]) where
            info _ =
              compoundField
                $(litE (stringL (nameBase tyName)))
                $(listE
                    [ [| ( $(litE (stringL (nameBase fieldName)))
                         , info (Proxy :: Proxy $(return fieldTy))
                         )
                      |]
                    | (fieldName, _, fieldTy) <- fields
                    ]
                  )
        |]
    x ->
      error . show $ x

-- | Derive 'HasSerInfo' for a record type that doesn't need further constraints.
deriveHasSerInfo :: Name -> DecsQ
deriveHasSerInfo typeName = do
  reify typeName >>= \case
    TyConI (DataD [] tyName tyVars Nothing [RecC conName fields] []) -> do
      [d| instance HasSerInfo $(foldl appT (conT tyName) [ varT (tyVarName bndr) | bndr <- tyVars ]) where
            info _ =
              compoundField
                $(litE (stringL (nameBase tyName)))
                $(listE
                    [ [| ( $(litE (stringL (nameBase fieldName)))
                         , info (Proxy :: Proxy $(return fieldTy))
                         )
                      |]
                    | (fieldName, _, fieldTy) <- fields
                    ]
                  )
        |]
    x ->
      error . show $ x

-- <$> :: (a -> b) -> f a -> f b
-- <*> :: f (a -> b) -> f a -> f b
foldApplicative :: ExpQ -> [ExpQ] -> ExpQ
foldApplicative initial [] = [| pure $initial |]
foldApplicative initial [x] = [| $initial <$> $x |]
foldApplicative initial (x:xs) =
  foldl (\y x -> [| $y <*> $x |]) [| $initial <$> $x |] xs

-- | Derive 'IsSerItem' for a record type that must be qualified with a type
-- argument that has a 'Crypto' instance, and whose associated 'KES' and
-- 'DSIGN' types have 'KESAlgorithm' and 'DSIGNAlgorithm' instances,
-- respectively.
deriveIsSerItemWithCrypto :: Name -> DecsQ
deriveIsSerItemWithCrypto typeName = do
  reify typeName >>= \case
    TyConI (DataD [] tyName tyVars Nothing [RecC conName fields] []) -> do
      [d| instance 
              ( KESAlgorithm (KES $(varT $ tyVarName $ head tyVars))
              , DSIGNAlgorithm (DSIGN $(varT $ tyVarName $ head tyVars))
              , MonadThrow m
              , MonadST m
              , MonadSTM m
              , (forall x y. Coercible x y => Coercible (m x) (m y))
              )
              => IsSerItem m $(foldl appT (conT tyName) [ varT (tyVarName bndr) | bndr <- tyVars ]) where
            sendItem s item =
              $(foldr1 (\a b -> varE '(>>) `appE` a `appE` b)
                [ [| sendItem s ($(varE fieldName) item) |]
                | (fieldName, _, _) <- fields
                ]
               )
            receiveItem s =
              $(foldApplicative
                  (conE conName)
                  [ [| receiveItem s |] | _ <- fields ]
               )
        |]
    x ->
      error . show $ x

-- | Derive 'IsSerItem' for a record type that doesn't need further constraints.
deriveIsSerItem :: Name -> DecsQ
deriveIsSerItem typeName = do
  reify typeName >>= \case
    TyConI (DataD [] tyName tyVars Nothing [RecC conName fields] []) -> do
      [d| instance
            ( MonadThrow m
            , MonadST m
            , MonadSTM m
            , (forall x y. Coercible x y => Coercible (m x) (m y))
            ) => IsSerItem m $(foldl appT (conT tyName) [ varT (tyVarName bndr) | bndr <- tyVars ]) where
            sendItem s item =
              $(foldr1 (\a b -> varE '(>>) `appE` a `appE` b)
                [ [| sendItem s ($(varE fieldName) item) |]
                | (fieldName, _, _) <- fields
                ]
               )
            receiveItem s =
              $(foldApplicative
                  (conE conName)
                  [ [| receiveItem s |] | _ <- fields ]
               )
        |]
    x ->
      error . show $ x

deriveSer :: Name -> DecsQ
deriveSer typeName =
  (++) <$> deriveHasSerInfo typeName
       <*> deriveIsSerItem typeName

deriveSerWithCrypto :: Name -> DecsQ
deriveSerWithCrypto typeName =
  (++) <$> deriveHasSerInfoWithCrypto typeName
       <*> deriveIsSerItemWithCrypto typeName

instance (KESAlgorithm kes, HasSerInfo (SignKeyKES kes))
          => HasSerInfo (SignKeyWithPeriodKES kes) where
  info _ =
    compoundField
      ("SignKeyWithPeriodKES " ++ algorithmNameKES (Proxy @kes))
      [ ("signKey", info (Proxy @(SignKeyKES kes)))
      , ("period", info (Proxy @Period))
      ]

instance ( MonadThrow m
         , MonadST m
         , KESAlgorithm kes
         , IsSerItem m (SignKeyKES kes)
         )
         => IsSerItem m (SignKeyWithPeriodKES kes) where
  sendItem s skp = do
    sendItem s (skWithoutPeriodKES skp)
    sendItem s (periodKES skp)

  receiveItem s =
    SignKeyWithPeriodKES <$> receiveItem s <*> receiveItem s

instance ( HasSerInfo (SignKeyKES (KES c))
         , Typeable c
         , Crypto c
         , KESAlgorithm (KES c)
         )
         => HasSerInfo (Bundle m c) where
  info _ =
    compoundField
      ("Bundle " ++ algorithmNameKES (Proxy @(KES c)) ++ " " ++ algorithmNameDSIGN (Proxy @(DSIGN c)))
      [ ("signKeyWithPeriod", info (Proxy @(CRef m (SignKeyWithPeriodKES (KES c)))))
      , ("ocert", info (Proxy @(OCert c)))
      ]

instance ( DSIGNAlgorithm dsign ) => HasSerInfo (SigDSIGN dsign) where
    info _ =
      basicField
        ("SigDSIGN " ++ algorithmNameDSIGN (Proxy @dsign))
        (FixedSize . fromIntegral $ sizeSigDSIGN (Proxy @dsign))
instance ( MonadST m
         , MonadSTM m
         , MonadThrow m
         , DSIGNAlgorithm dsign
         ) => IsSerItem m (SigDSIGN dsign) where
    sendItem s val =
      sendItem s (Sized (rawSerialiseSigDSIGN val) :: Sized (SizeSigDSIGN dsign) ByteString)

    receiveItem s = do
      sized <- receiveItem s
      let deser = rawDeserialiseSigDSIGN $ unSized (sized :: Sized (SizeSigDSIGN dsign) ByteString)
      case deser of
        Nothing -> ReadResultT . return $ ReadMalformed "Invalid serialised SigDSIGN"
        Just vk -> return vk


instance ( DSIGNAlgorithm dsign ) => HasSerInfo (VerKeyDSIGN dsign) where
    info _ =
      basicField
        ("VerKeyDSIGN " ++ algorithmNameDSIGN (Proxy @dsign))
        (FixedSize . fromIntegral $ sizeVerKeyDSIGN (Proxy @dsign))
instance ( MonadST m
         , MonadSTM m
         , MonadThrow m
         , DSIGNAlgorithm dsign
         ) => IsSerItem m (VerKeyDSIGN dsign) where
    sendItem s val =
      sendItem s (Sized (rawSerialiseVerKeyDSIGN val) :: Sized (SizeVerKeyDSIGN dsign) ByteString)

    receiveItem s = do
      sized <- receiveItem s
      let deser = rawDeserialiseVerKeyDSIGN $ unSized (sized :: Sized (SizeVerKeyDSIGN dsign) ByteString)
      case deser of
        Nothing -> ReadResultT . return $ ReadMalformed "Invalid serialised VerKeyDSIGN"
        Just vk -> return vk

instance ( MonadThrow m
         , MonadST m
         , MonadSTM m
         , IsSerItem m (SignKeyKES (KES c))
         , Typeable c
         , Crypto c
         , KESAlgorithm (KES c)
         )
         => IsSerItem m (Bundle m c) where
    sendItem s bundle = do
      sendItem s (bundleSKP bundle)
      sendItem s (bundleOC bundle)

    receiveItem s =
      Bundle <$> receiveItem s <*> receiveItem s

newtype ViaEnum a = ViaEnum { viaEnum :: a }

instance (Typeable a) => HasSerInfo (ViaEnum a) where
  info _ =
    basicField
      "Enum"
      (FixedSize 2)

typeName :: Typeable a => Proxy a -> String
typeName = tyConName . typeRepTyCon . typeRep

instance (MonadThrow m, MonadST m, Typeable a, Enum a, Bounded a) => IsSerItem m (ViaEnum a) where
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

instance HasSerInfo (SigDSIGN d) => HasSerInfo (SignedDSIGN d a) where
  info _ =
    info (Proxy @(SigDSIGN d))

instance (Monad m, IsSerItem m (SigDSIGN d)) => IsSerItem m (SignedDSIGN d a) where
  sendItem s (SignedDSIGN sig) = sendItem s sig
  receiveItem s = SignedDSIGN <$> receiveItem s

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
      n -> ReadResultT . return $ ReadMalformed "Bool"

deriving via (ViaEnum RecvResult)
  instance HasSerInfo RecvResult

deriving via (ViaEnum RecvResult)
  instance
    ( forall x y. Coercible x y => Coercible (m x) (m y)
    , MonadThrow m
    , MonadST m
    ) => IsSerItem m RecvResult

instance HasSerInfo a => HasSerInfo (Maybe a) where
  info _ =
    compoundField
      ("Maybe " ++ fieldType (info (Proxy @a)))
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

printSpec :: FieldInfo -> IO ()
printSpec =
  go 0
  where
    go :: Int -> FieldInfo -> IO ()
    go indent f = do
      printIndent indent
      goField indent f

    printIndent :: Int -> IO ()
    printIndent indent =
      putStr (replicate (indent * 4) ' ')

    printSubfield :: Int -> SubfieldInfo -> IO ()
    printSubfield indent sfi = do
      printIndent indent
      putStrLn $ subfieldName sfi ++ ":"
      goField (succ indent) (subfieldInfo sfi)

    printChoice :: Int -> Int -> FieldInfo -> IO ()
    printChoice indent n f = do
      printIndent indent
      putStrLn $ show n ++ " -> "
      goField (succ indent) f

    goField :: Int -> FieldInfo -> IO ()
    goField indent field = do
      printIndent indent
      putStrLn ("TYPE: " ++ fieldType field)

      printIndent indent
      putStrLn ("SIZE: " ++ formatFieldSize (fieldSize field))

      case field of
        CompoundField cfi -> do
          mapM_ (printSubfield $ succ indent) (compoundFieldSubfields cfi)

        ListField lfi -> do
          printIndent indent
          putStrLn $ "#ELEMS: " ++ formatFieldSize (listSize lfi)
          printIndent indent
          putStrLn $ "ELEM TYPE:"
          goField (succ indent) (listElemInfo lfi)

        ChoiceField cfi -> do
          printIndent indent
          putStrLn $ "CHOOSE BY: " ++ formatChoiceCondition (choiceCondition cfi)
          zipWithM_ (printChoice $ succ indent) [0,1..] (choiceFieldAlternatives cfi)
          
        _ ->
          return ()

formatChoiceCondition :: ChoiceCondition -> String
formatChoiceCondition (IndexField var) = var
formatChoiceCondition (IndexFlag var mask) = var ++ " & " ++ printf "0x%04x" mask

formatFieldSize :: FieldSize -> String
formatFieldSize (FixedSize n) = show n
formatFieldSize (VarSize var) = var
formatFieldSize UnknownSize = "VARIABLE"
formatFieldSize (RangeSize min max) = "(" ++ formatFieldSize min ++ " .. " ++ formatFieldSize max ++ ")"
formatFieldSize (BinopSize FSPlus a b) = "(" ++ formatFieldSize a ++ " + " ++ formatFieldSize b ++ ")"
formatFieldSize (BinopSize FSMul a b) = "(" ++ formatFieldSize a ++ " * " ++ formatFieldSize b ++ ")"
formatFieldSize (BinopSize FSMax a b) = "MAX(" ++ formatFieldSize a ++ ", " ++ formatFieldSize b ++ ")"
formatFieldSize (BinopSize FSMin a b) = "MIN(" ++ formatFieldSize a ++ ", " ++ formatFieldSize b ++ ")"
