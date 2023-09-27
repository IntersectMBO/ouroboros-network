{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoStarIsType #-}

module Cardano.KESAgent.Serialization.Spec.Types
where

import Data.Word

-- * 'FieldInfo' and related types

data FieldInfo
  = BasicField BasicFieldInfo
  | EnumField EnumFieldInfo
  | CompoundField CompoundFieldInfo
  | ChoiceField ChoiceFieldInfo
  | ListField ListFieldInfo
  | AliasField AliasFieldInfo
  deriving (Show)

data BasicFieldInfo =
  BasicFieldInfo
    { basicFieldType :: !String
    , basicFieldSize :: !FieldSize
    }
  deriving (Show)

data EnumFieldInfo =
  EnumFieldInfo
    { enumFieldType :: !String
    , enumFieldValues :: ![String]
    }
  deriving (Show)

data AliasFieldInfo =
  AliasFieldInfo
    { aliasFieldName :: !String
    , aliasFieldTarget :: FieldInfo
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

enumField :: String -> [String] -> FieldInfo
enumField ty values = EnumField $ EnumFieldInfo ty values

aliasField :: String -> FieldInfo -> FieldInfo
aliasField name ty = AliasField $ AliasFieldInfo name ty

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

-- * Field sizes

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

