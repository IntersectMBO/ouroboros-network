{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Serialization.Spec.Types
where

import Data.Data
import Data.Word
import Data.Coerce
import Language.Haskell.TH.Syntax (Lift)
import Data.String

-- * Documentation annotation types

newtype Description = Description { descriptionParagraphs :: [String] }
  deriving newtype (Show, Read, Eq, Semigroup, Monoid)
  deriving (Data, Typeable, Lift)

-- * 'FieldInfo' and related types

data FieldInfo
  = AnnField String FieldInfo
  | BasicField BasicFieldInfo
  | EnumField EnumFieldInfo
  | CompoundField CompoundFieldInfo
  | ChoiceField ChoiceFieldInfo
  | ListField ListFieldInfo
  | AliasField AliasFieldInfo
  | SumField SumFieldInfo
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
    , enumFieldValues :: ![(Int, String)]
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

data SumFieldInfo =
  SumFieldInfo
    { sumFieldType :: !String
    , sumFieldAlternatives :: ![(String, FieldInfo)]
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

annField :: String -> FieldInfo -> FieldInfo
annField = AnnField

basicField :: String -> FieldSize -> FieldInfo
basicField ty size = BasicField $ BasicFieldInfo ty size

enumField :: String -> [(Int, String)] -> FieldInfo
enumField ty values = EnumField $ EnumFieldInfo ty values

enumField_ :: String -> [String] -> FieldInfo
enumField_ ty values = enumField ty (zip [0,1..] values)

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

sumField :: String -> [(String, FieldInfo)] -> FieldInfo
sumField name alternatives =
  SumField $
    SumFieldInfo
      name
      alternatives

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

