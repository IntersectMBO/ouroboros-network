{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Cardano.KESAgent.Serialization.Spec (
  module Cardano.KESAgent.Serialization.Spec,
  module Cardano.KESAgent.Serialization.Spec.Types,
  module Cardano.KESAgent.Serialization.Spec.Class,
  module Cardano.KESAgent.Serialization.Spec.OrphansBase,
  module Cardano.KESAgent.Serialization.Spec.OrphansCardano,
  module Cardano.KESAgent.Serialization.Spec.TH,
  Proxy (..),
  StandardCrypto,
)
where

import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Serialization.Spec.Class
import Cardano.KESAgent.Serialization.Spec.OrphansBase
import Cardano.KESAgent.Serialization.Spec.OrphansCardano
import Cardano.KESAgent.Serialization.Spec.TH
import Cardano.KESAgent.Serialization.Spec.Types

import Control.Monad (zipWithM_)
import Data.Proxy
import Text.Printf

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

      goFieldContents indent field

    goFieldContents :: Int -> FieldInfo -> IO ()
    goFieldContents indent field =
      case field of
        AliasField afi -> do
          goFieldContents indent (aliasFieldTarget afi)
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
          zipWithM_ (printChoice $ succ indent) [0, 1 ..] (choiceFieldAlternatives cfi)
        _ ->
          return ()

formatChoiceCondition :: ChoiceCondition -> String
formatChoiceCondition (IndexField var) = var
formatChoiceCondition (IndexFlag var mask) = var ++ " & " ++ printf "0x%04x" mask

formatFieldSize :: FieldSize -> String
formatFieldSize (FixedSize n) = show n
formatFieldSize (VarSize var) = var
formatFieldSize UnknownSize = "VARIABLE"
formatFieldSize (RangeSize lo hi) = "(" ++ formatFieldSize lo ++ " .. " ++ formatFieldSize hi ++ ")"
formatFieldSize (BinopSize FSPlus a b) = "(" ++ formatFieldSize a ++ " + " ++ formatFieldSize b ++ ")"
formatFieldSize (BinopSize FSMul a b) = "(" ++ formatFieldSize a ++ " * " ++ formatFieldSize b ++ ")"
formatFieldSize (BinopSize FSMax a b) = "MAX(" ++ formatFieldSize a ++ ", " ++ formatFieldSize b ++ ")"
formatFieldSize (BinopSize FSMin a b) = "MIN(" ++ formatFieldSize a ++ ", " ++ formatFieldSize b ++ ")"
