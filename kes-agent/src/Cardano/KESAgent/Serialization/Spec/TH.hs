{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Cardano.KESAgent.Serialization.Spec.TH
( deriveSer
, deriveSerWithCrypto
)
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.Serialization.Spec.Types
import Cardano.KESAgent.Serialization.Spec.Class

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow ( MonadThrow )
import Data.Coerce
import Data.List
import Data.Proxy
import Language.Haskell.TH
import Data.Char

-- * Deriving 'HasSerInfo' and 'IsSerItem' with Template Haskell

strippedFieldName :: Name -> Name -> String
strippedFieldName tyName fieldName =
  let tyStr = nameBase tyName
      fieldStr = nameBase fieldName
      lcfirst [] = []
      lcfirst (x:xs) = toLower x : xs
      tyStrLC = lcfirst tyStr
  in
    if tyStrLC `isPrefixOf` fieldStr then
      drop (length tyStrLC) fieldStr
    else
      fieldStr

-- | Derive 'HasSerInfo' for a record type that must be qualified with a type
-- argument that has a 'Crypto' instance, and whose associated 'KES' and
-- 'DSIGN' types have 'KESAlgorithm' and 'DSIGNAlgorithm' instances,
-- respectively.
deriveHasSerInfoWithCrypto :: Name -> DecsQ
deriveHasSerInfoWithCrypto typeName = do
  reify typeName >>= \case
    TyConI (DataD [] tyName tyVars Nothing [RecC _ fields] []) -> do
      let tyParamE = varT . tyVarName . head $ tyVars
      [d| instance
              ( KESAlgorithm (KES $tyParamE)
              , DSIGNAlgorithm (DSIGN $tyParamE)
              , HasSerInfo (VerKeyKES (KES $tyParamE))
              ) => HasSerInfo $(foldl appT (conT tyName) [ varT (tyVarName bndr) | bndr <- tyVars ]) where
            info _ =
              compoundField
                $(litE (stringL (nameBase tyName)))
                $(listE
                    [ [| ( $(litE (stringL (strippedFieldName tyName fieldName)))
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
    TyConI (DataD [] tyName tyVars Nothing [RecC _ fields] []) -> do
      [d| instance HasSerInfo $(foldl appT (conT tyName) [ varT (tyVarName bndr) | bndr <- tyVars ]) where
            info _ =
              compoundField
                $(litE (stringL (nameBase tyName)))
                $(listE
                    [ [| ( $(litE (stringL (strippedFieldName tyName fieldName)))
                         , info (Proxy :: Proxy $(return fieldTy))
                         )
                      |]
                    | (fieldName, _, fieldTy) <- fields
                    ]
                  )
        |]
    x ->
      error . show $ x

-- | Derive 'IsSerItem' for a record type that must be qualified with a type
-- argument that has a 'Crypto' instance, and whose associated 'KES' and
-- 'DSIGN' types have 'KESAlgorithm' and 'DSIGNAlgorithm' instances,
-- respectively.
deriveIsSerItemWithCrypto :: Name -> DecsQ
deriveIsSerItemWithCrypto typeName = do
  reify typeName >>= \case
    TyConI (DataD [] tyName tyVars Nothing [RecC conName fields] []) -> do
      let tyParamE = varT . tyVarName . head $ tyVars
      [d| instance
              ( KESAlgorithm (KES $tyParamE)
              , DSIGNAlgorithm (DSIGN $tyParamE)
              , HasSerInfo (VerKeyKES (KES $tyParamE))
              , IsSerItem m (VerKeyKES (KES $tyParamE))
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

-- <$> :: (a -> b) -> f a -> f b
-- <*> :: f (a -> b) -> f a -> f b
foldApplicative :: ExpQ -> [ExpQ] -> ExpQ
foldApplicative initial [] = [| pure $initial |]
foldApplicative initial [x] = [| $initial <$> $x |]
foldApplicative initial (x:xs) =
  foldl (\a b -> [| $a <*> $b |]) [| $initial <$> $x |] xs

tyVarName :: TyVarBndr a -> Name
tyVarName (PlainTV n _) = n
tyVarName (KindedTV n _ _) = n
