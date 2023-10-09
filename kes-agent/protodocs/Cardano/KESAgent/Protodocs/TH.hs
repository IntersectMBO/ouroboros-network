{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.KESAgent.Protodocs.TH
where

import Control.Monad
import Network.TypedProtocol.Core
import Language.Haskell.TH
import Data.Maybe
import Cardano.KESAgent.Serialization.Spec
import Cardano.KESAgent.Serialization.Spec.Class
import Data.Char

data ProtocolDescription =
  ProtocolDescription
    { protocolName :: String
    , protocolDescription :: Maybe Description
    , protocolStates :: [(String, Maybe Description)]
    }
    deriving (Show)

data MessageDescription =
  MessageDescription
    { messageName :: String
    , messageDescription :: Maybe Description
    , messagePayload :: [String]
    , messageFromState :: String
    , messageToState :: String
    , messageInfo :: FieldInfo
    }

getConName :: Con -> Name
getConName = \case
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c -> getConName c
  GadtC (n:_) _ _ -> n
  RecGadtC (n:_) _ _ -> n
  x -> error $ "Cannot get constructor name for " ++ show x

describeProtocol :: Name -> ExpQ
describeProtocol protocol = do
  info <- reify protocol
  protoDescription <- getDescription protocol
  case info of
    TyConI (DataD _ tyName binders _ valCons _) -> do
      let pname = nameBase tyName
      pstates <- forM valCons $ \valCon -> do
        let conName = getConName valCon
        stateDescription <- getDescription conName
        return (conName, stateDescription)
      [| ProtocolDescription
            $(litE (stringL pname))
            protoDescription
            $(listE
                [ [| ( $(litE . stringL . nameBase $ conName), stateDescription) |]
                | (conName, stateDescription) <- pstates
                ]
             )
       |]
    x -> error $ show x

argSplit :: Type -> ([Type], Type)
argSplit t@(AppT (AppT (AppT (ConT m) _) _) _)
  | m == ''Message
  = ([], t)
argSplit (AppT a b) =
  let (xs, z) = argSplit b
  in (unearthType a : xs, z)
argSplit x = ([], x)

unearthType :: Type -> Type
unearthType (AppT (AppT MulArrowT _) t) = unearthType t
-- unearthType (AppT a _) = unearthType a
unearthType (SigT a _) = unearthType a
unearthType t = t


extractStates :: Type -> (Type, Type)
extractStates (AppT (AppT (AppT (ConT m) _) a) b)
  | m == ''Message
  = (a, b)
extractStates x = error $ show x

prettyTy :: Type -> String
prettyTy = snd . go
  where
    go (ConT n) = (False, nameBase n)
    go (PromotedT n) = (False, nameBase n)
    go (VarT n) = (False, nameBase n)
    go (AppT a b) =
      let
        (_, a') = go a
        (wrap, b') = go b
      in
        (True, a' ++ " " ++ if wrap then "(" ++ b' ++ ")" else b')
    go (ForallT _ _ a) = go a
    go (ForallVisT _ a) = go a
    go (AppKindT _ a) = go a
    go t = (True, show t)

getDescription :: Name -> Q (Maybe Description)
getDescription name = do
  haddockMay <- getDoc (DeclDoc name)
  return (Description . lines <$> haddockMay)

describeProtocolMessage :: Name -> Name -> Name -> ExpQ
describeProtocolMessage protocolName crypto msgName = do
  msgInfo <- reify msgName
  msgDescription <- getDescription msgName
  runIO . putStrLn $ show msgName ++ ": " ++ show msgDescription
  case msgInfo of
    DataConI conName (ForallT (tvM : tvC : tyVars) [] x) tyName -> do
      let (payloads, lastArg) = argSplit x
      let (fromState, toState) = extractStates lastArg
          fromStateName = prettyTy . unearthType $ fromState
          toStateName = prettyTy . unearthType $ toState
          fromStateTy = (if isUpper (head fromStateName) then conT else varT) $ mkName fromStateName
          toStateTy = (if isUpper (head toStateName) then conT else varT) $ mkName toStateName

      [e| MessageDescription
            { messageName = $(litE . stringL . nameBase $ msgName)
            , messageDescription = msgDescription
            , messagePayload = $(listE (map (litE . stringL . prettyTy) payloads))
            , messageFromState = $(litE (stringL . prettyTy $ unearthType fromState))
            , messageToState = $(litE (stringL . prettyTy $ unearthType toState))
            , messageInfo =
                info
                  (Proxy ::
                    Proxy
                      ( Message
                          ($(conT protocolName) IO $(conT crypto))
                          $fromStateTy
                          $toStateTy
                      )
                  )
            }
        |]
    _ -> error $ show msgInfo
  where
    formatT :: Type -> (Bool, String)
    -- formatT (AppT a b) =
    --   let
    --     (_, l) = formatT a
    --     (complexR, r) = formatT b
    --     in
    --       (True, l ++ " " ++ (if complexR then "(" ++ r ++ ")" else r))
    -- formatT (ConT n) = (False, nameBase n)
    -- formatT (VarT n) = (False, nameBase n)
    -- formatT (SigT t _) = formatT t
    -- formatT (PromotedT n) = (False, nameBase n)
    formatT x = (True, show x)

conName :: Con -> [String]
conName con = case con of
  NormalC conName _ -> [nameBase conName]
  RecC conName _ -> [nameBase conName]
  InfixC _ conName _ -> [nameBase conName]
  ForallC _ _ con -> conName con
  GadtC names _ _ -> map nameBase $ names
  RecGadtC names _ _ -> map nameBase $ names
