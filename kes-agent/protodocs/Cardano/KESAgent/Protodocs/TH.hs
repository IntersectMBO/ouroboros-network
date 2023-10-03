{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

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
    , protocolStates :: [String]
    }
    deriving (Show)

data MessageDescription =
  MessageDescription
    { messageName :: String
    , messagePayload :: [String]
    , messageFromState :: String
    , messageToState :: String
    , messageInfo :: FieldInfo
    }

describeProtocolStates :: Name -> ExpQ
describeProtocolStates protocol = do
  info <- reify protocol
  case info of
    TyConI (DataD _ tyName binders _ valCons _) -> do
      let pname = nameBase tyName
          pstates = map conName valCons
      [| ProtocolDescription
            $(litE (stringL pname))
            $(listE (map (litE . stringL) $ concat pstates))
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

describeProtocolMessage :: Name -> Name -> Name -> ExpQ
describeProtocolMessage protocolName crypto msgName = do
  msgInfo <- reify msgName
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
