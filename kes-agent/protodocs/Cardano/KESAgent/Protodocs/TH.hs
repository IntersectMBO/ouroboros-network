{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveLift #-}

module Cardano.KESAgent.Protodocs.TH
where

import Control.Monad
import Network.TypedProtocol.Core
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax (Lift (..))
import Data.Maybe
import Cardano.KESAgent.Serialization.Spec
import Cardano.KESAgent.Serialization.Spec.Class
import Cardano.KESAgent.Protocols.VersionedProtocol
import Data.Char

data AgencyID
  = ClientAgencyID
  | ServerAgencyID
  | NobodyAgencyID
  deriving (Show, Read, Ord, Eq, Enum, Bounded, Lift)

data ProtocolDescription =
  ProtocolDescription
    { protocolName :: String
    , protocolDescription :: [Description]
    , protocolIdentifier :: VersionIdentifier
    , protocolStates :: [(String, [Description], AgencyID)]
    , protocolMessages :: [MessageDescription]
    }
    deriving (Show)

data MessageDescription =
  MessageDescription
    { messageName :: String
    , messageDescription :: [Description]
    , messagePayload :: [String]
    , messageFromState :: String
    , messageToState :: String
    , messageInfo :: FieldInfo
    }
    deriving (Show)

getConName :: Con -> Name
getConName = \case
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c -> getConName c
  GadtC (n:_) _ _ -> n
  RecGadtC (n:_) _ _ -> n
  x -> error $ "Cannot get constructor name for " ++ show x

describeProtocol :: Name -> Name -> ExpQ
describeProtocol protocol crypto = do
  info <- reifyDatatype protocol
  protoDescription <- getDescription protocol
  let pname = nameBase (datatypeName info)
  pstates <- forM (datatypeCons info) $ \conInfo -> do
    let conName = constructorName conInfo
    stateDescription <- getDescription conName
    let agencyID = NobodyAgencyID

    return (conName, stateDescription, agencyID)

  [DataInstD _ _ ty _ cons _] <- reifyInstances ''Message [AppT (AppT (ConT protocol) (ConT ''IO)) (ConT crypto)]

  let messageInfos = map (describeProtocolMessage protocol crypto . extractConName) cons

  [| ProtocolDescription
        $(litE (stringL pname))
        protoDescription
        (versionIdentifier (Proxy :: Proxy ($(conT protocol) IO $(conT crypto))))
        $(listE
            [ [| ( $(litE . stringL . nameBase $ conName), stateDescription, agencyID) |]
            | (conName, stateDescription, agencyID) <- pstates
            ]
         )
         $(listE messageInfos)
   |]

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
extractStates (AppT (AppT (AppT (ConT m) _c) a) b)
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

getDescription :: Name -> Q [Description]
getDescription name = do
  haddock <- maybeToList <$> getDoc (DeclDoc name)
  annotations <- reifyAnnotations (AnnLookupName name)
  return $ (Description . (:[]) <$> haddock) ++ annotations

setTyVars2 :: Type -> Type -> Type -> Type
setTyVars2 (AppT (AppT a _m) _c) m c = AppT (AppT a m) c
setTyVars2 t _ _ = error $ show t

unSigTy :: Type -> Type
unSigTy (SigT t _) = t
unSigTy t@(VarT {}) = t
unSigTy t = error $ show t

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
          undefinedMessageExpr = foldl appE (conE msgName) [ [e|undefined|] | _ <- payloads ]

      [e| MessageDescription
            { messageName = $(litE . stringL . nameBase $ msgName)
            , messageDescription = msgDescription
            , messagePayload = $(listE (map (litE . stringL . prettyTy) payloads))
            , messageFromState = $(litE (stringL . prettyTy $ unearthType fromState))
            , messageToState = $(litE (stringL . prettyTy $ unearthType toState))
            , messageInfo =
                infoOf (
                      $undefinedMessageExpr ::
                        ( Message
                            ($(conT protocolName) IO $(conT crypto))
                            $(pure $ unSigTy fromState)
                            $(pure $ unSigTy toState)
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

extractConName :: Con -> Name
extractConName con = case con of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ con -> extractConName con
  GadtC (name:_) _ _ -> name
  RecGadtC (name:_) _ _ -> name
  x -> error $ "Cannot extract constructor name from " ++ show x
