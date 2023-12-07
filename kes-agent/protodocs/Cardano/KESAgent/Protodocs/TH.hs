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
import Cardano.KESAgent.Protocols.VersionedProtocol
import Data.Char
import Debug.Trace
import Data.SerDoc.Class
import Data.SerDoc.Info

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

applyTyArgs :: Type -> [Name] -> Type
applyTyArgs t [] = t
applyTyArgs t (x:xs) =
  applyTyArgs (AppT t (ConT x)) xs

describeProtocol :: Name -> [Name] -> ExpQ
describeProtocol protocol tyArgs = do
  info <- reifyDatatype protocol
  protoDescription <- getDescription protocol
  let pname = nameBase (datatypeName info)
  pstates <- forM (datatypeCons info) $ \conInfo -> do
    let conName = constructorName conInfo
    stateDescription <- getDescription conName
    let agencyID = NobodyAgencyID

    return (conName, stateDescription, agencyID)

  let protocolTy = applyTyArgs (ConT protocol) tyArgs

  [DataInstD _ _ ty _ cons _] <- reifyInstances ''Message [protocolTy]

  let messageInfos = map (describeProtocolMessage protocol tyArgs . extractConName) cons

  [| ProtocolDescription
        $(litE (stringL pname))
        protoDescription
        (versionIdentifier (Proxy :: Proxy ($(pure protocolTy))))
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

unSigTy :: Type -> Type
unSigTy (SigT t _) = t
unSigTy t@(VarT {}) = t
unSigTy t@(PromotedT {}) = t
unSigTy t = error $ show t

describeProtocolMessage :: Name -> [Name] -> Name -> ExpQ
describeProtocolMessage protocolName tyArgs msgName = do
  msgInfo <- reifyConstructor msgName
  msgTyInfo <- reifyDatatype msgName
  msgDescription <- getDescription msgName


  let payloads = constructorFields msgInfo
      undefinedMessageExpr = foldl appE (conE msgName) [ [e|undefined|] | _ <- payloads ]

      tyVarName :: TyVarBndr a -> Name
      tyVarName (PlainTV n _) = n
      tyVarName (KindedTV n _ _) = n

      findType :: Name -> Cxt -> Type
      findType n (AppT (AppT EqualityT (VarT vn)) t : xs)
        | vn == n
        = t
      findType n (x : xs) = findType n xs
      findType n [] = VarT n

      fromStateVar = tyVarName . last . init $ datatypeVars msgTyInfo
      toStateVar = tyVarName . last $ datatypeVars msgTyInfo
      fromState = findType fromStateVar (constructorContext msgInfo)
      toState = findType toStateVar (constructorContext msgInfo)

  [e| MessageDescription
        { messageName = $(litE . stringL . nameBase $ msgName)
        , messageDescription = msgDescription
        , messagePayload = $(listE (map (litE . stringL . prettyTy) payloads))
        , messageFromState = $(litE (stringL . prettyTy $ unearthType fromState))
        , messageToState = $(litE (stringL . prettyTy $ unearthType toState))
        , messageInfo =
            infoOf (
                  $undefinedMessageExpr ::
                    ( $(conT $ datatypeName msgTyInfo)
                        $(pure $ applyTyArgs (ConT protocolName) tyArgs)
                        $(pure $ unSigTy fromState)
                        $(pure $ unSigTy toState)
                    )
                )
        }
    |]

extractConName :: Con -> Name
extractConName con = case con of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ con -> extractConName con
  GadtC (name:_) _ _ -> name
  RecGadtC (name:_) _ _ -> name
  x -> error $ "Cannot extract constructor name from " ++ show x
