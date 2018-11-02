{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Payload (
      Payload
    , PayloadType(..)
    , Sing(..)
    , readPayloadType
    , allPayloadTypes
    , PayloadImplementation
    , fixupBlock
    , chainFrom
    , toChain
    ) where

import           Data.List (intercalate)
import           Data.Semigroup ((<>))

import           Block
import           Chain (Chain)
import qualified DummyPayload as Dummy
import           Infra.Util
import qualified MockPayload as Mock
import           Ouroboros
import           Util.Singletons

data PayloadType =
    DummyPayloadType
  | MockPayloadType
  deriving (Enum, Bounded)

data instance Sing (k :: PayloadType) where
  SDummyPayload :: Sing 'DummyPayloadType
  SMockPayload  :: Sing 'MockPayloadType

instance SingI 'DummyPayloadType where sing = SDummyPayload
instance SingI 'MockPayloadType  where sing = SMockPayload

instance SingKind PayloadType where
  type Demote PayloadType = PayloadType

  fromSing SDummyPayload = DummyPayloadType
  fromSing SMockPayload  = MockPayloadType

  toSing DummyPayloadType = SomeSing SDummyPayload
  toSing MockPayloadType  = SomeSing SMockPayload

readPayloadType :: String -> PayloadType
readPayloadType "dummy" = DummyPayloadType
readPayloadType "mock"  = MockPayloadType
readPayloadType x = error $ "Invalid block type: " <> x <> "."
                         <> "Available choices: " <> allPayloadTypes

allPayloadTypes :: String
allPayloadTypes = intercalate "," (map condense ([minBound .. maxBound] :: [PayloadType]))

instance Condense PayloadType where
    condense DummyPayloadType = "dummy"
    condense MockPayloadType  = "mock"

class PayloadImplementation (block :: PayloadType) where
    type Payload block = (b :: OuroborosProtocol -> *) | b -> block
    fixupBlock :: KnownOuroborosProtocol p => Chain (Payload block p) -> (Payload block p) -> (Payload block p)
    chainFrom  :: KnownOuroborosProtocol p => Chain (Payload block p) -> Int -> [Payload block p]
    toChain    :: KnownOuroborosProtocol p => [Int] -> Chain (Payload block p)

instance PayloadImplementation 'MockPayloadType where
    type Payload 'MockPayloadType = Block 'MockLedgerDomain
    fixupBlock = Mock.fixupBlock
    chainFrom  = Mock.chainFrom
    toChain    = Mock.toChain

instance PayloadImplementation 'DummyPayloadType where
    type Payload 'DummyPayloadType = Dummy.DummyPayload
    fixupBlock = Dummy.fixupBlock
    chainFrom  = Dummy.chainFrom
    toChain    = Dummy.toChain
