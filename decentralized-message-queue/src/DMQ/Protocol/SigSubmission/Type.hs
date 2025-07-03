{-# LANGUAGE DerivingStrategies #-}

module DMQ.Protocol.SigSubmission.Type
  ( -- * Data types
    BlockNo (..)
  , SigHash (..)
  , SigId (..)
  , SigBody (..)
  , SigTTL (..)
  , SigKesSignature (..)
  , SigOpCertificate (..)
  , Sig (..)
    -- * `TxSubmission` mini-protocol
  , SigSubmission
  , module SigSubmission
  ) where

import Data.ByteString (ByteString)
import Data.Word

import Ouroboros.Network.Protocol.TxSubmission2.Type as SigSubmission hiding
           (TxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type as TxSubmission2
import Ouroboros.Network.Util.ShowProxy


newtype SigHash = SigHash { getSigHash :: ByteString }
  deriving stock (Show, Eq, Ord)

newtype SigId = SigId { getSigId :: SigHash }
  deriving stock (Show, Eq, Ord)

instance ShowProxy SigId where
  showProxy _ = "SigId"

newtype SigBody = SigBody { getSigBody :: ByteString }
  deriving stock (Show, Eq)

newtype SigTTL = SigTTL { getSigTTL :: Word16 }
  deriving stock (Show, Eq)

newtype SigKesSignature = SigKesSignature { getSigKesSignature :: ByteString }
  deriving stock (Show, Eq)

-- TODO: `cardano` is using `Word64` block numbers
newtype BlockNo = BlockNo { getBlockNo :: Word32 }
  deriving stock (Show, Eq)

newtype SigOpCertificate = SigOpCertificate { getSigOpCertificate :: ByteString }
  deriving stock (Show, Eq)


data Sig = Sig {
    sigId            :: SigId,
    sigBody          :: SigBody,
    sigBlockNumber   :: BlockNo,
    sigTTL           :: SigTTL,
    sigKesSignature  :: SigKesSignature,
    sigOpCertificate :: SigOpCertificate
  }
  deriving stock (Show, Eq)

instance ShowProxy Sig where
  showProxy _ = "Sig"


type SigSubmission = TxSubmission2.TxSubmission2 SigId Sig
