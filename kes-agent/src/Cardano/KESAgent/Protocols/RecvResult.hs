module Cardano.KESAgent.Protocols.RecvResult
where

data RecvResult
  = RecvOK
  | RecvErrorKeyOutdated -- ^ Newer key is already present
  | RecvErrorInvalidOpCert -- ^ OpCert did not validate
  | RecvErrorNoKey -- ^ No key exists that could be used
  | RecvErrorUnknown -- ^ Something else went wrong, we don't know what
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
