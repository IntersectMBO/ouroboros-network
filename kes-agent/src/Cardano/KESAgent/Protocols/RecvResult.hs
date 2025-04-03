module Cardano.KESAgent.Protocols.RecvResult
where

data RecvResult
  = RecvOK
  | -- | Newer key is already present
    RecvErrorKeyOutdated
  | -- | OpCert did not validate
    RecvErrorInvalidOpCert
  | -- | No key exists that could be used
    RecvErrorNoKey
  | -- | The requested operation isn't supported
    RecvErrorUnsupportedOperation
  | -- | Something else went wrong, we don't know what
    RecvErrorUnknown
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
