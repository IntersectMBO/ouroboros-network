-- | Custom pretty-printing API. Mostly used for trace logging and printing
-- values to the command line.
module Cardano.KESAgent.Util.Pretty
where

-- | 'length', but the type is fixed to avoid ambiguous types in the face of
-- @OverloadedStrings@.
strLength :: String -> Int
strLength = length

class Pretty a where
  pretty :: a -> String

newtype PrettyStr = PrettyStr String
  deriving (Show)

instance Pretty PrettyStr where
  pretty (PrettyStr s) = s
