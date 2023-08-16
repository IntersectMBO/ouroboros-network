module Cardano.KESAgent.Pretty
where

-- | 'length', but the type is fixed to avoid ambiguous types in the face of
-- @OverloadedStrings@.
strLength :: String -> Int
strLength = length

class Pretty a where
  pretty :: a -> String
