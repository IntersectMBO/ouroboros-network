-- | Custom pretty-printing API. Mostly used for trace logging and printing
-- values to the command line.
module Cardano.KESAgent.Util.Pretty
where

import Cardano.KESAgent.Util.HexBS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

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

instance Pretty ByteString where
  pretty bs
    | BS.length bs > 8 =
        hexShowBS (BS.take 7 bs) ++ "..."
    | otherwise =
        hexShowBS bs
