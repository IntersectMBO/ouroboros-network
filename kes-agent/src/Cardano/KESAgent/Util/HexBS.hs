module Cardano.KESAgent.Util.HexBS
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Text.Printf

hexShowBS :: ByteString -> String
hexShowBS = concatMap (printf "%02x") . BS.unpack
