import qualified Data.ByteString.Lazy as LBS
import           System.IO (stdin)
import           Text.JSON.Canonical (parseCanonicalJSON, renderCanonicalJSON)

main :: IO ()
main = do
  rawData <- LBS.hGetContents stdin
  let
    Right value = parseCanonicalJSON rawData
    cleaned = renderCanonicalJSON value
  LBS.putStr cleaned
