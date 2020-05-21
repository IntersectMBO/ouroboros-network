module Main
where
import Test.Tasty (defaultMain, testGroup)
import Cardano.Client.Subscription ()

-- At the moment, there are no reasonable tests for the cardano-client library.
-- However the dummy test-suite is still needed because,
-- the CI generates the set of required builds bases on the test-suits.
-- A dummy test-suite is the easiest way to make the cardano-client library a required build.
-- Real tests may be added later.

main :: IO ()
main = defaultMain $ testGroup "cardano-client-tests" []
