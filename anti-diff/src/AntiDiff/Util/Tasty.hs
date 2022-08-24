-- | Utilities for running tests using @Tasty@.
module AntiDiff.Util.Tasty (testGroupWithProxy) where

import           Data.Proxy
import           Data.Typeable

import           Test.Tasty



testGroupWithProxy :: Typeable a => Proxy a -> [Proxy a -> TestTree] -> TestTree
testGroupWithProxy p tts = testGroup (show $ typeOf p) (fmap ($ p) tts)
