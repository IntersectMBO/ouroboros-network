{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances.UnorderedContainers ()
import Data.Maybe

import qualified LedgerOnDisk
import qualified LedgerOnDisk.Suite

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ledger-ondisk"
  [ testProperty "SimpleT lookup idempotent" $ monadKVSimple prop_kv_lookup_idempotent
  , LedgerOnDisk.Suite.tests
  ]
  -- TODO simple properties for simple implemetnation, i.e. insert, delete

monadKVSimple :: (Testable t) => ((forall a. LedgerOnDisk.SimpleT IO a -> IO a) -> t) -> Property
monadKVSimple go =  property $ \m -> go $ \x -> snd <$> LedgerOnDisk.runSimpleT x m

prop_kv_lookup_idempotent ::  (MonadFail m, LedgerOnDisk.SimpleMonadKV m) => (forall a. m a -> IO a) -> LedgerOnDisk.SimpleKey -> Property
prop_kv_lookup_idempotent f k = ioProperty . f $ do
    Right l1 <- LedgerOnDisk.lookup k
    Right l2 <- LedgerOnDisk.lookup k
    let key_present = isJust l1
    pure . coverTable "KeyPresent" [("True", 10), ("False", 10)] .
      collect key_present $ l1 == l2
