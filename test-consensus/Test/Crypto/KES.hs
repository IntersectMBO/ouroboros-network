{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Crypto.KES
    ( tests
    ) where

import           Data.Proxy (Proxy (..))
import           Numeric.Natural (Natural)
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.KES
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.Serialise (Serialise, prop_serialise)

--
-- The list of all tests
--
tests :: TestTree
tests =
  testGroup "Crypto.KES"
  [ testKESAlgorithm (Proxy :: Proxy MockKES)                "MockKES"
  , testKESAlgorithm (Proxy :: Proxy (SimpleKES Ed448DSIGN)) "SimpleKES (with Ed448)"
  ]

testKESAlgorithm :: KESAlgorithm v => proxy v -> String -> TestTree
testKESAlgorithm p n =
    testGroup n
        [ testProperty "serialise VerKey"                $ prop_KES_serialise_VerKey p
        , testProperty "serialise SignKey"               $ prop_KES_serialise_SignKey p
        , testProperty "serialise Sig"                   $ prop_KES_serialise_Sig p
        , testProperty "verify positive"                 $ prop_KES_verify_pos p
        , testProperty "verify negative (wrong key)"     $ prop_KES_veriy_neg_key p
        , testProperty "verify negative (wrong message)" $ prop_KES_veriy_neg_msg p
        , testProperty "verify negative (wrong time)"    $ prop_KES_veriy_neg_time p
        ]

prop_KES_serialise_VerKey :: KESAlgorithm v
                          => proxy v
                          -> Duration_Seed_SK v
                          -> Property
prop_KES_serialise_VerKey _ (Duration_Seed_SK _ _ sk) =
    prop_serialise (deriveVerKeyKES sk)

prop_KES_serialise_SignKey :: KESAlgorithm v
                           => proxy v
                           -> Duration_Seed_SK v
                           -> Property
prop_KES_serialise_SignKey _ (Duration_Seed_SK _ _ sk) =
    prop_serialise sk

prop_KES_serialise_Sig :: KESAlgorithm v
                       => proxy v
                       -> Duration_Seed_SK_Times v String
                       -> Seed
                       -> Property
prop_KES_serialise_Sig _ d seed = case withSeed seed $ trySign d of
    Left e   -> counterexample e False
    Right xs -> conjoin [prop_serialise sig |(_, _, sig) <- xs]

prop_KES_verify_pos :: KESAlgorithm v
                    => proxy v
                    -> Duration_Seed_SK_Times v String
                    -> Seed
                    -> Property
prop_KES_verify_pos _ d seed =
    let vk = getVerKey d
    in  case withSeed seed $ trySign d of
            Left e   -> counterexample e False
            Right xs -> conjoin [verifyKES vk j a sig | (j, a, sig) <- xs]

prop_KES_veriy_neg_key :: KESAlgorithm v
                       => proxy v
                       -> Duration_Seed_SK_Times v Int
                       -> VerKeyKES v
                       -> Seed
                       -> Property
prop_KES_veriy_neg_key _ d vk seed = getVerKey d /= vk ==>
    case withSeed seed $ trySign d of
        Left e   -> counterexample e False
        Right xs -> conjoin [not $ verifyKES vk j a sig | (j, a, sig) <- xs]

prop_KES_veriy_neg_msg :: KESAlgorithm v
                       => proxy v
                       -> Duration_Seed_SK_Times v Double
                       -> Double
                       -> Seed
                       -> Property
prop_KES_veriy_neg_msg _ d a seed =
    let vk = getVerKey d
    in  case withSeed seed $ trySign d of
            Left e   -> counterexample e False
            Right xs -> conjoin [a /= a' ==> not $ verifyKES vk j a sig | (j, a', sig) <- xs]

prop_KES_veriy_neg_time :: KESAlgorithm v
                        => proxy v
                        -> Duration_Seed_SK_Times v Double
                        -> Integer
                        -> Seed
                        -> Property
prop_KES_veriy_neg_time _ d i seed =
    let vk = getVerKey d
        t  = fromIntegral $ abs i
    in  case withSeed seed $ trySign d of
            Left e   -> counterexample e False
            Right xs -> conjoin [t /= j ==> not $ verifyKES vk t a sig | (j, a, sig) <- xs]

getVerKey :: KESAlgorithm v => Duration_Seed_SK_Times v a -> VerKeyKES v
getVerKey d = case d of
    (Duration_Seed_SK_Times _ _ sk _) -> deriveVerKeyKES sk

trySign :: forall m v a.
           ( MonadRandom m
           , KESAlgorithm v
           , Serialise a
           , Show a)
        => Duration_Seed_SK_Times v a
        -> m (Either String [(Natural, a, SigKES v)])
trySign (Duration_Seed_SK_Times _ _ sk ts) =
    go sk ts
  where
    go :: SignKeyKES v
       -> [(Natural, a)]
       -> m (Either String [(Natural, a, SigKES v)])
    go _ []              = return $ Right []
    go sk' ((j, a) : xs) = do
        m <- signKES j a sk'
        case m of
            Nothing          -> return $ Left $
                                    "trySign: error signing " ++
                                    show a ++ " at " ++ show j ++ " with " ++ show sk
            Just (sig, sk'') -> do
                e <- go sk'' xs
                return $ case e of
                    Right ys -> Right ((j, a, sig) : ys)
                    Left  _  -> e
