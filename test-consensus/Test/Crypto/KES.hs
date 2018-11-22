{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveAnyClass      #-}

module Test.Crypto.KES
    ( tests
    ) where

import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)
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
        , testProperty "verify negative (wrong key)"     $ prop_KES_verify_neg_key p
        , testProperty "verify negative (wrong message)" $ prop_KES_verify_neg_msg p
        , testProperty "verify negative (wrong time)"    $ prop_KES_verify_neg_time p
        ]

prop_KES_serialise_VerKey :: KESAlgorithm v
                          => proxy v
                          -> Duration_Seed_SK v
                          -> Property
prop_KES_serialise_VerKey _ (Duration_Seed_SK _ _ sk _) =
    prop_serialise (deriveVerKeyKES sk)

prop_KES_serialise_SignKey :: KESAlgorithm v
                           => proxy v
                           -> Duration_Seed_SK v
                           -> Property
prop_KES_serialise_SignKey _ (Duration_Seed_SK _ _ sk _) =
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
    let vk = getFirstVerKey d
    in  case withSeed seed $ trySign d of
            Left e   -> counterexample e False
            Right xs -> conjoin [verifyKES vk j a sig | (j, a, sig) <- xs]

prop_KES_verify_neg_key :: KESAlgorithm v
                        => proxy v
                        -> Duration_Seed_SK_Times v Int
                        -> Seed
                        -> Property
prop_KES_verify_neg_key _ d seed = getDuration d > 0 ==>
    case withSeed seed $ trySign d of
        Left e   -> counterexample e False
        Right xs -> conjoin [ not $ verifyKES (getSecondVerKey d) j a sig
                            | (j, a, sig) <- xs]

prop_KES_verify_neg_msg :: KESAlgorithm v
                        => proxy v
                        -> Duration_Seed_SK_Times v Double
                        -> Double
                        -> Seed
                        -> Property
prop_KES_verify_neg_msg _ d a seed =
    let vk = getFirstVerKey d
    in  case withSeed seed $ trySign d of
            Left e   -> counterexample e False
            Right xs -> conjoin [a /= a' ==> not $ verifyKES vk j a sig | (j, a', sig) <- xs]

prop_KES_verify_neg_time :: KESAlgorithm v
                         => proxy v
                         -> Duration_Seed_SK_Times v Double
                         -> Integer
                         -> Property
prop_KES_verify_neg_time _ d i =
    let seed = Seed (1, 0, 2, 0, 1)
        vk   = getFirstVerKey d
        t    = fromIntegral $ abs i
    in  case withSeed seed $ trySign d of
            Left e   -> counterexample e False
            Right xs -> conjoin [t /= j ==> not $ verifyKES vk t a sig | (j, a, sig) <- xs]

getDuration :: KESAlgorithm v => Duration_Seed_SK_Times v a -> Natural
getDuration d = case d of
    (Duration_Seed_SK_Times duration _ _ _ _) -> duration

getFirstVerKey :: KESAlgorithm v => Duration_Seed_SK_Times v a -> VerKeyKES v
getFirstVerKey d = case d of
    (Duration_Seed_SK_Times _ _ sk _ _) -> deriveVerKeyKES sk

getSecondVerKey :: KESAlgorithm v => Duration_Seed_SK_Times v a -> VerKeyKES v
getSecondVerKey d = case d of
    (Duration_Seed_SK_Times _ _ _ vk _) -> vk

trySign :: forall m v a.
           ( MonadRandom m
           , KESAlgorithm v
           , Serialise a
           , Show a)
        => Duration_Seed_SK_Times v a
        -> m (Either String [(Natural, a, SigKES v)])
trySign (Duration_Seed_SK_Times _ _ sk _ ts) =
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

data Duration_Seed_SK v = Duration_Seed_SK Natural Seed (SignKeyKES v) (VerKeyKES v)
    deriving Generic

deriving instance KESAlgorithm v => Show (Duration_Seed_SK v)
deriving instance KESAlgorithm v => Eq (Duration_Seed_SK v)
deriving instance KESAlgorithm v => Serialise (Duration_Seed_SK v)

instance KESAlgorithm v => Arbitrary (Duration_Seed_SK v) where

    arbitrary = do
        duration <- genNat
        seed <- arbitrary
        return $ duration_Seed_SK duration seed

    shrink (Duration_Seed_SK duration seed _ _) =
        [duration_Seed_SK d seed | d <- shrinkNat duration]

data Duration_Seed_SK_Times v a =
    Duration_Seed_SK_Times Natural Seed (SignKeyKES v) (VerKeyKES v) [(Natural, a)]
    deriving Generic

instance (KESAlgorithm v, Arbitrary a) => Arbitrary (Duration_Seed_SK_Times v a) where

    arbitrary = arbitrary >>= gen_Duration_Seed_SK_Times

    shrink (Duration_Seed_SK_Times d s sk vk ts) = do
        Duration_Seed_SK d' s' sk' vk' <- shrink $ Duration_Seed_SK d s sk vk
        let ts' = filter ((< d') . fst) ts
        return $ Duration_Seed_SK_Times d' s' sk' vk' ts'

deriving instance (KESAlgorithm v, Show a) => Show (Duration_Seed_SK_Times v a)
deriving instance (KESAlgorithm v, Eq a) => Eq (Duration_Seed_SK_Times v a)
deriving instance (KESAlgorithm v, Serialise a) => Serialise (Duration_Seed_SK_Times v a)

duration_Seed_SK :: KESAlgorithm v => Natural -> Seed -> Duration_Seed_SK v
duration_Seed_SK duration seed =
    let (sk, vk) = withSeed seed go
    in  Duration_Seed_SK duration seed sk vk
  where
    go = do
      sk  <- genKeyKES duration
      sk' <- genKeyKES duration
      let vk = deriveVerKeyKES sk'
      if duration > 0 && deriveVerKeyKES sk == vk
          then go
          else return (sk, vk)

gen_Duration_Seed_SK_Times :: Arbitrary a
                           => Duration_Seed_SK v
                           -> Gen (Duration_Seed_SK_Times v a)
gen_Duration_Seed_SK_Times (Duration_Seed_SK duration seed sk vk) = do
    ts <- genTimes 0
    return $ Duration_Seed_SK_Times duration seed sk vk ts
  where
    genTimes :: Arbitrary a => Natural -> Gen [(Natural, a)]
    genTimes j
        | j >= duration = return []
        | otherwise = do
            k  <- genNatBetween j (duration - 1)
            a  <- arbitrary
            ns <- genTimes $ k + 1
            return ((k, a) : ns)
