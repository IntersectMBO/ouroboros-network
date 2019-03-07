{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Mock implementations of verifiable random functions.
module Ouroboros.Consensus.Crypto.VRF.Simple
  ( SimpleVRF
  ) where

import           Crypto.Number.Generate (generateBetween)
import qualified Crypto.PubKey.ECC.Prim as C
import qualified Crypto.PubKey.ECC.Types as C
import           Crypto.Random (MonadRandom (..))
import           Data.Function (on)
import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)
import           Codec.Serialise (Serialise (..))

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Crypto.VRF.Class
import           Ouroboros.Consensus.Util.HList

data SimpleVRF

type H = MD5

curve :: C.Curve
curve = C.getCurveByName C.SEC_t113r1

q :: Integer
q = C.ecc_n $ C.common_curve curve

newtype Point = Point C.Point
    deriving (Eq, Generic)

instance Show Point where
    show (Point p) = show p

instance Ord Point where
    compare = compare `on` show

instance Serialise Point where
    encode (Point p) = encode $ pointToMaybe p
    decode = (Point . pointFromMaybe) <$> decode

instance Semigroup Point where
    Point p <> Point r = Point $ C.pointAdd curve p r

instance Monoid Point where
    mempty  = Point C.PointO
    mappend = (<>)

pointToMaybe :: C.Point -> Maybe (Integer, Integer)
pointToMaybe C.PointO      = Nothing
pointToMaybe (C.Point x y) = Just (x, y)

pointFromMaybe :: Maybe (Integer, Integer) -> C.Point
pointFromMaybe Nothing       = C.PointO
pointFromMaybe (Just (x, y)) = C.Point x y

pow :: Integer -> Point
pow = Point . C.pointBaseMul curve

pow' :: Point -> Integer -> Point
pow' (Point p) n = Point $ C.pointMul curve n p

h :: Serialise a => a -> Natural
h = fromHash . hash @H

h' :: Serialise a => a -> Integer -> Point
h' a l = pow $ mod (l * (fromIntegral $ h a)) q

getR :: MonadRandom m => m Integer
getR = generateBetween 0 (q - 1)

instance VRFAlgorithm SimpleVRF where

    newtype VerKeyVRF SimpleVRF = VerKeySimpleVRF Point
        deriving (Show, Eq, Ord, Generic, Serialise)

    newtype SignKeyVRF SimpleVRF = SignKeySimpleVRF C.PrivateNumber
        deriving (Show, Eq, Ord, Generic)

    data CertVRF SimpleVRF = CertSimpleVRF
        { certU :: Point
        , certC :: Natural
        , certS :: Integer
        } deriving (Show, Eq, Ord, Generic)

    maxVRF _ = 2 ^ (8 * byteCount (Proxy :: Proxy H)) - 1

    genKeyVRF = SignKeySimpleVRF <$> C.scalarGenerate curve

    deriveVerKeyVRF (SignKeySimpleVRF k) =
        VerKeySimpleVRF $ pow k

    evalVRF a sk@(SignKeySimpleVRF k) = do
        let u                 = h' a k
            y                 = h $ a :* u :* Nil
            VerKeySimpleVRF v = deriveVerKeyVRF sk
        r <- getR
        let c = h $ a :* v :* pow r :* h' a r :* Nil
            s = mod (r + k * fromIntegral c) q
        return (y, CertSimpleVRF u c s)

    verifyVRF (VerKeySimpleVRF v) a (y, cert) =
        let u   = certU cert
            c   = certC cert
            c'  = - fromIntegral c
            s   = certS cert
            b1  = y == h (a :* u :* Nil)
            rhs = h $  a
                    :* v
                    :* (pow s <> pow' v c')
                    :* (h' a s <> pow' u c')
                    :* Nil
        in  b1 && c == rhs

instance Serialise (SignKeyVRF SimpleVRF)
instance Serialise (CertVRF SimpleVRF)
