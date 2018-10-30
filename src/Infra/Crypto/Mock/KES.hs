{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Mock implementations of a Key Evolving Scheme (KES).
module Infra.Crypto.Mock.KES
  (
    VerKeyKES
  , SignKeyKES
  , periodFromSignKey
  , genKeyPairKES
  , deriveVerKeyKES
  , KESError (..)
  , SigKES
  , signKES
  , SignedKES
  , SigMismatchKES(..)
  , verSigKES
  ) where

import           Control.Monad.Except
import           Crypto.Random
import           GHC.Generics (Generic)
import           GHC.Stack
import           Numeric.Natural

import           Infra.Util
import           Serialise

newtype VerKeyKES = VerKeyKES Int
    deriving (Show, Eq, Ord, Generic)

instance Condense VerKeyKES where
  condense (VerKeyKES sid) = "vk(" ++ condense sid ++ ")"

data SignKeyKES = SignKeyKES VerKeyKES Natural Natural
  deriving (Show, Eq, Ord, Generic)

instance Condense SignKeyKES where
  condense (SignKeyKES (VerKeyKES sid) j _) =
    "sk(" ++ condense sid ++ "," ++ condense j ++ ")"

deriveVerKeyKES :: SignKeyKES -> VerKeyKES
deriveVerKeyKES (SignKeyKES vk _ _) = vk

periodFromSignKey :: SignKeyKES -> Natural
periodFromSignKey (SignKeyKES _ j _) = j

genKeyPairKES :: MonadRandom m => Int -> Natural -> m (VerKeyKES, SignKeyKES)
genKeyPairKES sid duration = do
    let vk = VerKeyKES sid
        sk = SignKeyKES vk 0 duration
    return (vk, sk)

data KESError =
      SigningOldMessage CallStack SignKeyKES Natural
    | PeriodOverflow CallStack SignKeyKES
    deriving Show

newtype SigKES a = SigKES SignKeyKES
  deriving (Show, Eq, Ord, Generic, Condense)

newtype SignedKES a = Signed (DecoratedWith (SigKES a) a)
  deriving (Show, Eq, Ord, Generic, Condense)

instance Decorates a (SignedKES a) where
  type Decoration (SignedKES a) = SigKES a
  decoration (Signed (DecoratedWith b _)) = b
  decorateWith b a = Signed (DecoratedWith b a)
  undecorate (Signed (DecoratedWith _ a)) = a

signKES :: (MonadRandom m, HasCallStack)
        => SignKeyKES
        -> Natural
        -> a
        -> ExceptT KESError m (SigKES a, SignKeyKES)
signKES sk@(SignKeyKES vk k t) j _
    | j < k     = throwError $ SigningOldMessage callStack sk j
    | j >= t    = throwError $ PeriodOverflow callStack sk
    | otherwise = return (SigKES $ SignKeyKES vk j t, SignKeyKES vk (j + 1) t)

data SigMismatchKES where
    SigMismatchKES :: Show a
                   => CallStack
                   -> VerKeyKES
                   -> SignedKES a
                   -> Natural
                   -> SigMismatchKES

deriving instance Show SigMismatchKES

verSigKES :: (Monad m, HasCallStack, Show a)
          => VerKeyKES
          -> SignedKES a
          -> Natural
          -> ExceptT SigMismatchKES m ()
verSigKES vk signed period
  | deriveVerKeyKES sk == vk && periodFromSignKey sk == period = return ()
  | otherwise = throwError $ SigMismatchKES callStack vk signed period
  where
    SigKES sk = decoration signed

instance Serialise VerKeyKES
instance Serialise SignKeyKES
instance Serialise (SigKES a)
instance Serialise a => Serialise (SignedKES a)
