{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Mock implementations of digital signatures.
module Infra.Crypto.Mock.DSIGN
(   VerKeyDSIGN
  , SignKeyDSIGN
  , genKeyPairDSIGN
  , deriveVerKeyDSIGN
  , SigDSIGN
  , signDSIGN
  , SignedDSIGN
  , SigMismatchDSIGN(..)
  , verSigDSIGN
  ) where

import Control.Monad.Except
import Crypto.Random        (MonadRandom)
import GHC.Generics         (Generic)
import GHC.Stack

import Infra.Util
import Serialise

newtype VerKeyDSIGN = VerKeyDSIGN Int
    deriving (Show, Eq, Ord, Generic)

instance Condense VerKeyDSIGN where
  condense (VerKeyDSIGN sid) = "vk(" ++ condense sid ++ ")"

data SignKeyDSIGN = SignKeyDSIGN VerKeyDSIGN
  deriving (Show, Eq, Ord, Generic)

instance Condense SignKeyDSIGN where
  condense (SignKeyDSIGN (VerKeyDSIGN sid)) = "sk(" ++ condense sid ++ ")"

genKeyPairDSIGN :: MonadRandom m => Int -> m (VerKeyDSIGN, SignKeyDSIGN)
genKeyPairDSIGN sid = do
    let vk = VerKeyDSIGN sid
    return (vk, SignKeyDSIGN vk)

deriveVerKeyDSIGN :: SignKeyDSIGN -> VerKeyDSIGN
deriveVerKeyDSIGN (SignKeyDSIGN vk) = vk

newtype SigDSIGN a = SigDSIGN SignKeyDSIGN
  deriving (Show, Eq, Ord, Generic, Condense)

newtype SignedDSIGN a = SignedDSIGN (DecoratedWith (SigDSIGN a) a)
  deriving (Show, Eq, Ord, Generic, Decorates a, Condense)

signDSIGN :: MonadRandom m => SignKeyDSIGN -> a -> m (SigDSIGN a)
signDSIGN sk _ = return $ SigDSIGN sk

data SigMismatchDSIGN where
    SigMismatchDSIGN :: Show a
                     => CallStack
                     -> VerKeyDSIGN
                     -> SignedDSIGN a
                     -> SigMismatchDSIGN

deriving instance Show SigMismatchDSIGN

verSigDSIGN :: (Monad m, HasCallStack, Show a)
            => VerKeyDSIGN -> SignedDSIGN a -> ExceptT SigMismatchDSIGN m ()
verSigDSIGN vk signed
  | deriveVerKeyDSIGN sk == vk = return ()
  | otherwise                  = throwError $ SigMismatchDSIGN callStack vk signed
  where
    SigDSIGN sk = decoration signed

instance Serialise VerKeyDSIGN
instance Serialise SignKeyDSIGN
instance Serialise (SigDSIGN a)
instance Serialise a => Serialise (SignedDSIGN a)
