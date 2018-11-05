{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Mock implementations of crypto primitives
--
-- The intend is that the datatypes (and the choice of which constructors to
-- export and which not) and functions defined in this module faithfully
-- model the properties we want from the crypto primitives, but without
-- actually doing any cryptography.
module Ouroboros.Infra.Crypto.Mock (
    -- * Hashing
    Hash
  , hash
  , Hashable
    -- * PKI
  , PrivateKey
  , PublicKey
  , mkPrivateKey
  , derivePublicKey
    -- * Signatures
  , Signature
  , signature
  , Signed
  , SignatureMismatch(..)
  , verifySignature
  ) where

import           Control.Monad.Except
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Infra.Util

{-------------------------------------------------------------------------------
  Hashing
-------------------------------------------------------------------------------}

newtype Hash a = Hash String
  deriving (Show, Eq, Ord, Generic)

instance Condense (Hash a) where
  condense (Hash h) = show h

hash :: String -> Hash a
hash = Hash

newtype Hashable b a = Hashable (DecoratedWith (Hash b) a)
  deriving (Show, Eq, Ord, Generic, Decorates a, Condense)

{-------------------------------------------------------------------------------
  PKI
-------------------------------------------------------------------------------}

newtype PrivateKey = PrivateKey Int
  deriving (Show, Eq, Generic)

instance Condense PrivateKey where
  condense (PrivateKey n) = "sk" ++ condense n

data PublicKey = PublicKey PrivateKey
  deriving (Show, Eq, Generic)

instance Condense PublicKey where
  condense (PublicKey (PrivateKey n)) = "pk" ++ condense n

mkPrivateKey :: Int -> PrivateKey
mkPrivateKey = PrivateKey

derivePublicKey :: PrivateKey -> PublicKey
derivePublicKey = PublicKey

{-------------------------------------------------------------------------------
  Signatures
-------------------------------------------------------------------------------}

newtype Signature a = Signature PrivateKey
  deriving (Show, Eq, Generic, Condense)

newtype Signed a = Signed (DecoratedWith (Signature a) a)
  deriving (Show, Eq, Generic, Decorates a, Condense)

signature :: PrivateKey -> a -> Signature a
signature sk _ = Signature sk

verifySignature :: (Monad m, HasCallStack, Show a)
                => PublicKey -> Signed a -> ExceptT SignatureMismatch m ()
verifySignature pk@(PublicKey sk) signed
  | sk == sk' = return ()
  | otherwise = throwError $ SignatureMismatch callStack pk signed
  where
    Signature sk' = decoration signed

data SignatureMismatch = forall a. Show a => SignatureMismatch CallStack PublicKey (Signed a)

deriving instance Show SignatureMismatch
