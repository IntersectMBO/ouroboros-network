{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Test.Data.CDDL
  ( Any (..)
  , tests
  ) where

import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..), deserialise, serialise)
import           Data.Bifunctor (bimap)

import           GHC.Generics (Generic (..))

import           Test.QuickCheck as QC
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Instances.Text ()

import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)

newtype Any = Any { getAny :: CBOR.Term }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Serialise

instance Arbitrary Any where
    -- | Generate a subset of `CBOR.Term` for which `deserialise` is a left
    -- inverse of `serialise`.  `CBOR.Integer, `CBOR.Tagged`, `CBOR.Half` and
    -- `CBOR.Simple` are excluded.
    arbitrary = oneof [ Any . CBOR.TInt     <$> arbitrary
                      , Any . CBOR.TBytes   <$> arbitrary
                      , Any . CBOR.TBytesI  <$> arbitrary
                      , Any . CBOR.TString  <$> arbitrary
                      , Any . CBOR.TStringI <$> arbitrary
                      , Any . CBOR.TList  . map getAny <$> resized arbitrary
                      , Any . CBOR.TListI . map getAny <$> resized arbitrary
                      , Any . CBOR.TMap   . map (bimap getAny getAny) <$> resized arbitrary
                      , Any . CBOR.TMapI  . map (bimap getAny getAny) <$> resized arbitrary
                      , Any . CBOR.TBool    <$> arbitrary
                      , pure (Any CBOR.TNull)
                      , Any . CBOR.TFloat   <$> arbitrary
                      , Any . CBOR.TDouble  <$> arbitrary
                      ]
      where
        resized gen = sized $ \s -> resize (s `div` 4 `min` 8) gen

    shrink (Any term) =
      case term of
        CBOR.TInt a      -> Any . CBOR.TInt       <$> shrink a
        CBOR.TBytes a    -> Any . CBOR.TBytes     <$> shrink a
        CBOR.TBytesI a   -> Any . CBOR.TBytesI    <$> shrink a
        CBOR.TString a   -> Any . CBOR.TString    <$> shrink a
        CBOR.TStringI a  -> Any . CBOR.TStringI   <$> shrink a
        CBOR.TList a     -> Any . CBOR.TList  . map getAny <$> shrink (map Any a)
        CBOR.TListI a    -> Any . CBOR.TListI . map getAny <$> shrink (map Any a)
        CBOR.TMap a      -> Any . CBOR.TMap   . map (bimap getAny getAny)
                        <$> shrink (bimap Any Any `map` a)
        CBOR.TMapI a     -> Any . CBOR.TMapI  . map (bimap getAny getAny)
                        <$> shrink (bimap Any Any `map` a)
        CBOR.TBool a     -> Any . CBOR.TBool      <$> shrink a
        CBOR.TNull       -> []
        CBOR.TFloat a    -> Any . CBOR.TFloat  <$> shrink a
        CBOR.TDouble a   -> Any . CBOR.TDouble <$> shrink a
        _                -> []

instance CoArbitrary Any where
instance CoArbitrary CBOR.Term where
deriving instance Generic CBOR.Term


tests :: TestTree
tests = testGroup "Test.Data.CDDL"
          [ testProperty "serialise Any"  prop_serialise_any
          ]

prop_serialise_any :: Any -> Property
prop_serialise_any a = deserialise (serialise a) === a
