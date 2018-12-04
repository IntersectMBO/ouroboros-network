{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Ouroboros.Consensus.Util (
    -- * Miscellaneous
    foldlM'
  , repeatedly
  , repeatedlyM
  , chunks
  , byteStringChunks
  , lazyByteStringChunks
    -- * Decorating one value with another
  , DecoratedWith(..)
  , Decorates(..)
    -- * Thread-safe input/output
  , threadSafeOutput
  , threadSafeOutputHex
  , threadSafeInput
    -- * Exceptions
  , wrapExceptions
  , wrapExceptionsIO
    -- * Containers
  , withoutKeys
    -- * Pretty-printing
  , Condense(..)
  ) where

import           Codec.Serialise
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base16.Lazy as Lazy.Base16
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import           Data.List (foldl', intercalate)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack
import           Numeric.Natural
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Printf (printf)
import           UnliftIO

import           Ouroboros.Consensus.Util.HList (All, Fn, HList (..), IsList,
                     SList)
import qualified Ouroboros.Consensus.Util.HList as HList

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

foldlM' :: forall m a b. Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM' f = go
  where
    go :: b -> [a] -> m b
    go !acc []     = return acc
    go !acc (x:xs) = f acc x >>= \acc' -> go acc' xs

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . foldl' . flip

repeatedlyM :: Monad m => (a -> b -> m b) -> ([a] -> b -> m b)
repeatedlyM = flip . foldlM' . flip

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (chunk, xs') = splitAt n xs
              in chunk : chunks n xs'

byteStringChunks :: Int -> Strict.ByteString -> [Strict.ByteString]
byteStringChunks n = map Strict.pack . chunks n . Strict.unpack

lazyByteStringChunks :: Int -> Lazy.ByteString -> [Lazy.ByteString]
lazyByteStringChunks n bs
  | Lazy.null bs = []
  | otherwise    = let (chunk, bs') = Lazy.splitAt (fromIntegral n) bs
                   in chunk : lazyByteStringChunks n bs'

{-------------------------------------------------------------------------------
  Decorating one value with another
-------------------------------------------------------------------------------}

data DecoratedWith b a = DecoratedWith b a
  deriving (Show, Eq, Ord, Generic)

instance (Serialise b, Serialise a) => Serialise (DecoratedWith b a)

class Decorates b a | a -> b where
  type Decoration a :: *
  decoration   :: a -> Decoration a
  decorateWith :: Decoration a -> b -> a
  undecorate   :: a -> b

instance Decorates a (DecoratedWith b a) where
  type Decoration (DecoratedWith b a) = b
  decoration (DecoratedWith b _) = b
  undecorate (DecoratedWith _ a) = a
  decorateWith = DecoratedWith

instance (Condense b, Condense a) => Condense (DecoratedWith b a) where
  condense (DecoratedWith b a) = condense (b, a)

{-------------------------------------------------------------------------------
  Thread-safe input/output
-------------------------------------------------------------------------------}

globalIOLock :: MVar ()
{-# NOINLINE globalIOLock #-}
globalIOLock = unsafePerformIO $ newMVar ()

threadSafeOutput :: MonadUnliftIO m => Lazy.ByteString -> m ()
threadSafeOutput a = withMVar globalIOLock $ \() ->
    liftIO $ Lazy.Char8.putStrLn a

threadSafeOutputHex :: MonadUnliftIO m => Lazy.ByteString -> m ()
threadSafeOutputHex = threadSafeOutput . Lazy.Base16.encode

threadSafeInput :: MonadUnliftIO m => m Lazy.ByteString
threadSafeInput = withMVar globalIOLock $ \() ->
    liftIO $ Lazy.fromStrict <$> Strict.getLine

{-------------------------------------------------------------------------------
  Condensed but human-readable output
-------------------------------------------------------------------------------}

class Condense a where
  condense :: a -> String

instance Condense String where
  condense = id

instance Condense Int where
  condense = show

instance Condense Word where
  condense = show

instance Condense Natural where
  condense = show

instance Condense Rational where
  condense = printf "%.8f" . (fromRational :: Rational -> Double)

instance {-# OVERLAPPING #-} Condense [String] where
  condense ss = "[" ++ intercalate "," ss ++ "]"

instance {-# OVERLAPPABLE #-} Condense a => Condense [a] where
  condense as = "[" ++ intercalate "," (map condense as) ++ "]"

instance All Condense as => Condense (HList as) where
  condense as = "(" ++ intercalate "," (HList.collapse (Proxy @Condense) condense as) ++ ")"

instance (Condense a, Condense b) => Condense (a, b) where
  condense (a, b) = condense (a :* b :* Nil)

instance Condense a => Condense (Set a) where
  condense = condense . Set.toList

instance (Condense k, Condense a) => Condense (Map k a) where
  condense = condense . Map.toList

{-------------------------------------------------------------------------------
  Wrap exceptions
-------------------------------------------------------------------------------}

wrapExceptions :: (MonadUnliftIO m, HasCallStack, All Show as)
               => Fn as (m b) -> HList as -> m b
wrapExceptions fn args =
    catch (HList.applyFn fn args) $ \e ->
      throwIO $ WrappedException
                  callStack
                  (HList.collapse (Proxy @Show) show args)
                  e

wrapExceptionsIO :: forall m as b.
                    (MonadUnliftIO m, HasCallStack, All Show as, IsList as)
                 => Fn as (IO b) -> HList as -> m b
wrapExceptionsIO fn =
    wrapExceptions $
      HList.afterFn (HList.isList :: SList as) (liftIO :: IO b -> m b) fn

data WrappedException = WrappedException CallStack [String] SomeException

instance Show WrappedException where
  show (WrappedException cs as e) = unlines [
        "Exception " ++ displayException e
      , "Arguments [" ++ intercalate ", " as ++ "]"
      , prettyCallStack cs
      ]

instance Exception WrappedException

{-------------------------------------------------------------------------------
  Containers
-------------------------------------------------------------------------------}

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
#if MIN_VERSION_containers(0,5,8)
withoutKeys = Map.withoutKeys
#else
m `withoutKeys` s = Map.filterWithKey (\k _ -> k `Set.notMember` s) m
#endif
