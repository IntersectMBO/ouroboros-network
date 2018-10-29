{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Ouroboros.Infra.Util (
    -- * Miscellaneous
    repeatedly
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
    -- * Pretty-printing
  , Condense(..)
  ) where

import qualified Data.ByteString             as Strict
import qualified Data.ByteString.Base16.Lazy as Lazy.Base16
import qualified Data.ByteString.Lazy        as Lazy
import qualified Data.ByteString.Lazy.Char8  as Lazy.Char8
import           Data.List                   (foldl', intercalate)
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Proxy
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           GHC.Generics                (Generic)
import           System.IO.Unsafe            (unsafePerformIO)
import           UnliftIO

import           Ouroboros.Infra.Util.HList  (All, HList (..))
import qualified Ouroboros.Infra.Util.HList  as HList

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . foldl' . flip

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
