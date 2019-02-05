{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal part of the mock file system
--
-- Intended for qualified import
--
-- > import Ouroboros.Storage.FS.Sim.FsTree (FsTree)
-- > import Ouroboros.Storage.FS.Sim.FsTree as FS
module Ouroboros.Storage.FS.Sim.FsTree (
    -- * FsTree type and indexing functions
    FsTree(..)
  , FsTreeError(..)
  , example
    -- * Construction
  , empty
    -- * Indexing
  , index
  , getFile
  , getDir
    -- * File system operations
  , touch
  , replace
  , createDirIfMissing
  , createDirWithParents
  , removeFile
    -- * Pretty-printing
  , pretty
  ) where

import           Data.Functor.Const
import           Data.List (inits)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Tree
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Util (repeatedlyM)
import           Ouroboros.Storage.FS.API.Types

{-------------------------------------------------------------------------------
  FsTree type and general indexing functions
-------------------------------------------------------------------------------}

-- | Simple in-memory representation of a file system
data FsTree a = File a | Folder (Folder a)
  deriving (Show, Eq, Generic, Functor)

type Folder a = Map String (FsTree a)

-- | Example
example :: Monoid a => FsTree a
example =
    Folder $ M.fromList [
        ("usr", Folder $ M.fromList [
            ("local", Folder $ M.fromList [
                ("bin", Folder mempty)
              ])
          ])
      , ("var", Folder $ M.fromList [
            ("log",  Folder mempty)
          , ("mail", Folder mempty)
          , ("run",  Folder mempty)
          , ("tmp",  Folder $ M.fromList [
                ("foo.txt", File mempty)
              ])
          ])
      ]

-- | File access error
data FsTreeError =
    -- | A path @../a/..@ where @a@ is a file rather than a dir
    --
    -- We record both the full path and the invalid suffix.
    FsExpectedDir FsPath (NonEmpty String)

    -- | A path @../a/..@ where @a@ is a dir rather than a file
    --
    -- No suffix is specified (it /must/ be the last part of the file)
  | FsExpectedFile FsPath

    -- | A path @../a/..@ or @../a@ where directory or file @a@ is missing
    --
    -- We record both the full path and the missing suffix.
  | FsMissing FsPath (NonEmpty String)
  deriving (Show)

setFsTreeErrorPath :: FsPath -> FsTreeError -> FsTreeError
setFsTreeErrorPath fp (FsExpectedDir  _ suffix) = FsExpectedDir  fp suffix
setFsTreeErrorPath fp (FsExpectedFile _)        = FsExpectedFile fp
setFsTreeErrorPath fp (FsMissing      _ suffix) = FsMissing      fp suffix

{-------------------------------------------------------------------------------
  Altering
-------------------------------------------------------------------------------}

-- | Most general indexing function
alterF :: forall f a. Functor f
       => FsPath                                -- ^ Path to look for
       -> (FsTreeError -> f (Maybe (FsTree a))) -- ^ Action on error
       -> (FsTree a    -> f (Maybe (FsTree a))) -- ^ Alter the tree when found
       -> (FsTree a    -> f (FsTree a))
alterF path onErr f = fmap (fromMaybe empty) . go path
  where
    go :: FsPath -> FsTree a -> f (Maybe (FsTree a))
    go []     t          = f t
    go (p:ps) (File   _) = onErr (FsExpectedDir path (p :| ps))
    go (p:ps) (Folder m) = Just . Folder <$> M.alterF f' p m
      where
        f' :: Maybe (FsTree a) -> f (Maybe (FsTree a))
        f' Nothing  = onErr (FsMissing path (p :| ps))
        f' (Just t) = go ps t

alterDir :: forall f a. Functor f
         => FsPath
         -> (FsTreeError -> f (FsTree a)) -- ^ Action on error
         -> f (Folder a)                  -- ^ If directory does not exist
         -> (Folder a -> f (Folder a))    -- ^ If directory exists
         -> (FsTree a -> f (FsTree a))
alterDir p onErr onNotExists onExists =
    alterF p (fmap Just . onErr') (fmap Just . f)
  where
    onErr' :: FsTreeError -> f (FsTree a)
    onErr' (FsMissing _ (_ :| [])) = Folder <$> onNotExists
    onErr' err                     = onErr err

    f :: FsTree a -> f (FsTree a)
    f (Folder m) = Folder <$> onExists m
    f (File   _) = onErr $ FsExpectedDir p (last p :| [])

alterFileMaybe :: forall f a. Functor f
               => FsPath
               -> (FsTreeError -> f (Maybe (FsTree a))) -- ^ Action on error
               -> f (Maybe a)                           -- ^ If file does not exist
               -> (a -> f (Maybe a))                    -- ^ If file exists
               -> (FsTree a -> f (FsTree a))
alterFileMaybe p onErr onNotExists onExists = alterF p onErr' f
  where
    onErr' :: FsTreeError -> f (Maybe (FsTree a))
    onErr' (FsMissing _ (_ :| [])) = fmap File <$> onNotExists
    onErr' err                     = onErr err

    f :: FsTree a -> f (Maybe (FsTree a))
    f (File   a) = fmap File <$> onExists a
    f (Folder _) = onErr $ FsExpectedFile p

alterFile :: forall f a. Functor f
          => FsPath
          -> (FsTreeError -> f (FsTree a)) -- ^ Action on error
          -> f a                           -- ^ If file does not exist
          -> (a -> f a)                    -- ^ If file exists
          -> (FsTree a -> f (FsTree a))
alterFile p onErr onNotExists onExists =
    alterFileMaybe p (fmap Just . onErr) (fmap Just onNotExists)
      (fmap Just . onExists)


{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: FsTree a
empty = Folder M.empty

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

-- | Index the FsTree by the given FsPath.
index :: FsPath -> FsTree a -> Either FsTreeError (FsTree a)
index fp = getConst . alterF fp (Const . Left) (Const . Right)

getFile :: FsPath -> FsTree a -> Either FsTreeError a
getFile fp = getConst . alterFile fp (Const . Left) errNotExist (Const . Right)
  where
    errNotExist = Const . Left $ FsMissing fp (last fp :| [])

getDir :: FsPath -> FsTree a -> Either FsTreeError (Folder a)
getDir fp = getConst . alterDir fp (Const . Left) errNotExist (Const . Right)
  where
    errNotExist = Const . Left $ FsMissing fp (last fp :| [])

{-------------------------------------------------------------------------------
  Specific file system functions
-------------------------------------------------------------------------------}

-- | Make sure the specified file exists; create it if necessary.
touch :: Monoid a => FsPath -> FsTree a -> Either FsTreeError (FsTree a)
touch fp = alterFile fp Left (Right mempty) Right

-- | Replace the contents of the specified file (which must exist)
replace :: FsPath -> a -> FsTree a -> Either FsTreeError (FsTree a)
replace fp new = alterFile fp Left errNotExist (\_old -> Right new)
  where
    errNotExist = Left (FsMissing fp (last fp :| []))

-- | Create a directory if it does not already exist
createDirIfMissing :: FsPath -> FsTree a -> Either FsTreeError (FsTree a)
createDirIfMissing fp = alterDir fp Left (Right M.empty) Right

-- | Create a directory and its parents if they do not already exist
createDirWithParents :: FsPath -> FsTree a -> Either FsTreeError (FsTree a)
createDirWithParents fp =
      -- Report full path in the error, not the prefix at the point of failure
      either (Left . setFsTreeErrorPath fp) Right
    . repeatedlyM createDirIfMissing (inits fp)

-- | Remove a file (which must exist)
removeFile :: FsPath -> FsTree a -> Either FsTreeError (FsTree a)
removeFile fp = alterFileMaybe fp Left errNotExist (const (Right Nothing))
  where
    errNotExist = Left (FsMissing fp (last fp :| []))

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

pretty :: forall a. (a -> String) -> FsTree a -> String
pretty f = drawTree . fmap renderNode . toTree
  where
    renderNode :: (String, Maybe a) -> String
    renderNode (fp, Nothing) = fp
    renderNode (fp, Just a)  = fp ++ ": " ++ f a

-- | Translate to a tree
toTree :: FsTree a -> Tree (String, Maybe a)
toTree = \case
    File   _ -> error "toTree: root must be directory"
    Folder m -> Node ("/", Nothing) $ map go (M.toList m)
  where
    go :: (String, FsTree a) -> Tree (String, Maybe a)
    go (parent, File   a) = Node (parent, Just a) []
    go (parent, Folder m) = Node (parent, Nothing) $ map go (M.toList m)
