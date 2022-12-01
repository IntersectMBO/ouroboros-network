{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cabal.Lint.UX.Output (
  -- *
  Occurrences (..),
  oneOccurrence,
  -- *
  ComponentError (..),
  Message (..),
  MessageSeverity (..),
  PackageError (..),
  ProjectMessage (..),
  messageSeverity,
  renderMessageOneLine,
  ) where

import           Cabal.Lint.Ids
import           Cabal.Lint.Ord
import           Cabal.Lint.Summygroup
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Distribution.Compat.NonEmptySet as NES

import qualified Distribution.Pretty                                       as C
import qualified Distribution.Types.ComponentName                          as C
import qualified Distribution.Types.UnqualComponentName                    as C
import qualified Distribution.Types.VersionRange.Internal                  as C
import qualified Language.Haskell.Extension                                as C

-----

-- | A map from values (eg a default language) to the components in which they
-- occur
--
-- Example use: a mapping @vrange |-> comps@ means each of those the components
-- might constrain the dependency to that range. If it's possible a dependency
-- is constrained to more than one range, the linter raises an error about the
-- components not obviously agreeing.
newtype Occurrences a puid = Occurrences (Map a (NES.NonEmptySet (CompId puid)))
  deriving (Eq, Ord)

oneOccurrence :: a -> CompId puid -> Occurrences a puid
oneOccurrence a cid = Occurrences $ Map.singleton a $ NES.singleton cid

instance (Ord a, Ord puid) => Monoid (Occurrences a puid) where
  mempty = Occurrences Map.empty

-- | @unionWith (<>)@
instance (Ord a, Ord puid) => Semigroup (Occurrences a puid) where
  Occurrences l <> Occurrences r = Occurrences $ Map.unionWith (<>) l r

-- | Note that @(<+>) = (<>)@
instance (Ord a, Ord puid) => Summygroup (Occurrences a puid) where (<+>) = (<>)

-----

-- | Error at the component scope
--
-- See definition of 'renderMessageOneLineText'
data ComponentError =
    RepeatDeps (NES.NonEmptySet DepId)
  |
    RepeatOptions (NES.NonEmptySet String)
  |
    MissingOptions (NES.NonEmptySet String)
  |
    NonEmptyDefaultExtensions (NES.NonEmptySet C.Extension)
  deriving (Eq, Ord)

-- | Error at the package scope
--
-- See definition of 'renderMessageOneLineText'
data PackageError =
    NonEmptyForeignLibraries (NES.NonEmptySet C.UnqualComponentName)
  |
    CouldNotParse
  deriving (Eq, Ord)

-- | Error at the project scope
--
-- See definition of 'renderMessageOneLineText'
data ProjectMessage puid =
    EmptyProject
  |
    ConsistentVersionRange DepId MyVersionRange
  |
    ConsistentDefaultLanguage (Maybe MyLanguage)
  |
    InconsistentVersionRanges DepId (Occurrences MyVersionRange puid)
      -- ^ INVARIANT: non-empty
      --
      -- INVARIANT: all contained 'Occurrences' have multiple entries
  |
    InconsistentDefaultLanguages (Occurrences (Maybe MyLanguage) puid)
      -- ^ INVARIANT: multiple entries
  deriving (Eq, Ord)

-- | All errors the linter can raise
data Message puid =
    ProjectMessage (ProjectMessage puid)
  |
    PackageMessage (PrjPkgId puid) PackageError
  |
    ComponentMessage (CompId puid) ComponentError

-- | How severe is the message?
data MessageSeverity = InfoSeverity | ErrorSeverity

instance Monoid    MessageSeverity where mempty = InfoSeverity
instance Semigroup MessageSeverity where
    InfoSeverity <> InfoSeverity = InfoSeverity
    _            <> _            = ErrorSeverity

-----

renderMessageOneLine :: (PrjPkgId puid -> String) -> Message puid -> String
renderMessageOneLine sho msg =
    "[" <> severityText <> "-" <> code <> "] " <> renderMessageOneLineText sho msg
  where
    (severity, code) = classifyMessage msg
    severityText = case severity of
        InfoSeverity  -> "I"
        ErrorSeverity -> "E"

messageSeverity :: Message puid -> MessageSeverity
messageSeverity = fst . classifyMessage

-- | The severity and code of a 'Message'
classifyMessage :: Message puid -> (MessageSeverity, String)
classifyMessage = \case
    ProjectMessage msg -> case msg of
        EmptyProject{} -> (ErrorSeverity, "EmptyProject")
        ConsistentVersionRange{} -> (InfoSeverity, "ConsistentVersions")
        ConsistentDefaultLanguage{} -> (InfoSeverity, "ConsistentLanguages")
        InconsistentVersionRanges{} -> (ErrorSeverity, "InconsistentVersions")
        InconsistentDefaultLanguages{} -> (ErrorSeverity, "InconsistentLanguages")
    PackageMessage _puid msg -> (,) ErrorSeverity $ case msg of
        CouldNotParse{} -> "NoParse"
        NonEmptyForeignLibraries{} -> "ForeignLibs"
    ComponentMessage _cid msg -> (,) ErrorSeverity $ case msg of
        RepeatDeps{} -> "RepeatDeps"
        RepeatOptions{} -> "RepeatOptions"
        MissingOptions{} -> "MissingOptions"
        NonEmptyDefaultExtensions{} -> "DefaultExtensions"

-- | The user-readable meaning of a 'Message'
renderMessageOneLineText :: forall puid. (PrjPkgId puid -> String) -> Message puid -> String
renderMessageOneLineText sho e = case e of
    ProjectMessage err -> case err of
        EmptyProject -> "There were no .cabal files to analyze; pass at least one command-line argument, each a path to a .cabal file"
        ConsistentVersionRange (DepId pname libname) (MyVersionRange vrange) -> "Dependency " <> C.prettyShow pname <> ":" <> C.prettyShow (C.CLibName libname) <> " is consistently " <> prettyV vrange
        ConsistentDefaultLanguage mbLang -> "The default language is consistently " <> prettyL mbLang
        InconsistentVersionRanges (DepId pname libname) (Occurrences m) -> "Dependency " <> C.prettyShow pname <> ":" <> C.prettyShow (C.CLibName libname) <> " is " <> differences (prettyV . unMyVersionRange) m
        InconsistentDefaultLanguages (Occurrences m) -> "The default language is " <> differences prettyL m
    PackageMessage puid err -> case err of
        CouldNotParse -> "Package " <> sho puid <> " could not be parsed as a `.cabal` file"
        NonEmptyForeignLibraries cnames -> "Package " <> sho puid <> " declares foreign libraries: " <> intercalate ", " (map C.prettyShow (NES.toList cnames))
    ComponentMessage (CompId puid cname) err -> "Component " <> C.prettyShow cname <> " in package " <> sho puid <> " " <> case err of
        RepeatDeps pkgnames -> "repeats dependencies in its `build-depends' fields: " <> intercalate ", " [ C.prettyShow pname <> ":" <> C.prettyShow (C.CLibName libname) | DepId pname libname <- NES.toList pkgnames ]
        RepeatOptions opt -> "repeats options in its `ghc-options' fields: " <> unwords (NES.toList opt)
        MissingOptions opts -> "is missing required options in its `ghc-options' fields: " <> unwords (NES.toList opts)
        NonEmptyDefaultExtensions exts -> "declares default language extensions: " <> unwords (map C.prettyShow $ NES.toList exts)
  where
    prettyV vrange =
        if C.anyVersion == vrange
        then "unconstrained"
        else "constrained to " <> C.prettyShow vrange

    prettyL = \case
        Nothing                -> "unset"
        Just (MyLanguage lang) -> "set to " <> show lang

    differences :: (k -> String) -> Map k (NES.NonEmptySet (CompId puid)) -> String
    differences prettyK m =
        intercalate "; but "
        [ prettyK k <> " in " <> intercalate ", "
            [ "component " <> C.prettyShow cname <> " of package " <> sho puid
            | CompId puid cname <- NES.toList comps
            ]
        | (k, comps) <- Map.toList m
        ]
