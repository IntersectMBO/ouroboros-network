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
import           Control.Monad (join)
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Distribution.Compat.NonEmptySet as NES

import qualified Distribution.Pretty                                       as C
import qualified Distribution.Types.ComponentName                          as C
import qualified Distribution.Types.UnqualComponentName                    as C
import qualified Distribution.Types.Version                                as C
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
    NonEmptyExtensions (NES.NonEmptySet C.Extension)
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
        NonEmptyExtensions{} -> "Extensions"

-- | The user-readable meaning of a 'Message'
renderMessageOneLineText :: forall puid. (PrjPkgId puid -> String) -> Message puid -> String
renderMessageOneLineText sho e = case e of
    ProjectMessage err -> case err of
        EmptyProject -> "There were no .cabal files to analyze; pass at least one command-line argument, each a path to a .cabal file"
        ConsistentVersionRange (DepId pname libname) (MyVersionRange vrange) -> "Dependency " <> C.prettyShow pname <> ":" <> C.prettyShow (C.CLibName libname) <> " is consistently " <> prettyV vrange
        ConsistentDefaultLanguage mbLang -> "The default language is consistently " <> prettyL mbLang
        InconsistentVersionRanges (DepId pname libname) (Occurrences m) -> "Dependency " <> C.prettyShow pname <> ":" <> C.prettyShow (C.CLibName libname) <> " is " <> differences (prettyV . unMyVersionRange) m <> "; " <> theIntersection (Occurrences m)
        InconsistentDefaultLanguages (Occurrences m) -> "The default language is " <> differences prettyL m
    PackageMessage puid err -> case err of
        CouldNotParse -> "Package " <> sho puid <> " could not be parsed as a `.cabal` file"
        NonEmptyForeignLibraries cnames -> "Package " <> sho puid <> " declares foreign libraries: " <> intercalate ", " (map C.prettyShow (NES.toList cnames))
    ComponentMessage (CompId puid cname) err -> "Component " <> C.prettyShow cname <> " in package " <> sho puid <> " " <> case err of
        RepeatDeps pkgnames -> "repeats dependencies in its `build-depends' fields: " <> intercalate ", " [ C.prettyShow pname <> ":" <> C.prettyShow (C.CLibName libname) | DepId pname libname <- NES.toList pkgnames ]
        RepeatOptions opt -> "repeats options in its `ghc-options' fields: " <> unwords (NES.toList opt)
        MissingOptions opts -> "is missing required options in its `ghc-options' fields: " <> unwords (NES.toList opts)
        NonEmptyExtensions exts -> "declares `default-extensions' and/or `other-extensions': " <> unwords (map C.prettyShow $ NES.toList exts)
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

    theIntersection (Occurrences m) =
        let cons (MyVersionRange x) y = intersection <$> versionRangeToInterval x <*> y
            nil                       = Just full
        in
        case foldr cons nil (Map.keys m) of
            Nothing   -> "those constraints are not all simple intervals"
            Just ival -> "the intersection of those constraints as simple intervals is " <> C.prettyShow (intervalToVersionRange ival)

-----

bottom :: C.Version
bottom = C.version0   -- not C.nullVersion = C.mkVersion [] because our lower bound is inclusive

next :: C.Version -> C.Version
next = C.mkVersion . (++ [0]) . C.versionNumbers

-- | Partial because the predecessor of @V.1@ could
-- be @V.0.xyz@ where @xyz@ can be arbitrarily long
--
-- INVARIANT: @'prev' y = Just x@ if and only if @y = 'next' x@
prev :: C.Version -> Maybe C.Version
prev =
    fmap C.mkVersion . go . C.versionNumbers
  where
    go = \case
        [0]  -> Just []
        x:xs -> (x:) <$> go xs
        _    -> Nothing

-----

-- | A type that is either zero or negative epsilon
--
-- We use this to simplify the handling of strict upper bounds. If 'prev' were
-- total, we could use that instead, and 'Nudge' would be unnecessary.
data Nudge = LessEpsilon | NoNudge
  deriving (Eq, Ord, Show)

-- | An enriched type for the upper bound
--
-- The extra structure represents unboundedness and the distinction between @<@
-- and @<=@ (see 'Nudge').
data Upper a = Middle a Nudge | Top
  deriving (Eq, Ord, Show)

-- | Lower is inclusive, upper is exclusive
data Interval a = Interval a (Upper a)
  deriving (Show)

empty :: a -> Interval a
empty x = x `Interval` Middle x LessEpsilon

intersection :: Ord a => Interval a -> Interval a -> Interval a
intersection (loL `Interval` hiL) (loR `Interval` hiR)
    | hiL < Middle loR NoNudge = empty loR   -- they don't abut
    | hiR < Middle loL NoNudge = empty loR   -- they don't abut
    | otherwise                = max loL loR `Interval` min hiL hiR

-- | Fails when they don't abut
union :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
union (loL `Interval` hiL) (loR `Interval` hiR)
    | hiL < Middle loR NoNudge = Nothing   -- they don't abut
    | hiR < Middle loL NoNudge = Nothing   -- they don't abut
    | otherwise                = Just $ min loL loR `Interval` max hiL hiR

-----

full :: Interval C.Version
full = bottom `Interval` Top

intervalToVersionRange :: Interval C.Version -> C.VersionRange
intervalToVersionRange (lo `Interval` ehi) = case ehi of
    Middle hi nudge -> case compare lo hi of
        LT -> (if lo == bottom then id else C.IntersectVersionRanges lower) $ case nudge of
            NoNudge     -> C.OrEarlierVersion hi
            LessEpsilon -> C.EarlierVersion   hi
        EQ -> case nudge of
            NoNudge     -> C.ThisVersion lo
            LessEpsilon -> C.noVersion
        GT -> C.noVersion

    Top -> lower
  where
    lower = case prev lo of
            Just v -> C.LaterVersion v
            _      -> C.OrLaterVersion lo

-- | Fails if there's a union of intervals that do not abut
versionRangeToInterval :: C.VersionRange -> Maybe (Interval C.Version)
versionRangeToInterval = \case
    C.ThisVersion       v -> Just $ v      `Interval` Middle v NoNudge
    C.LaterVersion      v -> Just $ next v `Interval` Top
    C.OrLaterVersion    v -> Just $ v      `Interval` Top
    C.EarlierVersion    v -> Just $ bottom `Interval` Middle v LessEpsilon
    C.OrEarlierVersion  v -> Just $ bottom `Interval` Middle v NoNudge
    C.MajorBoundVersion v -> Just $ v      `Interval` Middle (C.majorUpperBound v) LessEpsilon

    C.UnionVersionRanges     l r -> join $ union        <$> versionRangeToInterval l <*> versionRangeToInterval r
    C.IntersectVersionRanges l r ->        intersection <$> versionRangeToInterval l <*> versionRangeToInterval r
