{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The core analyses of the tool.

module Cabal.Lint (
  -- *
  LintConfig (..),
  ResultP (ResultP),
  Summary,
  lintPackageDescription,
  lintProject,
  ) where

import           Data.Maybe (maybeToList)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Distribution.Compat.NonEmptySet as NES
import           Cabal.Lint.Ids
import           Cabal.Lint.Ord
import           Cabal.Lint.Summygroup
import           Cabal.Lint.UX.Output

import qualified Distribution.Compiler                                     as C
import qualified Distribution.Package                                      as C
import qualified Distribution.Types.Benchmark                              as C
import qualified Distribution.Types.BuildInfo                              as C
import qualified Distribution.Types.ComponentName                          as C
import qualified Distribution.Types.CondTree                               as C
import qualified Distribution.Types.Executable                             as C
import qualified Distribution.Types.GenericPackageDescription              as C
import qualified Distribution.Types.Library                                as C
import qualified Distribution.Types.LibraryName                            as C
import qualified Distribution.Types.TestSuite                              as C

-----

-- | A multiplicity
--
-- Example use: if a component constrains a dependency more than once, the
-- linter emits a 'RepeatDeps' for that component.
--
-- INVARIANT: The values are always >0.
newtype Multiplicity = Multiplicity Int
  deriving (Eq, Ord)

oneMultiplicity :: Multiplicity
oneMultiplicity = Multiplicity 1

instance Semigroup  Multiplicity where Multiplicity l <> Multiplicity r = Multiplicity $ l + r
instance Summygroup Multiplicity where Multiplicity l <+> Multiplicity r = Multiplicity $ max l r

-----

newtype PerDep a = PerDep (Map DepId a)
  deriving (Eq, Ord)

instance Semigroup a => Monoid     (PerDep a) where mempty = PerDep Map.empty
instance Semigroup a => Semigroup  (PerDep a) where PerDep l <> PerDep r = PerDep $ Map.unionWith (<>) l r

-- | TODO in the future, we might sometimes want 'Map.intersectionWith' here, but for now we always want 'Map.unionWith'
instance Summygroup a => Summygroup (PerDep a) where PerDep l <+> PerDep r = PerDep $ Map.unionWith (<+>) l r

-----

data LintConfig puid = LintConfig {
    -- | The package being linted
    thisPackage :: PrjPkgId puid
  ,
    requiredOptions :: Set String
  }

-----

-- | Check for 'ProjectMessage'
lintProject :: Ord puid => Summary puid -> Set (ProjectMessage puid)
lintProject (Summary (PerDep m1, Occurrences m2)) =
         mempty
      <> mconcat
         [ Set.singleton $ case Map.minViewWithKey m of
              -- there's just this one
              Just ((vrange, _), rest) | Map.null rest -> ConsistentVersionRange dep vrange
              _                                        -> InconsistentVersionRanges dep (Occurrences m)
         | (dep, Occurrences m) <- Map.toList m1
         ]
      <> ( Set.singleton $ case Map.minViewWithKey m2 of
              Just ((mbLang, _), rest) | Map.null rest -> ConsistentDefaultLanguage mbLang
              _                                        -> InconsistentDefaultLanguages (Occurrences m2)
         )

-----

newtype Summary puid =
    Summary
        ( PerDep (Occurrences MyVersionRange puid)
        , Occurrences (Maybe MyLanguage) puid
        )
  deriving (Monoid, Semigroup, Summygroup) 

-- | Result of analyzing a component within a package
data ResultC puid = ResultC (Summary puid) (Set ComponentError)

instance Ord puid => Monoid    (ResultC puid) where mempty = ResultC mempty mempty
instance Ord puid => Semigroup (ResultC puid) where ResultC depsL errsL <> ResultC depsR errsR = ResultC (depsL <> depsR) (errsL `Set.union` errsR)

-- | Result of analyzing a package
data ResultP puid =
    ResultP
        (Summary puid)
        (Set PackageError)
        (Map C.ComponentName (NES.NonEmptySet ComponentError))

instance Ord puid => Monoid    (ResultP puid) where mempty = ResultP mempty mempty mempty
instance Ord puid => Semigroup (ResultP puid) where ResultP depsL errsL cerrsL <> ResultP depsR errsR cerrsR = ResultP (depsL <> depsR) (Set.union errsL errsR) (Map.unionWith (<>) cerrsL cerrsR)

-- | Check for 'PackageError's and 'ComponentError's and also collect the
-- summary info necessary for 'lintProject'
lintPackageDescription :: forall puid. Ord puid => LintConfig puid -> C.GenericPackageDescription -> ResultP puid
lintPackageDescription cfg gpd =
       ResultP mempty (NonEmptyForeignLibraries `ifJust` mkNES (Set.fromList (map fst forLibs))) mempty
    <> foldMap (\     x  -> go (C.CLibName C.LMainLibName    ) (fmap C.libBuildInfo       x)) mbLib
    <> foldMap (\(cn, x) -> go (C.CLibName (C.LSubLibName cn)) (fmap C.libBuildInfo       x)) subLibs
    <> foldMap (\(cn, x) -> go (C.CExeName                cn ) (fmap C.buildInfo          x)) exes
    <> foldMap (\(cn, x) -> go (C.CTestName               cn ) (fmap C.testBuildInfo      x)) tests
    <> foldMap (\(cn, x) -> go (C.CBenchName              cn ) (fmap C.benchmarkBuildInfo x)) benchs
  where
    LintConfig _ _dummy_cfg = cfg
    LintConfig {
        thisPackage = puid
      ,
        requiredOptions = options0
      } = cfg

    C.GenericPackageDescription _ _ _ _ _ _ _ _ _dummy_gpd = gpd
    C.GenericPackageDescription {
        C.packageDescription = _
      ,
        C.gpdScannedVersion = _
      ,
        C.condLibrary = mbLib
      ,
        C.condSubLibraries = subLibs
      ,
        C.condForeignLibs = forLibs
      ,
        C.condExecutables = exes
      ,
        C.condTestSuites = tests
      ,
        C.condBenchmarks = benchs
      } = gpd

    go :: C.ComponentName -> C.CondTree v c C.BuildInfo -> ResultP puid
    go cname tree =
      let ResultC deps cerrs =
            (goDepends <> goGhcOptions <> goLanguage)
              (CompId puid cname)
              tree
      in
        ResultP deps mempty
      $ Map.fromList [ (cname, x) | x <- maybeToList (mkNES cerrs) ]

    getDepends :: CompId puid -> C.BuildInfo -> PerDep (Occurrences MyVersionRange puid, Multiplicity)
    getDepends me bi =
          PerDep
        $ Map.fromListWith (<>)
        $ [ ( DepId pname libname
            , ( oneOccurrence (MyVersionRange vrange) me
              , oneMultiplicity
              )
            )
          | C.Dependency pname vrange libnames <- C.targetBuildDepends bi
          , libname <- NES.toList libnames
          ]

    goDepends :: CompId puid -> C.CondTree v c C.BuildInfo -> ResultC puid
    goDepends me tree =
        let PerDep m     = goTree $ fmap (getDepends me) tree
            repeatedDeps = Map.keysSet $ Map.filter ((> oneMultiplicity) . snd) m
        in
        ResultC
          (Summary (PerDep (Map.map fst m), mempty))
          (RepeatDeps `ifJust` mkNES repeatedDeps)

    getGhcOptions :: C.BuildInfo -> Always String
    getGhcOptions bi =
          mconcat
        $ [ oneAlways opt
          | (C.GHC, opts) <- C.perCompilerFlavorToList (C.options bi)
          , opt <- opts
          ]

    goGhcOptions :: CompId puid -> C.CondTree v c C.BuildInfo -> ResultC puid
    goGhcOptions _me tree =
        let Always counts = goTree $ fmap getGhcOptions tree
        in
          ResultC mempty
        $    mempty
          <> (RepeatOptions  `ifJust` mkNES (Map.keysSet $ Map.filter (> 1) counts))
          <> (MissingOptions `ifJust` mkNES (Set.difference options0 (Map.keysSet counts)))

    getLanguage :: CompId puid -> C.BuildInfo -> (Occurrences (Maybe MyLanguage) puid, Sometimes ComponentError)
    getLanguage me bi =
        ( oneOccurrence (MyLanguage <$> C.defaultLanguage bi) me
        ,   mconcat
          $ [ oneSometimes $ NonEmptyDefaultExtensions exts'
            | exts' <- maybeToList $ mkNES $ Set.fromList $ C.defaultExtensions bi
            ]
        )

    goLanguage :: CompId puid -> C.CondTree v c C.BuildInfo -> ResultC puid
    goLanguage me tree =
        let (defaultLanguages, Sometimes errs) =
                goTree $ fmap (getLanguage me) tree
        in
        ResultC (Summary (mempty, defaultLanguages)) (Map.keysSet errs)

-----

goTree :: (Monoid m, Summygroup m) => C.CondTree v c m -> m
goTree tree =
    let C.CondNode _ _ _dummy = tree
        C.CondNode {
            C.condTreeData = x
          ,
            C.condTreeConstraints = _
          ,
            C.condTreeComponents = subtrees
          } = tree
    in
    foldr (<>) x (map goBranch subtrees)

goBranch :: (Monoid m, Summygroup m) => C.CondBranch v c m -> m
goBranch branch =
    let C.CondBranch _ _ _dummy = branch
        C.CondBranch {
            C.condBranchCondition = _
          ,
            C.condBranchIfTrue = tree
          ,
            C.condBranchIfFalse = mbTree
          } = branch
    in
    goTree tree <+> maybe mempty goTree mbTree

-----

ifJust :: (a -> b) -> Maybe a -> Set b
ifJust mkErr mb = maybe Set.empty (Set.singleton . mkErr) mb

mkNES :: Ord a => Set a -> Maybe (NES.NonEmptySet a)
mkNES xs = case Set.minView xs of
    Nothing       -> Nothing
    Just (x, xs') -> Just $ foldr NES.insert (NES.singleton x) xs'
