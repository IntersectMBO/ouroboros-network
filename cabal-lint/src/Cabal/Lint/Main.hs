{-# LANGUAGE LambdaCase #-}

-- | The exe's entrypoint

module Cabal.Lint.Main (main) where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Data.Foldable (forM_)
import qualified GHC.Conc as STM
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Void (absurd)
import           System.Environment (getArgs)
import           System.Exit (die, exitFailure, exitSuccess)

import qualified Cabal.Lint                                             as Lint
import qualified Cabal.Lint.Ids                                         as Lint
import qualified Cabal.Lint.UX.Output                                   as Lint

import qualified Distribution.PackageDescription.Parsec                    as C

main :: IO ()
main = do
    cabalPaths <- getArgs

    when (null cabalPaths) $ do
        die $ Lint.renderMessageOneLine (absurd . Lint.unPrjPkgId) (Lint.ProjectMessage Lint.EmptyProject)

    accSummary  <- STM.newTVarIO (mempty :: Lint.Summary FilePath)
    accSeverity <- STM.newTVarIO (mempty :: Lint.MessageSeverity)

    let showMessage :: Lint.Message FilePath -> IO ()
        showMessage msg = do
            putStrLn $ Lint.renderMessageOneLine Lint.unPrjPkgId msg
            modifyTVarIO accSeverity (<> Lint.messageSeverity msg)

    -- analyze each .cabal file path given on the command line
    --
    -- Every message is printed as a single line, so concurrent printing is
    -- fine.
    forConcurrently_ cabalPaths $ \path -> do
        let puid = Lint.PrjPkgId path
        bytes <- BS.readFile path
        case C.parseGenericPackageDescriptionMaybe bytes of
            Nothing -> die $ Lint.renderMessageOneLine Lint.unPrjPkgId (Lint.PackageMessage puid Lint.CouldNotParse)
            Just x -> do
                let cfg = Lint.LintConfig {
                        Lint.thisPackage = puid
                      ,
                        Lint.requiredOptions = Set.fromList requiredOptions
                      }
                    Lint.ResultP summary perrs cerrs = Lint.lintPackageDescription cfg x

                modifyTVarIO accSummary (<> summary)

                forM_ perrs $ \perr -> showMessage $ Lint.PackageMessage puid perr

                forM_ (Map.assocs cerrs) $ \(cname, errs) ->
                    forM_ errs $ \err ->
                        showMessage $ Lint.ComponentMessage (Lint.CompId puid cname) err

    -- analyze the given .cabal files together as a project
    do
        summary <- STM.readTVarIO accSummary
        forM_ (Lint.lintProject summary) $ \msg -> showMessage $ Lint.ProjectMessage msg

    STM.readTVarIO accSeverity >>= \case
        Lint.InfoSeverity  -> exitSuccess
        Lint.ErrorSeverity -> exitFailure

modifyTVarIO :: STM.TVar a -> (a -> a) -> IO ()
modifyTVarIO tvar f = STM.atomically $ do
    a <- STM.readTVar tvar
    STM.writeTVar tvar (f a)

-----

-- | Every component must certinaly include at least these in `ghc-options'
requiredOptions :: [String]
requiredOptions = words "\
  \-Wall\n\
  \-Wcompat\n\
  \-Widentities\n\
  \-Wincomplete-record-updates\n\
  \-Wincomplete-uni-patterns\n\
  \-Wmissing-export-lists\n\
  \-Wpartial-fields\n\
  \-Wredundant-constraints\n\
  \"

{-
    -- parse the list of allowed dependencies
    cfg <- do
        let ss = filter (not . null) allowedBounds
        myDepss <- forM ss $ \s ->
            case C.explicitEitherParsec (parseOneDep <* C.eof) s of
                Left err -> die $ "could not parse bounds: " <> s <> "; " <> show err
                Right deps -> pure deps
        let _ = myDepss :: [NES.NonEmptySet MyDep]
        pure LintConfig {
            bounds = foldMap NES.toSet myDepss
          ,
            options = Set.fromList requiredOptions
          }
-}

{-
parseOneDep :: C.ParsecParser (NES.NonEmptySet MyDep)
parseOneDep =
    f <$> C.parsec
  where
    f (C.Dependency pname vrange libnames) =
      NES.map (\libname -> MyDep pname libname vrange) libnames
-}
