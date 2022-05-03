{-# LANGUAGE OverloadedStrings #-}

module Test.Ouroboros.Storage.FsTree (tests) where

import           Data.List ((\\))
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?))
import           Text.Show.Pretty (ppShow, ppShowList)



import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath,
                     fsPathFromList)
import           Test.Util.FS.Sim.FsTree (FsTree (File, Folder),
                     FsTreeError (FsMissing), find)

tests :: TestTree
tests =
    testGroup "FsTree"
    [ testGroup "find command returns exactly what's expected"
      [ testCase "usr"      $ checkResultsOfFind ["usr"]         example findUsr
      , testCase "var"      $ checkResultsOfFind ["var"]         example findVar
      , testCase "var/log"  $ checkResultsOfFind ["var", "log"]  example findVarLog
      , testCase "root"     $ checkResultsOfFind []              example findRoot

      -- Bad weather
      , testCase "boom" $ ["boom"] `shouldReportMissingFileIn` example
      ]
    ]

checkResultsOfFind :: [Text] -> FsTree () -> [FsPath] -> Assertion
checkResultsOfFind fp fs expectedResult = do
    (expectedResult \\ filePathsFound)
      `shouldBeEmptyOtherwise` "Not all expected paths were found"
    (filePathsFound \\ expectedResult)
      `shouldBeEmptyOtherwise` "Find returned unexpected paths"
  where
    filePathsFound = either (error . ppShow) id
                   $ find (fsPathFromList fp) fs
    shouldBeEmptyOtherwise  x msg =
      null x @? msg ++ ":\n" ++ ppShowList x

shouldReportMissingFileIn :: [Text] -> FsTree () -> Assertion
shouldReportMissingFileIn fp fs =
    case find (fsPathFromList fp) fs of
      Left FsMissing {} -> pure ()
      Left err          -> assertFailure $ "Unexpected error: " ++ ppShow err
      Right _           -> assertFailure $ ppShow fp
                                         ++ " was found on this filesystem:\n"
                                         ++ ppShow fs

{-------------------------------------------------------------------------------
  Examples and expected results
-------------------------------------------------------------------------------}

example :: FsTree ()
example =
    Folder $ M.fromList [
        ("usr", Folder $ M.fromList [
            ("local", Folder $ M.fromList [
                ("bin", Folder mempty)
              ])
          ])
      , ("var", Folder $ M.fromList [
            ("log",  Folder $ M.fromList [
                  ("some.log", File mempty)
                , ("apt",     Folder mempty)
                , ("cups",    Folder $ M.fromList [
                        ("bar.txt", File mempty)
                      , ("baz.txt", File mempty)
                      , ("buz",     Folder $ M.fromList [
                            ("sample.log", File mempty)
                          ])
                      , ("biz", Folder mempty)
                    ])
              ])
          , ("mail", Folder mempty)
          , ("run",  Folder mempty)
          , ("tmp",  Folder $ M.fromList [
                ("foo.txt", File mempty)
              ])
          ])
      ]

findUsr :: [FsPath]
findUsr =
    fmap fsPathFromList [ ["usr"]
                        , ["usr", "local"]
                        , ["usr", "local", "bin"]
                        ]


findVar :: [FsPath]
findVar =
    fmap fsPathFromList [ ["var"]
                        , ["var", "log"]
                        , ["var", "log", "some.log"]
                        , ["var", "log", "apt"]
                        , ["var", "log", "cups"]
                        , ["var", "log", "cups", "bar.txt"]
                        , ["var", "log", "cups", "baz.txt"]
                        , ["var", "log", "cups", "buz"]
                        , ["var", "log", "cups", "buz", "sample.log"]
                        , ["var", "log", "cups", "biz"]
                        , ["var", "mail"]
                        , ["var", "run"]
                        , ["var", "tmp"]
                        , ["var", "tmp", "foo.txt"]
                        ]

findVarLog :: [FsPath]
findVarLog =
    fmap fsPathFromList [ ["var", "log"]
                        , ["var", "log", "some.log"]
                        , ["var", "log", "apt"]
                        , ["var", "log", "cups"]
                        , ["var", "log", "cups", "bar.txt"]
                        , ["var", "log", "cups", "baz.txt"]
                        , ["var", "log", "cups", "buz"]
                        , ["var", "log", "cups", "buz", "sample.log"]
                        , ["var", "log", "cups", "biz"]
                        ]

findRoot :: [FsPath]
findRoot = [fsPathFromList []] <> findUsr <> findVar
