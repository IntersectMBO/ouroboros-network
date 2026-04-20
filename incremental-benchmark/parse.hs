#!/usr/bin/env cabal

{- cabal:
build-depends: base, tdigest
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}

module Main where

import Data.Foldable
import Data.Maybe (listToMaybe, mapMaybe)
import Data.TDigest
import Text.Read  (readMaybe)

data Report = Report
  { header  :: ![LogEntry]
  , details :: ![ExperimentSummary]
  }

instance Show Report where
  show (Report hdr dt) =
    let xs = map show hdr
        ys = map show dt
     in unlines $ zipWith (\x y -> x <> "\n" <> y) xs ys


data ExperimentSummary = ExperimentSummary
  { label      :: !String
  , idleTime   :: !Double
  , decodeTime :: !Double
  , stats      :: !(Maybe Double)
  }
  deriving Show

data Event = IdleRx | Decoding
  deriving (Eq, Show)

data EventKind = Start | Step !Source !Event
  deriving (Show, Eq)

data Source = Client | Decoder | Encoder
  deriving (Eq, Show)

data LogEntry = LogEntry
  { duration  :: !Double
  , eventKind :: !EventKind
  }
  deriving Show


parseEventKind :: String -> Maybe EventKind
parseEventKind ev = case words ev of
  "DURATION" : _rest -> Just Start
  "STEP" : _experiment : source : which : rest ->
    let source' = case source of
          "decoder" -> Decoder
          "encoder"  -> Encoder
          "client"  -> Client
        which' = case which of
          "run" -> Decoding
          "recv" -> IdleRx
     in Just $ Step source' which'
  _otherwise -> Nothing


parseLine :: String -> Maybe LogEntry
parseLine line = case words line of
  ts : _ts1 :  _ts2 : _cap : _id : ev | ek@(Just{}) <- parseEventKind (unwords ev)->
    LogEntry <$> fmap fst (listToMaybe (reads ts)) <*> ek
  ts : _ts1 : _cap : _id : ev ->
    LogEntry <$> fmap fst (listToMaybe (reads ts)) <*> parseEventKind (unwords ev)
  e -> Nothing


splitExperiments :: [LogEntry] -> [[LogEntry]]
splitExperiments entries =
    case dropWhile (notStep . eventKind) entries of
      []            -> []
      (step:rest) ->
        let (body, remainder) = break ((== Start) . eventKind) rest
        in  (step : body) : splitExperiments (drop 1 remainder)
  where
    notStep e = case e of
      Step{} -> False
      _otherwise -> True


summarize :: String -> [LogEntry] -> ExperimentSummary
summarize label entries = summary { stats = mkStats (tdigest ts :: TDigest 5) }
  where
    (summary, ts) = foldr accumulate ((ExperimentSummary label 0 0 Nothing), []) entries

    accumulate (LogEntry duration (Step _src IdleRx)) (!s, !ts) =
      (s { idleTime = idleTime s + duration}, ts)
    accumulate (LogEntry duration (Step _src Decoding)) (!s, !ts) =
      (s { decodeTime = decodeTime s + duration }, duration : ts)
    accumulate _ s = s


processLog :: String -> String -> Report --([String], [ExperimentSummary])
processLog label input = Report header' summary
  where
    (header, details) = splitSections
                      . lines
                      $ input
    header' = mapMaybe parseLine header
    summary = fmap (fmap (summarize label) . init) splitExperiments
            . mapMaybe parseLine
            $ details


splitSections :: [String] -> ([String], [String])
splitSections input = (init header, tail details)
  where
    (header, details) = break divider input
    divider line = case words line of
      "---" : rest -> True
      _otherwise -> False


mkStats :: TDigest comp -> Maybe Double
mkStats = mean

tdigest' :: Foldable f => f Double -> TDigest 5
tdigest' = tdigest


main :: IO ()
main = do
  contents <- readFile "small_nonincremental"
  let report = processLog "small non-incremental" contents
      meanDecodeT = mean . tdigest' . foldl' (\ts exp -> decodeTime exp : ts) [] . details $ report
  putStrLn $ "small non-incremental mean decode time " <> show meanDecodeT
  print report

  contents <- readFile "small_incremental"
  let report = processLog "small incremental" contents
      meanDecodeT = mean . tdigest' . foldl' (\ts exp -> decodeTime exp : ts) [] . details $ report
  putStrLn $ "small incremental mean decode time " <> show meanDecodeT
  print report

  contents <- readFile "large_nonincremental"
  print $ processLog "large non-incremental" contents

  contents <- readFile "large_incremental"
  print $ processLog "large incremental" contents
