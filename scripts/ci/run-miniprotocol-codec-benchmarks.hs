import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (callCommand)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [machine, ghcVersion] -> runBenchmarks machine ghcVersion
        _                     -> putStrLn "Usage: runBenchmarks <machine> <ghc-version>"

runBenchmarks :: String -> String -> IO ()
runBenchmarks machine ghcVersion = do
    let baseline = "benchmarks/baseline-" ++ machine ++ "-8.10.7.csv"
    let filename = "benchmarks/baseline-" ++ machine ++ "-" ++ ghcVersion ++ ".csv"

    createDirectoryIfMissing True "benchmarks"

    baselineExists <- doesFileExist baseline
    filenameExists <- doesFileExist filename

    if baselineExists && filenameExists
        then do
            putStrLn "Running benchmarks with baseline..."
            callCommand $ "cabal run ouroboros-network-protocols:bench -- --baseline " ++ baseline ++ " --fail-if-slower 5"
        else do
            putStrLn "Running benchmarks to create baseline..."
            callCommand $ "cabal run ouroboros-network-protocols:bench -- --csv " ++ filename

