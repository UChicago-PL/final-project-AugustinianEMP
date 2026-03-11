module Main where

import Control.Monad.Except
import BioParser
import Plotter
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            let runName = takeBaseName fileName

            -- data has to be in /plots or else filename will be printed weirdly 
            putStrLn $ "\n >> BioFlow: Loading " ++ drop 5 fileName ++ "..."
            content <- readFile fileName

            let pipeline = do
                    rawRows <- parseLabExport content
                    aggregateData rawRows

            result <- runExceptT pipeline

            case result of
                Left err -> putStrLn $ "\n ERROR: " ++ show err
                Right finalGrps -> do
                    putStrLn "\n SUCCESS: BioFlow has generated all your qPCR plots."
                    putStrLn "\n--- Data Quality Report ---"
                    mapM_ (\g -> do
                        let msg = gTreatment g ++ " / " ++ gPrimer g ++ " (CT: " ++ show (gCtMean g) ++ ")"

                        if gCtMean g >= 33.0
                        then putStrLn $ "WARNING - High CT: " ++ msg
                        else putStrLn $ "Good Data: " ++ msg
                        ) finalGrps

                    plotAllPrimers runName finalGrps

        _ -> putStrLn "\n Incorrect entry. Usage: cabal run bioflow -- data/<filename.csv>"