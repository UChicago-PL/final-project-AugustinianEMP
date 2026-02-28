module Main where

import Control.Monad.Except
import BioParser
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            putStrLn $  "\n >> BioFlow: Loading " ++ fileName ++ "..."
            
            content <- readFile fileName
            
            result <- runExceptT (parseLabExport content)
            
            case result of
                Left err -> putStrLn $ "\n ERROR: " ++ show err
                Right rows -> do
                    putStrLn $ "\n SUCCESS: Extracted " ++ show (length rows) ++ " rows."
                    mapM_ (print) (take 20 rows)
        _ -> putStrLn "\n Usage: cabal run bioflow -- <filename.csv>"