module Main where

import Control.Monad.Except
import BioParser
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            