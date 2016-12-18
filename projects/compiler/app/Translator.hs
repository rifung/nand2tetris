module Main where

import Translator.Commands
import Translator.Parser
import Translator.Translator
import System.Environment
import System.IO

main = do
    [inPath, outPath] <- getArgs
    Right results <- translateFile inPath 
    writeFile outPath results
