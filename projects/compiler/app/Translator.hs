module Main where

import Translator.Commands
import Translator.Parser
import Translator.Translator
import System.Environment
import System.IO

main = do
    filePaths <- getArgs
    results <- translateProgram filePaths
    either (hPutStr stderr) (putStr) results
