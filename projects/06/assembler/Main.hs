module Main where

import Assembler
import Instruction
import Parser
import System.Environment
import System.IO
import SymbolTable

main = do
    [inPath, outPath] <- getArgs
    contents <- readFile inPath
    {-
    let Right voss = parseLines $ lines contents
        Right table = generateSymbolTable voss
        Right ss = replaceSymbols table voss
    putStrLn $ contents
    putStrLn $ unlines $ map show ss
    -}
    either (hPutStr stderr) (writeFile outPath) $ assemble contents
