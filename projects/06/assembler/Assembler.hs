module Assembler
( assemble
, assembleLine
, assembleLines
, assembleProgram
) where

import Data.Char (intToDigit)
import Data.Either
import Data.List as List
import Instruction
import Numeric (showIntAtBase)
import SymbolTable
import Parser
import Debug.Trace

assemble :: String -> Either String String
assemble asm =
    let withSymbolsMaybe = parseString asm
        symbolTableMaybe = withSymbolsMaybe >>= generateSymbolTable
        valuesOnlyMaybe = symbolTableMaybe >>=
                          (\table -> withSymbolsMaybe >>=
                          (\withSymbols -> replaceSymbols table withSymbols))
    in valuesOnlyMaybe >>= (pure . assembleProgram)

assembleLine :: Instruction Value -> String
assembleLine (AS v) =
    let prependZeroes v' = (replicate (15 - (length v')) '0') ++ v'
        vBase2 = showIntAtBase 2 intToDigit (read $ getValue v) ""
    in '0':(prependZeroes $ vBase2)
assembleLine CS { getDst=dst, getComp=comp, getJmp=jmp } =
    -- TODO we can make this a map and also remove Parser.allowedCommands
    let compString = case comp of "0" -> "0101010"
                                  "1" -> "0111111"
                                  "-1" -> "0111010"
                                  "D" -> "0001100"
                                  "A" -> "0110000"
                                  "M" -> "1110000"
                                  "!D" -> "0001101"
                                  "!A" -> "0110001"
                                  "!M" -> "1110001"
                                  "-D" -> "0001111"
                                  "-A" -> "0110011"
                                  "-M" -> "1110011"
                                  "D+1" -> "0011111"
                                  "A+1" -> "0110111"
                                  "M+1" -> "1110111"
                                  "D-1" -> "0001110"
                                  "A-1" -> "0110010"
                                  "M-1" -> "1110010"
                                  "D+A" -> "0000010"
                                  "D+M" -> "1000010"
                                  "D-A" -> "0010011"
                                  "D-M" -> "1010011"
                                  "A-D" -> "0000111"
                                  "M-D" -> "1000111"
                                  "D&A" -> "0000000"
                                  "D&M" -> "1000000"
                                  "D|A" -> "0010101"
                                  "D|M" -> "1010101"
        dstBit :: Char -> Char
        dstBit c = if List.elem c dst then '1' else '0'
        dstString = foldr (\c l -> (dstBit c):l) [] "ADM"
        -- TODO we can make this a map and remove Parser.allowedJumps
        jmpString = case jmp of "" -> "000"
                                "JGT" -> "001"
                                "JEQ" -> "010"
                                "JGE" -> "011"
                                "JLT" -> "100"
                                "JNE" -> "101"
                                "JLE" -> "110"
                                "JMP" -> "111"
        in "111" ++ compString ++ dstString ++ jmpString
assembleLine _ = ""

assembleLines :: [Instruction Value] -> [String]
assembleLines =
    let notLsOrEmpty (LS s) = False
        notLsOrEmpty EmptyLine = False
        notLsOrEmpty _ = True
    in map assembleLine . filter notLsOrEmpty

assembleProgram :: [Instruction Value] -> String
assembleProgram = concat . List.intersperse "\n" . assembleLines
