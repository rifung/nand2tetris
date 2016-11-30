module Parser
( parseLine
, parseLines
, parseString
) where

import Data.Char (intToDigit, isDigit, isSpace)
import Data.Either
import Data.List as List
import Data.Maybe
import Data.Set as Set
import Instruction
import Numeric (showIntAtBase)

allowedCommands = Set.fromList $ [ "0", "1", "-1", "D", "A", "M", "!D", "!A"
                                 , "!M", "-D", "-A", "-M", "D+1", "A+1", "M+1"
                                 , "D-1", "A-1", "M-1", "D+A", "D+M", "D-A"
                                 , "D-M", "A-D", "M-D", "D&A", "D&M", "D|A"
                                 , "D|M"]
allowedJumps = Set.fromList $ ["JGT", "JEQ", "JGE", "JLT", "JNE", "JLE", "JMP"]
invalidSymbolString = "Invalid char(s) in symbol"

parseLine :: String -> Either String (Instruction ValueOrSymbol)
parseLine line =
    let cleanedLine = let cleanLine ('/':'/':rest) = []
                          cleanLine (c:cs) = c:(cleanLine cs)
                          cleanLine [] = []
                          stripLine s = List.dropWhileEnd isSpace $
                                        List.dropWhile isSpace s
                      in stripLine $ cleanLine line
        parseInstruction [] = Right EmptyLine
        parseInstruction l@('@':rest) = parseA l
        parseInstruction l@('(':rest) = parseL l
        parseInstruction l@('/':'/':_) = Right EmptyLine
        parseInstruction l = parseC l
    in parseInstruction cleanedLine

parseLines :: [String] -> Either String [Instruction ValueOrSymbol]
parseLines lines =
    let f :: String
          -> [Instruction ValueOrSymbol]
          -> Either String [Instruction ValueOrSymbol]
        f l is = (parseLine l) >>= (\i -> Right $ i:is)
    in List.foldr (\l e -> e >>= (f l)) (Right []) lines

parseString :: String -> Either String [Instruction ValueOrSymbol]
parseString = parseLines . lines

parseA :: String -> Either String (Instruction ValueOrSymbol)
parseA "@" = Left "Expected value or variable"
parseA ('@':valueOrSymbol) =
    if isDigit $ head valueOrSymbol then case value valueOrSymbol of
        Left e -> Left e
        Right v -> Right $ AS $ Left v
    else case symbol valueOrSymbol of
        Left e -> Left e
        Right s -> Right $ AS $ Right s

parseC :: String -> Either String (Instruction ValueOrSymbol)
parseC line =
    let parseAssignment s =
            let helper [] _ = Right []
                helper s [] = Left $ "Syntax error with assignment: " ++ s
                helper (c:cs) (o:os) = if c == o then fmap (c:) $ helper cs os
                                       else helper (c:cs) os
            in helper s "AMD"
        parseCommand s = if Set.member s allowedCommands then Right s
                         else Left $ "Syntax error with command: " ++ s
        parseJump s = if Set.member s allowedJumps then Right s
                      else Left $ "Syntax error with jump: " ++ s
        assignmentPos = elemIndex '=' line
        assignment = fromMaybe (Right "")
                   $ fmap (\n -> parseAssignment $ take n line) assignmentPos
        afterAssignment = drop (fromMaybe 0 $ fmap (+1) assignmentPos) line
        jumpPos = elemIndex ';' afterAssignment
        jump = fromMaybe (Right "")
             $ fmap (\n -> parseJump $ drop (n+1) afterAssignment) jumpPos
        command = parseCommand
                $ take (fromMaybe (length afterAssignment) jumpPos)
                       afterAssignment
        eithersAcj = [assignment, command, jump]
    in if all isRight eithersAcj then
           let [a,c,j] = rights eithersAcj
           in Right $ CS a c j
       else Left $ head $ lefts eithersAcj

parseL :: String -> Either String (Instruction ValueOrSymbol)
parseL ('(':rest) =
    let (s, last) = List.splitAt (length rest - 1) rest
    in if last == ")" then fmap LS $ symbol s
       else Left "Expected ')'"
