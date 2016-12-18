module Translator.Parser
( parseLine
, parseLines
, parseSegment
, parseString
) where

import Data.Char (isDigit)
import Data.Either
import Data.List as L
import Data.Map as M
import Translator.Commands

segmentMap = M.fromList [ ("argument", Argument)
                        , ("local", Local)
                        , ("static", Static)
                        , ("constant", Constant)
                        , ("this", This)
                        , ("that", That)
                        , ("pointer", Pointer)
                        , ("temp", Temp)]

parseIndex :: String -> Segment -> Either String Int
parseIndex s segment =
    let fail = Left $ "Invalid index " ++ s ++ " for segment " ++ (show segment)
    in if all isDigit s then
          let n = read s :: Int -- exception can be thrown here =[
              test = case segment of Pointer -> n <= 2
                                     Temp -> n <= 7
                                     otherwise -> True -- should we check others?
          in if test then Right n else fail
    else fail

parseLine :: String -> Either String Command
parseLine line =
    let ws = words line
        command:rest = ws
        pushOrPop :: (Segment -> Int -> Command) -> Either String Command
        pushOrPop c = do seg <- parseSegment (rest !! 0)
                         index <- parseIndex (rest !! 1) seg
                         requireArgs 2 $ c seg index
        requireArgs :: Int -> Command -> Either String Command
        requireArgs n c =
            if length rest == n then Right c
            else Left $ "Expected " ++ (show n) ++ " args but found " ++
                 (show $ length rest)
    in if L.null ws || isPrefixOf "//" command then Right Comment
       else case command of "add" -> requireArgs 0 Add
                            "sub" -> requireArgs 0 Sub
                            "neg" -> requireArgs 0 Neg
                            "eq" -> requireArgs 0 Eq
                            "gt" -> requireArgs 0 Gt
                            "lt" -> requireArgs 0 Lt
                            "and" -> requireArgs 0 And
                            "or" -> requireArgs 0 Or
                            "not" -> requireArgs 0 Not
                            "push" -> pushOrPop Push
                            "pop" -> pushOrPop Pop

parseLines :: [String] -> Either String [Command]
parseLines lines =
    let f :: String -> [Command] -> Either String [Command]
        f line commands = (parseLine line) >>= (\c -> Right $ c:commands)
    in L.foldr (\l e -> e >>= (f l)) (Right []) lines

parseSegment :: String -> Either String Segment
parseSegment s =
    if member s segmentMap then Right $ segmentMap ! s
    else Left $ "Invalid segment: " ++ s

parseString :: String -> Either String [Command]
parseString = parseLines . lines
