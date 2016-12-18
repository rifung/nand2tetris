module Translator.Translator
( translate
, translateFile
, translateLine
, translateLines
) where

import Data.List (intersperse)
import Data.Map as M
import System.FilePath
import System.IO
import Translator.Commands
import Translator.Parser

segMap :: Map Segment String
segMap = M.fromList [ (Argument, "ARG")
                    , (Local, "LCL")
                    , (This, "THIS")
                    , (That, "THAT")
                    , (Pointer, "3")
                    , (Temp, "5")]

segValues = [Pointer, Temp]

pop :: Char -> [String]
pop c = [ "@SP" -- decrement stack pointer
        , "M=M-1" 
        , "A=M" -- set c to be last element
        , c:"=M"]

popToA :: [String]
popToA = pop 'A'

popToD :: [String]
popToD = pop 'D'

pushD :: [String]
pushD = [ "@SP" -- set next element to D
        , "A=M"
        , "M=D"
        , "@SP" -- increment stack pointer
        , "M=M+1"]

translate :: String -> String -> Either String String
translate fileName s =
    parseString s >>=
        (\cs -> pure $ concat $ intersperse "\n" $ translateLines fileName cs)

translateFile :: FilePath -> IO (Either String String)
translateFile path =
    withFile path ReadMode $ \h -> do
        contents <- hGetContents h
        return $! translate (takeFileName path) contents

translateLine :: String -> Command -> Int -> ([String], Int)
translateLine fileName c labelCount = case c of
    Add -> (twoOps ["D=D+A"], labelCount)
    Sub -> (twoOps ["D=A-D"], labelCount)
    Neg -> oneOp ["D=!D", "D=D+1"]
    Eq -> let doneLabel = "__EQ_" ++ (show labelCount) ++ "__"
          in (twoOps [ "D=D-A"
                     , '@':doneLabel
                     , "D;JEQ"
                     , "D=1"
                     , makeLabel doneLabel
                     , "D=D-1" ], labelCount+1)
    Gt -> let doneLabel = "__GT_DONE_" ++ (show labelCount) ++ "__"
              gtLabel = "__GT_TRUE_" ++ (show labelCount) ++ "__"
          in (twoOps [ "D=A-D"
                     , '@':gtLabel
                     , "D;JGT"
                     , "D=1"
                     , '@':doneLabel
                     , "0;JMP"
                     , makeLabel gtLabel
                     , "D=0"
                     , makeLabel doneLabel
                     , "D=D-1"], labelCount+1)
    Lt -> let doneLabel = "__LT_DONE_" ++ (show labelCount) ++ "__"
              ltLabel = "__LT_TRUE_" ++ (show labelCount) ++ "__"
          in (twoOps [ "D=A-D"
                     , '@':ltLabel
                     , "D;JLT"
                     , "D=1"
                     , '@':doneLabel
                     , "0;JMP"
                     , makeLabel ltLabel
                     , "D=0"
                     , makeLabel doneLabel
                     , "D=D-1"], labelCount+1)
    And -> (twoOps ["D=D&A"], labelCount)
    Or -> (twoOps ["D=D|A"], labelCount)
    Not -> oneOp ["D=!D"]
    Push seg ind -> (
        (case seg of
            Static -> [ '@':fileName ++ "." ++ (show ind) , "D=M" ]
            Constant -> ['@':(show ind), "D=A"]
            seg ->
                let f = if seg `elem` segValues
                    then setDByValue
                    else setDByPointer
                in f $ segMap ! seg
        ) ++ pushD, labelCount)
        where setDByPointer :: String -> [String]
              setDByPointer s = [ '@':(show ind)
                                , "D=A"
                                , '@':s
                                , "A=M"
                                , "A=A+D"
                                , "D=M"]
              setDByValue :: String -> [String]
              setDByValue n = [ '@':(show ind)
                            , "D=A"
                            , '@':n
                            , "A=D+A"
                            , "D=M"]
    Pop seg ind ->
        ((case seg of
            Static -> popToD ++ ['@':fileName ++ ('.':(show ind)), "M=D"]
            Constant -> popToD
            seg ->
                let f = if seg `elem` segValues
                    then popSegByValue
                    else popSegByPointer
                in f $ segMap ! seg
        ), labelCount)
        where popSegByPointer :: String -> [String]
              popSegByPointer s = [ '@':(show ind) -- increment s pointer by ind
                                  , "D=A"
                                  , '@':s
                                  , "M=M+D"] ++
                                  popToD ++ -- set D to last element on stack
                                  [ '@':s -- set *s to D
                                  , "A=M"
                                  , "M=D"
                                  , '@':(show ind) -- decrement s pointer by ind
                                  , "D=A"
                                  , '@':s
                                  , "M=M-D"]
              popSegByValue :: String -> [String]
              popSegByValue n = popToD ++ [ '@':(show $ (read n :: Int) + ind)
                                          , "M=D"]
    where makeLabel :: String -> String
          makeLabel s = '(':s ++ ")"
          oneOp :: [String] -> ([String], Int)
          oneOp cs = (popToD ++ cs ++ pushD, labelCount)
          twoOps :: [String] -> [String]
          twoOps cs = popToD ++ popToA ++ cs ++ pushD

translateLines :: String -> [Command] -> [String]
translateLines fileName cs =
    let f :: ([String], Int) -> Command -> ([String], Int)
        f (prev,i) c = let (s, i') = translateLine fileName c i
                    in (prev++s, i')
    in fst $ Prelude.foldl f ([], 0) $ Prelude.filter (/= Comment) cs
