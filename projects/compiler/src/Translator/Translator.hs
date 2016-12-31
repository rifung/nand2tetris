module Translator.Translator
( addHeader
, translate
, translateFile
, translateFiles
, translateLine
, translateLines
, translateProgram
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

-- Note that this assumes we can use any labelCount for translateLine
header :: String
header = let (syscall, _, _) = translateLine "" (Call "Sys.init" 0) 0 ""
             setSp = [ "@256" -- SP = 256
                     , "D=A"
                     , "@SP"
                     , "M=D"]
         in concat $ intersperse "\n" $ setSp ++ syscall

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

addHeader :: String -> String
addHeader s = header ++ "\n" ++ s

translate :: String -> String -> Either String String
translate fileName s =
    let translateString :: [Command] -> Either String String
        translateString cs = pure $ concat $ intersperse "\n"
                           $ translateLines fileName cs
    in parseString s >>= translateString

translateFile :: FilePath -> IO (Either String String)
translateFile path =
    withFile path ReadMode $ \h -> do
        contents <- hGetContents h
        return $! translate (takeFileName path) contents

translateFiles :: [FilePath] -> IO (Either String String)
translateFiles [] = return (Right "")
translateFiles (path:rest) = do
    f0 <- translateFile path
    -- probably not the best way to do this..
    case f0 of
        Left e -> return f0
        Right s -> do next <- translateFiles rest
                      case next of Left e -> return next
                                   Right r -> return $ (s ++) <$> ('\n':)
                                          <$> next

translateLine :: String -> Command -> Int -> String
             -> ([String], Int, String)
translateLine fileName c labelCount function = case c of
    Add -> (twoOps ["D=D+A"], labelCount, function)
    Sub -> (twoOps ["D=A-D"], labelCount, function)
    Neg -> oneOp ["D=!D", "D=D+1"]
    Eq -> let doneLabel = "__EQ_" ++ (show labelCount) ++ "__"
          in (twoOps [ "D=D-A"
                     , '@':doneLabel
                     , "D;JEQ"
                     , "D=1"
                     , makeLabel doneLabel
                     , "D=D-1" ], labelCount+1, function)
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
                     , "D=D-1"], labelCount+1, function)
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
                     , "D=D-1"], labelCount+1, function)
    And -> (twoOps ["D=D&A"], labelCount, function)
    Or -> (twoOps ["D=D|A"], labelCount, function)
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
        ) ++ pushD, labelCount, function)
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
        ), labelCount, function)
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
    Label l -> ([makeLabel $ namespaceLabel l], labelCount, function)
    Goto l -> (['@':(namespaceLabel l), "0;JMP"], labelCount, function)
    IfGoto l -> ( popToD ++ ['@':(namespaceLabel l), "D;JNE"]
                , labelCount
                , function)
    Function f i -> ( [ makeLabel f, "D=0"] ++ (concat $ replicate i pushD)
                    , labelCount
                    , f)
    Call f i -> let retLabel = "__RET_" ++ (show labelCount) ++ "__"
                    pushMemLabel l = ['@':l, "D=M"] ++ pushD
                in ( ['@':retLabel, "D=A"]
                  ++ pushD
                  ++ (pushMemLabel "LCL")
                  ++ (pushMemLabel "ARG")
                  ++ (pushMemLabel "THIS")
                  ++ (pushMemLabel "THAT")
                  ++ [ "@SP" -- ARG=SP-n-5
                     , "D=M"
                     , "@5"
                     , "D=D-A"
                     , '@':(show i)
                     , "D=D-A"
                     , "@ARG"
                     , "M=D"
                     , "@SP" -- LCL = SP
                     , "D=M"
                     , "@LCL"
                     , "M=D"
                     , '@':f -- goto f
                     , "0;JMP"
                     , makeLabel retLabel], labelCount+1, function)
    Return -> let frameReg = "13"
                  retReg = "14"
                  restore :: String -> Int -> [String]
                  restore reg ind = [ '@':frameReg -- set D = *(FRAME-i)
                                    , "D=M"
                                    , '@':(show ind)
                                    , "D=D-A"
                                    , "A=D"
                                    , "D=M"
                                    , '@':reg -- set reg = *(FRAME-i)
                                    , "M=D"]
                  assembly =  [ "@LCL" -- save FRAME
                              , "D=M"
                              , '@':frameReg
                              , "M=D"
                              , "@5" -- RET = *(FRAME-5)
                              , "A=D-A"
                              , "D=M"
                              , '@':retReg
                              , "M=D"]
                           ++ popToD
                           ++ [ "@ARG" -- *ARG = pop()
                              , "A=M"
                              , "M=D"
                              , "@ARG" -- SP = ARG+1
                              , "D=M+1"
                              , "@SP"
                              , "M=D"]
                           ++ (restore "THAT" 1)
                           ++ (restore "THIS" 2)
                           ++ (restore "ARG" 3)
                           ++ (restore "LCL" 4)
                           ++ ['@':retReg, "A=M", "0;JMP"]
              in (assembly, labelCount, function)
              -- if we use labelCount, we need to change header
    where makeLabel :: String -> String
          makeLabel s = '(':s ++ ")"
          namespaceLabel :: String -> String
          namespaceLabel l = function ++ "$" ++ l
          oneOp :: [String] -> ([String], Int, String)
          oneOp cs = (popToD ++ cs ++ pushD, labelCount, function)
          twoOps :: [String] -> [String]
          twoOps cs = popToD ++ popToA ++ cs ++ pushD

translateLines :: String -> [Command] -> [String]
translateLines fileName cs =
    let f :: ([String], Int, String) -> Command -> ([String], Int, String)
        f (prev,i, f) c = let (s, i', f') = translateLine fileName c i f
                    in (prev++s, i', f')
        (answer, _, _) = Prelude.foldl f ([], 0, "") $
                         Prelude.filter (/= Comment) cs
    in answer

translateProgram :: [FilePath] -> IO (Either String String)
translateProgram paths = do
    asm <- translateFiles paths
    return $ addHeader <$> asm
