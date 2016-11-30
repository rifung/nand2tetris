{-# OPTIONS_GHC -XBangPatterns #-}
module SymbolTable
( generateSymbolTable
, generateSymbolTableIns
, initialTable
, replaceSymbol
, replaceSymbols
) where

import Data.Either
import Data.List as List
import Data.Map as Map
import Instruction
import Debug.Trace

type SymbolTable = Map.Map Symbol Value

{- first pass: go through program counting instruction number without labels
 -             and insert all labels into SymbolTable.
 - second pass: go through program again and replace all symbols with the
 -              following strategy: if this symbol is in the SymbolTable then
 -              replace it with the value in the SymbolTable. Otherwise add a
 -              new entry into the SymbolTable and increment the count of our
 -              next memory address
 -}

{- first pass just look for Label Instructions and add them to the map -}
generateSymbolTable :: [Instruction ValueOrSymbol]
                    -> Either String SymbolTable
generateSymbolTable instructions =
    let f ins (table, line) = generateSymbolTableIns ins table line
        e = List.foldl (\e ins -> e >>= (f ins))
                       (Right (initialTable, 0))
                       instructions
    in e >>= (Right . fst)

{- generate symbol table for a single instruction/line -}
generateSymbolTableIns :: Instruction ValueOrSymbol
                       -> SymbolTable
                       -> Int
                       -> Either String (SymbolTable, Int)
generateSymbolTableIns (LS symbol') !table !line = case value $ show line of
    Right v -> Right (Map.insert symbol' v table, line)
    Left e -> Left e
generateSymbolTableIns EmptyLine !table !line = Right (table, line)
generateSymbolTableIns _ !table !line = Right (table, line+1)

initialTable :: SymbolTable
initialTable =
    let v v' = head $ rights [value $ show v']
        s s' = head $ rights [symbol s']
    in Map.fromList [ (s "SP", v 0)
                    , (s "LCL", v 1)
                    , (s "ARG", v 2)
                    , (s "THIS", v 3)
                    , (s "THAT", v 4)
                    , (s "R0", v 0)
                    , (s "R1", v 1)
                    , (s "R2", v 2)
                    , (s "R3", v 3)
                    , (s "R4", v 4)
                    , (s "R5", v 5)
                    , (s "R6", v 6)
                    , (s "R7", v 7)
                    , (s "R8", v 8)
                    , (s "R9", v 9)
                    , (s "R10", v 10)
                    , (s "R11", v 11)
                    , (s "R12", v 12)
                    , (s "R13", v 13)
                    , (s "R14", v 14)
                    , (s "R15", v 15)
                    , (s "SCREEN", v 16384)
                    , (s "KBD", v 24576)]

replaceSymbol :: Instruction ValueOrSymbol
                 -> SymbolTable
                 -> Int
                 -> Either String (Instruction Value, SymbolTable, Int)
replaceSymbol (AS (Left v)) table count = Right (AS v, table, count)
replaceSymbol (AS (Right symbol')) table count
    | Map.member symbol' table = Right (AS $ table Map.! symbol', table, count)
    | otherwise = case value $ show count of
        Left e -> Left e
        Right v -> Right (AS v, Map.insert symbol' v table, count+1)
replaceSymbol (CS a c j) table count = Right (CS a c j, table, count)
replaceSymbol (LS symbol') table count = Right (LS symbol', table, count)
replaceSymbol EmptyLine table count = Right (EmptyLine, table, count)

replaceSymbols :: SymbolTable
               -> [Instruction ValueOrSymbol]
               -> Either String [Instruction Value]
replaceSymbols table instructions =
    let f :: Instruction ValueOrSymbol
          -> ([Instruction Value], SymbolTable, Int)
          -> Either String ([Instruction Value], SymbolTable, Int)
        f i (prevInstructions, table, count) =
            let g :: (Instruction Value, SymbolTable, Int)
                  -> ([Instruction Value], SymbolTable, Int)
                g (ins, table, count) = (ins:prevInstructions, table, count)
            in (replaceSymbol i table count) >>= (pure . g)
        -- Note instructions are reversed here but we can't use foldr
        -- because of count
        maybeAnswer = List.foldl (\e i -> e >>= (f i))
                                 (Right ([], table, 16)) instructions
        getInstructions (e,_,_) = reverse e
    in maybeAnswer >>= (pure . getInstructions)
