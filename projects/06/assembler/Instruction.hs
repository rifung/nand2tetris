module Instruction
( Instruction(..)
, Symbol(getSymbol)
, Value(getValue)
, ValueOrSymbol
, symbol
, value
) where

import Data.Char (isDigit)
import Data.Either
import Data.List as List
import Data.Maybe

-- You should 
newtype Symbol = Symbol { getSymbol::String } deriving (Eq, Ord, Show)
newtype Value = Value { getValue::String } deriving (Eq, Show)
type ValueOrSymbol = Either Value Symbol

data Instruction a = AS a -- should be either Value or Variable
                   | CS { getDst::String
                        , getComp::String
                        , getJmp::String }
                   | LS Symbol
                   | EmptyLine deriving (Eq, Show)

allowedSymbols = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_', '.', '$', ':']
maxInt = "65535"

maybeError b s = if b then Nothing else Just s

symbol :: String -> Either String Symbol
symbol s =
    let symbolIsOk =
            maybeError (length s > 0 && not (isDigit $ head s) && 
                        (List.null $ List.filter (\c -> not $
                                                        elem c allowedSymbols)
                                                 s))
                       $ "Invalid symbol: " ++ s
    in if isNothing symbolIsOk then Right $ Symbol s
       else Left $ fromJust symbolIsOk

value :: String -> Either String Value
value s =
    let areDigits = maybeError (all isDigit s) $ "Invalid value: " ++ s
        canFit = maybeError (length s < length maxInt ||
                             ((length s == length maxInt) && (s < maxInt)))
                          $ "Value too large: " ++ s
        tests = [areDigits, canFit]
    in if all isNothing tests then Right $ Value s
       else Left $ fromJust $ head $ filter isJust tests
