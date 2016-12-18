module Translator.Commands
( Command(..)
, Segment(..)
) where

data Command = Add | Sub | Neg | Eq | Gt | Lt | And | Or | Not
             | Push Segment Int
             | Pop Segment Int
             | Comment deriving (Eq, Show)

data Segment = Argument | Local | Static | Constant | This | That | Pointer
             | Temp deriving (Eq, Ord, Show)
