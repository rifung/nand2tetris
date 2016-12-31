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
import Data.Maybe
import Data.Set as S
import Translator.Commands


labelValidChars :: S.Set Char
labelValidChars = S.fromList $
    ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '.', ':']

segmentMap :: M.Map String Segment
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

parseLine :: String -> Maybe String -> Either String (Command, Maybe String)
parseLine line mf =
    let removeComments :: String -> String
        removeComments [] = []
        removeComments ('/':'/':rest) = []
        removeComments (c:cs) = c:(removeComments cs)
        ws = words $ removeComments line
        command:rest = ws
        labelIsValid :: String -> Either String String
        labelIsValid s =
            if length s > 0 && (not $ isDigit $ s !! 0) &&
               all ((flip S.member) labelValidChars) s then return s
            else Left "Invalid label"
        pushOrPop :: (Segment -> Int -> Command)
                  -> Either String (Command, Maybe String)
        pushOrPop c = do requireArgs 2 c
                         seg <- parseSegment (rest !! 0)
                         index <- parseIndex (rest !! 1) seg
                         return $ (c seg index, mf)
        requireArgs :: Int -> a -> Either String a
        requireArgs n c =
            if length rest == n then Right c
            else Left $ "Expected " ++ (show n) ++ " args but found " ++
                 (show $ length rest)
        requireLabel :: Int -> (String -> Command) -> Either String Command
        requireLabel n c = do requireArgs n ()
                              l <- labelIsValid $ rest !! 0
                              return $ c l
    in if L.null ws || isPrefixOf "//" command then Right (Comment, mf)
       else case command of "add" -> requireArgs 0 (Add, mf)
                            "sub" -> requireArgs 0 (Sub, mf)
                            "neg" -> requireArgs 0 (Neg, mf)
                            "eq" -> requireArgs 0 (Eq, mf)
                            "gt" -> requireArgs 0 (Gt, mf)
                            "lt" -> requireArgs 0 (Lt, mf)
                            "and" -> requireArgs 0 (And, mf)
                            "or" -> requireArgs 0 (Or, mf)
                            "not" -> requireArgs 0 (Not, mf)
                            "push" -> pushOrPop Push
                            "pop" -> pushOrPop Pop
                            "label" -> 
                                do l <- requireLabel 1 Label
                                   return (l, mf)
                            "goto" -> do g <- requireLabel 1 Goto
                                         return (g, mf)
                            "if-goto" -> do g <- requireLabel 1 IfGoto
                                            return (g, mf)
                            "function" ->
                                let name = rest !! 1
                                in do f <- requireLabel 2 $ (flip Function) 
                                         $ read name
                                      return (f, Just name)
                            "call" -> 
                                do c <- requireLabel 2 $ flip Call $ read
                                        $ rest !! 1
                                   return (c, mf)
                            "return" -> requireArgs 0 (Return, mf)

parseLines :: [String] -> Either String [Command]
parseLines lines =
    let f :: String -> ([Command], Maybe String) -> Either String ([Command], Maybe String)
        f line (commands, mf) = (parseLine line mf) >>= (\(c, mf') -> Right (commands ++ [c], mf'))
    in fst <$> L.foldl (\e l -> e >>= (f l)) (Right ([], Nothing)) lines

parseSegment :: String -> Either String Segment
parseSegment s =
    if M.member s segmentMap then Right $ segmentMap ! s
    else Left $ "Invalid segment: " ++ s

parseString :: String -> Either String [Command]
parseString = parseLines . lines
