module Lexer (tokenize, Token (..), TokenType (..) ) where

import Data.List
import Data.Char (ord)

import Debug.Trace;

data TokenType = Quotes | Dot | Comma | Colon | EndStatement | Numeric | Literal | Assignment | OpenParen | CloseParen | OpenCurved | CloseCurved | OpenSquared | CloseSquared | Arithmetic | Comparison | Bitwise | Empty | SoftComp | CompositeAssign | VarKeyword deriving (Show, Eq)
data Token = Token {value :: [Char], token_type :: TokenType} deriving (Show)

-- makes token from single char
parseSingleToken :: Char -> Token
parseSingleToken c
    | c == '(' = Token [c] OpenParen
    | c == ')' = Token [c] CloseParen
    | c == '{' = Token [c] OpenCurved
    | c == '}' = Token [c] CloseCurved
    | c == '[' = Token [c] OpenSquared
    | c == ']' = Token [c] CloseSquared
    | c == '+' || c == '-' || c == '*' || c == '/' || c == '%' = Token [c] Arithmetic
    | c == '<' || c == '>' = Token [c] Comparison
    | c == '&' || c == '|' || c == '^' = Token [c] Bitwise
    | c == '=' = Token [c] Assignment
    | c == ';' = Token [c] EndStatement
    | c == ':' = Token [c] Colon
    | c == '.' = Token [c] Dot
    | c == '"' = Token [c] Quotes
    | c == ',' = Token [c] Comma
    | elem c ['0'..'9'] = Token [c] Numeric
    | elem c $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] = Token [c] Literal
    | otherwise = Token " " Empty

-- makes tokens from every char of code.
makeTokenFromEveryChar :: [Char] -> [Token]
makeTokenFromEveryChar code = map (\c -> parseSingleToken c) code

-- entry point, which should be called to start lexer.
tokenize :: [Char] -> [Token]
tokenize sourceCode = keyworder (excludeEmpty (checkFor (makeTokenFromEveryChar sourceCode)))

keyworder :: [Token] -> [Token]
keyworder t
    | tv == "var" = (Token tv VarKeyword):(keyworderG tt)
    | otherwise = th:(keyworderG tt)
    where th = head t
          tt = tail t
          tv = value th

keyworderG :: [Token] -> [Token]
keyworderG t = if length t > 0 then keyworder t else []

excludeEmpty :: [Token] -> [Token]
excludeEmpty t = filter (\c -> (token_type c) /= Empty) t

-- another helper method, which makes an integer array with the size of Tokens array
getByMod :: [Token] -> Int -> [Int]
getByMod t n = filter (\c -> c `mod` 2 == n) [0..((length t)-1)]

-- helper method, which extracts only odd/even tokens from list (needed for reducer)
getTokensByMod :: [Token] -> Int -> [Token]
getTokensByMod t n = map (\c -> t !! c ) (getByMod t n)

reducerGuard :: [Token] -> Int -> [Token]
--reducerGuard t i = if (length t) <= (trace ("Lol: " ++ (show (length t)) ++ " <= " ++ (show (i+1))) (i+1)) then [] else (reducerItself t i)
reducerGuard t i
    | l <= i = []
    | l <= succ i = (last t):[]
    | otherwise = reducerItself t i
    where l = length t

-- reducer itself
reducerItself :: [Token] -> Int -> [Token]
reducerItself t i
    | h == Literal && ( g == Literal || g == Numeric ) = (Token ((value e)++(value o)) Literal):(reducerGuard t (i+2))
    | h == Numeric && (g == Numeric || g == Dot) = (Token ((value e)++(value o)) Numeric):(reducerGuard t (i+2))
    | h == Comparison && g == Assignment = (Token ((value e)++(value o)) SoftComp):(reducerGuard t (i+2))
    | (h == Arithmetic || h == Bitwise) && g == Assignment = (Token ((value e)++(value o)) CompositeAssign):(reducerGuard t (i+2))
    | otherwise = e:(reducerGuard t (succ i))
    where e = t !! i
          o = t !! (succ i)
          h = token_type e
          g = token_type o

-- method, which used for reducing token amout (actually to specify some tokens e.g. ! = -> !=)
reduceTokens :: [Token] -> [Token]
reduceTokens t = reducerItself t 0

hasGuard :: [Token] -> Int -> Bool
hasGuard t i = if length t <= (succ i) then False else hasItself t i

-- method that help checks
hasItself :: [Token] -> Int -> Bool
hasItself t i
    | h == Literal && ( g == Literal || g == Numeric ) = True
    | h == Numeric && ( g == Numeric || g == Dot ) = True
    | h == Comparison && g == Assignment = True
    | (h == Arithmetic || h == Bitwise) && g == Assignment = True
    | otherwise = hasGuard t (succ i)
    where e = t !! i
          o = t !! (succ i)
          h = token_type e
          g = token_type o

-- method that checks if there are equal Literals and Numerics
checkFor :: [Token] -> [Token]
checkFor t = if hasItself t 0 then checkFor (reduceTokens t) else t