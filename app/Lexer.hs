module Lexer (tokenize, Token (..), TokenType (..) ) where

import Data.List
import Data.Char (ord)

data TokenType = Quotes | Dot | Comma | Colon | EndStatement | EOF | QString | Numeric | Literal | Digit | Assignment | OpenParen | CloseParen | OpenCurved | CloseCurved | OpenSquared | CloseSquared | Arithmetic | Comparison | Bitwise | Empty deriving (Show, Eq)
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
tokenize sourceCode = excludeEmpty (checkFor (reduceTokens $ makeTokenFromEveryChar sourceCode))

excludeEmpty :: [Token] -> [Token]
excludeEmpty t = filter (\c -> (token_type c) /= Empty) t

-- another helper method, which makes an integer array with the size of Tokens array
getByMod :: [Token] -> Int -> [Int]
getByMod t n = filter (\c -> c `mod` 2 == n) [0..((length t)-1)]

-- helper method, which extracts only odd/even tokens from list (needed for reducer)
getTokensByMod :: [Token] -> Int -> [Token]
getTokensByMod t n = map (\c -> t !! c ) (getByMod t n)

reducerGuard :: [Token] -> [Token] -> [Token]
reducerGuard et ot = if length et == 0 || length ot == 0 then et++ot else reducerItself et ot

-- reducer itself
reducerItself :: [Token] -> [Token] -> [Token]
reducerItself et ot
    | h == Literal && ( g == Literal || g == Numeric ) = (Token ((value e)++(value o)) Literal):(reducerGuard (tail et) (tail ot))
    | otherwise = [e,o]++(reducerGuard (tail et) (tail ot))
    where e = head et
          o = head ot
          h = token_type e
          g = token_type o

-- method, which used for reducing token amout (actually to specify some tokens e.g. ! = -> !=)
reduceTokens :: [Token] -> [Token]
reduceTokens t = reducerItself (getTokensByMod t 0) (getTokensByMod t 1)

hasGuard :: [Token] -> [Token] -> Bool
hasGuard et ot = if length et == 0 || length ot == 0 then False else hasItself et ot

-- method that help checks
hasItself :: [Token] -> [Token] -> Bool
hasItself et ot
    | h == Literal && ( g == Literal || g == Numeric ) = True
    | otherwise = hasGuard (tail et) (tail ot)
    where e = head et
          o = head ot
          h = token_type e
          g = token_type o

-- method that checks if there are equal Literals and Numerics
checkFor :: [Token] -> [Token]
checkFor t = if hasItself (getTokensByMod t 0) (getTokensByMod t 1) then checkFor (reduceTokens t) else t