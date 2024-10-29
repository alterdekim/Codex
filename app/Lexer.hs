module Lexer (tokenize, Token (..), TokenType (..) ) where

import Data.List
import Data.Char (ord)

data TokenType = Quotes | Dot | Comma | Colon | EndStatement | EOF | QString | Literal | Number | Letter | Digit | Assignment | OpenParen | CloseParen | OpenCurved | CloseCurved | OpenSquared | CloseSquared | Arithmetic | Comparison | Bitwise | Empty deriving (Show, Eq)
data Token = Token {value :: [Char], token_type :: TokenType} deriving (Show)

parseSingleChar :: Char -> Token
parseSingleChar c
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
    | c == '\'' = Token [c] Quotes
    | c == ',' = Token [c] Comma
    | elem c ['0'..'9'] = Token [c] Digit
    | elem c $ ['a'..'z'] ++ ['A'..'Z'] = Token [c] Letter
    | otherwise = Token " " Empty

tokenize :: [Char] -> [Token]
tokenize sourceCode = filter (\t -> token_type t /= Empty && token_type t /= EOF) (reduceTokens ((map parseSingleChar sourceCode) ++ [Token " " EOF]) 0)

conTokType :: TokenType -> TokenType
conTokType tt
    | tt == Digit = Number
    | otherwise = Literal

reduceTokens :: [Token] -> Int -> [Token]
reduceTokens all_tokens i
    | token_type ct == Digit || token_type ct == Letter = (Token (makeIdentifier (drop i all_tokens) 0) (conTokType $ token_type ct)):(_reduceTokens all_tokens (findFirstEmpty all_tokens i))
    | token_type ct == Quotes = (Token (_makeQuoteString (drop (succ i) all_tokens) 0) QString):(_reduceTokens all_tokens (findFirstQuotes all_tokens (succ i)))
    | otherwise = ct:(_reduceTokens all_tokens (succ i))
    where ct = all_tokens !! i

_reduceTokens :: [Token] -> Int -> [Token]
_reduceTokens all_tokens i
    | i >= length all_tokens = [Token " " EOF]
    | otherwise = reduceTokens all_tokens i

_makeQuoteString :: [Token] -> Int -> [Char]
_makeQuoteString t i
    | i >= length t = ""
    | otherwise = makeQuoteString t i

makeQuoteString :: [Token] -> Int -> [Char]
makeQuoteString t i
    | tt == Quotes = ""
    | otherwise = ((value $ t !! i) !! 0):(_makeQuoteString t (succ i))
    where tt = token_type $ t !! i

makeIdentifier :: [Token] -> Int -> [Char]
makeIdentifier t i
    | i >= length t || (tt /= Letter && tt /= Digit) = ""
    | otherwise = ((value $ t !! i) !! 0):(makeIdentifier t (succ i))
    where tt = token_type $ t !! i

findFirstEmpty :: [Token] -> Int -> Int
findFirstEmpty t i
    | i >= length t || (tt /= Letter && tt /= Digit) = i
    | otherwise = findFirstEmpty t $ succ i
    where tt = token_type $ t !! i

findFirstQuotes :: [Token] -> Int -> Int
findFirstQuotes t i
    | i >= length t || tt == Quotes = succ i
    | otherwise = findFirstQuotes t $ succ i
    where tt = token_type $ t !! i