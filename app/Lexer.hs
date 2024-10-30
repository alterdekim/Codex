module Lexer (tokenize, Token (..), TokenType (..), TrailType (..), TrailRule (..) ) where

import Data.List
import Data.Char (ord)

data TrailType = OnlyDigits | LettersOrDigits | OnlySpecial deriving (Show, Eq)
data TokenType = Quotes | Dot | Comma | Colon | EndStatement | EOF | QString | Numeric | Literal | Digit | Assignment | OpenParen | CloseParen | OpenCurved | CloseCurved | OpenSquared | CloseSquared | Arithmetic | Comparison | Bitwise | Empty deriving (Show, Eq)
data Token = Token {value :: [Char], token_type :: TokenType} deriving (Show)
data TrailRule = TrailRule {rule_val :: [Char], trail_type :: TrailType} deriving (Show)

parseSingleToken :: [Char] -> Token
parseSingleToken code
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
    | elem c ['0'..'9'] = Token code Numeric
    | elem c $ ['a'..'z'] ++ ['A'..'Z'] = Token code Literal
    | otherwise = Token " " Empty
    where c = head code

-- shall return [Token]
tokenize :: [Char] -> [Token]
tokenize sourceCode
    | elem f ['0'..'9'] = parseRule t $ TrailRule [f] OnlyDigits
    | elem f $ ['a'..'z'] ++ ['A'..'Z'] = parseRule t $ TrailRule [f] LettersOrDigits
    | otherwise = parseRule t $ TrailRule [f] OnlySpecial
    where f = head sourceCode
          t = tail sourceCode

joinTokens :: (Token, [Char]) -> [Token]
joinTokens t = (fst t):(tokenize $ snd t)

parseRule :: [Char] -> TrailRule -> [Token]
parseRule code rule
    | tt == OnlyDigits = joinTokens $ parseDigitsRule code rule
    | otherwise = []
    where tt = trail_type rule

parseDigitsRule :: [Char] -> TrailRule -> (Token, [Char])
parseDigitsRule code rule
    | elem c ['0'..'9'] = parseDigitsRule t $ TrailRule (c:rv) OnlyDigits
    | otherwise = parseSingleToken rv code
    where rv = rule_val rule
          c  = head code
          t  = tail code