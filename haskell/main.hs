import System.IO
import Data.List
import Data.Char (ord)

data TokenType = Quotes | Dot | Colon | EndStatement | EOF | Literal | Number | Letter | Digit | Equals | OpenParen | CloseParen | BinaryOperator | Empty deriving (Show, Eq)
data Token = Token {value :: [Char], token_type :: TokenType} deriving (Show)

parseSingleChar :: Char -> Token
parseSingleChar c
    | c == '(' = Token [c] OpenParen
    | c == ')' = Token [c] CloseParen
    | c == '+' || c == '-' || c == '*' || c == '/' || c == '<' || c == '>' = Token [c] BinaryOperator
    | c == '=' = Token [c] Equals
    | c == ';' = Token [c] EndStatement
    | c == ':' = Token [c] Colon
    | c == '.' = Token [c] Dot
    | c == '\'' = Token [c] Quotes
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
    | token_type ct == Quotes = (Token (_makeQuoteString (drop (succ i) all_tokens) 0) Literal):(_reduceTokens all_tokens (findFirstQuotes all_tokens (succ i)))
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

main = do
    handle <- openFile "test.pas" ReadMode
    contents <- hGetContents handle
    let x = tokenize contents
    print x
    --print $ findFirstEmpty x 0
    --print $ take (findFirstEmpty x 0) x
    --print $ makeInt x
    --print $ makeIdentifier x
    hClose handle