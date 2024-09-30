module Main where

import System.IO
import Lexer
import Parser

{-
main = do
    let x = [Token "(" OpenParen, Token "1" Number, Token ")" CloseParen, Token ";" EndStatement]
    print (_extractExpression x) -}

main = do
    handle <- openFile "../pascal/test.pas" ReadMode
    contents <- hGetContents handle
    let x = tokenize contents
    print x
    let y = parseIntoTree x
    --print y
    --print $ findFirstEmpty x 0
    --print $ take (findFirstEmpty x 0) x
    --print $ makeInt x
    --print $ makeIdentifier x
    hClose handle