module Parser (parseIntoTree, TreeNode, _extractExpression) where

import Lexer (Token (..), TokenType (..) )

data NodeName = BinOperator | HaltNode | StringConstant | Constant | Void deriving (Show, Eq)
data TreeNode = TreeNode {name :: NodeName, children :: [TreeNode], node_val :: [Char]} deriving (Show)

processVoid :: [Token] -> [TreeNode]
processVoid tokens = (TreeNode Void (parseIntoTree (fst ft)) []):(parseIntoTree (snd ft))
    where ft = _extractExpression (tail tokens)

_parseIntoTree :: [Token] -> [TreeNode]
_parseIntoTree tokens
    | token_type ft == Literal = parseLiteral tokens
    | token_type ft == QString = (TreeNode StringConstant [] (value ft)):[]
    | token_type ft == Number = (TreeNode Constant [] (value ft)):[]
    | token_type ft == BinaryOperator = (TreeNode BinOperator [] (value ft)):[]
    | token_type ft == EndStatement = []
    | otherwise = processVoid tokens
    where ft = head tokens

parseIntoTree :: [Token] -> [TreeNode]
parseIntoTree tokens
    | length tokens > 0 = _parseIntoTree tokens
    | otherwise = []

parseLiteral :: [Token] -> [TreeNode]
parseLiteral tokens
    | value ft == "halt" = processHalt tokens
    where ft = head tokens

processHalt :: [Token] -> [TreeNode]
processHalt tokens = (TreeNode HaltNode (parseIntoTree (fst hn)) []):(parseIntoTree (snd hn))
    where hn = _extractExpression (tail tokens)

_extractExpression :: [Token] -> ([Token], [Token])
_extractExpression tt = extractExpression tt tt 0 (-1)

__extractExpression :: [Token] -> [Token] -> Int -> Int -> ([Token], [Token])
__extractExpression tt at sto ski
    | length tt > 0 = extractExpression tt at sto ski
    | otherwise = ([], [])

extractExpression :: [Token] -> [Token] -> Int -> Int -> ([Token], [Token])
extractExpression tt at sto ski
    | token_type ft == OpenParen = __extractExpression (tail tt) at (sto+1) (ski+1)
    | token_type ft == CloseParen && ski > 0 = __extractExpression (tail tt) at (sto+1) (ski-1)
    | token_type ft == CloseParen && ski <= 0 = (take (sto-1) (drop 1 at), drop sto at)
    | otherwise = __extractExpression (tail tt) at (sto+1) ski
    where ft = head tt