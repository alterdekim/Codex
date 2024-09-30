module Parser (parseIntoTree, TreeNode) where

import Lexer (Token (..), TokenType (..) )

data NodeName = BinOperator | HaltNode | StringConstant | Constant | Void deriving (Show, Eq)
data TreeNode = TreeNode {name :: NodeName, children :: [TreeNode], node_val :: [Char]} deriving (Show)

parseIntoTree :: [Token] -> [TreeNode]
parseIntoTree tokens
    | length tokens > 0 = _parseIntoTree tokens
    | otherwise = []