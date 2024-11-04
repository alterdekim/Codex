module Parser where

import Lexer (Token (..), TokenType (..) )

data StmtType = Primary | Numeric deriving (Show, Eq)

class (Show a) => TreeNode a where
    stype :: a -> StmtType
    children :: a -> [a]

instance TreeNode StmtType where
    stype Primary = Primary
    children Primary = []

--data PrimaryNode = PrimaryNode { val :: [Char] } deriving (TreeNode)

parseTokens :: [Token] -> TreeNode
parseTokens tt
    | k == Literal = PrimaryNode Primary [] (value t)
    | otherwise = TreeNode Parser.Numeric []
    where t = head tt
          k = token_type t