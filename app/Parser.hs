module Parser where

import Control.Exception
import Lexer (Token (..), TokenType (..) )

data StmtType = Void | VariableDeclaration | AssignmentExpression | BinaryExpression deriving (Show, Eq)

data TreeNode = TreeNode { stype :: StmtType, children :: [TreeNode], val :: [Char] } deriving (Show, Eq)

parseTokens :: [Token] -> [TreeNode]
parseTokens tt 
    | k == VariableDeclaration = parseVariableDeclaration tt
    | otherwise = TreeNode Void [] ""
    where t = head tt
          k = token_type t

-- Keyword(var) Literal Colon Literal Assignment (Expression) EndStatement
parseVariableDeclaration :: [Token] -> TreeNode
parseVariableDeclaration tt = 