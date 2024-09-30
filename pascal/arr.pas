[
    Token {value = "halt", token_type = Literal},
    Token {value = "(", token_type = OpenParen},
    Token {value = "2", token_type = Number},
    Token {value = "+", token_type = BinaryOperator},
    Token {value = "3", token_type = Number},
    Token {value = ")", token_type = CloseParen},
    Token {value = ";", token_type = EndStatement}
]


[
    TreeNode {name = HaltNode, children = [
        TreeNode {name = Constant, children = [], node_val = "2"}
    ], node_val = ""},
    TreeNode {name = Void, children = [
        Token {value = ")", token_type = CloseParen},
        Token {value = ";", token_type = EndStatement}
    ]
[], node_val = ""}]