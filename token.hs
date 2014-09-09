module Token where

data Token = TokenFunction
           | TokenVars
           | TokenSemicolon
           | TokenComma
           | TokenBegin
           | TokenEnd
           | TokenEquals
           | TokenParenOpen
           | TokenParenClose
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenReturn
           | TokenNum Int
           | TokenId String
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDivide
           | TokenLT
           | TokenGT
           | TokenEQ
           deriving (Eq, Show)
