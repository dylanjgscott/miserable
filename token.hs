module Token where

data Token = Function
           | Vars
           | Semicolon
           | Comma
           | Begin
           | End
           | Equals
           | ParenOpen
           | ParenClose
           | If
           | Then
           | Else
           | Return
           | Num Int
           | Id String
           | Op String
           deriving (Eq, Show)
