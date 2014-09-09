module AST where
import Prelude

data Expression = Num
                | Id String
                | Id Arguments
                | Expression Op Expression
                deriving Show

