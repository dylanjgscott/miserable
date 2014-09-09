module AST where
import Prelude

-- TODO Use list instead of custom Cons
-- NOTE This is pretty gross.

data Program = EmptyProgram
             | Program Function Program
             deriving Show

data Function = Function Id IdList IdList Statements
              deriving Show


data IdList = EmptyIdList
            | IdList Id IdList
            deriving Show

data Statements = EmptyStatements
                | Statements Statement Statements
                deriving Show

data Statement = Assign Id Exp
               | If Id Statements
               | IfElse Id Statements Statements
               | Return Id
               deriving Show

data Exp = ExpNum AST.Num
         | ExpId Id
         | ExpFun Id IdList
         | ExpOp Op Exp Exp
         deriving Show

data Num = Num Integer
           deriving Show

data Id = Id String
        deriving Show

data Op = OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpLT
        | OpGT
        | OpEq
        deriving Show
