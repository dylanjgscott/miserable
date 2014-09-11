module Program where

-- TODO Use list instead of custom Cons
-- NOTE This is pretty gross.

data Program = Program Functions
             deriving Show

data Functions = EmptyFunction
               | Functions Function Functions
              deriving Show

data Function = Function Id IdList IdList Statements
              deriving Show

data Vars = VarsEmpty
          | Vars IdList
          deriving Show

data Args = ArgsEmpty
          | Args IdList
          deriving Show

data IdList = IdList Id IdList
            deriving Show

data Block = Block Statements
           deriving Show

data Statements = EmptyStatements
                | Statements Statement Statements
                deriving Show

data Statement = Assign Id Exp
               | If Id Statements
               | IfElse Id Statements Statements
               | Return Id
               deriving Show

data Exp = ExpNum Program.Num
         | ExpId Id
         | ExpFun Id Args
         | ExpOp Op Exp Exp
         deriving Show

data Num = Num Int
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
