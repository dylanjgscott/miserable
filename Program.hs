module Program where

-- TODO Use list instead of custom Cons
-- NOTE This is pretty gross.

type Program = [Function]

data Function = Function Id Args Vars Block
              deriving Show

data Vars = Vars [Id]
          deriving Show

data Args = Args [Id]
          deriving Show

type Block = [Statement]

data Statement = Assign Id Exp
               | If Id Block
               | IfElse Id Block Block
               | Return Id
               deriving Show

data Exp = ExpNum Program.Num
         | ExpId Id
         | ExpFun Id Args
         | ExpOp Op Exp Exp
         deriving Show

type Num = Int

type Id = String

data Op = OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpLT
        | OpGT
        | OpEq
        deriving Show
