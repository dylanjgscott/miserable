module Assembly where

data Instruction = LoadCon Reg Integer
                 | LoadIns Reg Id
                 | Store Id Reg
                 | Add Reg Reg Reg
                 | Sub Reg Reg Reg
                 | Mul Reg Reg Reg
                 | LessThan Reg Reg Reg
                 | GreaterThan Reg Reg Reg
                 | Equal Reg Reg Reg
                 | Branch Reg Integer Integer
                 | Return Reg
                 | Call Id [Reg]

type Reg = Integer
