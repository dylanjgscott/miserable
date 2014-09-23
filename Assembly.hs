module Assembly where

type AsmProgram = [AsmFunction]

data AsmFunction = AsmFunction AsmId [AsmId] [AsmBlock]
                 deriving (Show, Eq)

data AsmBlock = AsmBlock AsmNum [AsmInstruction]
              deriving (Show, Eq)

data AsmInstruction = AsmLc AsmReg AsmNum
                    | AsmLd AsmReg AsmId
                    | AsmSt AsmId AsmReg
                    | AsmAdd AsmReg AsmReg AsmReg
                    | AsmSub AsmReg AsmReg AsmReg
                    | AsmMul AsmReg AsmReg AsmReg
                    | AsmDiv AsmReg AsmReg AsmReg
                    | AsmLt AsmReg AsmReg AsmReg
                    | AsmGt AsmReg AsmReg AsmReg
                    | AsmEq AsmReg AsmReg AsmReg
                    | AsmBr AsmReg AsmNum AsmNum
                    | AsmRet AsmReg
                    | AsmCall AsmReg AsmId [AsmReg]
                    deriving (Show, Eq)

type AsmNum = Integer

type AsmReg = Integer

type AsmId = String
