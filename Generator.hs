module Generator where

import Program

type ExeProgram = [ExeFunction]
type ExeFunction = (ExeId, [ExeId], [ExeBlock])
type ExeBlock = (Integer, [ExeInstruction])
type ExeInstruction = [String]
type ExeId = String

genProgram :: Program -> ExeProgram
genProgram [] = []
genProgram (f:fs) = genFunction f : genProgram fs


genFunction :: Function -> ExeFunction
genFunction (Function id args _ block)
    = (id, genArgs args, genBlocks block)

genArgs :: Args -> [String]
genArgs (Args x) = x

genBlocks :: Block -> [ExeBlock]
genBlocks b = [(0, [["Test"]])]
