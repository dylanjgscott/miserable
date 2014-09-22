module Generator where

import Program

type ExeProgram = [ExeFunction]
type ExeFunction = (ExeId, [ExeId], [ExeBlock])
type ExeBlock = (Integer, [ExeInstruction])
type ExeInstruction = [String]
type ExeId = String
