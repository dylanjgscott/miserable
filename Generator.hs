module Generator where

import Program

type ExeProgram = [ExeFunction]
type ExeFunction = (ExeId, [ExeId], [ExeBlock])
type ExeBlock = (Integer, [ExeInstruction])
type ExeInstruction = [String]
type ExeId = String
type ExeRegister = Integer


genProgram :: Program -> ExeProgram
genProgram = map genFunction

genFunction :: Function -> ExeFunction
genFunction (Function id (Args args) _ block) = (id, args, buildBlocks block 0)

-- There are some issues keeping track of block numbers
-- When calling buildBlocks twice for IfElse the numbering goes out the window.
buildBlocks :: Block -> Integer -> [ExeBlock]
buildBlocks (Block b) n =
    let
        buildInstructions :: [Statement] -> [ExeInstruction]
        buildInstructions [] = []
        buildInstructions (s:ss) =
            case s of (Assign id exp) -> ["assign"] : buildInstructions ss
                      (If id exp) -> ["if"] : buildInstructions ss
                      (IfElse id exp1 exp2) -> ["ifelse"] : buildInstructions ss
                      (Return id) -> ["return"] : buildInstructions ss
        buildMoreBlocks :: [Statement] -> Integer -> [ExeBlock]
        buildMoreBlocks [] _ = []
        buildMoreBlocks (s:ss) n =
            case s of (Assign id exp) -> buildMoreBlocks ss n
                      (If id b) -> buildBlocks b (n+1) ++ buildMoreBlocks ss n
                      (IfElse id b1 b2) -> buildBlocks b1 (n+1) ++ buildBlocks b2 (n+2) ++ buildMoreBlocks ss n
                      (Return id) -> buildMoreBlocks ss n
    in 
        (n, buildInstructions b) : buildMoreBlocks b (n)
