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
buildBlocks (Block s:xs) n =
    let
        buildBlock :: Statment -> ([ExeInstruction], [ExeBlock])
        buildBlock (Assign id exp) = 
          (instructs ++ buildAssign id (reg), [])
          where (reg, instructs) = buildExpression exp

        buildBlock (Return id) = (buildReturn id, [])

        buildBlock (If cond block) = 
          (buildCond + instructs, blocks)
          where (instructs, blocks) = buildBlocks block n

        buildBlock (IfElse cond block1 block2) = 
          (buildCond cond + instructs1 + instructs2, blocks1 + block2)
          where
            (instructs1, blocks1) = buildBlocks block1 n
            (instructs2, blocks2) = buildBlocks block2 n + length(blocks1)
    in 
        (n, wholeBlock) : blocks ++ buildBlocks xs (n + 1 + length(blocks))
        where
          wholeBlock, blocks = buildBlock s
