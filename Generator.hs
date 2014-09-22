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
buildBlocks (Block blk) n = 
  (n, instructs) : blocks
  where (instructs, blocks) = buildBlock blk n

buildBlock :: [Statement] -> Integer -> ([ExeInstruction], [ExeBlock])
buildBlock [] n = ([], [])
buildBlock (s:xs) n =
  let
    (instructs, blocks) = case s of
      (Assign id exp) -> 
        (instructs ++ buildAssign id (reg), [])
        where (reg, instructs) = buildExpression exp
      (Return id) -> (buildReturn id, [])
      (If cond block) -> 
        (buildCond cond, buildBlocks block n)
      (IfElse cond block1 block2) -> 
        (buildCond cond, blocks1 ++ blocks2)
        where
          blocks1 = buildBlocks block1 n
          blocks2 = buildBlocks block2 (n + (fromIntegral . length $ blocks1))

    (restInstructs, restBlocks) = buildBlock xs (n + 1 + (fromIntegral . length $ blocks))
  in
    (instructs ++ restInstructs, blocks ++ restBlocks)

buildCond _ = [["cond"]]
buildAssign _ _= [["assign"]]
buildExpression _ = (0, [["expression"]])
buildReturn _ = [["return"]]
