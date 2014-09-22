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
genFunction (Function id (Args args) _ block) = (id, args, (0, instructs) : blocks)
  where (instructs, blocks) = buildBlocks block 0

-- There are some issues keeping track of block numbers
-- When calling buildBlocks twice for IfElse the numbering goes out the window.
buildBlocks :: Block -> Integer -> ([ExeInstruction], [ExeBlock])
buildBlocks (Block (s:xs)) n =
  let 
    (wholeBlock, blocks) = case s of
      (Assign id exp) -> 
        (instructs ++ buildAssign id (reg), [])
        where (reg, instructs) = buildExpression exp
      (Return id) -> (buildReturn id, [])
      (If cond block) -> 
        (buildCond cond ++ instructs, blocks)
        where (instructs, blocks) = buildBlocks block n
      (IfElse cond block1 block2) -> 
        (buildCond cond ++ instructs1 ++ instructs2, blocks1 ++ blocks2)
        where
          (instructs1, blocks1) = buildBlocks block1 n
          (instructs2, blocks2) = buildBlocks block2 (n + (fromIntegral . length $ blocks1))

    (restBlock, restBlocks) = case xs of 
       [] -> ([], [])
       xs' -> buildBlocks (Block xs') (n + 1 + (fromIntegral . length $ blocks))
  in 
    (wholeBlock ++ restBlock,  blocks ++ restBlocks) 
          

buildCond _ = [["cond"]]
buildAssign _ _= [["assign"]]
buildExpression _ = (0, [["expression"]])
buildReturn _ = [["return"]]
