module Generator where

import Program
import Data.List

type ExeProgram = [ExeFunction]
type ExeFunction = (ExeId, [ExeId], [ExeBlock])
type ExeBlock = (ExeBlockId, [ExeInstruction])
type ExeInstruction = [String]
type ExeId = String
type ExeBlockId = Integer
type ExeRegister = Integer


-- maps generated functions from the ast function list
genProgram :: Program -> ExeProgram
genProgram = map genFunction

-- Generated a function recursively from the ast
genFunction :: Function -> ExeFunction
genFunction (Function idr (Args args) _ block) =
   (idr, args, blocks)
   where (_, blocks) = buildBlocks block 0 1

-- wrapper function to partition the boundary of blocks
-- contains an agrigator to generate the remaining blocks
buildBlocks :: Block -> ExeBlockId -> ExeRegister -> (ExeRegister, [ExeBlock])
buildBlocks (Block blk) n reg = 
  let
    (retReg, instructs, blocks) = buildBlock blk (n+1) reg
    comb (accReg, acc) next = (nexReg, acc ++ tmpBlocks)
      where (nexReg, tmpBlocks) = next accReg
    finalBlocks = (foldl comb (retReg, []) blocks)
  in
    (fst finalBlocks, (n, instructs) : snd finalBlocks)
  


-- Block builder, performs preorder traversal of as to produce a single block,
-- as well as a list of all child blocks (i.e. blocks that branch from this one)
-- the tricky part is ensuring consistent block naming
buildBlock :: [Statement] -> ExeBlockId -> ExeRegister -> (ExeRegister, [ExeInstruction], [ExeRegister -> (ExeRegister, [ExeBlock])])
buildBlock [] n reg = (reg, [], [])
buildBlock (s:xs) n reg =
  let
  -- Match on each statement and generate appropriate instructions
    (nextReg, instructs, blocks) = case s of
      (Assign idr expr) -> 
        (retReg + 1, instructs ++ asgnInstr, [])
        where
          (nextReg, instructs) = buildExpression expr reg
          (retReg, asgnInstr) = buildAssign idr nextReg
      (Return idr) -> (retReg + 1, retInstr, [])
        where (retReg, retInstr) = buildReturn idr reg
      (IfElse cond block1 block2) -> 
        (retReg + 1, condInstr, [blocks1, blocks2])
        where
          -- lazily store blocks until entire previous block is generated
          blocks1 = buildBlocks block1 n
          blocks2 = buildBlocks block2 (n + 1)
          (retReg, condInstr) = buildCond cond n (n + 1) reg

    -- Recurse through remaining statements to gather remaining instructions
    -- Also store Dependant blocks
    (lastReg, restInstructs, restBlocks) = buildBlock xs (n + (fromIntegral . length $ blocks)) nextReg
  in
    -- Group all functions for this block together
    (lastReg, instructs ++ restInstructs, blocks ++ restBlocks)

-- 
buildExpression :: Exp -> ExeRegister -> (ExeRegister, [ExeInstruction])
-- Base cases
buildExpression (ExpNum x) reg = (reg, [["lc", showReg reg, show x]])
buildExpression (ExpId x) reg = (reg, [["ld", showReg reg, x]])

-- generate instructions to load arguments into registers so they can be called
buildExpression (ExpFun name args) reg = 
  ((retReg), argInstructs ++ [["call", showReg (retReg), name] ++ (map showReg argRegs)])
  where
    (argRegs, argInstructs) = loadArgs args reg
    retReg = (last argRegs) + 1 

-- Recursively build all required instructions for an expression
-- perpend them to the final expression
buildExpression (ExpOp op expr1 expr2) reg = 
  (retReg, expr1Instructs ++
        expr2Instructs ++
        [[show op,
          showReg retReg,
          showReg expr1reg,
          showReg expr2reg]])
  where
    (expr1reg, expr1Instructs) = buildExpression expr1 reg
    (expr2reg, expr2Instructs) = buildExpression expr2 (expr1reg + 1)
    retReg = expr2reg + 1

-- generate all the load instructions for use with call
loadArgs :: Args -> ExeRegister -> ([ExeRegister], [ExeInstruction])
loadArgs (Args args) reg =
  let
    combineArgs (regAcc, instAcc) expression = 
      (regAcc ++ [reg], instAcc ++ inst)
      where
        reg = ((last regAcc) + 1)
        (nextReg, inst) = buildExpression (ExpId expression) reg

    (argRegs, argInstr) = foldl combineArgs ([reg-1],[]) args
  in
    (tail argRegs, argInstr)

-- assign a register value to an id
buildAssign :: Id -> ExeRegister -> (ExeRegister, [ExeInstruction])
buildAssign idr reg =
  (reg, [["st", idr, showReg reg]])

-- given register and 2 block ids generate the appropriate branch instruction
buildCond :: Id -> ExeBlockId -> ExeBlockId -> ExeRegister -> (ExeRegister, [ExeInstruction])
buildCond idr block1 block2 reg =
  (retReg, idInstr ++ [["br", showReg retReg, show block1, show block2]])
  where (retReg, idInstr) = buildExpression (ExpId idr) reg

-- load the id into a register and build return instruction
buildReturn :: Id -> ExeRegister -> (ExeRegister, [ExeInstruction])
buildReturn idr reg =
  (retReg, (instructs ++ [["ret", showReg retReg]]))
  where (retReg, instructs) = buildExpression (ExpId idr) reg

-- Pretty printing functions, simple traversal of data structure
-- Fairly self explanatory
showProgram :: ExeProgram -> String
showProgram prog =  "( "
                 ++ intercalate "\n  " (map showFunction prog)
                 ++ " )\n"

showFunction :: ExeFunction -> String
showFunction (name, args, blocks) =  "("
                  ++ name
                  ++ " "
                  ++ showArgs args
                  ++ "\n    "
                  ++ showBlocks blocks
                  ++ " )"

showArgs :: [Id] -> String
showArgs args =  "("
              ++ intercalate " " args
              ++ ")"

showBlocks :: [ExeBlock] -> String
showBlocks blocks = intercalate "\n    " (map showBlock blocks)

showBlock :: ExeBlock -> String
showBlock (blkId, instructs) =  "( "
                            ++ show blkId
                            ++ " "
                            ++ intercalate "\n        " (map showInstruct instructs)
                            ++ " )"

showInstruct :: ExeInstruction -> String
showInstruct instruct = "(" ++ intercalate " " instruct ++ ")"

showReg :: ExeRegister -> String
showReg reg = "r" ++ show reg
