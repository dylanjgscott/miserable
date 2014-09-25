module Generator where

import Program
import Data.List

type ExeProgram = [ExeFunction]
type ExeFunction = (ExeId, [ExeId], [ExeBlock])
type ExeBlock = (ExeBlockId, [ExeInstruction])
type ExeInstruction = [String]
type ExeId = String
type ExeBlockId = Int
type ExeRegister = Int
--               Start         Return          Next Block  Next Reg
type LazyBlock = ExeBlockId -> ExeRegister -> (ExeBlockId, ExeRegister, [ExeBlock])

-- maps generated functions from the ast function list
genProgram :: Program -> ExeProgram
genProgram = map genFunction

-- Generated a function recursively from the ast
genFunction :: Function -> ExeFunction
genFunction (Function idr (Args args) _ block) =
   (idr, args, blocks)
   where blocks = buildBlocks block


buildBlocks :: Block -> [ExeBlock]
buildBlocks block =
  let
  zeroRegister = ["lc", showReg 0, show 0]
  zeroBlock = (0, zeroRegister : (buildJump 1))
  (_, _, blocks) = buildBlocks' block 1 (-1) 2 1 
  in 
    zeroBlock : blocks


--              Block    Root          Return
buildBlocks' :: Block -> ExeBlockId -> ExeBlockId  -> LazyBlock
buildBlocks' (Block statements) rootBlockId returnBlockId  start reg =
  let
    -- Build root block
    rootBlock' = buildJump start
    rootBlock   = (rootBlockId,  rootBlock'  ) -- build actual start block
    nextBlockId = start

    -- Current block aggrigator
    combine :: (ExeBlockId, ExeRegister, [ExeBlock], [LazyBlock]) -> Statement -> (ExeBlockId, ExeRegister, [ExeBlock], [LazyBlock])
    combine (startBlockId, startReg, blockAcc, superBlockAcc) statement =
      (accBlockId, accRegister, blockAcc ++ [block], superBlockAcc ++ superBlocks)
      where
        (accBlockId, accRegister, block, superBlocks) = buildBlock statement startBlockId startReg

    -- LazyBlock aggrigator
    apply :: (ExeBlockId, ExeRegister, [ExeBlock]) -> LazyBlock -> (ExeBlockId, ExeRegister, [ExeBlock])
    apply (startBlockId, startReg, blockAcc) superBlock =
      (accBlockId, accRegister, blockAcc ++ blocks)
      where
        (accBlockId, accRegister, blocks) = superBlock startBlockId startReg

    -- Build intermediate blocks
    (finalBlockId', finalReg', blocks, superBlocks) = foldl combine (nextBlockId, reg, [], []) statements

    -- Build return block
    returnBlock' = buildJump returnBlockId
    returnBlock = (finalBlockId', returnBlock') -- build actual return block

    -- Build final meta-block
    block = rootBlock : blocks ++ [returnBlock]

    -- Evaluate lazy blocks
    -- Add 1 to finals to account for return block
    (finalBlockId, finalReg, restBlocks) = foldl apply (finalBlockId' + 1, finalReg' + 1, []) superBlocks
  in
    (finalBlockId, finalReg, block ++ restBlocks)
    
buildBlock :: Statement -> ExeBlockId -> ExeRegister -> (ExeBlockId, ExeRegister, ExeBlock, [LazyBlock])
buildBlock statement blockId register =
  let
    (nextBlockId, nextReg, instructs, blocks) = case statement of
      (Assign idr expr) -> 
        (ret, retReg + 1, instructs ++ asgnInstr, [])
        where
          ret = blockId + 1
          (nextReg, instructs) = buildExpression expr register
          (retReg, asgnInstr) = buildAssign idr nextReg

      (Return idr) -> 
        (ret, retReg + 1, retInstr, [])
        where
          ret = blockId + 1
          (retReg, retInstr) = buildReturn idr register
        
      (IfElse cond block1 block2) -> 
        (ret, retReg + 1, condInstr, [blocks1, blocks2])
        where
          -- lazily store blocks until entire previous block is generated
          ref1 = blockId + 1 -- first branch of ifelse
          ref2 = blockId + 2 -- second branch of iflse
          ret  = blockId + 3 -- return block after iflse
          blocks1 = buildBlocks' block1 ref1 ret
          blocks2 = buildBlocks' block2 ref2 ret
          (retReg, condInstr) = buildCond cond ref1 ref2 register

    jump = buildJump nextBlockId
    block = (blockId, instructs ++ jump)
    in
      (nextBlockId, nextReg, block, blocks)


buildJump :: ExeBlockId -> [ExeInstruction]
buildJump target = [["br", showReg 0, show target, show target]]

buildExpression :: Exp -> ExeRegister -> (ExeRegister, [ExeInstruction])
-- Base cases
buildExpression (ExpNum x) reg = (reg, [["lc", showReg reg, show x]])
buildExpression (ExpId x) reg = (reg, [["ld", showReg reg, x]])

-- generate instructions to load arguments into registers so they can be called
buildExpression (ExpFun name args) reg = 
 ((retReg), argInstructs ++ [["call", showReg (retReg), name] ++ (map showReg argRegs)])
 where
   (argRegs, argInstructs) = loadArgs args reg
   retReg = if (not (null argRegs)) then (last argRegs) + 1 else reg

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
