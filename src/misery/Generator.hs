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


-- input         Start Id      Return Id
type LazyBlock = ExeBlockId -> ExeRegister -> 
-- return        Next Block  Next Reg     Blocks
                (ExeBlockId, ExeRegister, [ExeBlock])

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
  -- Set the value of register zero to 0
  -- not necessary for the spec, but should
  -- protect badly written interpreters.
  zeroRegister = ["lc", showReg 0, show 0]
  zeroBlock = (0, zeroRegister : (buildJump 1))

  -- Build this block and all children blocks
  -- Currently the return jump at the end of a 
  -- root block is set to -1, this is not in the
  -- grammar but if this jump is hit, the interpreter
  -- should crash anyway, as this will only occur
  -- if a functions doesn't return
  (_, _, blocks) = buildBlocks' block 1 (-1) 2 1 
  in 
    zeroBlock : blocks


--              Block    Root          Return
buildBlocks' :: Block -> ExeBlockId -> ExeBlockId  -> LazyBlock
buildBlocks' (Block statements) rootBlockId returnBlockId  start reg =
  let
    -- Build root block
    -- A single block with only a jump
    -- It is the entry point from 
    -- a branch
    rootBlock' = buildJump start
    nextBlockId = start
    rootBlock   = (rootBlockId,  rootBlock'  ) -- build actual start block

    -- Current block aggrigator
    -- Build the actual instructions with each 
    -- statement being given a new block.
    -- It also builds a list of partially constructed
    -- blocks which are direct children of this block
    -- i.e. they are a block in an if else statement
    combine :: (ExeBlockId, ExeRegister, [ExeBlock], [LazyBlock]) -> Statement -> (ExeBlockId, ExeRegister, [ExeBlock], [LazyBlock])
    combine (startBlockId, startReg, blockAcc, superBlockAcc) statement =
      (accBlockId, accRegister, blockAcc ++ [block], superBlockAcc ++ superBlocks)
      where
        (accBlockId, accRegister, block, superBlocks) = buildBlock statement startBlockId startReg

    -- LazyBlock aggrigator
    -- Recursively applies the partial blocks
    -- Constructing all children in a pre-order
    -- traversal
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

    -- Build final block list
    block = rootBlock : blocks ++ [returnBlock]

    -- Evaluate lazy blocks
    -- Add 1 to finals to account for return block
    (finalBlockId, finalReg, restBlocks) = foldl apply (finalBlockId' + 1, finalReg' + 1, []) superBlocks
  in
    (finalBlockId, finalReg, block ++ restBlocks)
    
buildBlock :: Statement -> ExeBlockId -> ExeRegister -> (ExeBlockId, ExeRegister, ExeBlock, [LazyBlock])
buildBlock statement blockId register =
  let
    -- Match each statment and build all its instructions
    (nextBlockId, nextReg, instructs, blocks) = case statement of
      -- Assignment Statement
      (Assign idr expr) -> 
        (ret, retReg + 1, instructs ++ asgnInstr, [])
        where
          ret = blockId + 1
          (nextReg, instructs) = buildExpression expr register
          (retReg, asgnInstr) = buildAssign idr nextReg

      -- Return Statement
      (Return idr) -> 
        (ret, retReg + 1, retInstr, [])
        where
          ret = blockId + 1
          (retReg, retInstr) = buildReturn idr register
        
      -- IfElse Statement
      (IfElse cond block1 block2) -> 
        (ret, retReg + 1, condInstr, [blocks1, blocks2])
        where
          -- lazily store blocks until entire previous block is generated
          ref1 = blockId + 1 -- first branch of ifelse
          ref2 = blockId + 2 -- second branch of iflse
          ret  = blockId + 3 -- return block after iflse
          -- Partially construct childdren
          blocks1 = buildBlocks' block1 ref1 ret 
          blocks2 = buildBlocks' block2 ref2 ret
          -- Build condition instructions
          (retReg, condInstr) = buildCond cond ref1 ref2 register

    -- Add jump instruction to the end of the statement
    -- to point to the next block in the block list
    jump = buildJump nextBlockId
    block = (blockId, instructs ++ jump)
    in
      (nextBlockId, nextReg, block, blocks)


-- Helper function to build psudo jump instruction
-- consists of a branch instruction where
-- both outcomes point to the same location
buildJump :: ExeBlockId -> [ExeInstruction]
buildJump target = [["br", showReg 0, show target, show target]]

-- Recursively build all intructions required for a givn expression
-- Uses a preorder traversion of the expression tree
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
