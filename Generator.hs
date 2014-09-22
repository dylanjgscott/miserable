module Generator where

import Program

type ExeProgram = [ExeFunction]
type ExeFunction = (ExeId, [ExeId], [ExeBlock])
type ExeBlock = (ExeBlockId, [ExeInstruction])
type ExeInstruction = [String]
type ExeId = String
type ExeRegister = Integer
type ExeBlockId = Integer

genProgram :: Program -> ExeProgram
genProgram = map genFunction

genFunction :: Function -> ExeFunction
genFunction (Function idr (Args args) _ block) = (idr, args, buildBlocks block 0)

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
      (Assign idr expr) -> 
        (instructs ++ buildAssign idr reg, [])
        where (reg, instructs) = buildExpression expr
      (Return idr) -> (buildReturn idr, [])
      (IfElse cond block1 block2) -> 
        (buildCond cond (fst (head blocks1)) (fst (head blocks2)), blocks1 ++ blocks2)
        where
          blocks1 = buildBlocks block1 n
          blocks2 = buildBlocks block2 (n + (fromIntegral . length $ blocks1))

    (restInstructs, restBlocks) = buildBlock xs (n + 1 + (fromIntegral . length $ blocks))
  in
    (instructs ++ restInstructs, blocks ++ restBlocks)

buildExpression :: Exp -> (ExeRegister, [ExeInstruction])
buildExpression (ExpNum x) = (reg, [["lc", show reg, show x]]) where reg = 0
buildExpression (ExpId x) = (reg, [["ld", show reg, x]]) where reg = 0
buildExpression (ExpFun name args) = 
  (reg, argInstructs ++ [["call", show reg, name] ++ (map show argRegs)])
  where
    (argRegs, argInstructs) = loadArgs args
    reg = 0
buildExpression (ExpOp op expr1 expr2) = 
  (reg, expr1Instructs ++
        expr2Instructs ++
        [[show op,
          show reg,
          show expr1reg,
          show expr2reg]])
  where
    (expr1reg, expr1Instructs) = buildExpression expr1
    (expr2reg, expr2Instructs) = buildExpression expr2
    reg = 0

loadArgs :: Args -> ([ExeRegister], [ExeInstruction])
loadArgs (Args args) =
  let
    combineArgs (regAcc, instAcc) expression = 
      (regAcc ++ [reg], instAcc ++ inst)
      where (reg, inst) = buildExpression (ExpId expression)
  in
  foldl combineArgs ([],[]) args

buildAssign :: Id -> ExeRegister -> [ExeInstruction]
buildAssign idr reg =
  [["st", idr, show reg]]

buildCond :: Id -> ExeBlockId -> ExeBlockId -> [ExeInstruction]
buildCond idr block1 block2 =
  idInstr ++ [["br", show idreg, show block1, show block2]]
  where (idreg, idInstr) = buildExpression (ExpId idr)

buildReturn :: Id -> [ExeInstruction]
buildReturn idr =
  (instructs ++ [["ret", show reg]])
  where (reg, instructs) = buildExpression (ExpId idr)
