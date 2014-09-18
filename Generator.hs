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
genFunction (Function id (Args args) _ block) = (id, args, [(0, [["inst"]])])

buildBlocks :: Block -> Integer -> [ExeBlock]
buildBlocks (Block (s:ss)) n =
    let
        blocks = genBlockList s (n)
    in
        blocks ++ buildBlocks ss (length blocks)
        

genBlockList :: Statement -> Integer -> [ExeBlock]
genBlockList s n =
    let
        genBlockListHelper (Assign id exp) = (n, genInstruct(Assign id exp))
        genBlockListHelper (If id block) = 
        genBlockListHelper (IfElse id bl1 bl2) = 
        genBlockListHelper (Return id) = 
    in
        genBlockListHelper s n
