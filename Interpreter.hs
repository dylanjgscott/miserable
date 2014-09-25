module Interpreter where

import Assembly

type RegisterState = [(AsmReg, AsmNum)]
type MemoryState = [(AsmId, AsmNum)]
type MachineState = (MemoryState, RegisterState)

runProgram :: AsmProgram -> [AsmNum] -> Integer
runProgram p args = runFunction p "main" args
    where
        runFunction :: AsmProgram -> AsmId -> [AsmNum] -> Integer
        runFunction p id args = runFunctionHelper (findFunction p id) args

        runFunctionHelper :: AsmFunction -> [AsmNum] -> Integer
        runFunctionHelper f@(AsmFunction _ _ blocks) args = runBlock blocks 0 (buildFunctionState f args)
            where
                runBlock blocks num state = runBlockHelper (findBlock blocks num) state
                runBlockHelper :: AsmBlock -> MachineState -> Integer
                runBlockHelper (AsmBlock _ instructions) s = runInstructions instructions s

                runInstructions :: [AsmInstruction] -> MachineState -> Integer
                runInstructions (i:is) s =
                    case i of
                        (AsmLc reg num) -> runInstructions is (setReg reg num s)
                        (AsmLd reg id) -> runInstructions is (setReg reg (getMem id s) s)
                        (AsmSt id reg) -> runInstructions is (setMem id (getReg reg s) s)
                        (AsmAdd reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2 s) + (getReg reg3 s)) s)
                        (AsmSub reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2 s) - (getReg reg3 s)) s)
                        (AsmMul reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2 s) * (getReg reg3 s)) s)
                        (AsmDiv reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2 s) `div` (getReg reg3 s)) s)
                        (AsmLt reg1 reg2 reg3) -> runInstructions is (setReg reg1 (if ((getReg reg2 s) < (getReg reg3 s)) then 1 else 0) s)
                        (AsmGt reg1 reg2 reg3) -> runInstructions is (setReg reg1 (if ((getReg reg2 s) > (getReg reg3 s)) then 1 else 0) s)
                        (AsmEq reg1 reg2 reg3) -> runInstructions is (setReg reg1 (if ((getReg reg2 s) == (getReg reg3 s)) then 1 else 0) s)
                        (AsmBr reg b1 b2) -> if (getReg reg s) == 0 then runBlock blocks b2 s else runBlock blocks b1 s
                        (AsmRet r) -> getReg r s
                        (AsmCall reg id regs) ->
                            let
                                func = findFunction p id
                                regValues = map (\x -> getReg x s) regs
                                funcResult = runFunction p id regValues
                            in
                                runInstructions is (setReg reg funcResult s)

findFunction :: [AsmFunction] -> AsmId -> AsmFunction
findFunction [] _ = error "No main program defined."
findFunction (f@(AsmFunction funcId _ _):fs) id = if funcId == id then f else findFunction fs id

findBlock :: [AsmBlock] -> AsmNum -> AsmBlock
findBlock [] _ = error "Block does not exist."
findBlock (b@(AsmBlock blockNum _):bs) num = if blockNum == num then b else findBlock bs num

buildFunctionState :: AsmFunction -> [AsmNum] -> MachineState
buildFunctionState (AsmFunction _ args _) nums = (zip args nums, [])

getReg :: AsmReg -> MachineState -> AsmNum
getReg reg (ms, rs) = getRegHelper reg rs
    where
        getRegHelper :: AsmReg -> RegisterState -> Integer
        getRegHelper _ [] = 0
        getRegHelper reg (x:xs) = if fst x == reg then snd x else getRegHelper reg xs

setReg :: AsmReg -> AsmNum -> MachineState -> MachineState
setReg reg num (ms, rs) = (ms, setRegHelper reg num rs)
    where
        setRegHelper :: AsmReg -> AsmNum -> RegisterState -> RegisterState
        setRegHelper reg num [] = [(reg, num)]
        setRegHelper reg num (x:xs) = if fst x == reg then (reg, num) : xs else x : setRegHelper reg num xs

getMem :: AsmId -> MachineState -> AsmNum
getMem id (ms, rs) = getMemHelper id ms
    where
        getMemHelper :: AsmId -> MemoryState -> Integer
        getMemHelper _ [] = 0
        getMemHelper id (x:xs) = if fst x == id then snd x else getMemHelper id xs


setMem :: AsmId -> AsmNum -> MachineState -> MachineState
setMem id num (ms, rs) = (setMemHelper id num ms, rs)
    where
        setMemHelper :: AsmId -> AsmNum -> MemoryState -> MemoryState
        setMemHelper id num [] = [(id, num)]
        setMemHelper id num (x:xs) = if fst x == id then (id, num) : xs else x : setMemHelper id num xs
