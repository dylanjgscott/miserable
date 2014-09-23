module Interpreter where

import Assembly

type RegisterState = [(AsmReg, AsmNum)]
type MemoryState = [(AsmId, AsmNum)]
type MachineState = (MemoryState, RegisterState)

-- Clearly I am not a Haskell programmer.

--runProgram :: AsmProgram -> MachineState -> Integer
runProgram p s = runFunction p "main" s
    where
        --runFunction :: AsmProgram -> AsmId -> MachineState -> Integer
        runFunction p id s = runFunctionHelper (findFunction p id) s
            where
                --runFunctionHelper :: AsmFunction -> MachineState -> Integer
                runFunctionHelper (AsmFunction _ _ blocks) s = runBlock blocks 0 s
                    where
                    runBlock blocks num state = runBlockHelper (findBlock blocks num) state
                        where
                            --runBlockHelper :: AsmBlock -> MachineState -> Integer
                            runBlockHelper (Block _ instructions) s = runInstructions instructions s
                                where
                                    runInstructions :: [AsmInstruction] -> MachineState -> Integer
                                    runInstructions (i:is) s =
                                        case i of
                                            (AsmLc reg num) -> runInstructions is (setReg reg num)
                                            (AsmLd reg id) -> runInstructions is (setReg reg (getMem id))
                                            (AsmSt id reg) -> runInstructions is (setMem id (getReg reg))
                                            (AsmAdd reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2) + (getReg reg3)))
                                            (AsmSub reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2) - (getReg reg3)))
                                            (AsmMul reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2) * (getReg reg3)))
                                            (AsmDiv reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2) / (getReg reg3)))
                                            (AsmLt reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2) < (getReg reg3)))
                                            (AsmGt reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2) > (getReg reg3)))
                                            (AsmEq reg1 reg2 reg3) -> runInstructions is (setReg reg1 ((getReg reg2) == (getReg reg3)))
                                            (AsmBr reg b1 b2) -> if reg == 0 then runBlock blocks b2 s else runBlock blocks b1 s
                                            (AsmRet r) -> getReg r s
                                            (AsmCall r1 id rs) -> runFunction p --- ARRRGGHHLKSJDF:LSKJDF
                            findBlock :: [AsmBlock] -> AsmNum -> AsmBlock
                            findBlock [] _ = error "Block does not exist."
                            findBlock (b@(AsmBlock blockNum _):bs) num = if blockNum == num then b else findBlock bs num


                findFunction :: [AsmFunction] -> AsmId -> AsmFunction
                findFunction [] _ = error "No main program defined."
                findFunction (f@(AsmFunction funcId _ _):fs) id = if funcId == id then f else findFunction fs id

getReg :: AsmReg -> MachineState -> AsmNum
getReg reg (ms, rs) = getRegHelper reg rs
    where
        getRegHelper :: AsmReg -> RegisterState -> Integer
        getRegHelper _ [] = 0
        getRegHelper reg (x:xs) = if fst x == reg then snd x else getRegHelper reg xs

setReg :: AsmReg -> AsmNum -> MachineState -> MachineState
setReg reg num (ms, rs) = (ms, setRegHelper reg num)
    where
        setRegHelper :: AsmReg -> AsmNum -> RegisterState -> RegisterState
        setRegHelper reg num [] = [(reg, num)]
        setRegHelper reg num (x:xs) = if fst x == reg then (reg, num) : xs else x : setRegHelper reg num xs

getMem :: AsmId -> MachineState -> AsmNum
getMem id (ms, rs) = getRegHelper id ms
    where
        getMemHelper :: AsmId -> MemoryState -> Integer
        getMemHelper _ [] = 0
        getMemHelper id (x:xs) = if fst x == id then snd x else getMemHelper id xs


setMem :: AsmId -> AsmNum -> MachineState -> MachineState
setMem id num (ms, rs) = (setMemHelper id num, rs)
    where
        setMemHelper :: AsmId -> AsmNum -> MemoryState -> MemoryState
        setMemHelper id num [] = [(id, num)]
        setMemHelper id num (x:xs) = if fst x == id then (id, num) : xs else x : setMemHelper reg id xs
