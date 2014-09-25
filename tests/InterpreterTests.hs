module InterpreterTests where

import Interpreter
import Test.HUnit
import System.Process 
import AsmParser
import AsmLexer
-----------------------------
--test0: fact.txt 3 
--	3! with valid factorial program should retuen 6
--test1: interTest1.txt 3
-- 	main function with wrong name should faile
----------------------------


getAsm input = asmParse (alexScanTokens input)
getResult input args = runProgram (getAsm input) args

-- | Tests 
test0 = TestCase (do
        input <- readFile "tests/input/asm1.txt"
        let args = [] in
		assertEqual "Return constant" (getResult input args) 31)

test1 = TestCase (do
        input <- readFile "tests/input/asm2.txt"
        let args = [71] in
		assertEqual "Return argument 1" (getResult input args) 71)

test2 = TestCase (do
        input <- readFile "tests/input/asm2.txt"
        let args = [79] in
		assertEqual "Return argument 2" (getResult input args) 79)

test3 = TestCase (do
        input <- readFile "tests/input/asm3.txt"
        let args = [79] in
		assertEqual "add one main 1" (getResult input args) 80)

test4 = TestCase (do
        input <- readFile "tests/input/asm3.txt"
        let args = [31] in
		assertEqual "add one main 2" (getResult input args) 32)

test5 = TestCase (do
        input <- readFile "tests/input/asm4.txt"
        let args = [31] in
		assertEqual "add one func 1" (getResult input args) 32)

test6 = TestCase (do
        input <- readFile "tests/input/asm4.txt"
        let args = [79] in
		assertEqual "add one func 2" (getResult input args) 80)


-- | List of Tests we pass to Tester.hs

interpreterTests = TestList
    [
        TestLabel "Return constant" test0,
        TestLabel "Return argument1" test1,
        TestLabel "Return argument2" test2,
        TestLabel "add one main 1" test3,
        TestLabel "add one main 2" test4,
        TestLabel "add one func 1" test5,
        TestLabel "add one func 2" test6
    ]
