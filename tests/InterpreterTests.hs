module InterpreterTests where

import Interpreter
import Test.HUnit
import TestLib
import System.Process 
import AsmParser
import AsmLexer
import Control.Exception
import Control.Monad
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
        input <- readFile "tests/input/asm/asm1.txt"
        let args = [] in
		assertEqual "Return constant" (getResult input args) 31)

test1 = TestCase (do
        input <- readFile "tests/input/asm/asm2.txt"
        let args = [71] in
		assertEqual "Return argument 1" (getResult input args) 71)

test2 = TestCase (do
        input <- readFile "tests/input/asm/asm2.txt"
        let args = [79] in
		assertEqual "Return argument 2" (getResult input args) 79)

test3 = TestCase (do
        input <- readFile "tests/input/asm/asm3.txt"
        let args = [79] in
		assertEqual "add one main 1" (getResult input args) 80)

test4 = TestCase (do
        input <- readFile "tests/input/asm/asm3.txt"
        let args = [31] in
		assertEqual "add one main 2" (getResult input args) 32)

test5 = TestCase (do
        input <- readFile "tests/input/asm/asm4.txt"
        let args = [31] in
		assertEqual "add one func 1" (getResult input args) 32)

test6 = TestCase (do
        input <- readFile "tests/input/asm/asm4.txt"
        let args = [79] in
		assertEqual "add one func 2" (getResult input args) 80)

test7 = TestCase (do
            input <- readFile "tests/input/asm/asm5.txt"
            let args = [3]
            assertException (ErrorCall "No function defined named 'main'.")(evaluate (getResult input args)))

test8 = TestCase (do
            input <- readFile "tests/input/asm/asm6.txt"
            let args = [3]
            assertException (ErrorCall "No function defined named 'fact'.")(evaluate (getResult input args)))

test9 = TestCase (do
            input <- readFile "tests/input/asm/asm6.txt"
            let args = [3, 4]
            assertException (ErrorCall "Wrong number of arguments for function 'main'.")(evaluate (getResult input args)))

test10 = TestCase (do
            input <- readFile "tests/input/asm/asm7.txt"
            let args = [3]
            assertException (ErrorCall "Yikes! Reached end of block.")(evaluate (getResult input args)))

test11 = TestCase (do
            input <- readFile "tests/input/asm/asmbranch.txt"
            let args = [3]
            assertException (ErrorCall "Block does not exist.")(evaluate (getResult input args)))

test12 = TestCase (do
            input <- readFile "tests/input/asm/asm8.txt"
            let args = [3]
            assertException (ErrorCall "'x' not initialised!")(evaluate (getResult input args)))

test13 = TestCase (do
            input <- readFile "tests/input/asm/asm9.txt"
            let args = [3]
            assertException (ErrorCall "'r1' not initialised!")(evaluate (getResult input args)))

-- | List of Tests we pass to Tester.hs

interpreterTests = TestList
    [
        TestLabel "Return constant" test0,
        TestLabel "Return argument1" test1,
        TestLabel "Return argument2" test2,
        TestLabel "add one main 1" test3,
        TestLabel "add one main 2" test4,
        TestLabel "add one func 1" test5,
        TestLabel "add one func 2" test6,
        TestLabel "no main" test7,
        TestLabel "undefined func" test8,
        TestLabel "wrong args" test9,
        TestLabel "end of block" test10,
        TestLabel "missing block" test11,
        TestLabel "uninitialised memory" test12,
        TestLabel "uninitialised register" test13
    ]
