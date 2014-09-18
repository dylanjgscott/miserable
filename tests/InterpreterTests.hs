module InterpreterTests where

import Interpreter
import Test.HUnit
-----------------------------
--test0: fact.txt 3 
--	3! with valid factorial program should retuen 6
--test1: interTest1.txt 3
-- 	main function with wrong name should faile
----------------------------

--removes newline from files
filt input = filter (/= '\n') input

test0 = TestCase (do
		input <- readFile "tests/input/interTest0.txt"
		expected <- readFile "tests/expected/interTest0.txt"
		assertEqual "Factorial(3)"  "7" {- Run Company input 3 -} (filt expected)) 

test1 = TestCase (do
		input <- readFile "tests/input/interTest1.txt"
		expected <- readFile "tests/expected/interTest1.txt"
		assertEqual "no main" {- run Company input 3 -} "error" (filt expected))








interpreterTests = TestList [TestLabel "test0" test0, TestLabel "test1" test1]
