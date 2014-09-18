module InterpreterTests where

import Interpreter
import Test.HUnit
-----------------------------
--test0: fact.txt 3 
--	3! with valid factorial program should retuen 6
--test1: interTest1.txt 3
-- 	main function with wrong name should faile
----------------------------

test0 = TestCase (do
		input <- "tests/input/interTest0.txt"
		expected <- "tests/expected/interTest0.txt"
		assertEqual "Factorial(3)" ({- Run Company input 3 -})(expected)) 

test1 = TestCase (do
		input <- "tests/input/interTest1.txt"
		expectd <- "tests/expected/interTest1.txt"
		assertEqual "no main" ({- run Company input 3 -})(expected))








interpreterTests = TestList [TestLabel "test0" test0, TestLabel "test1" test1]
