module InterpreterTests where

import Interpreter
import Test.HUnit
import System.Process 
-----------------------------
--test0: fact.txt 3 
--	3! with valid factorial program should retuen 6
--test1: interTest1.txt 3
-- 	main function with wrong name should faile
----------------------------

-- | Helper Functions

-- removes newline from files
filt input = filter (/= '\n') input
res = "2"

-- | Tests 


-- Dummy Test
test0 = TestCase (do
		input <- readFile "tests/input/interTest0.txt"
        --put code here...
		assertEqual "Factorial(3)"  {-"7" Run Company input 3 -} res "2") 






-- | List of Tests we pass to Tester.hs

interpreterTests = TestList [TestLabel "test0" test0]
