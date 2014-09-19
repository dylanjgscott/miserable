module ParserTests where

--- package files
import TestLib 
import Parser
import Lexer

--- Third party Libraries
import Test.HUnit
import Control.Exception
import Control.Monad

----------------------------
--  Tests: input expected
--
--  test0: parse0.txt parse_out0.txt
--       empty file - should return an empty list [] - actually check the specs...
--
--  test1: parse1.txt parse_out0.txt
--       Non valid terms - error
--
--  test2: fact1.txt fact1.txt
--      factorial from pdf - should work (currently it's own output...
--
--
-----------------------------

--Define Tests


test0 = TestCase (do
            input <- readFile "tests/input/parse0.txt"
            expected <- readFile "tests/expected/parse_out0.txt"
            assertEqual "Empty Program: " (filt(realOut input)) (filt expected))

-- Syntax error
test1 = TestCase (do
            input <- readFile "tests/input/parse1.txt"
            expected <- readFile "tests/expected/parse_out1.txt"
            assertException (ErrorCall "Syntax Error.") (evaluate (calc (alexScanTokens input))))
--            assertRaises "Syntax Error" (calc (alexScanTokens input)))

test2 = TestCase ( do
            input <- readFile "tests/input/fact1.txt"
            expected <- readFile "tests/expected/fact1.txt"
            assertEqual "Factorial program from assignment pdf: " (filt(realOut input)) (filt expected))



parserTests = TestList [TestLabel "test0" test0, TestLabel "test1" test1, TestLabel "test2" test2]

