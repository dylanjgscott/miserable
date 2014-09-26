module SemanticTests where
-- Misery and Company
import Lexer
import Parser
import Semantic
import TestLib
-- Third Party
import Test.HUnit
import Control.Exception
import Control.Monad


-- | Define Tests

-- | the Factorial case from the assignement - shuld return True for good semantics
test0 = TestCase (do
			input <- readFile "tests/input/src/fact1.txt"
			assertEqual "Factorial(n) from pdf: " (semanticCheck (calc (alexScanTokens input))) True)

-- | No some errors - note that for Semantic.hs erros calls all Strings start with a "\n"

-- | No main function: should throw error.
test1 = TestCase (do
            input <- readFile "tests/input/src/fact_noMain.txt"
            assertException (ErrorCall "\nError: No main function defined.")(evaluate (semanticCheck (calc (alexScanTokens input)))))

-- | Undefined function: should throw error.
test2 = TestCase (do
            input <- readFile "tests/input/src/undefinedFunc.txt"
            assertException (ErrorCall "\nError: function 'myfunc' undefined.")(evaluate (semanticCheck (calc (alexScanTokens input)))))

-- | Redefined function: should throw error.
test3 = TestCase (do
            input <- readFile "tests/input/src/repeatFunc.txt"
            assertException (ErrorCall "\nError: function 'factorial' redefined.")(evaluate (semanticCheck (calc (alexScanTokens input)))))

-- | Wrong number of args for function: should throw error.
test4 = TestCase (do
            input <- readFile "tests/input/src/argsMismatch.txt"
            assertException (ErrorCall "\nError: function 'factorial' expects 1 argument(s).")(evaluate (semanticCheck (calc (alexScanTokens input)))))

-- | Undefined variable: should throw error.
test5 = TestCase (do
            input <- readFile "tests/input/src/undefinedVar.txt"
            assertException (ErrorCall "\nError: variable 'foo' undefined.")(evaluate (semanticCheck (calc (alexScanTokens input)))))

-- | Undefined variable: should throw error.
test6 = TestCase (do
            input <- readFile "tests/input/src/redefinedVar.txt"
            assertException (ErrorCall "\nError: variable 'n' redefined.")(evaluate (semanticCheck (calc (alexScanTokens input)))))

-- | Pass to the tester
semanticTests = TestList    [   TestLabel " test0" test0, 
                                TestLabel " No main " test1,
                                TestLabel " undefined function " test2,
                                TestLabel " redefined function " test3,
                                TestLabel " args mismatch " test4,
                                TestLabel "\nError: variable 'foo' undefined.\n" test5,
                                TestLabel "\nError: variable 'n' redefined.\n" test6   ]
