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


----------------------------------
-- Test:	input File	| expected 
--
--
--
--
--
--
--
--
-----------------------------------

-- | Define Tests

-- the Factorial case from the assignement
test0 = TestCase (do
			input <- readFile "tests/input/fact1.txt"
			--expected ... todo
			assertEqual "Factorial(n) from pdf: " (semanticCheck (calc (alexScanTokens input))) True)



-- | Pass to the tester
semanticTests = TestList [TestLabel "test0" test0]

