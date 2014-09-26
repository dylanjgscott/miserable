module GeneratorTests where
-- Misery & Company
import Lexer
import Parser
import Semantic
import TestLib
import Generator
-- Others
import Test.HUnit
import Control.Exception
import Control.Monad


-- | Define tests


--todo

test0 = TestCase (do
        input <- readFile "tests/input/src/fact1.txt"
        output <- readFile "tests/expected/fact1asm.txt"
        assertEqual "Fact1 asm generation" (showProgram (genProgram (calc (alexScanTokens input)))) output)

test1 = TestCase (do
        input <- readFile "tests/input/src/complex1.txt"
        output <- readFile "tests/expected/complex1asm.txt"
        assertEqual "Complex1 asm generation" (showProgram (genProgram (calc (alexScanTokens input)))) output)


-- | pass to tester
generatorTests = TestList [TestLabel " Fact1 test " test0, TestLabel " Complex1 test " test1]


