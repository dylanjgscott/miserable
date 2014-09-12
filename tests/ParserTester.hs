module ParserTester where


import Parser
import Lexer
import Test.HUnit

--General format for testing - at least that we got to work so far...

--Define Tests

test1 = TestCase(assertEqual "True == True"  True True)

-- Testing again it's own output so pretty dodgey but good example of how it works.

realOut input = show (calc (alexScanTokens input)) --hack to get around problem with do need to fix

test2 = TestCase ( do
            input <- readFile "tests/input/fact1.txt"
            expected <- readFile "tests/expected/fact1.txt"
            assertEqual "Factorial program from assignment pdf: " (realOut input ++ "\n") (expected))



parserTests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

