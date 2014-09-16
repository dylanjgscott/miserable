module LexerTests where

import Lexer
import Test.HUnit

test1 = TestCase(assertEqual "True /= False"  True True)

lexerTests = TestList [TestLabel "test1" test1]
