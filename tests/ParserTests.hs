module ParserTests where


import Parser
import Lexer
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
-- | Helper functions

-- Output function
realOut input = show (calc (alexScanTokens input)) 

-- Strip newlines from input file
filt input = filter (/= '\n') input




-- HUnit does not have an 'assertException or simmilar so need to make one
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
    where isWanted = guard . (== ex)


-- Need an instance decleration so I can pass a String in for the Error type
instance Eq ErrorCall where
    x == y = (show x) == (show y)


--Define Tests


test0 = TestCase (do
            input <- readFile "tests/input/parse0.txt"
            expected <- readFile "tests/expected/parse_out0.txt"
            assertEqual "Empty Program: " (filt(realOut input)) (filt expected))

-- Syntax error
test1 = TestCase (do
            input <- readFile "tests/input/parse1.txt"
            expected <- readFile "tests/expected/parse_out1.txt"
            assertException (ErrorCall "Syntax Error.") (print (calc (alexScanTokens input))))
--            assertRaises "Syntax Error" (calc (alexScanTokens input)))

test2 = TestCase ( do
            input <- readFile "tests/input/fact1.txt"
            expected <- readFile "tests/expected/fact1.txt"
            assertEqual "Factorial program from assignment pdf: " (filt(realOut input)) (filt expected))



parserTests = TestList [TestLabel "test0" test0, TestLabel "test1" test1, TestLabel "test2" test2]

