----------------------------------------------
-- Module for Helper functions common to all 
-- test files
--
---------------------------------------------

module TestLib where


-- Package files
import Lexer
import Parser
--- Third party Libraries
import Test.HUnit
import Control.Exception
import Control.Monad


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


