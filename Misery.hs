-- Main miserable source file.  
-- Compiling this file to make sure Parser and Lexer work


--import miserable_parser
import Data.Char
import System.Environment

import Parser
import Lexer
import Token
import Generator

-- 
main :: IO ()
main  
 = do   -- Use the first command-line argument as the file name.
        [fileName]      <- getArgs

        -- Read in the source file.
        source          <- readFile fileName

  

        -- Lex, parse and print result to the console,
        -- use the alex generated lexer.
        putStr (showProgram (genProgram (calc (alexScanTokens source))))
