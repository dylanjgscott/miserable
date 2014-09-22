-- Main miserable source file.  
-- Compiling this file to make sure Parser and Lexer work


--import miserable_parser
import Data.Char
import System.Environment

import Parser
import Lexer
import Token
import Generator
import Semantic


-- 
main :: IO ()
main  
 = do   -- Use the first command-line argument as the file name.
        [fileName]      <- getArgs

        -- Read in the source file.
        source          <- readFile fileName
        --print(semanticCheck(calc (alexScanTokens source)))

        -- Lex, parse and print result to the console,
        -- use the alex generated lexer.
		prog <- (calc(alexScanToken source))
		-- Check semantics of program
		if (senanticCheck prog)
			then putStr(ShowProgram(genProgram(prog)))
			else exitFailure
		--putStr (showProgram (genProgram (calc (alexScanTokens source))))
        --print (calc (alexScanTokens source))

