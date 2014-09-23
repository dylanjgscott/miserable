-- Main company source file.  

import Data.Char
import System.Environment

import AsmLexer
import AsmParser

-- 
main :: IO ()
main  
 = do   -- Use the first command-line argument as the file name.
        [fileName]      <- getArgs

        -- Read in the source file.
        source          <- readFile fileName

  

        -- Lex, parse and print result to the console,
        -- use the alex generated lexer.
        putStr (show (asmParse (alexScanTokens source)))
